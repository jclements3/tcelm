/*
 * tcelm_socket.c - TCP/UDP Socket implementation
 *
 * BSD socket operations for RTEMS and POSIX systems.
 */

#include "tcelm_socket.h"
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#ifdef __rtems__
#include <rtems.h>
#include <rtems/rtems_bsdnet.h>
#else
#include <poll.h>
#endif

#define MAX_SOCKET_CALLBACKS 32

/* Callback registration */
typedef struct {
    int fd;
    bool is_server;                    /* true = accept callback, false = data callback */
    tcelm_socket_accept_cb accept_cb;
    tcelm_socket_data_cb data_cb;
    void *user_data;
    bool active;
} socket_callback_t;

static socket_callback_t socket_callbacks[MAX_SOCKET_CALLBACKS];
static tcelm_socket_error_t last_error = TCELM_SOCK_OK;
static bool socket_initialized = false;

int tcelm_socket_init(void) {
    if (socket_initialized) return 0;

    memset(socket_callbacks, 0, sizeof(socket_callbacks));

#ifdef __rtems__
    /* RTEMS: Network stack should already be initialized by BSP */
    /* rtems_bsdnet_initialize_network() called during system startup */
#endif

    socket_initialized = true;
    return 0;
}

void tcelm_socket_shutdown(void) {
    /* Close all registered sockets */
    for (int i = 0; i < MAX_SOCKET_CALLBACKS; i++) {
        if (socket_callbacks[i].active && socket_callbacks[i].fd >= 0) {
            close(socket_callbacks[i].fd);
        }
    }
    memset(socket_callbacks, 0, sizeof(socket_callbacks));
    socket_initialized = false;
}

tcelm_socket_error_t tcelm_socket_error_from_errno(int err) {
    switch (err) {
        case ECONNREFUSED:
            return TCELM_SOCK_ERR_REFUSED;
        case ECONNRESET:
            return TCELM_SOCK_ERR_RESET;
        case ETIMEDOUT:
            return TCELM_SOCK_ERR_TIMEOUT;
        case EADDRINUSE:
            return TCELM_SOCK_ERR_ADDR_IN_USE;
        case EADDRNOTAVAIL:
            return TCELM_SOCK_ERR_ADDR_UNAVAIL;
        case ENETUNREACH:
            return TCELM_SOCK_ERR_NET_UNREACH;
        case EHOSTUNREACH:
            return TCELM_SOCK_ERR_HOST_UNREACH;
        case EAGAIN:
#if EAGAIN != EWOULDBLOCK
        case EWOULDBLOCK:
#endif
            return TCELM_SOCK_ERR_WOULD_BLOCK;
        case EINVAL:
            return TCELM_SOCK_ERR_INVALID;
        default:
            return TCELM_SOCK_ERR_UNKNOWN;
    }
}

tcelm_socket_error_t tcelm_socket_last_error(void) {
    return last_error;
}

static void set_error(int err) {
    last_error = tcelm_socket_error_from_errno(err);
}

/* Helper: Create sockaddr_in from IP and port */
static int make_sockaddr(struct sockaddr_in *addr, const char *ip, uint16_t port) {
    memset(addr, 0, sizeof(*addr));
    addr->sin_family = AF_INET;
    addr->sin_port = htons(port);

    if (ip == NULL || strcmp(ip, "0.0.0.0") == 0) {
        addr->sin_addr.s_addr = INADDR_ANY;
    } else if (strcmp(ip, "127.0.0.1") == 0) {
        addr->sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    } else {
        if (inet_pton(AF_INET, ip, &addr->sin_addr) <= 0) {
            /* Try hostname resolution */
            struct hostent *he = gethostbyname(ip);
            if (he == NULL) {
                return -1;
            }
            memcpy(&addr->sin_addr, he->h_addr_list[0], he->h_length);
        }
    }
    return 0;
}

/*
 * TCP Operations
 */

int tcelm_socket_tcp_connect(const char *ip, uint16_t port) {
    int fd = socket(AF_INET, SOCK_STREAM, 0);
    if (fd < 0) {
        set_error(errno);
        return -1;
    }

    struct sockaddr_in addr;
    if (make_sockaddr(&addr, ip, port) < 0) {
        close(fd);
        last_error = TCELM_SOCK_ERR_INVALID;
        return -1;
    }

    if (connect(fd, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
        set_error(errno);
        close(fd);
        return -1;
    }

    last_error = TCELM_SOCK_OK;
    return fd;
}

int tcelm_socket_tcp_listen(const char *ip, uint16_t port, int backlog) {
    int fd = socket(AF_INET, SOCK_STREAM, 0);
    if (fd < 0) {
        set_error(errno);
        return -1;
    }

    /* Enable address reuse */
    int opt = 1;
    setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));

    struct sockaddr_in addr;
    if (make_sockaddr(&addr, ip, port) < 0) {
        close(fd);
        last_error = TCELM_SOCK_ERR_INVALID;
        return -1;
    }

    if (bind(fd, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
        set_error(errno);
        close(fd);
        return -1;
    }

    if (listen(fd, backlog > 0 ? backlog : 5) < 0) {
        set_error(errno);
        close(fd);
        return -1;
    }

    last_error = TCELM_SOCK_OK;
    return fd;
}

int tcelm_socket_tcp_accept(int server_fd) {
    struct sockaddr_in client_addr;
    socklen_t addr_len = sizeof(client_addr);

    int client_fd = accept(server_fd, (struct sockaddr *)&client_addr, &addr_len);
    if (client_fd < 0) {
        set_error(errno);
        return -1;
    }

    last_error = TCELM_SOCK_OK;
    return client_fd;
}

/*
 * UDP Operations
 */

int tcelm_socket_udp_open(void) {
    int fd = socket(AF_INET, SOCK_DGRAM, 0);
    if (fd < 0) {
        set_error(errno);
        return -1;
    }

    last_error = TCELM_SOCK_OK;
    return fd;
}

int tcelm_socket_udp_bind(int fd, const char *ip, uint16_t port) {
    struct sockaddr_in addr;
    if (make_sockaddr(&addr, ip, port) < 0) {
        last_error = TCELM_SOCK_ERR_INVALID;
        return -1;
    }

    if (bind(fd, (struct sockaddr *)&addr, sizeof(addr)) < 0) {
        set_error(errno);
        return -1;
    }

    last_error = TCELM_SOCK_OK;
    return 0;
}

/*
 * Data Transfer
 */

int tcelm_socket_send(int fd, const char *data, size_t len) {
    ssize_t sent = send(fd, data, len, 0);
    if (sent < 0) {
        set_error(errno);
        return -1;
    }

    last_error = TCELM_SOCK_OK;
    return (int)sent;
}

int tcelm_socket_sendto(int fd, const char *ip, uint16_t port,
                        const char *data, size_t len) {
    struct sockaddr_in addr;
    if (make_sockaddr(&addr, ip, port) < 0) {
        last_error = TCELM_SOCK_ERR_INVALID;
        return -1;
    }

    ssize_t sent = sendto(fd, data, len, 0,
                          (struct sockaddr *)&addr, sizeof(addr));
    if (sent < 0) {
        set_error(errno);
        return -1;
    }

    last_error = TCELM_SOCK_OK;
    return (int)sent;
}

int tcelm_socket_recv(int fd, char *buffer, size_t max_len) {
    ssize_t received = recv(fd, buffer, max_len, 0);
    if (received < 0) {
        set_error(errno);
        return -1;
    }

    /* Null-terminate for string safety */
    if ((size_t)received < max_len) {
        buffer[received] = '\0';
    }

    last_error = TCELM_SOCK_OK;
    return (int)received;
}

int tcelm_socket_recvfrom(int fd, char *buffer, size_t max_len,
                          tcelm_socket_addr_t *sender) {
    struct sockaddr_in addr;
    socklen_t addr_len = sizeof(addr);

    ssize_t received = recvfrom(fd, buffer, max_len, 0,
                                (struct sockaddr *)&addr, &addr_len);
    if (received < 0) {
        set_error(errno);
        return -1;
    }

    /* Null-terminate for string safety */
    if ((size_t)received < max_len) {
        buffer[received] = '\0';
    }

    /* Fill in sender info */
    if (sender) {
        inet_ntop(AF_INET, &addr.sin_addr, sender->ip, sizeof(sender->ip));
        sender->port = ntohs(addr.sin_port);
    }

    last_error = TCELM_SOCK_OK;
    return (int)received;
}

/*
 * Socket Control
 */

int tcelm_socket_close(int fd) {
    /* Remove from callback list */
    for (int i = 0; i < MAX_SOCKET_CALLBACKS; i++) {
        if (socket_callbacks[i].active && socket_callbacks[i].fd == fd) {
            socket_callbacks[i].active = false;
        }
    }

    if (close(fd) < 0) {
        set_error(errno);
        return -1;
    }

    last_error = TCELM_SOCK_OK;
    return 0;
}

int tcelm_socket_shutdown(int fd, tcelm_shutdown_mode_t mode) {
    int how;
    switch (mode) {
        case TCELM_SHUT_RD:
            how = SHUT_RD;
            break;
        case TCELM_SHUT_WR:
            how = SHUT_WR;
            break;
        case TCELM_SHUT_RDWR:
        default:
            how = SHUT_RDWR;
            break;
    }

    if (shutdown(fd, how) < 0) {
        set_error(errno);
        return -1;
    }

    last_error = TCELM_SOCK_OK;
    return 0;
}

int tcelm_socket_set_option(int fd, tcelm_socket_option_t option, int value) {
    int level = SOL_SOCKET;
    int optname;
    struct timeval tv;

    switch (option) {
        case TCELM_SOCKOPT_REUSEADDR:
            optname = SO_REUSEADDR;
            break;
        case TCELM_SOCKOPT_KEEPALIVE:
            optname = SO_KEEPALIVE;
            break;
        case TCELM_SOCKOPT_NODELAY:
            level = IPPROTO_TCP;
            optname = TCP_NODELAY;
            break;
        case TCELM_SOCKOPT_BROADCAST:
            optname = SO_BROADCAST;
            break;
        case TCELM_SOCKOPT_RCVTIMEO:
            tv.tv_sec = value / 1000;
            tv.tv_usec = (value % 1000) * 1000;
            if (setsockopt(fd, SOL_SOCKET, SO_RCVTIMEO, &tv, sizeof(tv)) < 0) {
                set_error(errno);
                return -1;
            }
            last_error = TCELM_SOCK_OK;
            return 0;
        case TCELM_SOCKOPT_SNDTIMEO:
            tv.tv_sec = value / 1000;
            tv.tv_usec = (value % 1000) * 1000;
            if (setsockopt(fd, SOL_SOCKET, SO_SNDTIMEO, &tv, sizeof(tv)) < 0) {
                set_error(errno);
                return -1;
            }
            last_error = TCELM_SOCK_OK;
            return 0;
        case TCELM_SOCKOPT_RCVBUF:
            optname = SO_RCVBUF;
            break;
        case TCELM_SOCKOPT_SNDBUF:
            optname = SO_SNDBUF;
            break;
        default:
            last_error = TCELM_SOCK_ERR_INVALID;
            return -1;
    }

    if (setsockopt(fd, level, optname, &value, sizeof(value)) < 0) {
        set_error(errno);
        return -1;
    }

    last_error = TCELM_SOCK_OK;
    return 0;
}

int tcelm_socket_set_nonblocking(int fd, bool nonblocking) {
#ifdef __rtems__
    int flags = fcntl(fd, F_GETFL, 0);
    if (flags < 0) {
        set_error(errno);
        return -1;
    }

    if (nonblocking) {
        flags |= O_NONBLOCK;
    } else {
        flags &= ~O_NONBLOCK;
    }

    if (fcntl(fd, F_SETFL, flags) < 0) {
        set_error(errno);
        return -1;
    }
#else
    int flags = fcntl(fd, F_GETFL, 0);
    if (flags < 0) {
        set_error(errno);
        return -1;
    }

    if (nonblocking) {
        flags |= O_NONBLOCK;
    } else {
        flags &= ~O_NONBLOCK;
    }

    if (fcntl(fd, F_SETFL, flags) < 0) {
        set_error(errno);
        return -1;
    }
#endif

    last_error = TCELM_SOCK_OK;
    return 0;
}

/*
 * Async/Subscription Support
 */

int tcelm_socket_on_connection(int server_fd,
                               tcelm_socket_accept_cb callback,
                               void *user_data) {
    /* Find free slot */
    for (int i = 0; i < MAX_SOCKET_CALLBACKS; i++) {
        if (!socket_callbacks[i].active) {
            socket_callbacks[i].fd = server_fd;
            socket_callbacks[i].is_server = true;
            socket_callbacks[i].accept_cb = callback;
            socket_callbacks[i].data_cb = NULL;
            socket_callbacks[i].user_data = user_data;
            socket_callbacks[i].active = true;

            /* Set non-blocking for event loop */
            tcelm_socket_set_nonblocking(server_fd, true);
            return 0;
        }
    }

    last_error = TCELM_SOCK_ERR_INVALID;
    return -1;
}

int tcelm_socket_on_data(int fd,
                         tcelm_socket_data_cb callback,
                         void *user_data) {
    /* Find free slot */
    for (int i = 0; i < MAX_SOCKET_CALLBACKS; i++) {
        if (!socket_callbacks[i].active) {
            socket_callbacks[i].fd = fd;
            socket_callbacks[i].is_server = false;
            socket_callbacks[i].accept_cb = NULL;
            socket_callbacks[i].data_cb = callback;
            socket_callbacks[i].user_data = user_data;
            socket_callbacks[i].active = true;

            /* Set non-blocking for event loop */
            tcelm_socket_set_nonblocking(fd, true);
            return 0;
        }
    }

    last_error = TCELM_SOCK_ERR_INVALID;
    return -1;
}

int tcelm_socket_poll(int timeout_ms) {
    /* Count active sockets */
    int count = 0;
    for (int i = 0; i < MAX_SOCKET_CALLBACKS; i++) {
        if (socket_callbacks[i].active) {
            count++;
        }
    }

    if (count == 0) {
        return 0;
    }

    /* Build pollfd array */
    struct pollfd *fds = malloc(count * sizeof(struct pollfd));
    int *indices = malloc(count * sizeof(int));
    if (!fds || !indices) {
        free(fds);
        free(indices);
        return -1;
    }

    int j = 0;
    for (int i = 0; i < MAX_SOCKET_CALLBACKS; i++) {
        if (socket_callbacks[i].active) {
            fds[j].fd = socket_callbacks[i].fd;
            fds[j].events = POLLIN;
            fds[j].revents = 0;
            indices[j] = i;
            j++;
        }
    }

    /* Poll */
    int ready = poll(fds, count, timeout_ms);
    if (ready < 0) {
        set_error(errno);
        free(fds);
        free(indices);
        return -1;
    }

    /* Process ready sockets */
    for (int i = 0; i < count && ready > 0; i++) {
        if (fds[i].revents & POLLIN) {
            ready--;
            socket_callback_t *cb = &socket_callbacks[indices[i]];

            if (cb->is_server && cb->accept_cb) {
                /* Accept new connection */
                int client_fd = tcelm_socket_tcp_accept(cb->fd);
                if (client_fd >= 0) {
                    cb->accept_cb(client_fd, cb->user_data);
                }
            } else if (!cb->is_server && cb->data_cb) {
                /* Read data */
                char buffer[4096];
                int len = tcelm_socket_recv(cb->fd, buffer, sizeof(buffer) - 1);
                if (len > 0) {
                    buffer[len] = '\0';
                    cb->data_cb(buffer, len, cb->user_data);
                } else if (len == 0) {
                    /* Connection closed */
                    cb->active = false;
                }
            }
        }

        if (fds[i].revents & (POLLERR | POLLHUP | POLLNVAL)) {
            /* Socket error - deactivate */
            socket_callbacks[indices[i]].active = false;
        }
    }

    free(fds);
    free(indices);

    last_error = TCELM_SOCK_OK;
    return ready;
}

/*
 * DNS Resolution
 */

int tcelm_socket_resolve(const char *hostname, char *ip_out, size_t ip_len) {
    struct hostent *he = gethostbyname(hostname);
    if (he == NULL) {
        last_error = TCELM_SOCK_ERR_HOST_UNREACH;
        return -1;
    }

    struct in_addr **addr_list = (struct in_addr **)he->h_addr_list;
    if (addr_list[0] != NULL) {
        inet_ntop(AF_INET, addr_list[0], ip_out, ip_len);
        last_error = TCELM_SOCK_OK;
        return 0;
    }

    last_error = TCELM_SOCK_ERR_HOST_UNREACH;
    return -1;
}
