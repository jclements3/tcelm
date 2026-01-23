/*
 * tcelm_socket.h - TCP/UDP Socket API for RTEMS
 *
 * Provides BSD-style socket operations for network I/O.
 * Works with both RTEMS network stack and POSIX systems.
 */

#ifndef TCELM_SOCKET_H
#define TCELM_SOCKET_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

#ifdef __rtems__
#include <rtems.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <netdb.h>
#else
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>
#endif

/*
 * Socket error codes
 */
typedef enum {
    TCELM_SOCK_OK = 0,
    TCELM_SOCK_ERR_REFUSED = 1,      /* Connection refused */
    TCELM_SOCK_ERR_RESET = 2,        /* Connection reset */
    TCELM_SOCK_ERR_TIMEOUT = 3,      /* Operation timed out */
    TCELM_SOCK_ERR_ADDR_IN_USE = 4,  /* Address already in use */
    TCELM_SOCK_ERR_ADDR_UNAVAIL = 5, /* Address not available */
    TCELM_SOCK_ERR_NET_UNREACH = 6,  /* Network unreachable */
    TCELM_SOCK_ERR_HOST_UNREACH = 7, /* Host unreachable */
    TCELM_SOCK_ERR_WOULD_BLOCK = 8,  /* Would block (non-blocking mode) */
    TCELM_SOCK_ERR_INVALID = 9,      /* Invalid argument */
    TCELM_SOCK_ERR_UNKNOWN = 99      /* Unknown error */
} tcelm_socket_error_t;

/*
 * Socket option types
 */
typedef enum {
    TCELM_SOCKOPT_REUSEADDR = 0,
    TCELM_SOCKOPT_KEEPALIVE = 1,
    TCELM_SOCKOPT_NODELAY = 2,
    TCELM_SOCKOPT_BROADCAST = 3,
    TCELM_SOCKOPT_RCVTIMEO = 4,
    TCELM_SOCKOPT_SNDTIMEO = 5,
    TCELM_SOCKOPT_RCVBUF = 6,
    TCELM_SOCKOPT_SNDBUF = 7
} tcelm_socket_option_t;

/*
 * Shutdown modes
 */
typedef enum {
    TCELM_SHUT_RD = 0,   /* No more receives */
    TCELM_SHUT_WR = 1,   /* No more sends */
    TCELM_SHUT_RDWR = 2  /* No more I/O */
} tcelm_shutdown_mode_t;

/*
 * Address info for UDP receiveFrom
 */
typedef struct {
    char ip[46];       /* IPv4 or IPv6 string */
    uint16_t port;
} tcelm_socket_addr_t;

/*
 * Callback types for async operations
 */
typedef void (*tcelm_socket_accept_cb)(int client_fd, void *user_data);
typedef void (*tcelm_socket_data_cb)(const char *data, size_t len, void *user_data);

/*
 * Initialize socket subsystem
 */
int tcelm_socket_init(void);

/*
 * Shutdown socket subsystem
 */
void tcelm_socket_shutdown(void);

/*
 * TCP Operations
 */

/*
 * Connect to a TCP server
 * Returns: socket fd on success, -1 on error (check tcelm_socket_last_error)
 */
int tcelm_socket_tcp_connect(const char *ip, uint16_t port);

/*
 * Create a TCP listening socket
 * Returns: socket fd on success, -1 on error
 */
int tcelm_socket_tcp_listen(const char *ip, uint16_t port, int backlog);

/*
 * Accept an incoming TCP connection
 * Returns: client socket fd on success, -1 on error
 */
int tcelm_socket_tcp_accept(int server_fd);

/*
 * UDP Operations
 */

/*
 * Open a UDP socket
 * Returns: socket fd on success, -1 on error
 */
int tcelm_socket_udp_open(void);

/*
 * Bind a UDP socket to an address
 */
int tcelm_socket_udp_bind(int fd, const char *ip, uint16_t port);

/*
 * Data Transfer
 */

/*
 * Send data on a connected socket
 * Returns: bytes sent on success, -1 on error
 */
int tcelm_socket_send(int fd, const char *data, size_t len);

/*
 * Send data to a specific address (UDP)
 * Returns: bytes sent on success, -1 on error
 */
int tcelm_socket_sendto(int fd, const char *ip, uint16_t port,
                        const char *data, size_t len);

/*
 * Receive data from a socket
 * Returns: bytes received on success, 0 on connection closed, -1 on error
 */
int tcelm_socket_recv(int fd, char *buffer, size_t max_len);

/*
 * Receive data with sender address (UDP)
 * Returns: bytes received on success, -1 on error
 */
int tcelm_socket_recvfrom(int fd, char *buffer, size_t max_len,
                          tcelm_socket_addr_t *sender);

/*
 * Socket Control
 */

/*
 * Close a socket
 */
int tcelm_socket_close(int fd);

/*
 * Shutdown part of a connection
 */
int tcelm_socket_shutdown(int fd, tcelm_shutdown_mode_t mode);

/*
 * Set a socket option
 */
int tcelm_socket_set_option(int fd, tcelm_socket_option_t option, int value);

/*
 * Set socket to non-blocking mode
 */
int tcelm_socket_set_nonblocking(int fd, bool nonblocking);

/*
 * Async/Subscription Support
 */

/*
 * Register callback for incoming connections
 * Called from event loop when new connection arrives
 */
int tcelm_socket_on_connection(int server_fd,
                               tcelm_socket_accept_cb callback,
                               void *user_data);

/*
 * Register callback for incoming data
 * Called from event loop when data arrives
 */
int tcelm_socket_on_data(int fd,
                         tcelm_socket_data_cb callback,
                         void *user_data);

/*
 * Poll all registered sockets (call from event loop)
 * timeout_ms: -1 = block forever, 0 = non-blocking, >0 = timeout
 */
int tcelm_socket_poll(int timeout_ms);

/*
 * Get last error code
 */
tcelm_socket_error_t tcelm_socket_last_error(void);

/*
 * Convert system errno to socket error
 */
tcelm_socket_error_t tcelm_socket_error_from_errno(int err);

/*
 * DNS Resolution
 */

/*
 * Resolve hostname to IP address
 * Returns: 0 on success, -1 on error
 */
int tcelm_socket_resolve(const char *hostname, char *ip_out, size_t ip_len);

#endif /* TCELM_SOCKET_H */
