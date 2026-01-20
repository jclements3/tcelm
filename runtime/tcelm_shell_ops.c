/*
 * tcelm_shell_ops.c - Shell operations implementation
 *
 * Turtle/Shelly-style shell scripting for tcelm on RTEMS and POSIX.
 */

#include "tcelm_shell_ops.h"
#include "tcelm_basics.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <dirent.h>
#include <unistd.h>
#include <time.h>
#include <fcntl.h>
#include <errno.h>

#ifdef __rtems__
#include <rtems.h>
#include <rtems/libio.h>
#else
/* POSIX */
#include <sys/wait.h>
#include <fnmatch.h>
#endif

/*
 * ============================================================================
 * PATH OPERATIONS
 * ============================================================================
 */

tcelm_value_t *tcelm_path_join(tcelm_arena_t *arena, tcelm_value_t *a, tcelm_value_t *b) {
    const char *pa = TCELM_AS_STRING(a)->data;
    const char *pb = TCELM_AS_STRING(b)->data;
    size_t la = strlen(pa);
    size_t lb = strlen(pb);

    /* Check if a already ends with / */
    bool has_sep = (la > 0 && pa[la - 1] == '/');

    size_t len = la + lb + (has_sep ? 0 : 1);
    char *buf = tcelm_arena_alloc(arena, len + 1);

    if (has_sep) {
        snprintf(buf, len + 1, "%s%s", pa, pb);
    } else {
        snprintf(buf, len + 1, "%s/%s", pa, pb);
    }

    return tcelm_string(arena, buf);
}

tcelm_value_t *tcelm_path_parent(tcelm_arena_t *arena, tcelm_value_t *path) {
    const char *p = TCELM_AS_STRING(path)->data;
    size_t len = strlen(p);

    /* Find last / */
    const char *last_sep = strrchr(p, '/');
    if (!last_sep || last_sep == p) {
        return tcelm_string(arena, ".");
    }

    size_t parent_len = last_sep - p;
    char *buf = tcelm_arena_alloc(arena, parent_len + 1);
    memcpy(buf, p, parent_len);
    buf[parent_len] = '\0';

    return tcelm_string(arena, buf);
}

tcelm_value_t *tcelm_path_filename(tcelm_arena_t *arena, tcelm_value_t *path) {
    const char *p = TCELM_AS_STRING(path)->data;
    const char *last_sep = strrchr(p, '/');

    if (last_sep) {
        return tcelm_string(arena, last_sep + 1);
    }
    return path;
}

tcelm_value_t *tcelm_path_extension(tcelm_arena_t *arena, tcelm_value_t *path) {
    const char *p = TCELM_AS_STRING(path)->data;
    const char *last_dot = strrchr(p, '.');
    const char *last_sep = strrchr(p, '/');

    /* Extension must be after last separator */
    if (last_dot && (!last_sep || last_dot > last_sep)) {
        return tcelm_just(arena, tcelm_string(arena, last_dot + 1));
    }
    return tcelm_nothing(arena);
}

tcelm_value_t *tcelm_path_drop_extension(tcelm_arena_t *arena, tcelm_value_t *path) {
    const char *p = TCELM_AS_STRING(path)->data;
    const char *last_dot = strrchr(p, '.');
    const char *last_sep = strrchr(p, '/');

    if (last_dot && (!last_sep || last_dot > last_sep)) {
        size_t len = last_dot - p;
        char *buf = tcelm_arena_alloc(arena, len + 1);
        memcpy(buf, p, len);
        buf[len] = '\0';
        return tcelm_string(arena, buf);
    }
    return path;
}

tcelm_value_t *tcelm_path_add_extension(tcelm_arena_t *arena, tcelm_value_t *path, tcelm_value_t *ext) {
    const char *p = TCELM_AS_STRING(path)->data;
    const char *e = TCELM_AS_STRING(ext)->data;
    size_t lp = strlen(p);
    size_t le = strlen(e);

    char *buf = tcelm_arena_alloc(arena, lp + le + 2);
    snprintf(buf, lp + le + 2, "%s.%s", p, e);
    return tcelm_string(arena, buf);
}

/*
 * ============================================================================
 * DIRECTORY OPERATIONS
 * ============================================================================
 */

tcelm_value_t *tcelm_shell_pwd(tcelm_arena_t *arena) {
    char buf[PATH_MAX];
    if (getcwd(buf, sizeof(buf))) {
        return tcelm_string(arena, buf);
    }
    return tcelm_string(arena, ".");
}

tcelm_value_t *tcelm_shell_cd(tcelm_arena_t *arena, tcelm_value_t *path) {
    const char *p = TCELM_AS_STRING(path)->data;
    if (chdir(p) == 0) {
        return tcelm_ok(arena, TCELM_UNIT);
    }
    return tcelm_err(arena, tcelm_string(arena, strerror(errno)));
}

tcelm_value_t *tcelm_shell_home(tcelm_arena_t *arena) {
    const char *home = getenv("HOME");
    if (home) {
        return tcelm_string(arena, home);
    }
    return tcelm_string(arena, "/");
}

tcelm_value_t *tcelm_shell_ls(tcelm_arena_t *arena, tcelm_value_t *path) {
    const char *p = TCELM_AS_STRING(path)->data;
    DIR *dir = opendir(p);

    if (!dir) {
        return TCELM_NIL;
    }

    tcelm_value_t *result = TCELM_NIL;
    struct dirent *entry;

    while ((entry = readdir(dir)) != NULL) {
        /* Skip . and .. */
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0) {
            continue;
        }

        /* Build full path */
        size_t plen = strlen(p);
        size_t nlen = strlen(entry->d_name);
        char *full = tcelm_arena_alloc(arena, plen + nlen + 2);
        snprintf(full, plen + nlen + 2, "%s/%s", p, entry->d_name);

        result = tcelm_cons(arena, tcelm_string(arena, full), result);
    }

    closedir(dir);

    /* Reverse to get sorted order */
    return tcelm_list_reverse(arena, result);
}

/* Helper for recursive directory listing */
static void lstree_helper(tcelm_arena_t *arena, const char *path, tcelm_value_t **result) {
    DIR *dir = opendir(path);
    if (!dir) return;

    struct dirent *entry;
    while ((entry = readdir(dir)) != NULL) {
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0) {
            continue;
        }

        size_t plen = strlen(path);
        size_t nlen = strlen(entry->d_name);
        char *full = tcelm_arena_alloc(arena, plen + nlen + 2);
        snprintf(full, plen + nlen + 2, "%s/%s", path, entry->d_name);

        *result = tcelm_cons(arena, tcelm_string(arena, full), *result);

        /* Recurse into subdirectories */
        struct stat st;
        if (stat(full, &st) == 0 && S_ISDIR(st.st_mode)) {
            lstree_helper(arena, full, result);
        }
    }

    closedir(dir);
}

tcelm_value_t *tcelm_shell_lstree(tcelm_arena_t *arena, tcelm_value_t *path) {
    const char *p = TCELM_AS_STRING(path)->data;
    tcelm_value_t *result = TCELM_NIL;
    lstree_helper(arena, p, &result);
    return tcelm_list_reverse(arena, result);
}

tcelm_value_t *tcelm_shell_mkdir(tcelm_arena_t *arena, tcelm_value_t *path) {
    const char *p = TCELM_AS_STRING(path)->data;
    if (mkdir(p, 0755) == 0) {
        return tcelm_ok(arena, TCELM_UNIT);
    }
    return tcelm_err(arena, tcelm_string(arena, strerror(errno)));
}

tcelm_value_t *tcelm_shell_mkdirtree(tcelm_arena_t *arena, tcelm_value_t *path) {
    const char *p = TCELM_AS_STRING(path)->data;
    char *tmp = tcelm_arena_alloc(arena, strlen(p) + 1);
    strcpy(tmp, p);

    for (char *s = tmp + 1; *s; s++) {
        if (*s == '/') {
            *s = '\0';
            mkdir(tmp, 0755);  /* Ignore errors for intermediate dirs */
            *s = '/';
        }
    }

    if (mkdir(tmp, 0755) == 0 || errno == EEXIST) {
        return tcelm_ok(arena, TCELM_UNIT);
    }
    return tcelm_err(arena, tcelm_string(arena, strerror(errno)));
}

tcelm_value_t *tcelm_shell_rmdir(tcelm_arena_t *arena, tcelm_value_t *path) {
    const char *p = TCELM_AS_STRING(path)->data;
    if (rmdir(p) == 0) {
        return tcelm_ok(arena, TCELM_UNIT);
    }
    return tcelm_err(arena, tcelm_string(arena, strerror(errno)));
}

/* Helper for recursive removal */
static int rmtree_helper(const char *path) {
    struct stat st;
    if (lstat(path, &st) != 0) {
        return -1;
    }

    if (S_ISDIR(st.st_mode)) {
        DIR *dir = opendir(path);
        if (!dir) return -1;

        struct dirent *entry;
        while ((entry = readdir(dir)) != NULL) {
            if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0) {
                continue;
            }

            char full[PATH_MAX];
            snprintf(full, sizeof(full), "%s/%s", path, entry->d_name);
            rmtree_helper(full);
        }

        closedir(dir);
        return rmdir(path);
    } else {
        return unlink(path);
    }
}

tcelm_value_t *tcelm_shell_rmtree(tcelm_arena_t *arena, tcelm_value_t *path) {
    const char *p = TCELM_AS_STRING(path)->data;
    if (rmtree_helper(p) == 0) {
        return tcelm_ok(arena, TCELM_UNIT);
    }
    return tcelm_err(arena, tcelm_string(arena, strerror(errno)));
}

/*
 * ============================================================================
 * FILE OPERATIONS
 * ============================================================================
 */

tcelm_value_t *tcelm_shell_touch(tcelm_arena_t *arena, tcelm_value_t *path) {
    const char *p = TCELM_AS_STRING(path)->data;

    /* Try to update timestamp first */
    if (utimes(p, NULL) == 0) {
        return tcelm_ok(arena, TCELM_UNIT);
    }

    /* Create file if it doesn't exist */
    int fd = open(p, O_CREAT | O_WRONLY, 0644);
    if (fd >= 0) {
        close(fd);
        return tcelm_ok(arena, TCELM_UNIT);
    }

    return tcelm_err(arena, tcelm_string(arena, strerror(errno)));
}

tcelm_value_t *tcelm_shell_cp(tcelm_arena_t *arena, tcelm_value_t *src, tcelm_value_t *dst) {
    const char *sp = TCELM_AS_STRING(src)->data;
    const char *dp = TCELM_AS_STRING(dst)->data;

    FILE *sf = fopen(sp, "rb");
    if (!sf) {
        return tcelm_err(arena, tcelm_string(arena, strerror(errno)));
    }

    FILE *df = fopen(dp, "wb");
    if (!df) {
        fclose(sf);
        return tcelm_err(arena, tcelm_string(arena, strerror(errno)));
    }

    char buf[8192];
    size_t n;
    while ((n = fread(buf, 1, sizeof(buf), sf)) > 0) {
        if (fwrite(buf, 1, n, df) != n) {
            fclose(sf);
            fclose(df);
            return tcelm_err(arena, tcelm_string(arena, "write error"));
        }
    }

    fclose(sf);
    fclose(df);
    return tcelm_ok(arena, TCELM_UNIT);
}

tcelm_value_t *tcelm_shell_mv(tcelm_arena_t *arena, tcelm_value_t *src, tcelm_value_t *dst) {
    const char *sp = TCELM_AS_STRING(src)->data;
    const char *dp = TCELM_AS_STRING(dst)->data;

    if (rename(sp, dp) == 0) {
        return tcelm_ok(arena, TCELM_UNIT);
    }

    /* rename failed, try copy + delete */
    tcelm_value_t *cp_result = tcelm_shell_cp(arena, src, dst);
    if (tcelm_is_ok(cp_result)) {
        unlink(sp);
        return cp_result;
    }

    return tcelm_err(arena, tcelm_string(arena, strerror(errno)));
}

tcelm_value_t *tcelm_shell_rm(tcelm_arena_t *arena, tcelm_value_t *path) {
    const char *p = TCELM_AS_STRING(path)->data;
    if (unlink(p) == 0) {
        return tcelm_ok(arena, TCELM_UNIT);
    }
    return tcelm_err(arena, tcelm_string(arena, strerror(errno)));
}

tcelm_value_t *tcelm_shell_cat(tcelm_arena_t *arena, tcelm_value_t *path) {
    const char *p = TCELM_AS_STRING(path)->data;
    FILE *f = fopen(p, "r");

    if (!f) {
        return TCELM_NIL;
    }

    tcelm_value_t *result = TCELM_NIL;
    char line[4096];

    while (fgets(line, sizeof(line), f)) {
        /* Remove trailing newline */
        size_t len = strlen(line);
        if (len > 0 && line[len - 1] == '\n') {
            line[len - 1] = '\0';
        }
        result = tcelm_cons(arena, tcelm_string(arena, line), result);
    }

    fclose(f);
    return tcelm_list_reverse(arena, result);
}

tcelm_value_t *tcelm_shell_read_file(tcelm_arena_t *arena, tcelm_value_t *path) {
    const char *p = TCELM_AS_STRING(path)->data;
    FILE *f = fopen(p, "rb");

    if (!f) {
        return tcelm_string(arena, "");
    }

    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    fseek(f, 0, SEEK_SET);

    char *buf = tcelm_arena_alloc(arena, size + 1);
    size_t read = fread(buf, 1, size, f);
    buf[read] = '\0';

    fclose(f);
    return tcelm_string(arena, buf);
}

tcelm_value_t *tcelm_shell_write_file(tcelm_arena_t *arena, tcelm_value_t *path, tcelm_value_t *content) {
    const char *p = TCELM_AS_STRING(path)->data;
    const char *c = TCELM_AS_STRING(content)->data;

    FILE *f = fopen(p, "wb");
    if (!f) {
        return tcelm_err(arena, tcelm_string(arena, strerror(errno)));
    }

    size_t len = strlen(c);
    if (fwrite(c, 1, len, f) != len) {
        fclose(f);
        return tcelm_err(arena, tcelm_string(arena, "write error"));
    }

    fclose(f);
    return tcelm_ok(arena, TCELM_UNIT);
}

tcelm_value_t *tcelm_shell_append_file(tcelm_arena_t *arena, tcelm_value_t *path, tcelm_value_t *content) {
    const char *p = TCELM_AS_STRING(path)->data;
    const char *c = TCELM_AS_STRING(content)->data;

    FILE *f = fopen(p, "ab");
    if (!f) {
        return tcelm_err(arena, tcelm_string(arena, strerror(errno)));
    }

    size_t len = strlen(c);
    if (fwrite(c, 1, len, f) != len) {
        fclose(f);
        return tcelm_err(arena, tcelm_string(arena, "write error"));
    }

    fclose(f);
    return tcelm_ok(arena, TCELM_UNIT);
}

tcelm_value_t *tcelm_shell_file_exists(tcelm_arena_t *arena, tcelm_value_t *path) {
    const char *p = TCELM_AS_STRING(path)->data;
    struct stat st;
    return tcelm_bool(arena, stat(p, &st) == 0);
}

tcelm_value_t *tcelm_shell_dir_exists(tcelm_arena_t *arena, tcelm_value_t *path) {
    const char *p = TCELM_AS_STRING(path)->data;
    struct stat st;
    return tcelm_bool(arena, stat(p, &st) == 0 && S_ISDIR(st.st_mode));
}

tcelm_value_t *tcelm_shell_is_file(tcelm_arena_t *arena, tcelm_value_t *path) {
    const char *p = TCELM_AS_STRING(path)->data;
    struct stat st;
    return tcelm_bool(arena, stat(p, &st) == 0 && S_ISREG(st.st_mode));
}

tcelm_value_t *tcelm_shell_is_dir(tcelm_arena_t *arena, tcelm_value_t *path) {
    const char *p = TCELM_AS_STRING(path)->data;
    struct stat st;
    return tcelm_bool(arena, stat(p, &st) == 0 && S_ISDIR(st.st_mode));
}

tcelm_value_t *tcelm_shell_file_size(tcelm_arena_t *arena, tcelm_value_t *path) {
    const char *p = TCELM_AS_STRING(path)->data;
    struct stat st;
    if (stat(p, &st) == 0) {
        return tcelm_int(arena, st.st_size);
    }
    return tcelm_int(arena, -1);
}

/*
 * ============================================================================
 * TEXT PROCESSING
 * ============================================================================
 */

tcelm_value_t *tcelm_shell_stdin(tcelm_arena_t *arena) {
    tcelm_value_t *result = TCELM_NIL;
    char line[4096];

    while (fgets(line, sizeof(line), stdin)) {
        size_t len = strlen(line);
        if (len > 0 && line[len - 1] == '\n') {
            line[len - 1] = '\0';
        }
        result = tcelm_cons(arena, tcelm_string(arena, line), result);
    }

    return tcelm_list_reverse(arena, result);
}

tcelm_value_t *tcelm_shell_stdout(tcelm_arena_t *arena, tcelm_value_t *line) {
    const char *s = TCELM_AS_STRING(line)->data;
    printf("%s\n", s);
    return tcelm_unit(arena);
}

tcelm_value_t *tcelm_shell_stderr(tcelm_arena_t *arena, tcelm_value_t *line) {
    const char *s = TCELM_AS_STRING(line)->data;
    fprintf(stderr, "%s\n", s);
    return tcelm_unit(arena);
}

tcelm_value_t *tcelm_shell_echo(tcelm_arena_t *arena, tcelm_value_t *text) {
    const char *s = TCELM_AS_STRING(text)->data;
    printf("%s\n", s);
    return tcelm_unit(arena);
}

tcelm_value_t *tcelm_shell_printf(tcelm_arena_t *arena, tcelm_value_t *fmt, tcelm_value_t *args) {
    /* Simplified printf - just prints format with %s replacements */
    const char *f = TCELM_AS_STRING(fmt)->data;
    tcelm_value_t *arg_list = args;

    while (*f) {
        if (*f == '%' && *(f + 1) == 's') {
            if (!tcelm_is_nil(arg_list)) {
                tcelm_value_t *arg = tcelm_list_head(arg_list);
                if (TCELM_IS_STRING(arg)) {
                    printf("%s", TCELM_AS_STRING(arg)->data);
                }
                arg_list = tcelm_list_tail(arg_list);
            }
            f += 2;
        } else {
            putchar(*f);
            f++;
        }
    }

    return tcelm_unit(arena);
}

/*
 * ============================================================================
 * PROCESS EXECUTION
 * ============================================================================
 */

#ifndef __rtems__
/* POSIX implementation */

tcelm_value_t *tcelm_shell_proc(tcelm_arena_t *arena, tcelm_value_t *cmd, tcelm_value_t *args) {
    const char *c = TCELM_AS_STRING(cmd)->data;

    /* Count args */
    int argc = 1;
    tcelm_value_t *a = args;
    while (!tcelm_is_nil(a)) {
        argc++;
        a = tcelm_list_tail(a);
    }

    /* Build argv */
    char **argv = tcelm_arena_alloc(arena, (argc + 1) * sizeof(char *));
    argv[0] = (char *)c;
    int i = 1;
    a = args;
    while (!tcelm_is_nil(a)) {
        argv[i++] = (char *)TCELM_AS_STRING(tcelm_list_head(a))->data;
        a = tcelm_list_tail(a);
    }
    argv[argc] = NULL;

    pid_t pid = fork();
    if (pid == 0) {
        execvp(c, argv);
        _exit(127);
    } else if (pid > 0) {
        return tcelm_ok(arena, tcelm_int(arena, pid));
    }

    return tcelm_err(arena, tcelm_string(arena, strerror(errno)));
}

tcelm_value_t *tcelm_shell_shell(tcelm_arena_t *arena, tcelm_value_t *cmd) {
    const char *c = TCELM_AS_STRING(cmd)->data;
    int status = system(c);
    return tcelm_int(arena, WEXITSTATUS(status));
}

tcelm_value_t *tcelm_shell_inproc(tcelm_arena_t *arena, tcelm_value_t *cmd, tcelm_value_t *args, tcelm_value_t *stdin_lines) {
    const char *c = TCELM_AS_STRING(cmd)->data;
    (void)stdin_lines;  /* TODO: pipe stdin */

    /* Build command string */
    size_t len = strlen(c) + 1;
    tcelm_value_t *a = args;
    while (!tcelm_is_nil(a)) {
        len += strlen(TCELM_AS_STRING(tcelm_list_head(a))->data) + 3;
        a = tcelm_list_tail(a);
    }

    char *command = tcelm_arena_alloc(arena, len);
    strcpy(command, c);
    a = args;
    while (!tcelm_is_nil(a)) {
        strcat(command, " '");
        strcat(command, TCELM_AS_STRING(tcelm_list_head(a))->data);
        strcat(command, "'");
        a = tcelm_list_tail(a);
    }

    FILE *fp = popen(command, "r");
    if (!fp) {
        return TCELM_NIL;
    }

    tcelm_value_t *result = TCELM_NIL;
    char line[4096];
    while (fgets(line, sizeof(line), fp)) {
        size_t l = strlen(line);
        if (l > 0 && line[l - 1] == '\n') {
            line[l - 1] = '\0';
        }
        result = tcelm_cons(arena, tcelm_string(arena, line), result);
    }

    pclose(fp);
    return tcelm_list_reverse(arena, result);
}

tcelm_value_t *tcelm_shell_inshell(tcelm_arena_t *arena, tcelm_value_t *cmd, tcelm_value_t *stdin_lines) {
    return tcelm_shell_inproc(arena, cmd, TCELM_NIL, stdin_lines);
}

tcelm_value_t *tcelm_shell_proc_exit(tcelm_arena_t *arena, tcelm_value_t *cmd, tcelm_value_t *args) {
    const char *c = TCELM_AS_STRING(cmd)->data;

    /* Build command string */
    size_t len = strlen(c) + 1;
    tcelm_value_t *a = args;
    while (!tcelm_is_nil(a)) {
        len += strlen(TCELM_AS_STRING(tcelm_list_head(a))->data) + 3;
        a = tcelm_list_tail(a);
    }

    char *command = tcelm_arena_alloc(arena, len);
    strcpy(command, c);
    a = args;
    while (!tcelm_is_nil(a)) {
        strcat(command, " '");
        strcat(command, TCELM_AS_STRING(tcelm_list_head(a))->data);
        strcat(command, "'");
        a = tcelm_list_tail(a);
    }

    int status = system(command);
    return tcelm_int(arena, WEXITSTATUS(status));
}

tcelm_value_t *tcelm_shell_proc_strict(tcelm_arena_t *arena, tcelm_value_t *cmd, tcelm_value_t *args) {
    tcelm_value_t *lines = tcelm_shell_inproc(arena, cmd, args, TCELM_NIL);

    /* Join lines */
    size_t total = 0;
    tcelm_value_t *l = lines;
    while (!tcelm_is_nil(l)) {
        total += TCELM_AS_STRING(tcelm_list_head(l))->length + 1;
        l = tcelm_list_tail(l);
    }

    char *buf = tcelm_arena_alloc(arena, total + 1);
    buf[0] = '\0';
    l = lines;
    while (!tcelm_is_nil(l)) {
        strcat(buf, TCELM_AS_STRING(tcelm_list_head(l))->data);
        strcat(buf, "\n");
        l = tcelm_list_tail(l);
    }

    return tcelm_string(arena, buf);
}

tcelm_value_t *tcelm_shell_proc_strict_with_err(tcelm_arena_t *arena, tcelm_value_t *cmd, tcelm_value_t *args) {
    /* Simplified: only capture stdout, stderr goes to console */
    tcelm_value_t *stdout_str = tcelm_shell_proc_strict(arena, cmd, args);
    tcelm_value_t *exit_code = tcelm_shell_proc_exit(arena, cmd, args);

    return tcelm_record(arena, 3,
        "stdout", stdout_str,
        "stderr", tcelm_string(arena, ""),
        "exitCode", exit_code
    );
}

#else
/* RTEMS implementation - limited process support */

tcelm_value_t *tcelm_shell_proc(tcelm_arena_t *arena, tcelm_value_t *cmd, tcelm_value_t *args) {
    (void)cmd;
    (void)args;
    return tcelm_err(arena, tcelm_string(arena, "process execution not supported on RTEMS"));
}

tcelm_value_t *tcelm_shell_shell(tcelm_arena_t *arena, tcelm_value_t *cmd) {
    (void)cmd;
    return tcelm_err(arena, tcelm_string(arena, "shell commands not supported on RTEMS"));
}

tcelm_value_t *tcelm_shell_inproc(tcelm_arena_t *arena, tcelm_value_t *cmd, tcelm_value_t *args, tcelm_value_t *stdin_lines) {
    (void)cmd;
    (void)args;
    (void)stdin_lines;
    return TCELM_NIL;
}

tcelm_value_t *tcelm_shell_inshell(tcelm_arena_t *arena, tcelm_value_t *cmd, tcelm_value_t *stdin_lines) {
    (void)cmd;
    (void)stdin_lines;
    return TCELM_NIL;
}

tcelm_value_t *tcelm_shell_proc_exit(tcelm_arena_t *arena, tcelm_value_t *cmd, tcelm_value_t *args) {
    (void)cmd;
    (void)args;
    return tcelm_int(arena, -1);
}

tcelm_value_t *tcelm_shell_proc_strict(tcelm_arena_t *arena, tcelm_value_t *cmd, tcelm_value_t *args) {
    (void)cmd;
    (void)args;
    return tcelm_string(arena, "");
}

tcelm_value_t *tcelm_shell_proc_strict_with_err(tcelm_arena_t *arena, tcelm_value_t *cmd, tcelm_value_t *args) {
    (void)cmd;
    (void)args;
    return tcelm_record(arena, 3,
        "stdout", tcelm_string(arena, ""),
        "stderr", tcelm_string(arena, ""),
        "exitCode", tcelm_int(arena, -1)
    );
}

#endif

/*
 * ============================================================================
 * GLOB PATTERNS
 * ============================================================================
 */

/* Simple glob matching */
static bool glob_match_impl(const char *pattern, const char *string) {
    while (*pattern) {
        if (*pattern == '*') {
            pattern++;
            if (*pattern == '*') {
                /* ** matches anything including / */
                pattern++;
                if (*pattern == '\0') return true;
                while (*string) {
                    if (glob_match_impl(pattern, string)) return true;
                    string++;
                }
                return false;
            } else {
                /* * matches anything except / */
                while (*string && *string != '/') {
                    if (glob_match_impl(pattern, string)) return true;
                    string++;
                }
                return glob_match_impl(pattern, string);
            }
        } else if (*pattern == '?') {
            if (*string == '\0' || *string == '/') return false;
            pattern++;
            string++;
        } else if (*pattern == '[') {
            /* Character class */
            pattern++;
            bool invert = false;
            if (*pattern == '!') {
                invert = true;
                pattern++;
            }
            bool matched = false;
            while (*pattern && *pattern != ']') {
                if (*pattern == *string) matched = true;
                pattern++;
            }
            if (*pattern == ']') pattern++;
            if (invert ? matched : !matched) return false;
            string++;
        } else {
            if (*pattern != *string) return false;
            pattern++;
            string++;
        }
    }
    return *string == '\0';
}

tcelm_value_t *tcelm_shell_glob_match(tcelm_arena_t *arena, tcelm_value_t *pattern, tcelm_value_t *path) {
    const char *p = TCELM_AS_STRING(pattern)->data;
    const char *s = TCELM_AS_STRING(path)->data;
    return tcelm_bool(arena, glob_match_impl(p, s));
}

/* Helper for find */
static void find_helper(tcelm_arena_t *arena, const char *pattern, const char *dir, tcelm_value_t **result) {
    DIR *d = opendir(dir);
    if (!d) return;

    struct dirent *entry;
    while ((entry = readdir(d)) != NULL) {
        if (strcmp(entry->d_name, ".") == 0 || strcmp(entry->d_name, "..") == 0) {
            continue;
        }

        char full[PATH_MAX];
        snprintf(full, sizeof(full), "%s/%s", dir, entry->d_name);

        /* Check if filename matches pattern */
        if (glob_match_impl(pattern, entry->d_name)) {
            *result = tcelm_cons(arena, tcelm_string(arena, full), *result);
        }

        /* Recurse into directories */
        struct stat st;
        if (stat(full, &st) == 0 && S_ISDIR(st.st_mode)) {
            find_helper(arena, pattern, full, result);
        }
    }

    closedir(d);
}

tcelm_value_t *tcelm_shell_find(tcelm_arena_t *arena, tcelm_value_t *pattern, tcelm_value_t *dir) {
    const char *p = TCELM_AS_STRING(pattern)->data;
    const char *d = TCELM_AS_STRING(dir)->data;

    tcelm_value_t *result = TCELM_NIL;
    find_helper(arena, p, d, &result);
    return tcelm_list_reverse(arena, result);
}

/*
 * ============================================================================
 * UTILITIES
 * ============================================================================
 */

tcelm_value_t *tcelm_shell_date(tcelm_arena_t *arena) {
    time_t now = time(NULL);
    struct tm *tm = localtime(&now);
    char buf[64];
    strftime(buf, sizeof(buf), "%Y-%m-%d %H:%M:%S", tm);
    return tcelm_string(arena, buf);
}

tcelm_value_t *tcelm_shell_sleep(tcelm_arena_t *arena, tcelm_value_t *ms) {
    int64_t milliseconds = TCELM_AS_INT(ms);
#ifdef __rtems__
    rtems_task_wake_after(RTEMS_MILLISECONDS_TO_TICKS(milliseconds));
#else
    usleep(milliseconds * 1000);
#endif
    return tcelm_unit(arena);
}

tcelm_value_t *tcelm_shell_env(tcelm_arena_t *arena, tcelm_value_t *name) {
    const char *n = TCELM_AS_STRING(name)->data;
    const char *v = getenv(n);
    if (v) {
        return tcelm_just(arena, tcelm_string(arena, v));
    }
    return tcelm_nothing(arena);
}

tcelm_value_t *tcelm_shell_export(tcelm_arena_t *arena, tcelm_value_t *name, tcelm_value_t *value) {
    const char *n = TCELM_AS_STRING(name)->data;
    const char *v = TCELM_AS_STRING(value)->data;
    if (setenv(n, v, 1) == 0) {
        return tcelm_ok(arena, TCELM_UNIT);
    }
    return tcelm_err(arena, tcelm_string(arena, strerror(errno)));
}

tcelm_value_t *tcelm_shell_which(tcelm_arena_t *arena, tcelm_value_t *name) {
    const char *n = TCELM_AS_STRING(name)->data;
    const char *path = getenv("PATH");

    if (!path) {
        return tcelm_nothing(arena);
    }

    char *path_copy = tcelm_arena_alloc(arena, strlen(path) + 1);
    strcpy(path_copy, path);

    char *dir = strtok(path_copy, ":");
    while (dir) {
        char full[PATH_MAX];
        snprintf(full, sizeof(full), "%s/%s", dir, n);

        if (access(full, X_OK) == 0) {
            return tcelm_just(arena, tcelm_string(arena, full));
        }

        dir = strtok(NULL, ":");
    }

    return tcelm_nothing(arena);
}

tcelm_value_t *tcelm_shell_hostname(tcelm_arena_t *arena) {
    char buf[256];
    if (gethostname(buf, sizeof(buf)) == 0) {
        return tcelm_string(arena, buf);
    }
    return tcelm_string(arena, "localhost");
}
