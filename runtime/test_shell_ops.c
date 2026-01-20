/*
 * test_shell_ops.c - Test Turtle/Shelly-style shell operations
 *
 * Compile with: gcc -o test_shell_ops test_shell_ops.c tcelm_arena.c \
 *                   tcelm_types.c tcelm_basics.c tcelm_shell_ops.c -lm
 */

#include <stdio.h>
#include <string.h>
#include <assert.h>
#include <unistd.h>
#include "tcelm_arena.h"
#include "tcelm_types.h"
#include "tcelm_basics.h"
#include "tcelm_shell_ops.h"

#define TEST(name, expr) do { \
    printf("  %-50s ", name); \
    if (expr) { printf("[PASS]\n"); } \
    else { printf("[FAIL]\n"); failures++; } \
} while(0)

int main(void) {
    int failures = 0;
    tcelm_arena_t arena;

    printf("\n=== tcelm Shell Operations Test (Turtle/Shelly style) ===\n\n");

    /* Initialize arena */
    assert(tcelm_arena_init(&arena, 64 * 1024) == 0);
    tcelm_current_arena = &arena;

    /* Test path operations */
    printf("Path operations:\n");
    {
        tcelm_value_t *p1 = tcelm_string(&arena, "/home");
        tcelm_value_t *p2 = tcelm_string(&arena, "user");
        tcelm_value_t *joined = tcelm_path_join(&arena, p1, p2);
        TEST("path_join /home + user = /home/user",
             strcmp(TCELM_AS_STRING(joined)->data, "/home/user") == 0);

        tcelm_value_t *path = tcelm_string(&arena, "/home/user/file.txt");
        tcelm_value_t *parent = tcelm_path_parent(&arena, path);
        TEST("path_parent /home/user/file.txt = /home/user",
             strcmp(TCELM_AS_STRING(parent)->data, "/home/user") == 0);

        tcelm_value_t *fname = tcelm_path_filename(&arena, path);
        TEST("path_filename /home/user/file.txt = file.txt",
             strcmp(TCELM_AS_STRING(fname)->data, "file.txt") == 0);

        tcelm_value_t *ext = tcelm_path_extension(&arena, path);
        TEST("path_extension file.txt = Just txt",
             tcelm_is_just(ext) &&
             strcmp(TCELM_AS_STRING(tcelm_maybe_unwrap(ext))->data, "txt") == 0);

        tcelm_value_t *noext = tcelm_path_drop_extension(&arena, path);
        TEST("path_drop_extension file.txt = /home/user/file",
             strcmp(TCELM_AS_STRING(noext)->data, "/home/user/file") == 0);

        tcelm_value_t *base = tcelm_string(&arena, "readme");
        tcelm_value_t *withext = tcelm_path_add_extension(&arena, base, tcelm_string(&arena, "md"));
        TEST("path_add_extension readme + md = readme.md",
             strcmp(TCELM_AS_STRING(withext)->data, "readme.md") == 0);
    }
    printf("\n");

    /* Test directory operations */
    printf("Directory operations:\n");
    {
        tcelm_value_t *cwd = tcelm_shell_pwd(&arena);
        TEST("pwd returns non-empty string",
             TCELM_AS_STRING(cwd)->length > 0);
        printf("    Current dir: %s\n", TCELM_AS_STRING(cwd)->data);

        tcelm_value_t *home = tcelm_shell_home(&arena);
        TEST("home returns non-empty string",
             TCELM_AS_STRING(home)->length > 0);
        printf("    Home dir: %s\n", TCELM_AS_STRING(home)->data);

        /* Create test directory */
        tcelm_value_t *testdir = tcelm_string(&arena, "/tmp/tcelm_shell_test");
        tcelm_value_t *mk_result = tcelm_shell_mkdirtree(&arena, testdir);
        TEST("mkdirtree creates directory",
             tcelm_is_ok(mk_result) || TCELM_AS_BOOL(tcelm_shell_dir_exists(&arena, testdir)));

        /* List directory */
        tcelm_value_t *tmpdir = tcelm_string(&arena, "/tmp");
        tcelm_value_t *listing = tcelm_shell_ls(&arena, tmpdir);
        TEST("ls /tmp returns list",
             !tcelm_is_nil(listing));

        /* Check dir exists */
        TEST("dir_exists /tmp = True",
             TCELM_AS_BOOL(tcelm_shell_dir_exists(&arena, tmpdir)));

        TEST("dir_exists /nonexistent = False",
             !TCELM_AS_BOOL(tcelm_shell_dir_exists(&arena, tcelm_string(&arena, "/nonexistent_dir_xyz"))));
    }
    printf("\n");

    /* Test file operations */
    printf("File operations:\n");
    {
        tcelm_value_t *testfile = tcelm_string(&arena, "/tmp/tcelm_shell_test/test.txt");

        /* Write file */
        tcelm_value_t *content = tcelm_string(&arena, "Hello, Turtle!\nLine 2\nLine 3\n");
        tcelm_value_t *write_result = tcelm_shell_write_file(&arena, testfile, content);
        TEST("write_file creates file", tcelm_is_ok(write_result));

        /* File exists */
        TEST("file_exists after write = True",
             TCELM_AS_BOOL(tcelm_shell_file_exists(&arena, testfile)));

        /* Is file */
        TEST("is_file test.txt = True",
             TCELM_AS_BOOL(tcelm_shell_is_file(&arena, testfile)));

        /* File size */
        tcelm_value_t *size = tcelm_shell_file_size(&arena, testfile);
        TEST("file_size > 0",
             TCELM_AS_INT(size) > 0);
        printf("    File size: %ld bytes\n", (long)TCELM_AS_INT(size));

        /* Read file */
        tcelm_value_t *read_content = tcelm_shell_read_file(&arena, testfile);
        TEST("read_file returns content",
             strstr(TCELM_AS_STRING(read_content)->data, "Hello, Turtle!") != NULL);

        /* Cat (read as lines) */
        tcelm_value_t *lines = tcelm_shell_cat(&arena, testfile);
        int line_count = 0;
        tcelm_value_t *l = lines;
        while (!tcelm_is_nil(l)) {
            line_count++;
            l = tcelm_list_tail(l);
        }
        TEST("cat returns 3 lines", line_count == 3);

        /* Append */
        tcelm_value_t *more = tcelm_string(&arena, "Line 4\n");
        tcelm_shell_append_file(&arena, testfile, more);
        lines = tcelm_shell_cat(&arena, testfile);
        line_count = 0;
        l = lines;
        while (!tcelm_is_nil(l)) {
            line_count++;
            l = tcelm_list_tail(l);
        }
        TEST("append_file adds line", line_count == 4);

        /* Copy */
        tcelm_value_t *copyfile = tcelm_string(&arena, "/tmp/tcelm_shell_test/test_copy.txt");
        tcelm_value_t *cp_result = tcelm_shell_cp(&arena, testfile, copyfile);
        TEST("cp creates copy", tcelm_is_ok(cp_result));
        TEST("copy exists",
             TCELM_AS_BOOL(tcelm_shell_file_exists(&arena, copyfile)));

        /* Touch */
        tcelm_value_t *touchfile = tcelm_string(&arena, "/tmp/tcelm_shell_test/touched.txt");
        tcelm_value_t *touch_result = tcelm_shell_touch(&arena, touchfile);
        TEST("touch creates file", tcelm_is_ok(touch_result));
        TEST("touched file exists",
             TCELM_AS_BOOL(tcelm_shell_file_exists(&arena, touchfile)));

        /* Remove */
        tcelm_value_t *rm_result = tcelm_shell_rm(&arena, touchfile);
        TEST("rm removes file", tcelm_is_ok(rm_result));
        TEST("removed file doesn't exist",
             !TCELM_AS_BOOL(tcelm_shell_file_exists(&arena, touchfile)));
    }
    printf("\n");

    /* Test glob patterns */
    printf("Glob patterns:\n");
    {
        tcelm_value_t *pat = tcelm_string(&arena, "*.txt");
        tcelm_value_t *path1 = tcelm_string(&arena, "file.txt");
        tcelm_value_t *path2 = tcelm_string(&arena, "file.c");

        TEST("*.txt matches file.txt",
             TCELM_AS_BOOL(tcelm_shell_glob_match(&arena, pat, path1)));
        TEST("*.txt doesn't match file.c",
             !TCELM_AS_BOOL(tcelm_shell_glob_match(&arena, pat, path2)));

        tcelm_value_t *pat2 = tcelm_string(&arena, "test*");
        tcelm_value_t *path3 = tcelm_string(&arena, "test_runtime.c");
        TEST("test* matches test_runtime.c",
             TCELM_AS_BOOL(tcelm_shell_glob_match(&arena, pat2, path3)));

        /* Find files */
        tcelm_value_t *find_pat = tcelm_string(&arena, "*.txt");
        tcelm_value_t *find_dir = tcelm_string(&arena, "/tmp/tcelm_shell_test");
        tcelm_value_t *found = tcelm_shell_find(&arena, find_pat, find_dir);

        int found_count = 0;
        tcelm_value_t *f = found;
        while (!tcelm_is_nil(f)) {
            found_count++;
            printf("    Found: %s\n", TCELM_AS_STRING(tcelm_list_head(f))->data);
            f = tcelm_list_tail(f);
        }
        TEST("find *.txt finds 2 files", found_count == 2);
    }
    printf("\n");

    /* Test process execution (POSIX only) */
    printf("Process execution:\n");
    {
        /* inproc - capture output */
        tcelm_value_t *cmd = tcelm_string(&arena, "echo");
        tcelm_value_t *args = tcelm_cons(&arena,
            tcelm_string(&arena, "hello from process"),
            TCELM_NIL);
        tcelm_value_t *output = tcelm_shell_inproc(&arena, cmd, args, TCELM_NIL);

        TEST("inproc echo returns output", !tcelm_is_nil(output));
        if (!tcelm_is_nil(output)) {
            printf("    Output: %s\n", TCELM_AS_STRING(tcelm_list_head(output))->data);
        }

        /* proc_strict */
        tcelm_value_t *ls_cmd = tcelm_string(&arena, "ls");
        tcelm_value_t *ls_args = tcelm_cons(&arena,
            tcelm_string(&arena, "/tmp/tcelm_shell_test"),
            TCELM_NIL);
        tcelm_value_t *ls_output = tcelm_shell_proc_strict(&arena, ls_cmd, ls_args);
        TEST("proc_strict ls returns string",
             TCELM_AS_STRING(ls_output)->length > 0);
        printf("    ls output length: %zu bytes\n", TCELM_AS_STRING(ls_output)->length);

        /* proc_exit */
        tcelm_value_t *true_cmd = tcelm_string(&arena, "true");
        tcelm_value_t *exit_code = tcelm_shell_proc_exit(&arena, true_cmd, TCELM_NIL);
        TEST("true exits with 0", TCELM_AS_INT(exit_code) == 0);

        tcelm_value_t *false_cmd = tcelm_string(&arena, "false");
        tcelm_value_t *false_exit = tcelm_shell_proc_exit(&arena, false_cmd, TCELM_NIL);
        TEST("false exits with non-zero", TCELM_AS_INT(false_exit) != 0);
    }
    printf("\n");

    /* Test utilities */
    printf("Utilities:\n");
    {
        /* Date */
        tcelm_value_t *date = tcelm_shell_date(&arena);
        TEST("date returns non-empty string",
             TCELM_AS_STRING(date)->length > 0);
        printf("    Date: %s\n", TCELM_AS_STRING(date)->data);

        /* Hostname */
        tcelm_value_t *host = tcelm_shell_hostname(&arena);
        TEST("hostname returns non-empty string",
             TCELM_AS_STRING(host)->length > 0);
        printf("    Hostname: %s\n", TCELM_AS_STRING(host)->data);

        /* Env */
        tcelm_value_t *path_env = tcelm_shell_env(&arena, tcelm_string(&arena, "PATH"));
        TEST("env PATH returns Just",
             tcelm_is_just(path_env));

        tcelm_value_t *noenv = tcelm_shell_env(&arena, tcelm_string(&arena, "NONEXISTENT_VAR_XYZ"));
        TEST("env NONEXISTENT returns Nothing",
             !tcelm_is_just(noenv));

        /* Which */
        tcelm_value_t *ls_which = tcelm_shell_which(&arena, tcelm_string(&arena, "ls"));
        TEST("which ls returns Just",
             tcelm_is_just(ls_which));
        if (tcelm_is_just(ls_which)) {
            printf("    ls is at: %s\n", TCELM_AS_STRING(tcelm_maybe_unwrap(ls_which))->data);
        }

        tcelm_value_t *nonexistent_which = tcelm_shell_which(&arena, tcelm_string(&arena, "nonexistent_command_xyz"));
        TEST("which nonexistent returns Nothing",
             !tcelm_is_just(nonexistent_which));
    }
    printf("\n");

    /* Cleanup test directory */
    printf("Cleanup:\n");
    {
        tcelm_value_t *testdir = tcelm_string(&arena, "/tmp/tcelm_shell_test");
        tcelm_value_t *rm_result = tcelm_shell_rmtree(&arena, testdir);
        TEST("rmtree removes test directory",
             tcelm_is_ok(rm_result) || !TCELM_AS_BOOL(tcelm_shell_dir_exists(&arena, testdir)));
    }
    printf("\n");

    /* Cleanup */
    tcelm_arena_destroy(&arena);

    /* Summary */
    printf("=== Test Summary ===\n");
    if (failures == 0) {
        printf("All tests passed!\n");
    } else {
        printf("%d test(s) FAILED\n", failures);
    }

    return failures;
}
