/*
 * elm_qemu_test.c - Run tcelm-generated code in QEMU
 */

#include <rtems.h>
#include <stdint.h>
#include <stdbool.h>

/* Serial output */
extern void serial_putchar(char c);
extern void serial_puts(const char *s);
extern void serial_print_int(int64_t n);

/* Forward declare elm_Main_main */
typedef struct elm_value_s {
    uint32_t tag;
    union {
        int64_t i;
        double f;
        const char *s;
        void *p;
        struct elm_value_s *c;
    } data;
    struct elm_value_s *next;
} elm_value_t;

/* We'll compile the Elm code separately and link */
extern elm_value_t elm_Main_main(void);

/* Print elm value to serial */
void print_elm_value(elm_value_t v) {
    switch (v.tag) {
        case 0:  /* Int */
            serial_print_int(v.data.i);
            break;
        case 1:  /* Float */
            serial_puts("<float>");
            break;
        case 2:  /* Bool */
            serial_puts(v.data.i ? "True" : "False");
            break;
        case 3:  /* Char */
            serial_putchar('\'');
            serial_putchar((char)v.data.i);
            serial_putchar('\'');
            break;
        case 4:  /* String */
            serial_putchar('"');
            serial_puts(v.data.s);
            serial_putchar('"');
            break;
        default:
            serial_puts("<value:");
            serial_print_int(v.tag);
            serial_puts(">");
            break;
    }
}

/* Entry point */
void kmain(void) {
    serial_puts("\n========================================\n");
    serial_puts("tcelm Elm Program in QEMU\n");
    serial_puts("========================================\n\n");
    
    serial_puts("Running elm_Main_main()...\n");
    serial_puts("Result: ");
    
    elm_value_t result = elm_Main_main();
    print_elm_value(result);
    serial_puts("\n");
    
    serial_puts("\n========================================\n");
    serial_puts("Test complete.\n");
    serial_puts("========================================\n");
    
    /* Halt */
    for (;;) {
        __asm__ volatile("hlt");
    }
}
