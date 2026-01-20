/*
 * tcc-nuc-bsp/config.h - TCC configuration for Intel NUC RTEMS target
 *
 * This defines the target configuration for TCC cross-compiling
 * to the 60-node Intel NUC cluster running RTEMS 7.
 *
 * Target: i386-rtems7-nuc
 * CPU:    Intel Core i7 (running in 32-bit mode)
 * OS:     RTEMS 7
 * BSP:    pc686 (i386)
 */

#ifndef TCC_NUC_BSP_CONFIG_H
#define TCC_NUC_BSP_CONFIG_H

/* Target architecture - 32-bit i386 (TCC's most mature target) */
#define TCC_TARGET_I386         1
#define TCC_TARGET_RTEMS        1
#define TCC_TARGET_NUC          1

/* CPU features available on NUC i7 (using 32-bit mode) */
#define TCC_CPU_SSE2            1
#define TCC_CPU_SSE3            1

/* Memory model - 32-bit flat model */
#define TCC_MEMORY_MODEL_FLAT   1

/* RTEMS configuration */
#define RTEMS_VERSION           7
#define RTEMS_BSP               "pc686"
#define RTEMS_POSIX             1   /* POSIX API available */

/* Default paths (can be overridden) */
#ifndef RTEMS_PREFIX
#define RTEMS_PREFIX            "/home/james.clements/projects/hwil/rtems/7"
#endif

#define TCC_NUC_SYSROOT         RTEMS_PREFIX "/i386-rtems7"
#define TCC_NUC_INCLUDE         TCC_NUC_SYSROOT "/include"
#define TCC_NUC_LIB             TCC_NUC_SYSROOT "/lib"
#define TCC_NUC_BSP_LIB         RTEMS_PREFIX "/i386-rtems7/pc686/lib"

/* C runtime */
#define TCC_NUC_CRT0            "crt0.o"
#define TCC_NUC_CRTI            "crti.o"
#define TCC_NUC_CRTN            "crtn.o"

/* Default linker script */
#define TCC_NUC_LDSCRIPT        "linkcmds.nuc"

/* Stack size for RTEMS tasks */
#define TCC_NUC_STACK_SIZE      (64 * 1024)  /* 64 KB default */

/* Entry point */
#define TCC_NUC_ENTRY           "Init"

/* Output format */
#define TCC_NUC_OUTPUT_FORMAT   "elf32-i386"

#endif /* TCC_NUC_BSP_CONFIG_H */
