#!/bin/bash
#
# run-tcelm.sh - Run tcelm RTEMS application in QEMU
#
# Usage:
#   ./run-tcelm.sh <executable> [options]
#
# Options:
#   --video=MODE     Video mode: serial (default), vga, sxga
#   --fs-size=SIZE   Filesystem size in MB (default: 16)
#   --debug          Enable QEMU debug options
#   --gdb            Wait for GDB connection on port 1234
#

set -e

# Defaults
VIDEO_MODE="serial"
FS_SIZE_MB=16
DEBUG=0
GDB=0
EXECUTABLE=""

# Parse arguments
while [[ $# -gt 0 ]]; do
    case "$1" in
        --video=*)
            VIDEO_MODE="${1#*=}"
            shift
            ;;
        --fs-size=*)
            FS_SIZE_MB="${1#*=}"
            shift
            ;;
        --debug)
            DEBUG=1
            shift
            ;;
        --gdb)
            GDB=1
            shift
            ;;
        -*)
            echo "Unknown option: $1"
            exit 1
            ;;
        *)
            EXECUTABLE="$1"
            shift
            ;;
    esac
done

if [ -z "$EXECUTABLE" ]; then
    echo "Usage: $0 <executable> [options]"
    echo ""
    echo "Options:"
    echo "  --video=MODE     Video mode: serial (default), vga, sxga"
    echo "  --fs-size=SIZE   Filesystem size in MB (default: 16)"
    echo "  --debug          Enable QEMU debug options"
    echo "  --gdb            Wait for GDB connection on port 1234"
    exit 1
fi

# Build QEMU command
QEMU_CMD="qemu-system-i386"
QEMU_OPTS=""

# Memory (enough for RTEMS + filesystem)
QEMU_OPTS+=" -m $((128 + FS_SIZE_MB))"

# CPU
QEMU_OPTS+=" -cpu pentium3"

# No graphical output for serial mode
case "$VIDEO_MODE" in
    serial)
        QEMU_OPTS+=" -nographic"
        QEMU_OPTS+=" -serial mon:stdio"
        ;;
    vga)
        QEMU_OPTS+=" -vga std"
        QEMU_OPTS+=" -serial stdio"
        ;;
    sxga)
        # SXGA 1280x1024
        QEMU_OPTS+=" -vga std"
        QEMU_OPTS+=" -device VGA,vgamem_mb=16"
        QEMU_OPTS+=" -serial stdio"
        echo "Note: SXGA mode - RTEMS BSP must support VESA 1280x1024"
        ;;
    *)
        echo "Unknown video mode: $VIDEO_MODE"
        exit 1
        ;;
esac

# Create RAM disk image for filesystem testing
DISK_IMG="/tmp/tcelm-fs-$$.img"
dd if=/dev/zero of="$DISK_IMG" bs=1M count="$FS_SIZE_MB" 2>/dev/null
mkfs.fat "$DISK_IMG" >/dev/null 2>&1 || true

# Add disk
QEMU_OPTS+=" -drive file=$DISK_IMG,format=raw,if=ide"

# Debug options
if [ "$DEBUG" -eq 1 ]; then
    QEMU_OPTS+=" -d guest_errors,cpu_reset"
fi

# GDB support
if [ "$GDB" -eq 1 ]; then
    QEMU_OPTS+=" -s -S"
    echo "Waiting for GDB on localhost:1234..."
fi

# Kernel
QEMU_OPTS+=" -kernel $EXECUTABLE"

# Cleanup on exit
cleanup() {
    rm -f "$DISK_IMG"
}
trap cleanup EXIT

# Run
echo "=== tcelm RTEMS on QEMU ==="
echo "Executable: $EXECUTABLE"
echo "Video mode: $VIDEO_MODE"
echo "Filesystem: ${FS_SIZE_MB}MB RAM disk"
echo ""
echo "Press Ctrl+A X to exit QEMU"
echo "==========================="
echo ""

$QEMU_CMD $QEMU_OPTS
