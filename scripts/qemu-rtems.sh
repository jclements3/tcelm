#!/bin/bash
#
# qemu-rtems.sh - Run tcelm programs in QEMU with RTEMS
#
# Usage: ./qemu-rtems.sh <executable.exe>
#

set -e

RTEMS_PREFIX="${RTEMS_PREFIX:-$HOME/projects/hwil/rtems/7}"
RTEMS_BSP="${RTEMS_BSP:-amd64}"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

show_help() {
    cat << 'EOF'
RTEMS QEMU Runner for tcelm

SETUP REQUIRED:
---------------
The amd64 BSP requires UEFI boot support. You need:

1. OVMF (UEFI firmware for QEMU):
   sudo apt install ovmf
   export OVMF_PATH=/usr/share/OVMF/OVMF_CODE.fd

2. For full amd64 support, you also need FreeBSD boot images
   (complex setup - see RTEMS documentation)

SIMPLER ALTERNATIVE - Build i386/pc686 BSP:
-------------------------------------------
The i386 BSP uses multiboot (much simpler):

  cd ~/projects/hwil/rtems
  ./waf configure --prefix=$HOME/rtems-7 \
      --rtems-bsps=i386/pc686
  ./waf build install

Then run with:
  qemu-system-i386 -nographic -no-reboot \
      -kernel your_program.exe

CURRENT STATUS:
---------------
EOF

    echo -e "RTEMS prefix: ${RTEMS_PREFIX}"
    echo -e "RTEMS BSP:    ${RTEMS_BSP}"

    if [ -f "/usr/share/OVMF/OVMF_CODE.fd" ]; then
        echo -e "OVMF:         ${GREEN}Found${NC}"
    else
        echo -e "OVMF:         ${RED}Not found${NC} (apt install ovmf)"
    fi

    if command -v qemu-system-x86_64 &> /dev/null; then
        echo -e "QEMU x86_64:  ${GREEN}Found${NC}"
    else
        echo -e "QEMU x86_64:  ${RED}Not found${NC}"
    fi

    if command -v qemu-system-i386 &> /dev/null; then
        echo -e "QEMU i386:    ${GREEN}Found${NC}"
    else
        echo -e "QEMU i386:    ${RED}Not found${NC}"
    fi
}

run_with_i386_multiboot() {
    local exe="$1"
    echo -e "${GREEN}Running with i386 multiboot...${NC}"
    qemu-system-i386 \
        -nographic \
        -no-reboot \
        -m 128M \
        -kernel "$exe"
}

run_with_amd64_uefi() {
    local exe="$1"
    local ovmf="${OVMF_PATH:-/usr/share/OVMF/OVMF_CODE.fd}"

    if [ ! -f "$ovmf" ]; then
        echo -e "${RED}Error: OVMF not found at $ovmf${NC}"
        echo "Install with: sudo apt install ovmf"
        exit 1
    fi

    echo -e "${GREEN}Running with amd64 UEFI...${NC}"
    echo -e "${YELLOW}Note: Full amd64 requires FreeBSD boot image setup${NC}"

    # This is a simplified attempt - may not work without full setup
    qemu-system-x86_64 \
        -nographic \
        -no-reboot \
        -m 512M \
        -bios "$ovmf" \
        -kernel "$exe"
}

# Main
if [ "$1" = "--help" ] || [ "$1" = "-h" ] || [ -z "$1" ]; then
    show_help
    exit 0
fi

EXE="$1"

if [ ! -f "$EXE" ]; then
    echo -e "${RED}Error: File not found: $EXE${NC}"
    exit 1
fi

# Detect architecture from ELF
ARCH=$(file "$EXE" | grep -o "x86-64\|80386\|i386" | head -1)

case "$ARCH" in
    "x86-64")
        echo "Detected: x86_64 executable"
        run_with_amd64_uefi "$EXE"
        ;;
    "80386"|"i386")
        echo "Detected: i386 executable"
        run_with_i386_multiboot "$EXE"
        ;;
    *)
        echo -e "${YELLOW}Unknown architecture, trying i386 multiboot...${NC}"
        run_with_i386_multiboot "$EXE"
        ;;
esac
