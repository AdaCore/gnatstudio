#!/bin/bash
###############################################################################
# fix_toolchain.sh - Fix GNAT 15.1.2 Broken Headers on macOS 26
#
# PROBLEM: Alire's GNAT 15.1.2 toolchain has broken include-fixed headers
#          - Built for macOS 14 SDK in May 2025
#          - GCC's fixincludes generated broken stdio.h
#          - Uses FILE type before defining it
#          - Causes ALL C compilation to fail
#
# SYMPTOMS: C code fails with "unknown type name 'FILE'"
#
# SOLUTION: Remove broken include-fixed directory
#           - Forces GCC to use correct macOS 26 SDK headers
#           - All C code compiles successfully after fix
#
# USAGE: ./fix_toolchain.sh
#
# PERSISTENCE: This fix is TEMPORARY
#              - Will be lost if Alire reinstalls toolchain
#              - Run this script after fresh `alr toolchain --select`
#              - Required for all developers on macOS 26
###############################################################################

set -e

TOOLCHAIN_DIR="$HOME/.local/share/alire/toolchains/gnat_native_15.1.2_60748c54"
INCLUDE_FIXED="$TOOLCHAIN_DIR/lib/gcc/aarch64-apple-darwin23.6.0/15.0.1/include-fixed"

echo "=== GNAT Toolchain Header Fix for macOS 26 ==="
echo ""

# Check if toolchain exists
if [ ! -d "$TOOLCHAIN_DIR" ]; then
    echo "❌ Error: GNAT toolchain not found at:"
    echo "   $TOOLCHAIN_DIR"
    echo ""
    echo "Run: alr toolchain --select gnat_native"
    exit 1
fi

# Check if include-fixed exists
if [ ! -d "$INCLUDE_FIXED" ]; then
    echo "❌ Error: include-fixed directory not found at:"
    echo "   $INCLUDE_FIXED"
    exit 1
fi

# Backup if not already done
if [ ! -d "$INCLUDE_FIXED.backup" ]; then
    echo "📦 Creating backup of include-fixed..."
    cp -r "$INCLUDE_FIXED" "$INCLUDE_FIXED.backup"
    echo "   Saved to: $INCLUDE_FIXED.backup"
fi

# Remove broken headers
echo "🗑️  Removing broken fixincludes headers..."
rm -rf "$INCLUDE_FIXED"/*

# Create marker
echo "✅ Fixed on $(date) by fix_toolchain.sh" > "$INCLUDE_FIXED/README"
echo "   GCC will now use system headers from macOS 26 SDK directly."

echo ""
echo "✅ Toolchain fixed successfully!"
echo ""
echo "Verification:"
echo "  Run: echo 'int main() { return 0; }' | alr exec -- gcc -x c -"
echo "  Expected: No errors about 'unknown type name'"
echo ""
echo "⚠️  NOTE: This fix will be lost if Alire reinstalls the toolchain."
echo "   Re-run this script after any toolchain updates."
echo ""
