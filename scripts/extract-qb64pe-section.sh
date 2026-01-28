#!/bin/bash
# Extract a section from qb64pe.bas for incremental testing
# Usage: ./scripts/extract-qb64pe-section.sh <section_name> <start_line> <end_line>
#
# Example: Extract idstruct TYPE definition
#   ./scripts/extract-qb64pe-section.sh idstruct_type 596 656

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
QB64PE_SOURCE="$PROJECT_ROOT/../QB64pe/source/qb64pe.bas"
OUTPUT_DIR="$PROJECT_ROOT/tests/qb64pe_incremental/sections"

if [ $# -lt 3 ]; then
    echo "Usage: $0 <section_name> <start_line> <end_line>"
    echo ""
    echo "Extracts a section from qb64pe.bas and creates a test file."
    echo ""
    echo "Example:"
    echo "  $0 idstruct_type 596 656"
    echo ""
    echo "This creates: tests/qb64pe_incremental/sections/idstruct_type.bas"
    exit 1
fi

SECTION_NAME="$1"
START_LINE="$2"
END_LINE="$3"

mkdir -p "$OUTPUT_DIR"

OUTPUT_FILE="$OUTPUT_DIR/${SECTION_NAME}.bas"

# Create header
cat > "$OUTPUT_FILE" <<EOF
'==============================================================================
' Extracted Section: $SECTION_NAME
' Source: qb64pe.bas lines $START_LINE-$END_LINE
'==============================================================================
' NOTE: This is a raw extraction. Include this file AFTER setting up:
'   - Core includes (version, settings, constants)
'   - Required utility headers
'   - TYPE definitions this section depends on
'==============================================================================

EOF

# Extract the section
sed -n "${START_LINE},${END_LINE}p" "$QB64PE_SOURCE" >> "$OUTPUT_FILE"

# No footer - extracted sections are meant to be included, not run standalone

echo "Created: $OUTPUT_FILE"
echo "Lines extracted: $START_LINE-$END_LINE"
wc -l "$OUTPUT_FILE"
