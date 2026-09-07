#!/bin/bash
# validate-all.sh - Batch validation for all @lisp packages
# Usage: ./validate-all.sh [package_name]
# If no package name given, validates all packages.

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
LINT_BIN="$SCRIPT_DIR/../atlisp-lint/bin/atlisp-lint"
ERRORS=0
WARNINGS=0

if [ ! -f "$LINT_BIN" ]; then
  echo "ERROR: atlisp-lint not found at $LINT_BIN"
  exit 1
fi

lint_file() {
  local file="$1"
  local output
  output=$(node "$LINT_BIN" --file "$file" 2>&1)
  local file_errors=$(echo "$output" | grep -c "^错误:" || true)
  local file_warnings=$(echo "$output" | grep -c "^警告:" || true)
  ERRORS=$((ERRORS + file_errors))
  WARNINGS=$((WARNINGS + file_warnings))
  
  if [ "$file_errors" -gt 0 ] || [ "$file_warnings" -gt 0 ]; then
    echo "$output"
  fi
}

if [ -n "$1" ]; then
  # Validate single package
  echo "=== Validating package: $1 ==="
  if [ ! -d "$SCRIPT_DIR/$1" ]; then
    echo "ERROR: Package '$1' not found"
    exit 1
  fi
  for f in "$SCRIPT_DIR/$1"/*.lsp; do
    [ -f "$f" ] && lint_file "$f"
  done
else
  # Validate all packages
  echo "=== Validating all packages ==="
  for dir in "$SCRIPT_DIR"/*/; do
    pkg=$(basename "$dir")
    # Skip non-package directories
    [ ! -f "$dir/pkg.lsp" ] && continue
    echo "--- $pkg ---"
    for f in "$dir"*.lsp; do
      [ -f "$f" ] && lint_file "$f"
    done
  done
fi

echo ""
echo "=== Summary ==="
echo "  Errors:   $ERRORS"
echo "  Warnings: $WARNINGS"

if [ "$ERRORS" -gt 0 ]; then
  echo "  STATUS:   FAILED"
  exit 1
else
  echo "  STATUS:   PASSED"
  exit 0
fi
