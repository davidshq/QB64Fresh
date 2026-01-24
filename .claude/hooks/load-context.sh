#!/bin/bash
# QB64Fresh Context Loader
# Automatically loads critical AST/IR definitions at session start
# to prevent "fumbling with wrong variant names"

cd "$CLAUDE_PROJECT_DIR" || exit 1

echo "=== QB64Fresh Critical Context ==="
echo ""
echo "Loading AST and IR definitions to prevent variant name errors..."
echo ""

# Expression AST (full file - typically ~150 lines)
echo "### src/ast/expr.rs - ExprKind variants ###"
if [ -f "src/ast/expr.rs" ]; then
    cat src/ast/expr.rs
else
    echo "[ERROR] File not found: src/ast/expr.rs"
fi
echo ""

# Statement AST (first 200 lines - key variants)
echo "### src/ast/stmt.rs - StatementKind variants (first 200 lines) ###"
if [ -f "src/ast/stmt.rs" ]; then
    head -200 src/ast/stmt.rs
else
    echo "[ERROR] File not found: src/ast/stmt.rs"
fi
echo ""

# Typed IR (first 250 lines - TypedExprKind, TypedStatementKind)
echo "### src/semantic/typed_ir.rs - Typed IR (first 250 lines) ###"
if [ -f "src/semantic/typed_ir.rs" ]; then
    head -250 src/semantic/typed_ir.rs
else
    echo "[ERROR] File not found: src/semantic/typed_ir.rs"
fi
echo ""

echo "=== Context loaded. Ready for QB64Fresh development. ==="
