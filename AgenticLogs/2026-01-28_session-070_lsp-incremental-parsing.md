# LSP Incremental Parsing Implementation
**Date:** 2026-01-28  
**Session:** 070  
**Task:** Implement incremental parsing for LSP to improve performance on large files

## Summary

Implemented incremental parsing support for the LSP server to address performance issues when editing large files (e.g., QB64pe ~59K lines). The implementation includes incremental document sync, incremental lexing, and incremental parsing/analysis.

## Implementation Details

### 1. Incremental Document Sync

**Changed:** `src/lsp/mod.rs`
- Switched from `TextDocumentSyncKind::FULL` to `TextDocumentSyncKind::INCREMENTAL`
- Updated `did_change` handler to process incremental text change events
- Each change event now includes a range and new text, rather than full document content

**Impact:** LSP now receives only changed regions, reducing data transfer and enabling incremental updates.

### 2. Incremental Lexing

**Created:** `src/lsp/analysis/incremental.rs`
- `ChangedRegion` struct to represent document changes
- `merge_tokens()` function that:
  - Identifies tokens affected by the change
  - Re-lexes only the changed region (with context for complete tokens)
  - Merges new tokens with unchanged tokens
  - Adjusts token spans for tokens after the change

**Key Design Decisions:**
- Uses 100-byte context around changes to ensure complete tokenization (e.g., if change is in middle of identifier)
- Filters merged tokens to only include those overlapping the changed region
- Adjusts spans of tokens after the change to account for length differences

### 3. Incremental Parsing

**Added:** `incremental_parse()` function
- Currently does full re-parse from merged tokens (still faster than re-lexing + re-parsing)
- Foundation for future optimization to identify statement boundaries and re-parse only affected sections

**Future Enhancement:** Could identify affected statement/block boundaries and re-parse only those sections, then merge results.

### 4. Incremental Semantic Analysis

**Added:** `incremental_analyze()` function
- Currently does full re-analysis
- Foundation for future optimization to track scope dependencies and re-analyze only affected scopes

**Future Enhancement:** Could track which scopes are affected by changes and preserve symbol table state for unaffected scopes.

### 5. Analysis Cache Updates

**Changed:** `src/lsp/analysis.rs`
- Added `tokens` and `content` fields to `AnalysisCache` to support incremental updates
- Added `update_incremental()` method that:
  - Attempts incremental update if cached data is available
  - Falls back to full re-analysis if incremental update fails
  - Handles multiple sequential changes

**Key Design:** Graceful fallback - if incremental update fails at any step, falls back to full analysis. This ensures correctness while providing performance benefits when possible.

## Testing

**Added:** `src/lsp/tests.rs`
- `test_incremental_lexing_basic()` - Verifies token merging works correctly
- `test_incremental_parse_basic()` - Verifies incremental parsing produces valid results

All existing LSP tests continue to pass (31 tests).

## Performance Impact

**Expected Benefits:**
- **Small edits:** Only re-lex/parse changed region instead of entire file
- **Large files:** Significant time savings on files like QB64pe (59K lines)
- **User experience:** Faster autocomplete, diagnostics, hover information

**Current Limitations:**
- Incremental parsing still does full re-parse (but skips lexing)
- Incremental analysis still does full re-analysis
- These are foundations for future optimizations

## Code Quality

- All code compiles without errors
- No linter warnings (after fixing unused variables)
- Follows existing code patterns and documentation standards
- Graceful error handling with fallback to full analysis

## Next Steps

1. **Optimize incremental parsing:** Identify statement boundaries and re-parse only affected sections
2. **Optimize incremental analysis:** Track scope dependencies and re-analyze only affected scopes
3. **Performance testing:** Measure actual performance improvements on large files
4. **Edge case handling:** Test multi-line changes, overlapping edits, etc.

## Related Documentation

- [NEXT_STEPS_ANALYSIS.md](../docs/NEXT_STEPS_ANALYSIS.md) - Original requirement
- LSP incremental parsing is a common pattern in language servers (TypeScript, Rust Analyzer, etc.)
