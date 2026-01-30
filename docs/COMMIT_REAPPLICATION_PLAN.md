# Commit Re-application Plan: broken → fixing branch

**Created:** 2026-02-03  
**Purpose:** Selective re-application of commits from `broken` branch to `fixing` branch, excluding GUI/IDE troubleshooting commits

**From:** `d7e5bdc` (current HEAD on fixing branch)  
**To:** `eed3d21` (latest commit on broken branch)  
**Total Commits:** 142

---

## Quick Reference

### Commits to Skip (GUI Troubleshooting)
- ❌ `eed3d21` - IDE disclaimer dismiss (pure GUI troubleshooting)

### Commits Needing Manual Edit (Mixed)
- ⚠️ `6bf251e` - BYREF codegen + IDE window visibility
  - **Remove**: SUPPRESS_NEXT_SCREENHIDE changes
  - **Keep**: BYREF string codegen fixes, source size limits

### Commits to Keep (App Advancement)
- ✅ All other 141 commits - cherry-pick as-is

### Recommended Approach
**Option 1: Cherry-pick Individual Commits** (see script below)

---

## Strategy

We want to re-apply commits that advance the application functionality, but **exclude** commits that were implemented solely for troubleshooting GUI/IDE issues.

### Categorization Rules

1. **KEEP (App Advancement)**: Commits that add features, fix bugs, improve codegen, add runtime functionality, documentation, refactoring, etc.
2. **SKIP (GUI Troubleshooting)**: Commits that only fix IDE window behavior, disclaimer dismiss, input handling quirks, etc.
3. **REVIEW (Mixed)**: Commits that contain both app advancement AND GUI troubleshooting - these may need to be split or manually edited

---

## Commit Categories

### ✅ KEEP - App Advancement Commits

These commits should be cherry-picked as-is (141 commits total, excluding GUI troubleshooting):

**Note:** The list below excludes commit `eed3d21` (GUI troubleshooting) and `6bf251e` (mixed - see REVIEW section).

All commits are listed in chronological order (oldest first):

*[Full commit list with 141 commits would be inserted here - see COMMIT_LOG_fixing_to_broken.md for complete details]*

**Summary of commit types:**
- Code generation improvements: ~30 commits
- Bootstrap fixes and validation: ~15 commits
- Runtime enhancements: ~20 commits
- Language features and directives: ~15 commits
- Refactoring and code organization: ~10 commits
- Documentation updates: ~20 commits
- Testing infrastructure: ~10 commits
- LSP improvements: ~5 commits
- Bug fixes: ~16 commits

### ⚠️ REVIEW - Mixed Commits (App + GUI)

These commits contain both app advancement and GUI troubleshooting. They may need:
- Manual splitting into separate commits
- Manual editing to remove GUI parts
- Or keeping if the GUI part is minimal/non-breaking

1. **`6bf251e`** - "Fix BYREF string codegen, IDE window visibility, add source size limits"
   - **KEEP parts**: BYREF string codegen fixes, source size limits
   - **REMOVE parts**: SUPPRESS_NEXT_SCREENHIDE (IDE window visibility)
   - **Action**: Cherry-pick but manually remove the SUPPRESS_NEXT_SCREENHIDE changes

### ❌ SKIP - GUI Troubleshooting Only

These commits should be skipped entirely:

1. **`eed3d21`** - "IDE disclaimer dismiss: push each key twice for getinput inkey+keyhit"
   - Pure GUI troubleshooting for IDE behavior
   - **Action**: Skip entirely

---

## Implementation Plan

### Option 1: Cherry-pick Individual Commits (Recommended)

**Pros:**
- Full control over which commits are applied
- Can review each commit individually
- Can skip problematic commits easily

**Cons:**
- More manual work
- Need to handle dependencies

**Steps:**

1. Create a new branch from `fixing`:
   ```bash
   git checkout fixing
   git checkout -b fixing-reapply
   ```

2. Get list of commits to cherry-pick (excluding GUI troubleshooting):
   ```bash
   git log --reverse --format="%H" HEAD..broken | grep -v "eed3d21" > /tmp/commits_to_apply.txt
   ```

3. Cherry-pick commits one by one, handling conflicts:
   ```bash
   while read commit; do
     git cherry-pick $commit
     # Resolve conflicts if any
     # For mixed commits, manually edit to remove GUI parts
   done < /tmp/commits_to_apply.txt
   ```

4. For mixed commit `6bf251e`:
   ```bash
   git cherry-pick 6bf251e
   # Manually remove SUPPRESS_NEXT_SCREENHIDE changes
   git add -A
   git commit --amend
   ```

### Option 2: Interactive Rebase

**Pros:**
- Can edit commits during rebase
- Can drop commits interactively

**Cons:**
- More complex
- Harder to handle dependencies

**Steps:**

1. Create a branch from `broken`:
   ```bash
   git checkout broken
   git checkout -b fixing-reapply
   ```

2. Interactive rebase to drop GUI commits:
   ```bash
   git rebase -i d7e5bdc
   # Mark GUI commits as 'drop'
   # Mark mixed commits as 'edit'
   ```

3. For mixed commits, edit to remove GUI parts

### Option 3: Merge with Manual Cleanup

**Pros:**
- Fastest initial approach

**Cons:**
- Requires manual cleanup after merge
- Harder to track what was removed

**Steps:**

1. Merge broken into fixing:
   ```bash
   git checkout fixing
   git merge broken
   ```

2. Manually revert GUI troubleshooting commits:
   ```bash
   git revert eed3d21
   # Manually remove GUI parts from 6bf251e
   ```

---

## Recommended Approach

**Use Option 1 (Cherry-pick)** because:
- It gives the most control
- We can review each commit
- We can handle the mixed commit properly
- We can test after each major commit

### Detailed Cherry-pick Script

```bash
#!/bin/bash
# Script to cherry-pick commits from broken to fixing

set -e

# Ensure we're on fixing branch
git checkout fixing
git checkout -b fixing-reapply

# Commits to skip (GUI troubleshooting only)
SKIP_COMMITS=(
    "eed3d212f730ca41d0bab6165d43e982bcfd4df0"  # IDE disclaimer dismiss
)

# Mixed commits that need manual editing
MIXED_COMMITS=(
    "6bf251e046776ed9026b8a02c2881de943e86172"  # BYREF + IDE window visibility
)

# Function to check if commit should be skipped
should_skip() {
    local commit=$1
    for skip in "${SKIP_COMMITS[@]}"; do
        if [ "$commit" = "$skip" ]; then
            return 0
        fi
    done
    return 1
}

# Function to check if commit needs manual editing
needs_manual_edit() {
    local commit=$1
    for mixed in "${MIXED_COMMITS[@]}"; do
        if [ "$commit" = "$mixed" ]; then
            return 0
        fi
    done
    return 1
}

# Function to check if commit is mixed
is_mixed() {
    local commit=$1
    for mixed in "${MIXED_COMMITS[@]}"; do
        if [ "$commit" = "$mixed" ]; then
            return 0
        fi
    done
    return 1
}

# Get all commits
commits=$(git log --reverse --format="%H" HEAD..broken)

for commit in $commits; do
    if should_skip "$commit"; then
        echo "⏭️  Skipping GUI troubleshooting commit: $commit"
        continue
    fi
    
    if needs_manual_edit "$commit"; then
        echo "⚠️  Cherry-picking mixed commit (needs manual editing): $commit"
        git cherry-pick "$commit" || {
            echo "❌ Conflict in $commit - resolve manually"
            echo "   Remember to remove GUI troubleshooting parts!"
            read -p "Press enter after resolving conflicts..."
        }
        echo ""
        echo "⚠️  MANUAL EDIT REQUIRED: Remove GUI troubleshooting parts"
        echo "   For commit $commit:"
        echo "   - Remove SUPPRESS_NEXT_SCREENHIDE from runtime/src/graphics/sdl2.rs"
        echo "   - Remove related changes from runtime/src/graphics/mod.rs"
        echo "   - Keep BYREF string codegen fixes and source size limits"
        echo ""
        echo "   After editing, run: git add -A && git commit --amend"
        read -p "Press enter after completing manual edits..."
    else
        echo "✅ Cherry-picking: $commit"
        git cherry-pick "$commit" || {
            echo "❌ Conflict in $commit - resolve manually"
            read -p "Press enter after resolving conflicts..."
        }
    fi
done

echo "✅ Done! Review the branch and test before merging."
```

---

## Testing Checklist

After re-applying commits, verify:

- [ ] Code compiles successfully
- [ ] Tests pass (run `cargo test`)
- [ ] QB64pe bootstrap still works (if applicable)
- [ ] No GUI/IDE-specific code remains (check for SUPPRESS_NEXT_SCREENHIDE, disclaimer dismiss logic, etc.)
- [ ] Runtime functionality works correctly
- [ ] Codegen produces correct C code

---

## Notes

### Mixed Commit Details

**Commit `6bf251e`** contains:
- ✅ BYREF string codegen fixes (KEEP)
- ✅ Source size limits (KEEP)
- ❌ SUPPRESS_NEXT_SCREENHIDE (REMOVE)

**Files to check for GUI code removal:**
- `runtime/src/graphics/sdl2.rs` - Look for SUPPRESS_NEXT_SCREENHIDE
- `runtime/src/graphics/mod.rs` - Check for related changes
- `runtime/src/graphics_ffi.rs` - Verify no GUI-specific changes

### GUI Commit Details

**Commit `eed3d21`** is pure IDE troubleshooting:
- Changes to IDE disclaimer dismiss behavior
- Input handling for getinput/inkey/keyhit
- Should be completely skipped

---

## Commit List Reference

See `COMMIT_LOG_fixing_to_broken.md` for full details of all 142 commits.

---

## Next Steps

1. Review this plan
2. Execute cherry-pick script (or manual process)
3. Test thoroughly
4. Merge `fixing-reapply` into `fixing` when ready
