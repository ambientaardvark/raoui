# Update.ml Refactoring - Status

## Goal
Refactor `lib/update.ml` to use internal coordinates (`cursor_line`, `cursor_pos`) as the source of truth instead of terminal coordinates (`cursor_row`, `cursor_col`). This eliminates repeated `terminal_to_internal` conversions and enables cleaner, more efficient code.

## Completed

### New Model Fields (in `frontend_types.ml`)
- `cursor_line : int` - logical line index (0-indexed)
- `cursor_pos : int` - grapheme position within line

### Primitives Added (in `update.ml`)
- `current_line model` - get the line at cursor
- `line_length model` - length of current line
- `char_at model` - character at cursor (Option)
- `char_before model` - character before cursor (Option)
- `at_line_start model` - cursor at position 0
- `at_line_end model` - cursor at end of line
- `at_first_line model` - cursor on first logical line
- `at_last_line model` - cursor on last logical line
- `same_cursor_pos m1 m2` - compare cursor positions
- `sync_internal_coords model` - sync internal from terminal coords

### Functions Migrated
All cursor/editing functions now use internal coords as source of truth:
- `move_left`, `move_right`, `move_up`, `move_down`
- `insert_char`, `delete_char`
- `insert_newline`, `insert_paste`
- `move_cursor_to_end`
- `go_to_line_start`, `go_to_line_end`
- `go_to_next_word`, `go_to_last_word`
- `insert_matched_end`, `insert_matched_same`
- `user_input_delete`, `delete_before_cursor`, `delete_char_after_cursor`
- `handle_resize`
- Up/Down key handlers in `apply_key`
- `submit` (continuation logic)

### Entry Point
- `sync_internal_coords` called at start of `update` function
- This ensures internal coords are valid before any processing

## Remaining Work

### Test Failures (2 tests)
Tests that call `Update.submit` directly (bypassing `update`) fail because they set `cursor_col` but not `cursor_pos`:
- `submit / Continuation if paren`
- `submit / Continuation function paren`

**Fix options:**
1. Update tests to set both `cursor_col` and `cursor_pos`
2. Have tests call `update` with Enter key instead of `submit` directly
3. Add sync inside `submit` (less clean)

### Functions Not Migrated (by design)
- `handle_vertical_cursor_movement` - operates in terminal space for scrolling, doesn't modify cursor position

### Future Optimization
Once migration is complete and stable:
- Could potentially remove terminal coords from model entirely
- View could compute cursor position during rendering
- Would eliminate `internal_to_terminal` calls from hot path (e.g., `insert_char`)

## Architecture Notes

### Coordinate Systems
- **Internal**: `(line_idx, grapheme_pos)` - logical position in text
- **Terminal**: `(row, display_col)` - position on screen after wrapping

### Why Internal Coords?
- Many functions need logical position (insert, delete, etc.)
- Terminal coords only needed for rendering
- Avoids O(n) `terminal_to_internal` conversion on every operation

### Wide Characters
- CJK characters, emoji have `display_width > 1`
- `cursor_pos` counts graphemes, not display width
- `Unicode_string.prefix_width` and `grapheme_at_width` handle conversions

---

# Continuation Logic Redesign

## Goal
Rewrite continuation logic from scratch. Current version is buggy and convoluted.

## Rules

### Indentation
- One level per unclosed `{`
- One level for operator/expression continuation

### Continuation Triggers
- Unclosed brackets (parens, brackets, braces)
- Trailing operator
- Trailing comma
- Control flow keyword without body (`if (x)`, `function(x)`, etc.)
- Mid-string (lexer mode != Normal)

## Flow

1. **Special case: cursor inside empty brackets**
   - Detect: token before cursor is `LEFT_*`, token after is matching `RIGHT_*`
   - Action: expand brackets with proper indentation
   ```
   {|} → Enter →
   {
     |
   }
   ```

2. **Collect tokens up to cursor**
   - All tokens from lines `0` to `cursor_line - 1`
   - Tokens from `cursor_line` that end before `cursor_pos`

3. **Fold over tokens with state machine**

## State Machine

```
State:
  parens: int              # balance of ()
  brackets: int            # balance of []
  braces: int              # balance of {}
  last_significant: token  # last non-whitespace/comment
  pending_control: option  # control flow awaiting body

PendingControl:
  needs_header: bool       # if/for/while/function need ()
  header_done: bool        # have we closed the ()?

Transitions:
  WHITESPACE | COMMENT -> no change

  LEFT_PAREN  -> parens++; if pending needs header, mark header started
  RIGHT_PAREN -> parens--; if pending header closes, mark header_done

  LEFT_BRACE  -> braces++; clear pending (brace is body)
  RIGHT_BRACE -> braces--

  LEFT_BRACKET  -> brackets++
  RIGHT_BRACKET -> brackets--

  KEYWORD (if|for|while|function) -> pending = {needs_header=true, header_done=false}
  KEYWORD (else|repeat)           -> pending = {needs_header=false, header_done=true}

  OPERATOR | PUNCTUATION "," -> just update last_significant

  expr (NUMBER|STRING|IDENT|...) -> if pending.header_done, clear pending (expr is body)

Final decision:
  unclosed = parens > 0 || brackets > 0 || braces > 0
  trailing = last_significant in {OPERATOR, open bracket, comma, control keyword}
  incomplete = pending != None

  if unclosed || trailing || incomplete:
    indent = braces + (1 if last is OPERATOR else 0)
    return Continue(indent * INDENT_SIZE)
  else:
    return Submit
```

## Token Collection

Need helper to get tokens up to cursor position:

```
tokens_before_cursor(model):
  # Lines before cursor line: all tokens
  prev_lines = model.lex_cache[0..cursor_line-1]
  prev_tokens = concat(line.tokens for line in prev_lines)

  # Current line: tokens ending before cursor_pos
  current_line_tokens = model.lex_cache[cursor_line].tokens
  # Need to track token positions to know which end before cursor
  tokens_before = filter by position < cursor_pos

  return prev_tokens @ tokens_before
```

**Note:** Tokens don't currently store position info. Options:
1. Compute positions by summing token lexeme lengths
2. Store positions in lex_cache
3. Re-lex current line up to cursor_pos
