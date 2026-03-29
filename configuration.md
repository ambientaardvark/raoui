# Configuration

Raoui keeps configuration, state, and cache in separate directories.

## Paths

Raoui uses XDG-style locations when the corresponding environment variables are
set, and otherwise falls back to paths under `HOME`.

- Config directory: `$XDG_CONFIG_HOME/raoui` or `~/.config/raoui`
- State directory: `$XDG_STATE_HOME/raoui` or `~/.local/state/raoui`
- Cache directory: `$XDG_CACHE_HOME/raoui` or `~/.cache/raoui`

Default files and subdirectories:

- User config: `config.toml`
- History: `history`
- Log file: `raoui.log`
- Plot artifacts: `plots/`

With default fallbacks, that becomes:

- `~/.config/raoui/config.toml`
- `~/.local/state/raoui/history`
- `~/.local/state/raoui/raoui.log`
- `~/.cache/raoui/plots/`

Plot cache entries live in per-session directories under `plots/`, named with
the Raoui process pid and startup time. On normal shutdown, Raoui removes plot
session directories older than 48 hours when their pid is no longer active.

## Config File

Raoui reads `config.toml` on startup. Unknown or invalid values fall back to
built-in defaults.

Example:

```toml
theme = "tokyo_night"
plot_mode = "auto"
plot_renderer = "gr_devices"
inline_image_max_width_cols = 150
inline_image_max_height_rows = 30
```

Supported keys:

- `theme`: `default` or `tokyo_night`
- `plot_mode`: `auto`, `png`, `httpgd`, `ide`, or `off`
- `plot_renderer`: `gr_devices` or `ragg`
- `inline_image_max_width_cols`: positive integer column cap for inline images
  Default: `150`
- `inline_image_max_height_rows`: positive integer row cap for inline images
  Default: `30`

`plot_mode = "auto"` currently means:

- IDE session: prefer IDE/httpgd behavior
- Kitty/Ghostty terminal: prefer Raoui's SVG-first transport with inline image
  output
- Other terminals: prefer `httpgd`, falling back to Raoui's SVG/PNG transport
  when needed

When the SVG transport is available, Raoui uses a transient SVG internally and
keeps a durable high-resolution PNG artifact for external opening. Inline
terminal rendering uses a smaller preview PNG and prints a clickable `open
plot` banner pointing at the durable PNG.

## Runtime Paths

Raoui exports the resolved runtime paths into the R session:

- `RAOUI_CONFIG_DIR`
- `RAOUI_STATE_DIR`
- `RAOUI_CACHE_DIR`
- `RAOUI_OPTIONS_FILE`
- `RAOUI_HISTORY_FILE`
- `RAOUI_LOG_FILE`
- `RAOUI_PLOTS_DIR`

These are implementation details for the bundled startup helpers, not the
primary user configuration interface.
