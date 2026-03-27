# Configuration

Raoui keeps configuration, state, and cache in separate directories.

## Paths

Raoui uses XDG-style locations when the corresponding environment variables are set, and otherwise falls back to paths under `HOME`.

- Config directory: `$XDG_CONFIG_HOME/raoui` or `~/.config/raoui`
- State directory: `$XDG_STATE_HOME/raoui` or `~/.local/state/raoui`
- Cache directory: `$XDG_CACHE_HOME/raoui` or `~/.cache/raoui`

Default files and subdirectories:

- User options: `options.R`
- History: `history`
- Log file: `raoui.log`
- Plot artifacts: `plots/`

With default fallbacks, that becomes:

- `~/.config/raoui/options.R`
- `~/.local/state/raoui/history`
- `~/.local/state/raoui/raoui.log`
- `~/.cache/raoui/plots/`

## User Options

If `options.R` exists, Raoui sources it during startup before applying runtime policy such as graphics device selection.

The supported contract is to set Raoui options with `options(...)`, for example:

```r
options(
  raoui.theme = "tokyo_night",
  raoui.plot_mode = "auto",
  raoui.plot_open = "auto",
  raoui.plot_keep_files = FALSE
)
```

Built-in themes currently include:

- `default`
- `tokyo_night`

`options.R` is the primary user-facing configuration surface. It is intended for preferences, not for replacing the bundled startup logic.

## Precedence

Raoui currently resolves filesystem locations from XDG environment variables and built-in defaults, then exports the resolved paths into the R session through environment variables such as `RAOUI_OPTIONS_FILE`, `RAOUI_HISTORY_FILE`, `RAOUI_LOG_FILE`, and `RAOUI_PLOTS_DIR`.

Runtime preferences are expected to come from user `options.R` plus built-in defaults. CLI overrides may be added later, but they are not part of the current interface.
