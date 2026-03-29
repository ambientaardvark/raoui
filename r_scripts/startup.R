options(cli.num_colors = 256)
options(crayon.enabled = TRUE)

# Make modern MacTeX discoverable even when raoui is launched outside a login shell.
local({
    texbin <- "/Library/TeX/texbin"
    pdflatex <- file.path(texbin, "pdflatex")
    latex_cmd <- unname(Sys.which("pdflatex"))
    if (Sys.info()[["sysname"]] == "Darwin" &&
        file.exists(pdflatex) &&
        !nzchar(latex_cmd)) {
        current_path <- Sys.getenv("PATH")
        if (nzchar(current_path)) {
            Sys.setenv(PATH = paste(texbin, current_path, sep = .Platform$path.sep))
        } else {
            Sys.setenv(PATH = texbin)
        }
        latex_cmd <- unname(Sys.which("pdflatex"))
    }
    if (file.exists(pdflatex) &&
        identical(latex_cmd, pdflatex) &&
        is.null(getOption("tikzLatex"))) {
        options(tikzLatex = pdflatex)
    }
})

# Graphics helpers. OCaml owns startup policy and explicitly invokes one of
# these helpers after sourcing this file.
local({
    raoui_plot_counter <- 0L
    raoui_plot_registry <- new.env(parent = emptyenv())
    raoui_plot_dir <- Sys.getenv("RAOUI_PLOTS_DIR", tempdir())
    raoui_log_file <- Sys.getenv("RAOUI_LOG_FILE", "")
    raoui_dev_off_wrapped <- FALSE
    raoui_plot_callback_installed <- FALSE
    raoui_httpgd_opened <- FALSE

    plot_log <- function(...) {
        if (!nzchar(raoui_log_file)) {
            return(invisible(NULL))
        }
        message <- paste(..., collapse = "")
        timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        line <- sprintf("%s [R-PLOT] %s\n", timestamp, message)
        cat(line, file = raoui_log_file, append = TRUE)
        invisible(NULL)
    }

    if (!dir.exists(raoui_plot_dir)) {
        dir.create(raoui_plot_dir, recursive = TRUE, showWarnings = FALSE)
    }

    next_plot_path <- function() {
        raoui_plot_counter <<- raoui_plot_counter + 1L
        file.path(
            raoui_plot_dir,
            sprintf("plot-%06d.png", as.integer(raoui_plot_counter))
        )
    }

    next_live_plot_path <- function() {
        file.path(
            raoui_plot_dir,
            sprintf(".live-%06d.png", as.integer(raoui_plot_counter + 1L))
        )
    }

    notify_plot_ready <- function(path, width, height) {
        plot_log("emit image path=", path, " width=", width, " height=", height)
        .Call("raoui_emit_image", path, as.character(width), as.character(height), "image/png")
    }

    resolve_png_opener <- function(renderer) {
        if (identical(renderer, "gr_devices")) {
            plot_log("using grDevices::png renderer")
            return(function(path, width, height, ...) {
                plot_log("open live png path=", path, " width=", width, " height=", height)
                grDevices::png(path, width = width, height = height, ...)
            })
        }
        if (identical(renderer, "ragg")) {
            if (requireNamespace("ragg", quietly = TRUE)) {
                plot_log("using ragg::agg_png renderer")
                return(function(path, width, height, ...) {
                    plot_log("open live ragg png path=", path, " width=", width, " height=", height)
                    ragg::agg_png(path, width = width, height = height, units = "px", ...)
                })
            }
            plot_log("ragg requested but unavailable, falling back to grDevices::png")
            warning("plot_renderer is 'ragg' but ragg is not installed. Falling back to grDevices::png.\n",
                    "  Install it with: install.packages('ragg')", call. = FALSE)
            return(function(path, width, height, ...) {
                plot_log("open fallback png path=", path, " width=", width, " height=", height)
                grDevices::png(path, width = width, height = height, ...)
            })
        }
        plot_log("unknown renderer ", renderer, ", falling back to grDevices::png")
        warning(sprintf("Unknown Raoui PNG renderer '%s'. Falling back to grDevices::png.", renderer),
                call. = FALSE)
        function(path, width, height, ...) {
            plot_log("open fallback png path=", path, " width=", width, " height=", height)
            grDevices::png(path, width = width, height = height, ...)
        }
    }

    register_plot_device <- function(path, width, height, opener) {
        key <- as.character(grDevices::dev.cur())
        plot_log("register device dev=", key, " live_path=", path)
        raoui_plot_registry[[key]] <- list(
            path = path,
            width = width,
            height = height,
            opener = opener,
            last_snapshot = NULL
        )
    }

    snapshot_plot <- function(meta) {
        recorded <- tryCatch(recordPlot(), error = function(e) NULL)
        if (is.null(recorded)) {
            plot_log("recordPlot returned NULL")
            return(invisible(NULL))
        }
        snapshot <- serialize(recorded, NULL)
        if (!is.null(meta$last_snapshot) && identical(meta$last_snapshot, snapshot)) {
            plot_log("snapshot unchanged for live_path=", meta$path)
            return(invisible(NULL))
        }
        path <- next_plot_path()
        plot_log("snapshot changed, rendering artifact path=", path)
        meta$opener(path, meta$width, meta$height)
        tryCatch(
            {
                replayPlot(recorded)
                grDevices::dev.off()
            },
            error = function(e) {
                plot_log("snapshot replay failed: ", conditionMessage(e))
                try(grDevices::dev.off(), silent = TRUE)
                stop(e)
            }
        )
        if (!file.exists(path)) {
            plot_log("artifact missing after replay path=", path)
            return(invisible(NULL))
        }
        meta$last_snapshot <- snapshot
        notify_plot_ready(path, meta$width, meta$height)
        invisible(meta)
    }

    maybe_emit_plot_update <- function(dev_num) {
        key <- as.character(dev_num)
        if (!exists(key, envir = raoui_plot_registry, inherits = FALSE)) {
            plot_log("no registry entry for dev=", key)
            return(invisible(FALSE))
        }
        meta <- raoui_plot_registry[[key]]
        plot_log("checking plot update for dev=", key)
        updated <- snapshot_plot(meta)
        if (!is.null(updated)) {
            meta <- updated
            raoui_plot_registry[[key]] <- meta
            return(invisible(TRUE))
        }
        invisible(FALSE)
    }

    maybe_emit_current_plot <- function(dev_num) {
        key <- as.character(dev_num)
        emitted <- maybe_emit_plot_update(dev_num)
        if (exists(key, envir = raoui_plot_registry, inherits = FALSE)) {
            rm(list = key, envir = raoui_plot_registry)
        }
        emitted
    }

    # Wraps dev.off in .GlobalEnv; grDevices::dev.off() bypasses this wrapper.
    ensure_dev_off_wrapper <- function() {
        if (raoui_dev_off_wrapped) {
            return(invisible(NULL))
        }
        orig_dev_off <- grDevices::dev.off
        assign("dev.off", function(which = grDevices::dev.cur(), ...) {
            dev_num <- if (missing(which)) grDevices::dev.cur() else which
            plot_log("dev.off called for dev=", as.character(dev_num))
            result <- orig_dev_off(which = which, ...)
            maybe_emit_current_plot(dev_num)
            result
        }, envir = .GlobalEnv)
        raoui_dev_off_wrapped <<- TRUE
        invisible(NULL)
    }

    ensure_plot_task_callback <- function() {
        if (raoui_plot_callback_installed) {
            return(invisible(NULL))
        }
        if ("raoui-plot" %in% names(getTaskCallbackNames())) {
            removeTaskCallback("raoui-plot")
        }
        addTaskCallback(function(expr, value, ok, visible) {
            dev_num <- as.integer(grDevices::dev.cur())
            plot_log("task callback dev=", dev_num, " visible=", visible, " ok=", ok)
            if (dev_num > 1L) {
                maybe_emit_plot_update(dev_num)
            }
            TRUE
        }, name = "raoui-plot")
        plot_log("installed task callback raoui-plot")
        raoui_plot_callback_installed <<- TRUE
        invisible(NULL)
    }

    assign("raoui_after_top_level", function() {
        dev_num <- as.integer(grDevices::dev.cur())
        plot_log("after-top-level hook dev=", dev_num)
        if (dev_num > 1L) {
            maybe_emit_plot_update(dev_num)
        }
        invisible(TRUE)
    }, envir = .GlobalEnv)

    assign("raoui_use_png_device", function(renderer = "gr_devices") {
        plot_log("installing png device renderer=", renderer)
        opener <- resolve_png_opener(renderer)
        ensure_dev_off_wrapper()
        ensure_plot_task_callback()
        options(device = function(...) {
            path <- next_live_plot_path()
            width <- 480L
            height <- 360L
            opener(path, width, height, ...)
            grDevices::dev.control(displaylist = "enable")
            register_plot_device(path, width, height, opener)
        })
        plot_log("options(device=...) installed for png transport")
        invisible(TRUE)
    }, envir = .GlobalEnv)

    assign("raoui_use_httpgd", function(renderer_fallback = "gr_devices") {
        plot_log("installing httpgd transport")
        if (requireNamespace("httpgd", quietly = TRUE)) {
            options(device = function(...) {
                plot_log("opening httpgd device")
                httpgd::hgd(...)
                if (!raoui_httpgd_opened) {
                    plot_log("opening httpgd browser")
                    httpgd::hgd_browse()
                    raoui_httpgd_opened <<- TRUE
                }
            })
            return(invisible(TRUE))
        }
        warning("httpgd is not installed. Falling back to Raoui PNG transport.\n",
                "  Install it with: install.packages('httpgd')", call. = FALSE)
        plot_log("httpgd unavailable, falling back to png transport")
        raoui_use_png_device(renderer = renderer_fallback)
    }, envir = .GlobalEnv)

    assign("raoui_use_ide_viewer", function(renderer = "gr_devices") {
        plot_log("installing ide viewer transport")
        raoui_use_httpgd(renderer_fallback = renderer)
    }, envir = .GlobalEnv)
})

# Wrap system/system2 so the REPL can enter passthrough mode,
# and set a pager that does the same (for ?help, page(), etc.)
local({
    orig_system <- base::system
    orig_system2 <- base::system2
    unlockBinding("system", baseenv())
    unlockBinding("system2", baseenv())
    assign("system", function(command, ...) {
        .Call("raoui_enter_passthrough")
        on.exit(.Call("raoui_exit_passthrough"))
        orig_system(command, ...)
    }, envir = baseenv())
    assign("system2", function(command, ...) {
        .Call("raoui_enter_passthrough")
        on.exit(.Call("raoui_exit_passthrough"))
        orig_system2(command, ...)
    }, envir = baseenv())
    lockBinding("system", baseenv())
    lockBinding("system2", baseenv())

    options(pager = function(files, header, title, delete.file) {
        .Call("raoui_enter_passthrough")
        on.exit(.Call("raoui_exit_passthrough"))
        for (f in files) {
            orig_system(paste("less -R", shQuote(f)))
        }
        if (delete.file) file.remove(files)
    })
})

# VSCode integration
local({
    if (Sys.getenv("TERM_PROGRAM") == "vscode" && Sys.getenv("VSCODE_INIT_R") != "") {
        tryCatch({
            dir_init <- dirname(Sys.getenv("VSCODE_INIT_R"))

            # Cleanup previous session (if any)
            removeTaskCallback("vscode-R")
            options(vscodeR = NULL)
            if ("tools:vscode" %in% search()) {
                detach("tools:vscode", character.only = TRUE)
            }

            # Source vsc.R (parent=globalenv so functions can find grDevices::dev.cur etc)
            .vsc <- new.env(parent = globalenv())
            .vsc$dir_init <- dir_init
            source(file.path(dir_init, "vsc.R"), local = .vsc)

            # Create exports and attach
            exports <- local({
                .vsc <- .vsc
                .vsc.attach <- .vsc$attach
                .vsc.view <- .vsc$show_dataview
                .vsc.browser <- .vsc$show_browser
                .vsc.viewer <- .vsc$show_viewer
                .vsc.page_viewer <- .vsc$show_page_viewer
                View <- .vsc.view
                environment()
            })
            attach(exports, name = "tools:vscode", warn.conflicts = FALSE)

            # S3 overrides for help panel
            suppressWarnings({
                if (!identical(getOption("vsc.helpPanel", "Two"), FALSE)) {
                    .vsc$.S3method("print", "help_files_with_topic", .vsc$print.help_files_with_topic)
                    .vsc$.S3method("print", "hsearch", .vsc$print.hsearch)
                }
            })

            # Connect to VSCode
            exports$.vsc.attach()
        }, error = function(e) message("VSCode init error: ", conditionMessage(e)))
    }
})
