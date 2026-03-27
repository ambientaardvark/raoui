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

# Graphics: use httpgd (thread-safe) instead of Quartz (main-thread only)
local({
    plot_mode <- getOption("raoui.plot_mode", "auto")
    has_httpgd <- requireNamespace("httpgd", quietly = TRUE)

    setup_png_device <- function() {
        plot_dir <- Sys.getenv("RAOUI_PLOTS_DIR", tempdir())
        if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)
        plot_file <- file.path(plot_dir, "raoui_plot.png")
        options(device = function(...) {
            grDevices::png(plot_file, width = 800, height = 600, ...)
        })
    }

    if ((identical(plot_mode, "httpgd") || identical(plot_mode, "auto")) && has_httpgd) {
        opened <- FALSE
        options(device = function(...) {
            httpgd::hgd(...)
            if (!opened) {
                httpgd::hgd_browse()
                opened <<- TRUE
            }
        })
    } else if (identical(plot_mode, "httpgd")) {
        warning("raoui.plot_mode is 'httpgd' but httpgd is not installed. Falling back to PNG.\n",
                "  Install it with: install.packages('httpgd')", call. = FALSE)
        setup_png_device()
    } else if (identical(plot_mode, "auto")) {
        message("Install httpgd for plot support: install.packages('httpgd')")
        setup_png_device()
    } else if (identical(plot_mode, "png")) {
        setup_png_device()
    }
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
