options(cli.num_colors = 256)
options(crayon.enabled = TRUE)

# Wrap system/system2 so the REPL can enter passthrough mode
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
