args <- commandArgs(trailingOnly = TRUE)
out_path <- if (length(args) >= 1) args[[1]] else tempfile(fileext = ".svg")

if (!requireNamespace("svglite", quietly = TRUE)) {
  stop("svglite is not installed")
}

svglite::svglite(
  file = out_path,
  width = 600 / 96,
  height = 450 / 96,
  system_fonts = list(
    sans = "Arial",
    serif = "Times New Roman",
    mono = "Courier New"
  )
)

plot(mtcars$hp, mtcars$mpg, xlab = "hp", ylab = "mpg")
grDevices::dev.off()

cat(out_path, "\n", sep = "")
