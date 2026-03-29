args <- commandArgs(trailingOnly = TRUE)
out_path <- if (length(args) >= 1) args[[1]] else tempfile(fileext = ".svg")

grDevices::svg(
  filename = out_path,
  width = 600 / 96,
  height = 450 / 96,
  pointsize = 12,
  onefile = FALSE
)

plot(mtcars$hp, mtcars$mpg, xlab = "hp", ylab = "mpg")
grDevices::dev.off()

cat(out_path, "\n", sep = "")
