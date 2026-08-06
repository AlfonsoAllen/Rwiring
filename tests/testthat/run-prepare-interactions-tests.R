# Standalone runner for the current scripts in /mnt/data.
# Adjust paths if the files are inside an R package.

required_package <- "testthat"
if (!requireNamespace(required_package, quietly = TRUE)) {
  stop(
    "Package `testthat` is required. Install it with install.packages('testthat').",
    call. = FALSE
  )
}

scripts <- c(
  "/mnt/data/validate_flag(1).R",
  "/mnt/data/validation-data(2).R",
  "/mnt/data/validation-guilds(3).R",
  "/mnt/data/validation-strength(2).R",
  "/mnt/data/transform-links(2).R",
  "/mnt/data/temporal-interactions-class(3).R",
  "/mnt/data/prepare_interactions(2).R"
)

missing_scripts <- scripts[!file.exists(scripts)]
if (length(missing_scripts) > 0L) {
  stop(
    "Missing script files: ",
    paste(missing_scripts, collapse = ", "),
    call. = FALSE
  )
}

invisible(lapply(scripts, source))

testthat::test_file(
  "/mnt/data/test-prepare-interactions.R",
  reporter = "summary"
)
