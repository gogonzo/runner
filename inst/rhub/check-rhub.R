# Retrieve passed command line arguments
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
  stop("Incorrect number of args, needs 1: platform (string)")
}
platform <- args[[1L]]

# Check if passed platform is valid
if (!is.element(platform, rhub::platforms()[[1L]])) {
  stop(paste(
    "Given platform not in rhub::platforms()[[1L]]:",
    platform
  ))
}

# Run the check on the selected platform
# Use show_status = TRUE to wait for results
out <- capture.output({
  cr <- rhub::check(
    platform = platform,
    check_args = "--no-build-vignettes",
    show_status = TRUE
  )
})

# Get the statuses from private field status_
statuses <- cr[[".__enclos_env__"]][["private"]][["status_"]]

# Create and print a data frame with results
res <- do.call(rbind, lapply(statuses, function(thisStatus) {
  if (length(thisStatus[["result"]][["errors"]]) > 0) {
    print(thisStatus[["result"]][["errors"]])
  }
  if (length(thisStatus[["result"]][["warnings"]]) > 0) {
    print(thisStatus[["result"]][["warnings"]])
  }
  if (length(thisStatus[["result"]][["notes"]]) > 0) {
    print(thisStatus[["result"]][["notes"]])
  }

  data.frame(
    Plaform  = thisStatus[["platform"]][["name"]],
    Errors   = length(thisStatus[["result"]][["errors"]]),
    Warnings = length(thisStatus[["result"]][["warnings"]]),
    Notes    = length(thisStatus[["result"]][["notes"]]),
    stringsAsFactors = FALSE
  )
}))

print(res)

# Fail if any errors, warnings or notes found
if (any(colSums(res[c("Errors", "Warnings")]) > 0)) {
  stop("Some checks had errors, warnings or notes. See above for details.")
}
