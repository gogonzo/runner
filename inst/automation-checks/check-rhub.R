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
res <- do.call(rbind, lapply(statuses, function(this_status) {
  if (length(this_status[["result"]][["errors"]]) > 0) {
    print(this_status[["result"]][["errors"]])
  }
  if (length(this_status[["result"]][["warnings"]]) > 0) {
    print(this_status[["result"]][["warnings"]])
  }
  if (length(this_status[["result"]][["notes"]]) > 0) {
    print(this_status[["result"]][["notes"]])
  }

  data.frame(
    Plaform  = this_status[["platform"]][["name"]],
    Errors   = length(this_status[["result"]][["errors"]]),
    Warnings = length(this_status[["result"]][["warnings"]]),
    Notes    = length(this_status[["result"]][["notes"]]),
    stringsAsFactors = FALSE
  )
}))

print(res)

# Fail if any errors, warnings or notes found
if (any(colSums(res[c("Errors", "Warnings")]) > 0)) {
  stop("Some checks had errors, warnings or notes. See above for details.")
}
