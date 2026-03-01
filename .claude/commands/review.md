Act as a code reviewer for the runner R package. Review the recent changes with these priorities:

## CRAN Compliance
- No `browser()`, `debug()`, or `debugonce()` calls left in code
- No `cat()` or `print()` without `verbose` / `quiet` argument gating
- No absolute file paths or platform-specific assumptions
- Examples must run in < 5 seconds (use `\donttest{}` for slow ones)
- No installed.packages() usage

## Backward Compatibility
- Check that existing exported function signatures are not broken
- Verify default argument values are preserved
- Flag any changes to return types or class attributes

## Documentation
- Every exported function has `@export` and complete `@param` / `@return` tags
- Examples are present and correct
- NEWS.md entry exists for user-visible changes
- DESCRIPTION version is bumped if needed

## Tests
- New or changed functionality has corresponding tinytest tests
- Edge cases are covered (empty input, NA, single element, zero-length window)
- Tests do not depend on external resources or specific locale

## C++ / Performance
- No memory leaks or unprotected SEXPs
- Rcpp attributes are correct (`// [[Rcpp::export]]`)
- Bounds checking on vector access
- No unnecessary copies of large objects

Summarize findings with severity: **blocker**, **should fix**, or **nit**.
