Act as a product designer / API architect for the runner R package. Evaluate from the user's perspective:

## API Ergonomics
- Is the function name clear and discoverable? Does it follow runner's naming conventions?
- Are argument names intuitive? Do they match conventions users expect from base R / tidyverse?
- Are defaults sensible — does the common case require zero extra arguments?
- Is the function signature consistent with related functions in the package?

## Composability
- Does the feature work naturally with dplyr pipelines and grouped_df?
- Does it handle the input types runner supports (vector, data.frame, matrix, xts)?
- Are return types predictable and consistent with existing functions?

## Scope
- Does this feature belong in runner or is it better served by another package?
- Does it overlap with existing functionality? Can an existing function be extended instead?
- Is the feature general enough to justify inclusion in a CRAN package?

## User Experience
- What does the simplest usage look like? Show a minimal example.
- What are the most common mistakes a user will make? Can the API prevent them?
- Are error messages actionable — do they tell the user what to do, not just what went wrong?

Provide a recommendation: **proceed as-is**, **revise API**, or **reconsider scope**, with concrete suggestions.

## Clarification
If the issue is ambiguous, underspecified, or can be addressed in multiple ways:
- Post a comment on the issue asking the creator for clarification
- List the specific options or open questions clearly
- Do NOT proceed to implementation until the creator responds

## Next Step
If your recommendation is **proceed as-is** and there are no open questions, immediately follow up by executing the `/develop` role:
- Create a new branch from `main` named `feature/<short-description>`
- Implement the feature following all `/develop` guidelines
- Open a pull request back to `main` with the implementation
