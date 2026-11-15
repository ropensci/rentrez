<!-- Start the title with a conventional-commit prefix, or the pr-title check
     fails. The prefixes are build, chore, ci, docs, feat, fix, perf, refactor,
     revert, style and test. For example:

         fix: handle an esearch result with no hits
-->

<!-- Never paste an NCBI API key here, or any part of one. rentrez reads it
     from the ENTREZ_KEY environment variable, so nothing in a pull request
     needs it. -->

## Description

<!-- What the change does, and why. -->

## Related issue

<!-- If this closes an issue, say so: "closes #N". For several, repeat the
     word for each one, "closes #N, closes #M", since GitHub closes only the
     first issue in "closes #N, #M". If it only relates to an issue, mention
     the number. -->

## Example

<!-- For a new function, or a change to what an existing one returns, show the
     new behaviour briefly. -->

## Tests

<!-- Tests for the change, unless it only touches wording.

     Anything that calls NCBI goes through net() or skip_without_ncbi(), so a
     bad day at NCBI skips instead of failing the build, while a real bug still
     fails. See tests/testthat/helper-ncbi.R. A test file that calls NCBI also
     starts with skip_on_cran(), so CRAN and r-universe run only the tests
     that need no network. -->
