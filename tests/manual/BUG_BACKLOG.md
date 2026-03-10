# SADCAT Bug Backlog

Confirmed issues or follow-up items discovered during the hardening pass:

- `sadcat_pipeline_functions.R` still duplicates core package functions and can drift from the package implementation. Use `tests/manual/audit-standalone-parity.R` after package-side changes.
- The standalone script still depends on attached-package side effects more than the package does. The parity audit now reports those failures explicitly instead of crashing silently.
- Several legacy helpers depend on external WordNet, Java, or TreeTagger paths hard-coded for Windows (`C:\\dict`, `C:\\treetagger`, `C:\\Program Files\\Java\\jre-1.8`). They are now covered by opt-in integration lanes, but portability is still limited.
- Source-directory `R CMD check` can still be polluted by local artifacts if `nul` or `..Rcheck` exist in the repo root. Prefer checking a built tarball or remove those artifacts first.
- `R CMD check --no-manual` on the built tarball now passes without `ERROR`s, but two warnings remain: `License: NA` is non-standard, and `Obituary_data` still contains non-ASCII strings.
