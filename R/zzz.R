# Package-level constants and initialization
# These are internal to the package and not exported

# Primary SADCAT dimensions (those with hi/lo directional variants)
.SADCAT_DIR_DIMS <- c("Sociability", "Morality", "Ability", "Assertiveness",
                      "Status", "Warmth", "Competence", "Beliefs",
                      "Health", "Beauty", "Deviance")

# Non-directional dimensions (binary-only, no hi/lo variants)
.SADCAT_NDIR_DIMS <- c("Occupation", "Emotion", "Family", "Socialgroups",
                       "Geography", "Appearance", "Other", "OtherwFam")

# All dimensions
.SADCAT_ALL_DIMS <- c(.SADCAT_DIR_DIMS, .SADCAT_NDIR_DIMS)

# Negation patterns
.NEGATION_PATTERN <- "[Aa]nti-|[Nn]on-|[Bb]ad at |[Ll]imited |[Nn]ot |[Nn]o |[Nn]ever |[Nn]either |[Hh]ardly |[Ll]ess "
.NEGATION_PATTERN_VAL <- "[Aa]nti-|[Nn]on-"

# Cache environment for memoized dictionary preparation
.sadcat_cache <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  # Package initialization
  # Cache environment is already created above
}
