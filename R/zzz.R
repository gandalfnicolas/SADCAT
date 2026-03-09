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

# SOCATS social category dimensions
.SOCATS_DIMS <- c("SOCAT", "OtherID", "Jobs", "Geographies",
                   "Race.ethn", "Race.Geo.US.lo.Status", "Race.Geo.US.hi.Status",
                   "Multiracial", "White", "Black", "Euro.Aus.Can.Russ",
                   "Native.American", "Middle.Eastern.muslim", "Jewish.Israeli",
                   "East.Asian", "South.Asian", "SE.Asian", "Latin.American",
                   "African", "US.American", "Immigrant.foreigner",
                   "Age", "Children", "Teenager", "Adult", "Elderly",
                   "Sexual.orientation", "SexualOR.minority", "SexualOR.majority",
                   "Gender.id.and.expression", "Men", "Women",
                   "Nonbinary.mix.ambiguous.gender", "Cis", "Trans",
                   "Other.Gender.id.express", "Other_sexOrGenderId")

# Negation patterns
.NEGATION_PATTERN <- "[Aa]nti-|[Nn]on-|[Bb]ad at |[Ll]imited |[Nn]ot |[Nn]o |[Nn]ever |[Nn]either |[Hh]ardly |[Ll]ess "
.NEGATION_PATTERN_VAL <- "[Aa]nti-|[Nn]on-"
.NEGATION_TOKEN_SEPARATOR <- "__"
.NEGATION_SCOPE_MAX_TOKENS <- 4L
.NEGATION_CLAUSE_BREAKERS <- c(".", ",", ";", ":", "!", "?", "but", "however", "though", "although", "yet")
.NEGATION_CUE_HEADS <- c("anti", "non", "not", "no", "never", "neither", "hardly", "less", "limited")

# Cache environment for memoized dictionary preparation
.sadcat_cache <- new.env(parent = emptyenv())
.negation_cache <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  # Package initialization
  # Cache environment is already created above
}
