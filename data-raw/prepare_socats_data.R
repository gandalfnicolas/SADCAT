# Prepare SOCATS_Dictionaries data object for the SADCAT package
# Run this script to regenerate data/SOCATS_Dictionaries.rda

SOCATS_Dictionaries <- read.csv("data-raw/SOCATS_dictionaries_102422.csv",
                                 header = TRUE, stringsAsFactors = FALSE)

usethis::use_data(SOCATS_Dictionaries, overwrite = TRUE, compress = "xz")
