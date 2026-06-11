test_that("every exported function has a clean-session smoke case", {
  skip_if_not_installed("callr")
  skip_if_not_installed("pkgload")

  smoke <- callr::r(
    function(project, libpath) {
      .libPaths(libpath)
      pkgload::load_all(project, export_all = FALSE, helpers = FALSE, quiet = TRUE)

      skip_case <- function(reason) {
        list(status = "skip", message = reason)
      }

      run_case <- function(fun) {
        tryCatch({
          out <- fun()
          if (is.list(out) && identical(out$status, "skip")) {
            out
          } else {
            list(status = "ok", message = NULL)
          }
        }, error = function(e) {
          list(status = "error", message = conditionMessage(e))
        })
      }

      wordnet_ready <- function() {
        path <- Sys.getenv("SADCAT_WORDNET_DICT", "")
        nzchar(path) && dir.exists(path)
      }

      with_wordnet <- function(code) {
        if (!wordnet_ready()) {
          return(skip_case("WordNet dictionary not configured"))
        }
        wordnet::setDict(Sys.getenv("SADCAT_WORDNET_DICT"))
        code()
      }

      treetagger_ready <- function() {
        identical(tolower(Sys.getenv("SADCAT_RUN_TREETAGGER_TESTS", "false")), "true") &&
          dir.exists("C:\\treetagger") &&
          requireNamespace("koRpus.lang.en", quietly = TRUE)
      }

      with_treetagger <- function(code) {
        if (!treetagger_ready()) {
          return(skip_case("TreeTagger not configured"))
        }
        code()
      }

      suggest_ready <- function(pkgs) {
        all(vapply(pkgs, requireNamespace, logical(1), quietly = TRUE))
      }

      cases <- list(
        Code_sent = function() {
          out <- Code_sent(data.frame(rawword = "warm", word = "warm", stringsAsFactors = FALSE))
          stopifnot(is.data.frame(out))
        },
        Cohens_d_EMmeans = function() {
          if (!suggest_ready(c("emmeans", "multcomp", "lme4"))) {
            return(skip_case("emmeans/multcomp/lme4 not installed"))
          }
          dat <- transform(lme4::sleepstudy, day_group = factor(ifelse(Days < 5, "early", "late")))
          mod <- lme4::lmer(Reaction ~ day_group + (1 | Subject), data = dat)
          emm <- emmeans::emmeans(mod, ~ day_group)
          pairwises <- summary(pairs(emm))
          out <- Cohens_d_EMmeans(mod, pairwises)
          stopifnot(is.data.frame(out))
        },
        Full_Expand = function() {
          with_wordnet(function() {
            out <- Full_Expand(data.frame(term = "good", PoS = "ADJECTIVE", sense = NA, stringsAsFactors = FALSE), antonym = TRUE, syns = FALSE)
            stopifnot(length(out) >= 1)
          })
        },
        Full_preprocess = function() {
          with_treetagger(function() {
            out <- Full_preprocess("warm", parallelize = FALSE, print = FALSE)
            stopifnot(is.character(out), length(out) == 1)
          })
        },
        Lemmatize = function() {
          with_treetagger(function() {
            out <- Lemmatize("dogs", print = FALSE)
            stopifnot(is.character(out), length(out) == 1)
          })
        },
        Spellcheck = function() {
          out <- Spellcheck("warm", "warm", unique(SADCAT::All.steps_Dictionaries$values3), c("warm"))
          stopifnot(is.character(out), length(out) == 1)
        },
        Spellcheck2 = function() {
          out <- Spellcheck2("warm", rawlist = c("warm"))
          stopifnot(is.character(out), length(out) == 1)
        },
        aggregate_responses = function() {
          dat <- data.frame(
            Synonym.GroupX = c("a", "a"),
            Group = c("g", "g"),
            Level = c("l", "l"),
            warmth_dic_binary2 = c(1, 0),
            ValenceYesNA = c(1, -1),
            ValenceNoNA = c(1, -1),
            Warmth_Valence = c(1, NA),
            Warmth_valenceStrictNA = c(1, NA),
            Warmth_valenceNoNA = c(1, 0),
            Warmth_dirx3 = c(1, NA),
            stringsAsFactors = FALSE
          )
          out <- aggregate_responses(dat, verbose = FALSE)
          stopifnot(is.data.frame(out))
        },
        cldtable = function() {
          if (!suggest_ready(c("emmeans", "multcomp", "lme4"))) {
            return(skip_case("emmeans/multcomp/lme4 not installed"))
          }
          dat <- transform(lme4::sleepstudy, day_group = factor(ifelse(Days < 5, "early", "late")))
          mod <- lme4::lmer(Reaction ~ day_group + (1 | Subject), data = dat)
          emm <- emmeans::emmeans(mod, ~ day_group)
          out <- cldtable(mod, emm)
          stopifnot(is.data.frame(out))
        },
        clean_large_text = function() {
          out <- clean_large_text("It's warm.")
          stopifnot(is.character(out), length(out) == 1)
        },
        clean_naresponses = function() {
          out <- clean_naresponses("NA")
          stopifnot(is.character(out), length(out) == 1, is.na(out))
        },
        clean_symbols = function() {
          out <- clean_symbols("warm!")
          stopifnot(is.character(out), length(out) == 1)
        },
        compute_embeddings = function() {
          out <- compute_embeddings(
            data.frame(response = NA_character_, tv = NA_character_, stringsAsFactors = FALSE),
            methods = "gemini",
            verbose = FALSE
          )
          stopifnot(is.data.frame(out))
        },
        compute_seed_similarities = function() {
          out <- compute_seed_similarities(
            data.frame(response = "x", SBERT_1 = 1, SBERT_2 = 0),
            seed_vectors = data.frame(Dictionary = "Warmth", SBERT_1 = 1, SBERT_2 = 0),
            verbose = FALSE
          )
          stopifnot(is.data.frame(out))
        },
        cosinize = function() {
          out <- cosinize(c(1, 0), matrix(c(1, 0, 0, 1), nrow = 2))
          stopifnot(is.numeric(out))
        },
        delete_ending_Ss = function() {
          out <- delete_ending_Ss("dogs")
          stopifnot(is.character(out), length(out) == 1)
        },
        euclideanize = function() {
          out <- euclideanize(c(1, 0), matrix(c(1, 0, 0, 1), nrow = 2))
          stopifnot(is.numeric(out))
        },
        get_Adj_Expansion = function() {
          with_wordnet(function() {
            syn <- get_Syns("good", PoS = "ADJECTIVE")
            out <- get_Adj_Expansion(syn)
            stopifnot(length(out) >= 1 || all(is.na(out)))
          })
        },
        get_Antonyms = function() {
          with_wordnet(function() {
            syn <- get_Syns("good", PoS = "ADJECTIVE")
            out <- get_Antonyms(syn)
            stopifnot(length(out) >= 1 || all(is.na(out)))
          })
        },
        get_Hypos = function() {
          with_wordnet(function() {
            syn <- get_Syns("dog", PoS = "NOUN")
            out <- get_Hypos(syn)
            stopifnot(length(out) >= 1 || all(is.na(out)))
          })
        },
        get_Member_holonym = function() {
          with_wordnet(function() {
            syn <- get_Syns("tree", PoS = "NOUN")
            out <- get_Member_holonym(syn)
            stopifnot(length(out) >= 1 || all(is.na(out)))
          })
        },
        get_Part_holonym = function() {
          with_wordnet(function() {
            syn <- get_Syns("car", PoS = "NOUN")
            out <- get_Part_holonym(syn)
            stopifnot(length(out) >= 1 || all(is.na(out)))
          })
        },
        get_Syns = function() {
          with_wordnet(function() {
            out <- get_Syns("dog", PoS = "NOUN")
            stopifnot(length(out) >= 1 || all(is.na(out)))
          })
        },
        get_derivrelto = function() {
          with_wordnet(function() {
            syn <- get_Syns("runner", PoS = "NOUN")
            out <- get_derivrelto(syn)
            stopifnot(length(out) >= 1 || all(is.na(out)))
          })
        },
        get_part_meronyms = function() {
          with_wordnet(function() {
            syn <- get_Syns("car", PoS = "NOUN")
            out <- get_part_meronyms(syn)
            stopifnot(length(out) >= 1 || all(is.na(out)))
          })
        },
        match_dictionaries = function() {
          dat <- data.frame(response = "warm", tv = "warm", tv3 = "warm", stringsAsFactors = FALSE)
          scored <- score_valence(dat, text_col = "tv", response_col = "response")
          out <- match_dictionaries(scored, text_col = "tv3", response_col = "response", valence_col = "ValenceYesNA", valence_nona_col = "ValenceNoNA")
          stopifnot(is.data.frame(out))
        },
        prepare_sadcat_dictionaries = function() {
          out <- prepare_sadcat_dictionaries(cache = FALSE)
          stopifnot(inherits(out, "dictionary") || inherits(out, "dictionary2"))
        },
        prepare_socats_dictionaries = function() {
          out <- prepare_socats_dictionaries(cache = FALSE)
          stopifnot(inherits(out, "dictionary") || inherits(out, "dictionary2"))
        },
        preprocess_text = function() {
          out <- preprocess_text(data.frame(responsex = c("Warm", ""), stringsAsFactors = FALSE), spellcheck = FALSE, singularize = FALSE, verbose = FALSE)
          stopifnot(is.data.frame(out))
        },
        process_responses = function() {
          dat <- data.frame(responsex = c("warm", NA), response = c("warm", NA), stringsAsFactors = FALSE)
          out <- process_responses(dat, stages = c("preprocess", "valence", "dictionaries"), spellcheck = FALSE, singularize_text = FALSE, verbose = FALSE)
          stopifnot(is.list(out), "long" %in% names(out))
        },
        score_valence = function() {
          out <- score_valence(data.frame(response = "warm", tv = "warm", stringsAsFactors = FALSE), text_col = "tv", response_col = "response")
          stopifnot(is.data.frame(out))
        },
        singularize2 = function() {
          out <- singularize2("dogs", dictionary = FALSE)
          stopifnot(is.character(out), length(out) == 1)
        }
      )

      results <- lapply(cases, run_case)
      list(exports = sort(getNamespaceExports("SADCAT")), results = results)
    },
    args = list(normalizePath("."), .libPaths()),
    libpath = .libPaths()
  )

  expect_setequal(names(smoke$results), smoke$exports)

  error_cases <- Filter(function(x) identical(x$status, "error"), smoke$results)
  expect_equal(
    length(error_cases),
    0,
    info = paste(
      paste0(names(error_cases), ": ", vapply(error_cases, `[[`, "", "message")),
      collapse = "\n"
    )
  )
})
