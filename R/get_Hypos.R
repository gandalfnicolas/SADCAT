#' @title Wordnet hyponym retriever
#'
#' @description Gets the synsets/words (depending on \code{Syns}) of a term's
#'   hyponym hierarchy. Expansion runs level by level, and synsets visited at
#'   earlier levels are skipped on subsequent passes, so no synset is
#'   expanded twice. This deduplication prevents the combinatorial blow-up
#'   that earlier versions could exhibit when seed terms sat near the top of
#'   WordNet's hyponym graph (e.g., general or abstract nouns whose subtrees
#'   contain many thousands of synsets reachable via multiple inheritance
#'   paths).
#'
#' @param synsets synsets to obtain hyponyms for. May be a single synset or a
#'   list of synsets.
#' @param Syns Return the synsets? Defaults to TRUE. If FALSE, returns the
#'   various words that make up the synsets.
#' @param max_depth Maximum levels of hyponym recursion. Default \code{Inf}
#'   (expand until natural termination, still bounded by the visited-set
#'   deduplication). Set to a finite integer to cap worst-case runtime when
#'   working with very general seed terms.
#' @param verbose If TRUE, prints one progress message per expansion level
#'   showing the depth and number of new hyponym synsets retrieved. Default
#'   FALSE.
#' @return hyponym hierarchy (list of synsets if \code{Syns=TRUE}, otherwise
#'   the words drawn from those synsets)
#' @export get_Hypos

get_Hypos = function(synsets, Syns = TRUE, max_depth = Inf, verbose = FALSE){
  tryCatch({
    if (length(synsets) == 0L) {
      return(if (isFALSE(Syns)) character(0) else list())
    }

    visited <- new.env(hash = TRUE, parent = emptyenv())
    visit_key <- function(s) {
      w <- tryCatch(wordnet::getWord(s), error = function(e) NULL)
      if (is.null(w) || length(w) == 0L) return(NA_character_)
      paste(sort(unique(w)), collapse = "|")
    }

    collected <- list()
    frontier  <- if (is.list(synsets)) synsets else list(synsets)
    depth     <- 0L

    while (length(frontier) > 0L && depth < max_depth) {
      depth <- depth + 1L

      keys <- vapply(frontier, visit_key, character(1))
      keep <- !is.na(keys) &
        !vapply(keys,
                function(k) exists(k, envir = visited, inherits = FALSE),
                logical(1))
      frontier <- frontier[keep]
      for (k in keys[keep]) assign(k, TRUE, envir = visited)
      if (length(frontier) == 0L) break

      next_level <- unlist(lapply(frontier, function(s) {
        tryCatch(wordnet::getRelatedSynsets(s, pointerSymbol = "~"),
                 error = function(e) list())
      }))
      if (length(next_level) == 0L) break

      if (isTRUE(verbose)) {
        message(sprintf("get_Hypos: depth %d, %d new hyponym synsets",
                        depth, length(next_level)))
      }

      collected <- c(collected, next_level)
      frontier  <- next_level
    }

    if (isFALSE(Syns)) {
      return(unlist(lapply(collected, wordnet::getWord)))
    }
    collected
  },
  error = function(e) {
    message("ERROR in get_Hypos: ", conditionMessage(e))
    NA
  })
}
