#' @title Lemmatizer
#'
#' @description This function allows you lemmatize words using the treetag lemmatizer
#' @param word word to lemmatize, if multiple use loop/apply/dplyr
#' @param print Whether to print word being lemmatized. Useful for long lists of words. Defaults to TRUE
#' @param debug Whether to pass debug flag to treetag. Defaults to FALSE
#' @param treetagger_path Optional TreeTagger installation directory. If NULL, tries
#' environment variables, PATH, and common install locations across Windows/macOS/Linux.
#' @return lemmatized words
#' @export Lemmatize


Lemmatize = function(word, print =T, debug = F, treetagger_path = NULL){
  normalize_candidate <- function(path) {
    if (is.null(path) || length(path) == 0) {
      return(NULL)
    }

    path <- as.character(path)[1]
    if (is.na(path) || !nzchar(path)) {
      return(NULL)
    }

    path <- path.expand(path)
    path <- gsub("\\\\", "/", path)
    path <- sub("/+$", "", path)

    if (tolower(basename(path)) == "bin") {
      path <- dirname(path)
    }

    if (grepl("^tree-tagger(\\.exe)?$", tolower(basename(path)))) {
      path <- dirname(dirname(path))
    }

    path
  }

  has_treetagger_bin <- function(path) {
    if (is.null(path) || !nzchar(path)) {
      return(FALSE)
    }
    dir.exists(file.path(path, "bin"))
  }

  resolve_treetagger_path <- function(path_override = NULL) {
    override <- normalize_candidate(path_override)
    if (!is.null(override) && has_treetagger_bin(override)) {
      options(SADCAT.treetagger_path = override)
      return(override)
    }

    cached <- normalize_candidate(getOption("SADCAT.treetagger_path", ""))
    if (!is.null(cached) && has_treetagger_bin(cached)) {
      return(cached)
    }

    path_hits <- Sys.which(c("tree-tagger", "tree-tagger.exe"))
    path_hits <- unname(path_hits[nzchar(path_hits)])
    path_root <- if (length(path_hits) > 0) dirname(dirname(path_hits[[1]])) else ""

    env_candidates <- c(
      Sys.getenv("SADCAT_TREETAGGER_PATH", ""),
      Sys.getenv("TREETAGGER_HOME", ""),
      Sys.getenv("TREETAGGER_PATH", "")
    )

    common_candidates <- c(
      "C:/treetagger",
      "~/treetagger",
      "/usr/local/treetagger",
      "/opt/treetagger",
      "/opt/homebrew/opt/treetagger",
      "/opt/homebrew/treetagger",
      "/Applications/treetagger",
      "/Applications/TreeTagger"
    )

    candidates <- unique(c(path_root, env_candidates, common_candidates))
    candidates <- unlist(lapply(candidates, normalize_candidate), use.names = FALSE)
    candidates <- unique(candidates[!is.na(candidates) & nzchar(candidates)])

    for (candidate in candidates) {
      if (has_treetagger_bin(candidate)) {
        options(SADCAT.treetagger_path = candidate)
        return(candidate)
      }
    }

    stop(
      "TreeTagger was not found. Set `treetagger_path`, ",
      "`SADCAT_TREETAGGER_PATH`, `TREETAGGER_HOME`, or `TREETAGGER_PATH` ",
      "to your TreeTagger install directory (the folder that contains `bin`)."
    )
  }

  if (print == T){
    print(word)}
  if(!is.na(word)){
    if(word == ""){
      return("")}}
    tt_path <- resolve_treetagger_path(treetagger_path)
    lemmax = koRpus::treetag(as.character(word), treetagger="manual", format="obj", debug = debug, TT.tknz=T, lang="en", TT.options=list(path=tt_path, preset="en"))
  if(lemmax@tokens[["lemma"]] == "<unknown>"){
    if (print == T){
    print(lemmax@tokens[["token"]])}
    return (lemmax@tokens[["token"]])}
  else{
    if (print == T){
    print(lemmax@tokens[["lemma"]])}
    return(lemmax@tokens[["lemma"]])
}}

