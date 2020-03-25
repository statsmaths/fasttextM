#' Available Languages
#'
#' This function prints a data frame of the available languages,
#' language codes, and which ones are alread downloaded.
#'
#' @param  location   Path to directory where models should be saved. Defaults
#'                    to the directory where the package is installed.
#'
#' @return A data frame of available languages, along with
#'         information about which ones are already downloaded.
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#'
#' @examples
#'
#'ft_languages()
#'
#' @export
ft_languages <- function(location = NULL) {

  fname <- sprintf(
    "%s/language_codes.csv",
    system.file("extdata", package="fasttextM")
  )
  meta <- utils::read.csv(fname, as.is = TRUE)
  meta$installed <- ""
  meta$loaded <- ""

  if (is.null(location))
  {
    location <- system.file("extdata", package="fasttextM")
  }
  i_langs <- substr(dir(location, pattern = ".Rds$"), 1, 2)
  l_langs <- names(volatiles)

  meta$installed[!is.na(match(meta$iso_code, i_langs))] <- "*"
  meta$loaded[!is.na(match(meta$iso_code, l_langs))] <- "*"

  return(meta)
}

#' Load Fasttext Model Files
#'
#' Loads the model files into memory for the specified
#' language. You must first download the respective
#' language file using \code{\link{ft_download_model}}.
#'
#' @param  lang       the two letter language code
#'                    specifying the language you would
#'                    like to download. See the function
#'                    \code{\link{ft_languages}} for a
#'                    complete list of available choices.
#'
#' @param  location   Path to directory where models should be saved. Defaults
#'                    to the directory where the package is installed.
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#'
#' @examples
#'ft_load_model(lang = "en")
#'
#' @export
ft_load_model <- function(lang = "en", location = NULL) {

  if (is.null(location))
  {
    location <- system.file("extdata", package="fasttextM")
  }

  fname <- sprintf("%s/%s.Rds", location, lang)
  volatiles[[lang]] <- readRDS(fname)

  invisible(0L)
}

#' Word Embeddings
#'
#' Given a vector words, this function returns the
#' 300-dimensional embedding of each element. The
#' embedding is given in reference to a particular
#' language. The model must have been loaded
#' using the function \code{\link{ft_load_model}}.
#'
#'
#' @param  words      a character vector of the words
#'                    for which an embedding should be
#'                    returned
#'
#' @param  lang       the two letter language code
#'                    specifying the language to use for
#'                    the embedding
#'
#' @return A matrix with three hundred columns and one row
#'         for each word. Rownames record the input words.
#'         Rows corresponding the words not in the embedding
#'         are set to \code{NA}
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#'
#' @examples
#'ft_load_model(lang = "en")
#'ft_embed(c("the", "and", "I", "banana"), lang = "en")
#'
#' @export
ft_embed <- function(words, lang = "en") {

  if (!(lang %in% names(volatiles))) {
    stop(sprintf("language '%s' has not yet been loaded; ", lang),
         sprintf("call ft_load_model('%s') and try again", lang))
  }

  # create an empty matrix of missing values
  output <- matrix(NA_real_, ncol = 300, nrow = length(words))

  # determine which words match the vocabulary
  words_lower <- stringi::stri_trans_tolower(words)
  id <- !is.na(match(words_lower, rownames(volatiles[[lang]])))

  # for the matches, fill in with the embedding values
  if (any(id)) {
    output[id,] <- volatiles[[lang]][words_lower[id],]
  }

  return(output)
}

#' Nearest Neighbors
#'
#' Given a vector words, this function returns the
#' nearest neighbors in the vector space. Users may
#' select a different language for the nearest neighbors
#' than is used in the input embedding.
#'
#'
#' @param  words      a character vector of the words
#'                    for which an embedding should be
#'                    returned
#'
#' @param  lang       the two letter language code
#'                    specifying the language to use for
#'                    the embedding of the inputs
#'
#' @param  lang_out   the two letter language code
#'                    specifying the language to use for
#'                    the embedding of the outputs.
#'
#' @param nn          positive integer. The number of
#'                    nearest neighbors to return in the output.
#'
#' @return A matrix with \code{nn} columns and one row for each
#'         input word. Rows associated with input words not found
#'         in the embedding are set to \code{NA}.
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#'
#' @examples
#'ft_load_model(lang = "en")
#'ft_load_model(lang = "fr")
#'ft_nn(c("the", "and", "I"), lang = "en", lang_out = "fr", nn = 5)
#'
#' @export
ft_nn <- function(words, lang = "en", lang_out = lang, nn = 10L) {

  if (!(lang %in% names(volatiles))) {
    stop(sprintf("language '%s' has not yet been loaded; ", lang),
         sprintf("call ft_load_model('%s') and try again", lang))
  }
  if (!(lang_out %in% names(volatiles))) {
    stop(sprintf("language '%s' has not yet been loaded; ", lang_out),
         sprintf("call ft_load_model('%s') and try again", lang_out))
  }

  # create an empty matrix of missing values
  output <- matrix(NA_real_, ncol = nn, nrow = length(words))

  # determine which words match the vocabulary
  words_lower <- stringi::stri_trans_tolower(words)
  id <- !is.na(match(words_lower, rownames(volatiles[[lang]])))

  # for the matches, fill in with the embedding values
  if (any(id)) {
    v <- volatiles[[lang]][words_lower[id],,drop = FALSE]
    d <- v %*% t(volatiles[[lang_out]])
    d <- t(d) / sqrt(rowSums(volatiles[[lang_out]]^2))
    d <- t(d) / sqrt(rowSums(v^2))
    out <- t( apply(d, 1, function(u) colnames(d)[order(u, decreasing = TRUE)[1:nn]]) )
    output[id,] <- out
  }

  return(output)
}
