#' Download Fasttext Model Files
#'
#' To run embeddings with a specific language, you
#' must first download the respective language file.
#' This needs to be done only once. As the raw files
#' are quite large (over 6GB), this function grabs
#' only the most frequent words. You can control the
#' number of words by setting the number of megabytes
#' to download.
#'
#' @param  lang       the two letter language code
#'                    specifying the language you would
#'                    like to download. See the function
#'                    \code{\link{ft_languages}} for a
#'                    complete list of available choices.
#'
#' @param  mb         the number of megabytes to download
#'                    from the file. The default (500) gets
#'                    around 200k rows. Adjust as needed.
#'                    Set to \code{Inf} to get all rows.
#'                    This is a 6GB file.
#'
#' @return Invisibly returns the status code of the download.
#'        The embedding matrix is stored on disk.
#'
#' @author Taylor B. Arnold, \email{taylor.arnold@@acm.org}
#'
#' @examples
#'\dontrun{
#'ft_download_model(lang = "zh", mb = 200)
#'}
#'
#' @export
ft_download_model <- function(lang = "en", mb = 500) {

  # Download the fasttext model first
  h <- curl::new_handle()
  if (is.finite(mb)) {
    curl::handle_setopt(h, range = sprintf("0-%d000000", mb))
  } # otherwise, download the whole file
  base_url <- "https://dl.fbaipublicfiles.com/fasttext/vectors-wiki/wiki.en.vec"
  url <- sprintf("%s/wiki.%s.vec", base_url, lang)
  r <- curl::curl_fetch_memory(url, h)

  # Now, parse the model using the fast iotools based
  # raw vector functions; the last row is likely
  # incomplete, so remove it.
  z <- iotools::mstrsplit(r$content, sep = " ", nsep = " ",
                          type = "numeric", skip = 1L, ncol = 300L)
  z <- z[-nrow(z),]

  # Download the rotation vector for the given language
  base_url <- paste("https://raw.githubusercontent.com/Babylonpartners",
                    "/fastText_multilingual",
                    "/master/alignment_matrices", sep = "")
  url <- sprintf("%s/%s.txt", base_url, lang)

  # Parse the rotation matrix as well
  h <- curl::new_handle()
  r2 <- curl::curl_fetch_memory(url, h)
  rotation <- iotools::mstrsplit(r2$content, sep = " ", type = "numeric")

  # Apply the rotation
  z <- z %*% rotation

  # Save the model
  output_location <- system.file("extdata", package="fasttextM")
  saveRDS(z, sprintf("%s/%s.Rds", output_location, lang))

  # Invisibly status code of the (first) call to curl
  invisible(r$status_code)
}
