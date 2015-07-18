#' Source apiKey
#'
#' Source apiKey list PGP encrypted R script
#'
#' Source apiKey list PGP encrypted R script with keys for various webservices of National Statics Offices.
#'
#' @param file
#' @param password
#'
#' @return Creates decrypted file in the package installation folder
#'
#' @author Bo Werth <bo.werth@@gmail.com>
#' @keywords GPG
#' @export
#' @examples
#' nsoApiGPG(keep = TRUE)
#' file.apiKey <- system.file("apiKey.R", package = "nsoApi")
#'
#' if (file.exists(file.apiKey)) {
#'   filecon <- file(file.apiKey)
#'   cat(paste0(readLines(con = filecon), '\n'))
#'   close(filecon)
#' }


nsoApiGPG <- function(file = system.file("apiKey.R.gpg", package = "nsoApi"),
                      passphrase = NULL,
                      keep = FALSE
  ) {

  file.dec <- sub("[.][gG][pP][gG]", "", file)

  cmd <- "gpg"
  if (!is.null(passphrase)) cmd <- paste(cmd, '--passphrase', passphrase)
  cmd <- paste(cmd, '-d', file, '>', file.dec)
  
  system(cmd)

  source(file.dec)
  
  if (!keep) {
    if (file.exists(file.dec)) file.remove(file.dec)
  }
  
}
