#' Source apiKey
#'
#' Source apiKey list PGP encrypted R script
#'
#' Source apiKey list PGP encrypted R script with keys for various webservices of National Statics Offices.
#'
#' @param file path to gpg key file
#' @param gpg path to GnuPG executable
#' @param passphrase character password used for decryption. suggested to store as environment variable and use with \code{Sys.getenv()}
#' @param shell if on Windows, you may need to specify the path to cmd.exe
#' @param keep boolean set to TRUE if you want to keep the decoded file
#'
#' @return Creates decrypted file in the package installation folder. The key file is gpg encryped. In Windows, use GPG Plugin Portable: http://portableapps.com/apps/security/gpg-plugin-portable
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
                      gpg = file.path("D:", "GPG", "gpg2.exe"),
                      passphrase = NULL,
                      shell = file.path("C:", "Windows", "System32", "cmd.exe"),
                      keep = FALSE
  ) {

  file.dec <- sub("[.][gG][pP][gG]", "", file)

  if (is.null(gpg)) cmd <- "gpg" else cmd <- gpg
  if (!is.null(passphrase)) cmd <- paste(cmd, '--passphrase', passphrase)
  cmd <- paste(cmd, '-d', file, '>', file.dec)

  if (Sys.info()[["sysname"]]=="Windows")
      shell(cmd, shell = shell)
  else
      system(cmd)

  source(file.dec)

  if (!keep) {
    if (file.exists(file.dec)) file.remove(file.dec)
  }

}
