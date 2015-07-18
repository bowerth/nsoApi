#' Shiny Extra
#'
#' Additions to RStudio shiny functions
#'
#' Minor additions to existing RStudio Shiny functions.
#'
#' @param data a data frame
#' @param names a character vectors with columns of data.
#' @param prefix a character string to prepend inputIds.
#'
#' @author Bo Werth <bo.werth@@gmail.com>
#' @keywords shiny
#' @seealso \code{https://github.com/rstudio/shiny}
#' @import shiny
#' @export
#' @examples
#' cat(selectInputs(data = data, names = c("fruits", "color"), prefix = "dim_")

selectInputs <- function(data = stop("'data' must be provided"),
                         names = NULL,
                         prefix = "",
                         ...) {

    ui.all <- NULL
    if (is.null(names)) names <- names(data)
    for (col in names) {
        choices <- as.character(unique(data[[col]]))
        ui <- shiny::selectInput(
            inputId = paste0(prefix, col),
            label = col,
            choices = choices,
            selected = choices[1],
            multiple = TRUE,
            selectize = FALSE,
            ...)

        ui.all <- list(ui.all, ui)

    }
    return(ui.all)
}
