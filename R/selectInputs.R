#' Multiple selectInput
#'
#' Create multiple shiny \code{selectInput} widgets
#'
#' Create list of shiny ui widgets based on list with label and choices.
#'
#' @param list a named list object.
#' @param prefix a character string to prepend inputIds.
#'
#' @author Bo Werth <bo.werth@@gmail.com>
#' @keywords shiny
#' @seealso \code{https://github.com/rstudio/shiny}
#' @export
#' @examples
#' listexample <- list(fruits = c("mango", "apple"),
#'                     color = c("orange", "green"))
#' 
#' selectInputs(list = listexample, prefix = "dim_")
#' 
#' ## generate list from data frame
#' df <- data.frame(fruits = c("mango", "apple", "mango"),
#'                  color = c("orange", "green", "red"))
#'
#' list.df <- apply(df, 2, unique)
#' 
#' selectInputs(list = list.df, prefix = "dim_")


selectInputs <- function(list = stop("'list' must be provided"),
                         prefix = "",
                         minsize = 4,
                         ...) {

  ui.all <- NULL
  ## if (is.null(names)) names <- names(data)
  ## for (col in names) {
  for (label in names(list)) {
    ## choices <- as.character(unique(data[[col]]))
    choices <- list[[label]]
    ui <- shiny::selectInput(
      inputId = paste0(prefix, label),
      label = label,
      choices = choices,
      selected = choices[1],
      multiple = TRUE,
      selectize = FALSE,
      size = min(length(choices), minsize),
      ...)

    ui.all <- list(ui.all, ui)

  }
  return(ui.all)
}
