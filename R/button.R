#' LCARS button
#'
#' An LCARS wrapper around \code{shiny::actionButton} with additional color control.
#'
#' @param inputId The input slot that will be used to access the value.
#' @param label The contents of the button or link–usually a text label, but you could also use any other HTML, like an image.
#' @param icon An optional icon to appear on the button.
#' @param color button color. Can be any color given in hex format. Named colors must be LCARS colors. See \code{\link{lcarsdata}} for options.
#' @param hover_color Named colors must be LCARS colors. Other arbitrary colors are not supported for hovering. If \code{hover_color = NULL}, the button will darken on hover.
#' @param ... Named attributes to be applied to the button.
#'
#' @seealso \code{\link{lcarsdata}}
#' @export
lcarsButton <- function(inputId, label, icon = NULL, color = "atomic-tangerine",
                        hover_color = "red-damask", ...){
  cl <- if(!is.null(hover_color) && hover_color %in% lcars::lcarsdata$name)
    paste0("lcars-btn lcars-btn-", hover_color) else "lcars-btn lcars-btn-filtered"
  x <- .lcars_color_check(c(color, hover_color))
  style <- paste0("background-color:", x[1], ";")
  shiny::actionButton(inputId, label, icon, NULL, class = cl, style = style, ...)
}

#' LCARS button column
#'
#' An LCARS button column is a container for \code{lcarsButton} and \code{lcarsToggle} buttons that can be passed to \code{lcarsBox}.
#' The buttons occur vertically in the left side panel of the box. Toggle button width should be 150px.
#'
#'
#' @param ... \code{lcarsButton} objects.
#'
#' @export
buttonColumn <- function(...){
  div(class = "lcars-button-col", ...)
}
