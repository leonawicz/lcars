#' LCARS button
#'
#' An LCARS wrapper around `shiny::actionButton()` with additional color control.
#'
#' @param inputId The input slot that will be used to access the value.
#' @param label The contents of the button or link–usually a text label, but
#' you could also use any other HTML, like an image.
#' @param icon An optional icon to appear on the button.
#' @param color button color. Can be any color given in hex format. Named
#' colors must be LCARS colors. See [lcarsdata] for options.
#' @param hover_color Named colors must be LCARS colors. Other arbitrary colors
#' are not supported for hovering. If `hover_color = NULL`, the button will
#' darken on hover.
#' @param ... Named attributes to be applied to the button.
#'
#' @return HTML
#' @seealso [lcarsdata]
#' @export
#'
#' @examples
#' lcarsButton("btn", "A button")
lcarsButton <- function(inputId, label, icon = NULL, color = "atomic-tangerine",
                        hover_color = "red-damask", ...){
  cl <- if(!is.null(hover_color) &&
           hover_color %in% names(trekcolors::lcars_colors()))
    paste0("lcars-btn lcars-btn-", hover_color) else
      "lcars-btn lcars-btn-filtered"
  x <- .lcars_color_check(c(color, hover_color))
  style <- paste0("background-color:", x[1], ";")
  shiny::actionButton(inputId, label, icon, NULL, class = cl, style = style,
                      ...)
}

#' LCARS input column
#'
#' An LCARS input column is a container for inputs like `lcarsButton()` and
#' `lcarsToggle()` that can be passed to `lcarsBox()`. The inputs occur
#' vertically in the left or right side panel of the box. To fit properly, input
#' widths should be 150px or less, matching the widths of the side panels of the
#' given `lcarsBox()` container.
#'
#' @param ... div contents such as `lcarsButton()` elements.
#'
#' @return HTML
#' @export
#'
#' @examples
#' inputColumn()
inputColumn <- function(...){
  div(class = "lcars-button-col", ...)
}
