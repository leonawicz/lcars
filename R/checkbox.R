#' LCARS checkbox
#'
#' An LCARS styled toggle button that can be used in place of \code{checkboxInput}.
#'
#' @param inputId character, the input slot that will be used to access the value.
#' @param label character, display label for the control, or \code{NULL} for no label.
#' @param value logical, initial value.
#' @param color Check color. Can be any color given in hex format. Named colors must be LCARS colors. See \code{\link{lcarsColors}} for options.
#' @param background_color background color, as above.
#' @param label_color label text color, as above.
#' @param label_right logical, set to \code{TRUE} to right align the label.
#' @param width a valid CSS unit.
#'
#' @return A checkbox control that can be added to a UI definition
#' @export
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if(interactive()){
#'   ui <- lcarsPage(
#'     lcarsCheckbox("somevalue", "Some value", FALSE),
#'     verbatimTextOutput("value")
#'   )
#'   server <- function(input, output) {
#'     output$value <- renderText({ input$somevalue })
#'   }
#'   shinyApp(ui, server)
#' }
lcarsCheckbox <- function(inputId, label, value = FALSE, color = "atomic-tangerine",
                          background_color = "#000000", label_color = "#FFFFFF",
                          label_right = FALSE, width = NULL){
  x <- c(color, background_color, label_color)
  x <- .lcars_color_check(x)
  value <- shiny::restoreInput(id = inputId, default = value)
  inputTag <- tags$input(id = inputId, type = "checkbox")
  if(!is.null(value) && value) inputTag$attribs$checked <- "checked"
  width <- shiny::validateCssUnit(width)
  if(is.null(width)) width <- "150px"
  style <- paste0("text-align:", if(label_right) "right" else "left", ";")
  div(class = "form-group shiny-input-container", style = paste0(";width:", width, ";"),
      div(class = "checkbox lcars-checkbox", style = style,
          tags$label(
            inputTag,
            style = paste0("color:", x[3], ";--chk-main-color:", x[1], ";"),
            tags$span(class = "checkmark", style = paste0("--chk-bg-color:", x[2], ";")), label)))
}
