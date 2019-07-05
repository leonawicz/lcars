#' LCARS toggle button
#'
#' An LCARS styled toggle button that can be used in place of \code{checkboxInput} and \code{lcarsCheckbox}.
#'
#' @param inputId character, the input slot that will be used to access the value.
#' @param label character, display label for the control, or \code{NULL} for no label.
#' @param value logical, initial value.
#' @param pill logical, use an LCARS pill style with rounded ends instead of the default rounded rectangle.
#' @param inverse logical, invert the color presentation.
#' @param true character, text label for \code{TRUE} position.
#' @param false character, text label for \code{FALSE} position.
#' @param true_color Color for \code{TRUE} position. Can be any color given in hex format. Named colors must be LCARS colors. See \code{\link{lcarsdata}} for options.
#' @param false_color Color for \code{FALSE} position, as above.
#' @param background_color background color, as above.
#' @param border_color border color, as above.
#' @param outer_border logical, use outer border. This makes some adjustments to inner elements if used.
#' @param outer_color outer border color, as above.
#' @param label_color label text color, as above.
#' @param label_right logical, set to \code{TRUE} to right align label text.
#' @param width character, use only \code{px} units for this widget, e.g. \code{"150px"} (the default when \code{NULL}). Percentage is the only other unit allowed. It works, but not as well. Fixed widths recommended.
#'
#' @return A toggle button control that can be added to a UI definition.
#' @export
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if(interactive()){
#'   ui <- lcarsPage(
#'     lcarsToggle("somevalue", "Some value", FALSE),
#'     verbatimTextOutput("value")
#'   )
#'   server <- function(input, output) {
#'     output$value <- renderText({ input$somevalue })
#'   }
#'   shinyApp(ui, server)
#' }
lcarsToggle <- function(inputId, label, value = FALSE, pill = FALSE, inverse = FALSE,
                        true = "Yes", false = "No", true_color = "dodger-pale",
                        false_color = "atomic-tangerine", background_color = "#000000",
                        border_color = ifelse(inverse, false_color, background_color),
                        outer_border = FALSE, outer_color = "#000000",
                        label_color = "#FFFFFF", label_right = FALSE, width = NULL){
  value <- shiny::restoreInput(id = inputId, default = value)
  x <- c(true_color, false_color, background_color, border_color, outer_color, label_color)
  x <- .lcars_color_check(x)
  cl <- if(inverse) "inverse" else "default"
  cl <- paste0("lcars-toggle-", cl, " lcars-toggle-tf")
  if(is.null(width)) width <- "150px"
  r1 <- if(pill) "26px" else "4px"
  r2 <- if(pill) "18px" else "2px"
  f <- function(x){
    x <- gsub("px", "", x)
    x <- suppressWarnings(as.integer(x))
    if(is.na(x)) "auto" else paste0(x - 31, "px")
  }
  width0 <- width
  if(outer_border){
    if(grepl("px", width)) width <- paste0(as.integer(gsub("px", "", width)) - 12, "px") else
      width <- paste0(as.integer(gsub("\\%", "", width)) - 0, "%")
  }
  inputTag <- tags$input(type = "checkbox", name = "lcars-toggle", class = "lcars-toggle-checkbox", id = inputId)
  if(!is.null(value) && value) inputTag$attribs$checked <- "checked"
  shiny::tagList(
    div(class = "form-group shiny-input-container",
        style = paste0(if(label_right) "text-align:right;", "margin-bottom:0px;width:", width0, ";"),
        div(class = cl,
            tags$label(label, class = "control-label",
                       style = paste0("color:", x[6], ";", if(outer_border) "padding-left:4px;" else "")),
            style = paste0("--tog-true-color: ", x[1], ";--tog-false-color: ", x[2],
                           ";--tog-bg-color: ", x[3], ";",
                           if(outer_border)
                             paste0("border: 2px solid ", x[5],
                                    "; border-radius: 4px; padding-left:4px; padding-right:4px; margin-top:4px;margin-bottom:4px;")),
            div(class = "lcars-toggle", inputTag,
                tags$label(class = "lcars-toggle-label",
                           style = paste0("text-align:left;border: 2px solid ", x[4],
                                          ";width:", width, ";--tog-w:", width, ";",
                                          "--tog-radius-label:", r1, ";"), `for` = inputId,
                           tags$span(class = "lcars-toggle-inner",
                                     style = paste0("--tog-true-label: '", true, "';--tog-false-label: '", false, "';",
                                                    "--tog-true-color: ", x[1], ";--tog-false-color: ", x[2], ";")),
                           tags$span(class = "lcars-toggle-button",
                                     style = paste0("--tog-w1:", f(width), ";",
                                                    "--tog-w2:", 1, "px;", "--tog-radius-button:", r2, ";"))
                )
            )
        )
    )
  )
}
