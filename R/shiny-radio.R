#' LCARS radio buttons
#'
#' LCARS-styled radio buttons functions.
#'
#' \code{lcarsRadio} is a minimal replacement for \code{radioButtons} that
#' provides two additional color arguments for consistency with functions like
#' \code{lcarsCheckbox}.
#' \code{lcarsRadioToggle} is a more customized toggle style radio buttons
#' wrapper with more color controls.
#'
#' @param inputId character, the input slot that will be used to access the
#' value.
#' @param label character, display label for the control, or \code{NULL} for no
#' label.
#' @param choices see \code{shiny::radioButtons} for details.
#' @param selected The initially selected value (if not specified then defaults
#' to the first value).
#' @param inline If \code{TRUE}, render the choices inline (i.e. horizontally).
#' @param width a valid CSS unit.
#' @param choiceNames,choiceValues see \code{shiny::radioButtons} for details.
#' @param label_color,choice_color,background_color,checked_color,checked_background, Color for the label, choices text, choices background, checked text and checked background.
#' Can be any color given in hex format. Named colors must be LCARS colors.
#' See \code{\link{lcarsdata}} for options.
#'
#' @return A set of radio buttons that can be added to a UI definition.
#' @export
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#'   ui <- lcarsPage(
#'     fluidRow(
#'       column(6,
#'         lcarsRadio("dist1", "Distribution type:",
#'           c("Normal" = "norm",
#'             "Uniform" = "unif",
#'             "Log-normal" = "lnorm",
#'             "Exponential" = "exp"),
#'           inline = TRUE,
#'           label_color = "lilac",
#'           choice_color = "atomic-tangerine"
#'         ),
#'         plotOutput("distPlot1")
#'       ),
#'       column(6,
#'         lcarsRadioToggle("dist2", "Distribution type:",
#'           c("Normal" = "norm",
#'             "Uniform" = "unif",
#'             "Log-normal" = "lnorm",
#'             "Exponential" = "exp"),
#'           width = "100%"
#'         ),
#'         plotOutput("distPlot2")
#'       )
#'     )
#'   )
#'
#'   server <- function(input, output) {
#'     output$distPlot1 <- renderPlot({
#'       dist <- switch(input$dist1,
#'                      norm = rnorm,
#'                      unif = runif,
#'                      lnorm = rlnorm,
#'                      exp = rexp,
#'                      rnorm)
#'       hist(dist(500))
#'     })
#'     output$distPlot2 <- renderPlot({
#'       dist <- switch(input$dist2,
#'                      norm = rnorm,
#'                      unif = runif,
#'                      lnorm = rlnorm,
#'                      exp = rexp,
#'                      rnorm)
#'       hist(dist(500))
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
lcarsRadio <- function(inputId, label, choices = NULL, selected = NULL,
                       inline = FALSE, width = NULL, choiceNames = NULL,
                       choiceValues = NULL, label_color = "#FFFFFF",
                       choice_color = label_color){
  x <- c(label_color, choice_color)
  x <- .lcars_color_check(x)
  shiny::tagList(
    shiny::radioButtons(inputId, label, choices, selected, inline,
                 width, choiceNames, choiceValues),
    tags$style(paste0("#", inputId, " label{color:", x[1], ";}\n#", inputId,
                      " input[type='radio']+span{color: ", x[2], ";}")),
    tags$script(paste0("$('#", inputId,"').addClass('lcars-radio');"))
  )
}

#' @export
#' @rdname lcarsRadio
lcarsRadioToggle <- function(inputId, label, choices = NULL, selected = NULL,
                             width = NULL, choiceNames = NULL,
                             choiceValues = NULL,
                             label_color = "atomic-tangerine",
                             choice_color = "#000000",
                             background_color = label_color,
                             checked_color = choice_color,
                             checked_background = "pale-canary"){
  x <- c(label_color, choice_color, background_color, checked_color,
         checked_background)
  x <- .lcars_color_check(x)
  shiny::tagList(
    shiny::div(class = "lcars-radio-toggle",
               shiny::radioButtons(inputId, label, choices, selected,
                                   inline = FALSE, width, choiceNames,
                                   choiceValues)
    ),
    tags$style(
      paste0(".lcars-radio-toggle #", inputId,
             " .shiny-options-group .radio{margin:",
             if(is.null(label)) "0px 0 8px 0" else "6px 0 11px 0", ";}",
             ".lcars-radio-toggle #", inputId, " label{color:", x[1], ";}",
             ".lcars-radio-toggle #", inputId, "
             .shiny-options-group span{color:", x[2], ";background-color:",
             x[3], ";}",
             ".lcars-radio-toggle #", inputId,
             " .shiny-options-group input:checked + span{color:",
             x[4], ";background-color: ", x[5], ";}"
      )
    )
  )
}
