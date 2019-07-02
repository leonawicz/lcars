#' LCARS radio buttons
#'
#' A minimal replacement for \code{radioButtons} that provides two additional color arguments for consistency with functions like \code{lcarsCheckbox}.
#'
#' @param inputId character, the input slot that will be used to access the value.
#' @param label character, display label for the control, or \code{NULL} for no label.
#' @param choices see \code{shiny::radioButtons} for details.
#' @param selected The initially selected value (if not specified then defaults to the first value).
#' @param inline If \code{TRUE}, render the choices inline (i.e. horizontally).
#' @param width a valid CSS unit.
#' @param choiceNames see \code{shiny::radioButtons} for details.
#' @param choiceValues see \code{shiny::radioButtons} for details.
#' @param label_color Label color. Can be any color given in hex format. Named colors must be LCARS colors. See \code{\link{lcarsColors}} for options.
#' @param choice_color choice text color, as above.
#'
#' @return A set of radio buttons that can be added to a UI definition.
#' @export
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#'   ui <- lcarsPage(
#'     lcarsRadio("dist", "Distribution type:",
#'                  c("Normal" = "norm",
#'                    "Uniform" = "unif",
#'                    "Log-normal" = "lnorm",
#'                    "Exponential" = "exp"),
#'                label_color = "lilac",
#'                choice_color = "atomic-tangerine"
#'               ),
#'     plotOutput("distPlot")
#'   )
#'
#'   server <- function(input, output) {
#'     output$distPlot <- renderPlot({
#'       dist <- switch(input$dist,
#'                      norm = rnorm,
#'                      unif = runif,
#'                      lnorm = rlnorm,
#'                      exp = rexp,
#'                      rnorm)
#'
#'       hist(dist(500))
#'     })
#'   }
#' }
#'
#' shinyApp(ui, server)
lcarsRadio <- function(inputId, label, choices = NULL, selected = NULL, inline = FALSE,
                       width = NULL, choiceNames = NULL, choiceValues = NULL,
                       label_color = "#FFFFFF", choice_color = label_color){
  x <- c(label_color, choice_color)
  x <- .lcars_color_check(x)
  tagList(
    radioButtons(inputId, label, choices, selected, inline,
                 width, choiceNames, choiceValues),
    tags$style(paste0("#", inputId, " label{color:", x[1], ";}
                        #", inputId, " input[type='radio']+span{color: ", x[2], ";}")),
    tags$script(paste0("$('#", inputId,"').addClass('lcars-radio');"))
  )
}
