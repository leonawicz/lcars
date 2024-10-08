% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shiny-toggle.R
\name{lcarsToggle}
\alias{lcarsToggle}
\title{LCARS toggle button}
\usage{
lcarsToggle(
  inputId,
  label,
  value = FALSE,
  pill = FALSE,
  inverse = FALSE,
  true = "Yes",
  false = "No",
  true_color = "dodger-pale",
  false_color = "atomic-tangerine",
  background_color = "#000000",
  border_color = ifelse(inverse, false_color, background_color),
  outer_border = FALSE,
  outer_color = "#000000",
  label_color = "#FFFFFF",
  label_right = FALSE,
  width = NULL
)
}
\arguments{
\item{inputId}{character, the input slot that will be used to access the
value.}

\item{label}{character, display label for the control, or \code{NULL} for no label.}

\item{value}{logical, initial value.}

\item{pill}{logical, use an LCARS pill style with rounded ends instead of
the default rounded rectangle.}

\item{inverse}{logical, invert the color presentation.}

\item{true}{character, text label for \code{TRUE} position.}

\item{false}{character, text label for \code{FALSE} position.}

\item{true_color}{Color for \code{TRUE} position. Can be any color given in hex
format. Named colors must be LCARS colors. See \link{lcarsdata} for options.}

\item{false_color}{Color for \code{FALSE} position, as above.}

\item{background_color}{background color, as above.}

\item{border_color}{border color, as above.}

\item{outer_border}{logical, use outer border. This makes some adjustments
to inner elements if used.}

\item{outer_color}{outer border color, as above.}

\item{label_color}{label text color, as above.}

\item{label_right}{logical, set to \code{TRUE} to right align label text.}

\item{width}{character, use only \code{px} units for this widget, e.g. \code{"150px"}
(the default when \code{NULL}). Percentage is the only other unit allowed. It
works, but not as well. Fixed widths recommended.}
}
\value{
A toggle button control that can be added to a UI definition.
}
\description{
An LCARS styled toggle button that can be used in place of
\code{shiny::checkboxInput()} and \code{lcarsCheckbox()}.
}
\examples{
## Only run examples in interactive R sessions
if(interactive()){
  ui <- lcarsPage(
    lcarsToggle("somevalue", "Some value", FALSE),
    verbatimTextOutput("value")
  )
  server <- function(input, output) {
    output$value <- renderText({ input$somevalue })
  }
  shinyApp(ui, server)
}
}
