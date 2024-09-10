#' Create an LCARS outputs (server side)
#'
#' Server-side functions for creating dynamic `lcarBox()` and `lcarsSweep()`.
#'
#' @param expr An expression that returns a Shiny tag object, HTML, or a list
#' of such objects.
#' @param env The environment in which to evaluate `expr`.
#' @param quoted Is `expr` a quoted expression (with `quote()`)? This
#' is useful if you want to save an expression in a variable.
#'
#' @export
#' @name renderLcars
#'
#' @seealso [lcarsOutput()] for the corresponding UI-side function.
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#'   ui <- lcarsPage(
#'     lcarsBoxOutput("box"),
#'     lcarsSweepOutput("sweep")
#'   )
#'
#'   server <- function(input, output) {
#'     output$box <- renderLcarsBox({
#'       lcarsBox()
#'     })
#'     output$sweep <- renderLcarsSweep({
#'       lcarsSweep()
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
renderLcarsBox <- function(expr, env = parent.frame(), quoted = FALSE){
  f <- shiny::exprToFunction(expr, env, quoted)
  shiny::renderUI(f())
}

#' @export
#' @rdname renderLcars
renderLcarsSweep <- function(expr, env = parent.frame(), quoted = FALSE){
  f <- shiny::exprToFunction(expr, env, quoted)
  shiny::renderUI(f())
}

#' Create an LCARS output (client side)
#'
#' UI-side functions for creating dynamic `lcarBox()` and `lcarsSweep()`.
#'
#' @param outputId Output variable name.
#'
#' @export
#' @name lcarsOutput
#'
#' @seealso [renderLcars()] for the corresponding server-side
#' function.
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#'   ui <- lcarsPage(
#'     lcarsBoxOutput("box"),
#'     lcarsSweepOutput("sweep")
#'   )
#'
#'   server <- function(input, output) {
#'     output$box <- renderLcarsBox({
#'       lcarsBox()
#'     })
#'     output$sweep <- renderLcarsSweep({
#'       lcarsSweep()
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
lcarsBoxOutput <- function(outputId){
  shiny::uiOutput(outputId)
}

#' @export
#' @rdname lcarsOutput
lcarsSweepOutput <- function(outputId){
  shiny::uiOutput(outputId)
}
