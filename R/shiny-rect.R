#' LCARS rectangle element
#'
#' A basic rectangle HTML element that conforms to LCARS specifications.
#'
#' While \code{text} can be made arbitrarily large using \code{text_size}, the font size of the optional \code{title} is fixed at standard header size (\code{height = 0.5}).
#'
#' @param text character, rectangle text.
#' @param round character, sides of rectangle to round to make an LCARS pill or half pill.
#' @param decorate character, sides of rectangle to decorate with cut pill; applicable if a given side is rounded via \code{round}. Logical for \code{lcarsLeftPill} and \code{lcarsRightPill}.
#' @param color rectangle color. Any hex color or a named LCARS color.
#' @param text_color text color. Any hex color or a named LCARS color.
#' @param title_color title color. Any hex color or a named LCARS color.
#' @param text_size size of \code{text} in pixels.
#' @param title optional title text to insert in blank gap in rectangle. Used for header-style rectangles.
#' @param width a valid CSS unit.
#' @param height a valid CSS unit.
#'
#' @return a div
#' @export
#'
#' @examples
#' if (interactive()) {
#'
#'   ui <- lcarsPage(
#'     fluidRow(
#'       column(4,
#'         h4("Rectangle"),
#'         lcarsRect("Some text.", text_size = 24, width = 200),
#'         h4("Pill"),
#'         lcarsPill("Some text.", text_size = 24, width = 200)
#'       )
#'     )
#'   )
#'
#'   server <- function(input, output) {}
#'
#'   shinyApp(ui, server)
#' }
lcarsRect <- function(text = "", round = c("none", "both", "left", "right"),
                      decorate = c("none", "both", "left", "right"),
                      color = "golden-tanoi", text_color = "#000000",
                      title_color = color, text_size = 16, title = NULL, width = "100%", height = 30){
  width <- shiny::validateCssUnit(width)
  height <- shiny::validateCssUnit(height)
  r <- match.arg(round)
  d <- match.arg(decorate)
  r2 <- switch(r, none = "", both = "rounded", left = "left-rounded", right = "right-rounded")
  if(d == "both") r2 <- ""
  if(d == "left" & r2 == "rounded") r2 <- "right-rounded"
  if(d == "left" & r2 == "left-rounded") r2 <- ""
  if(d == "right" & r2 == "rounded") r2 <- "left-rounded"
  if(d == "right" & r2 == "right-rounded") r2 <- ""
  x <- .lcars_color_check(c(color, text_color, title_color))
  cl <- "lcars-element"
  if(r2 != "") cl <- paste(cl, r2)
  rect_style <- paste0("background-color:", x[1], ";color:", x[2],
                       ";font-size:", text_size, "px;line-height:", text_size + 10, "px;",
                       "width:", width, ";height:", height)
  div(class = cl, style = rect_style,
      if(r %in% c("both", "left") & d %in% c("both", "left"))
        div(class = "lcars-element-decorator",
            style = paste0("background-color:", x[1], ";color:", x[1], ";")),
      div(class = "lcars-element-text", style = "padding-right:6px;", text),
      if(!is.null(title))
        div(class = "lcars-element-addition",
            style = paste0("color:", x[3], ";font-size:30px;line-height:32px;"), title),
      if(r %in% c("both", "right") & d %in% c("both", "right"))
        div(class = "lcars-element-decorator right",
            style = paste0("background-color:", x[1], ";-color:", x[1], ";"))
  )
}

#' @export
#' @rdname lcarsRect
lcarsPill <- function(text = "", decorate = c("none", "both", "left", "right"),
                      color = "golden-tanoi", text_color = "#000000",
                      title_color = color, text_size = 16, title = NULL,
                      width = "100%", height = 30){
  decorate <- match.arg(decorate)
  lcarsRect(text, "both", decorate, color, text_color, title_color,
            text_size, title, width, height)
}

#' @export
#' @rdname lcarsRect
lcarsLeftPill <- function(text = "", decorate = FALSE, color = "golden-tanoi",
                          text_color = "#000000", title_color = color,
                          text_size = 16, title = NULL, width = "100%", height = 30){
  decorate <- if(decorate) "left" else "none"
  lcarsRect(text, "left", decorate, color, text_color, title_color,
            text_size, title, width, height)
}

#' @export
#' @rdname lcarsRect
lcarsRightPill <- function(text = "", decorate = FALSE, color = "golden-tanoi",
                           text_color = "#000000", title_color = color,
                           text_size = 16, title = NULL, width = "100%", height = 30){
  decorate <- if(decorate) "right" else "none"
  lcarsRect(text, "right", decorate, color, text_color, title_color,
            text_size, title, width, height)
}

#' LCARS bracket element
#'
#' A top and bottom bracket pair element used to visually group contents.
#'
#' @param ... div contents.
#' @param color bracket color. Any hex color or a named LCARS color.
#' @param background_color background color. Any hex color or a named LCARS color. Applies when \code{hollow = TRUE}.
#' @param hollow logical, use a hollow bracket. The cutout section has \code{background_color}.
#' @param width a valid CSS unit.
#'
#' @return an HTML widget
#' @export
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#'   ui <- lcarsPage(
#'     fluidRow(
#'       column(4,
#'         h4("Hollow bracket"),
#'         lcarsBracket(
#'           lcarsRect("Some text.", text_size = 24, height = 40)
#'         ),
#'         h4("Solid bracket"),
#'         lcarsBracket(
#'           lcarsRect("Some text.", color = "#000000",
#'                     text_color = "golden-tanoi",
#'                     text_size = 24, height = 40),
#'           hollow = FALSE
#'         )
#'       )
#'     )
#'   )
#'
#'   server <- function(input, output) {}
#'
#'   shinyApp(ui, server)
#' }
lcarsBracket <- function(..., color = "golden-tanoi", background_color = "#000000",
                         hollow = TRUE, width = "100%"){
  width <- shiny::validateCssUnit(width)
  x <- .lcars_color_check(c(color, background_color))
  if(!hollow) x <- rep(x[1], 2)
  cl <- paste0("lcars-bracket ", c("top", "bottom"), if(hollow) " hollow")
  style <- paste0("border-color:", x[1], ";background-color:", x[2], ";")
  div(style = paste0("width:", width, ";"),
    div(class = cl[1], style = style),
    div(...),
    div(class = cl[2], style = style)
  )
}
