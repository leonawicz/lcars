#' LCARS Shiny UI
#'
#' Create a Shiny UI page with an LCARS theme.
#'
#' The LCARS style heavily emphasizes uppercase text. Set \code{force_uppercase = TRUE} to force this standard via CSS.
#' This does not make everything uppercase; things like input labels are left alone (use \code{label_uppercase = TRUE}). However, text in general in uppercased.
#'
#' Set these to \code{FALSE} if you need control over casing.
#' This allows sensible judgment over how to balance the tension between
#' making something that conforms well to the familiar LCARS aesthetic
#' and making something that communicates information with a lower cognitive load for the user.
#' Similarly, set \code{lcars_font*} arguments to \code{FALSE} to use a more readable sans serif font as desired.
#' See examples for recommendations.
#'
#' @param ... The contents of the document body.
#' @param title The browser window title (defaults to the host URL of the page).
#' @param force_uppercase logical, see details.
#' @param label_uppercase logical, also make widget labels uppercase globally.
#' @param lcars_font_headers use LCARS-style font family for major heading text. See details.
#' @param lcars_font_labels use LCARS-style font family for LCARS widget labels/titles.
#' @param lcars_font_text use LCARS-style font family for general text (paragraphs, lists, etc.).

#'
#' @return A UI definition that can be passed to the shinyUI function.
#' @export
#'
#' @examples
#' \dontrun{
#' # Recommended settings
#' # for a more standard LCARS style: default settings.
#' lcarsPage()
#'
#' # for a more readable style: less uppercase, switch to sans font
#' lcarsPage(force_uppercase = FALSE, label_uppercase = FALSE,
#'   lcars_font_labels = FALSE, lcars_font_text = FALSE)
#' }
lcarsPage <- function (..., title = NULL, force_uppercase = TRUE, label_uppercase = TRUE,
                       lcars_font_headers = TRUE, lcars_font_labels = TRUE,
                       lcars_font_text = TRUE){
  shiny::bootstrapPage(
    div(
      class = "container-fluid",
      lcars_init(force_uppercase, label_uppercase, lcars_font_headers,
                 lcars_font_labels, lcars_font_text), ...),
    title = title
  )
}

lcars_init <- function(force_uppercase = FALSE, label_uppercase = FALSE, lcars_font_headers = TRUE,
                       lcars_font_labels = TRUE, lcars_font_text = TRUE){
  lcars_font_headers <- if(lcars_font_headers) "Oswald" else "sans-serif"
  lcars_font_labels <- if(lcars_font_labels) "Oswald" else "sans-serif"
  lcars_font_text <- if(lcars_font_text) "Oswald" else "sans-serif"
  shiny::addResourcePath("svg", system.file("www/svg", package = "lcars"))
  shiny::tagList(
    shiny::includeCSS(system.file("www/css/lcars.css", package = "lcars")),
    if(force_uppercase)
      tags$style("h1, h2, h3, h4, h5, h6, p, li, .blocktext_black, .blocktext_white, .lcars-btn,
                  .lcars-btn-filtered, .lcars-hdr-title, .lcars-box-title, .lcars-box-subtitle {
                  text-transform: uppercase;}"),
    if(label_uppercase) tags$style("label {text-transform: uppercase;}"),
    tags$style(paste0(
      "h1, h2, h3, h4, h5, h6, .lcars-hdr-title,
      .lcars-box-title, .lcars-box-subtitle {
      font-family: ", lcars_font_headers, ";}")),
    tags$style(paste0(
      "label, .lcars-btn, .lcars-btn-filtered, .lcars-checkbox label {
      font-family: ", lcars_font_labels, ";}")),
    tags$style(paste0(
      "p, li, .text, code, .blocktext_white, .blocktext_black {
      font-family: ", lcars_font_text, ";}"))
  )
}


#' LCARS header
#'
#' A, LCARS header panel.
#'
#' @param title character, optional title.
#' @param color header color. Any hex color or a named LCARS color.
#' @param title_color text color. Any hex color or a named LCARS color.
#'
#' @seealso \code{\link{lcarsColors}}.
#' @export
lcarsHeader <- function(title = NULL, color = "golden-tanoi", title_color = color){
  x <- .lcars_color_check(c(color, title_color))
  div(class = if(is.null(title)) "lcars-hdr2" else "lcars-hdr",
    div(class = "hdr-pill-left",
      shiny::HTML('<svg style = "fill:', x[1], ';">
           <use xlink:href="svg/sprites.svg#lcars-svg-endcap_left" height="30" width="45"></use>
           </svg>')
    ),
    div(class = "blocktext_black lcars-hdr-rect", style = paste0("background-color:", x[1], ";")),
    if(!is.null(title)) div(class = "lcars-hdr-title", style = paste0("color:", x[2], ";"), title),
    div(class = "hdr-pill-right",
      shiny::HTML('<svg style = "fill:', x[1], ';">
           <use xlink:href="svg/sprites.svg#lcars-svg-endcap_right" height="30" width="45"></use>
           </svg>')
    )
  )
}

#' Launch LCARS demo apps.
#'
#' Currently available apps include: \code{demo}, \code{toggle}.
#'
#' @param id character, app id.

#' @export
#' @examples
#' \dontrun{lcarsApp("demo")}
lcarsApp <- function(id = "demo"){
  if(id == "demo"){
    if(!requireNamespace("ggrepel", quietly = TRUE)){
      message("This app requires the `ggrepel` package. Install and rerun.")
      return(invisible())
    }
    if(!requireNamespace("showtext", quietly = TRUE)){
      message("This app requires the `showtext` package. Install and rerun.")
      return(invisible())
    }
  }
  app <- system.file(file.path("shiny", id), package = "lcars")
  shiny::runApp(app)
}

.lcars_color_check <- function(x){
  idx <- grepl("^#", x)
  if(all(idx)) return(x)
  y <- match(x[!idx], lcars::lcarsColors$name)
  if(any(is.na(y))) stop("Invalid LCARS color name. Provide non-LCARS colors in hex format.")
  x[!idx] <- lcars::lcarsColors$value[y]
  x
}
