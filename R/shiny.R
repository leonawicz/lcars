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
      tags$style("h1, h2, h3, h4, h5, h6, p, li, a, .blocktext_black, .blocktext_white, .lcars-btn,
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
      "p, li, a, .text, code, .blocktext_white, .blocktext_black {
      font-family: ", lcars_font_text, ";}"))
  )
}


#' LCARS header
#'
#' An LCARS header panel.
#'
#' In addition to \code{lcarsHeader} there are also some LCARS style heading replacers, \code{lcarsh1} through \code{lcarsh6}.
#' These default to centered text, whereas \code{lcarsHeader} is strictly right or left.
#'
#' @param title character, optional title.
#' @param color header color. Any hex color or a named LCARS color.
#' @param title_color text color. Any hex color or a named LCARS color.
#' @param background_color color behind text.
#' @param title_right logical, right align title.
#' @param title_align character, for the heading replacers: center, right or left.
#' @param title_invert logical, invert the color and background color for the title rectangle.
#' @param width a valid CSS unit.
#'
#' @seealso \code{\link{lcarsdata}}.
#' @name lcarsHeader
#' @export
lcarsHeader <- function(title = NULL, color = "golden-tanoi", title_color = color,
                        background_color = "#000000", title_right = TRUE,
                        title_invert = FALSE, width = "100%"){
  width <- shiny::validateCssUnit(width)
  if(is.null(width)) width <- "100%"
  x <- .lcars_color_check(c(color, title_color, background_color))
  if(is.null(title)){
    cl <- "lcars-hdr2"
  } else {
    cl <- if(title_right) "lcars-hdr" else "lcars-hdr-ljust"
  }
  title_div <- div(class = "lcars-hdr-title",
                   style = paste0("color:", if(title_invert) x[3] else x[2], ";",
                                  "background-color:", if(title_invert) x[2] else x[3], ";",
                                  ";font-size:30px;line-height:29px;"), title)
  div(class = cl, style = paste0("width:", width, ";"),
    div(class = "hdr-pill-left",
      shiny::HTML('<svg style = "fill:', x[1], ';height:30px;width:45px;">
           <use xlink:href="svg/sprites.svg#lcars-svg-endcap_left" height="30" width="45"></use>
           </svg>')
    ),
    if(!is.null(title) & !title_right) title_div,
    div(class = "blocktext_black lcars-hdr-rect", style = paste0("background-color:", x[1], ";")),
    if(!is.null(title) & title_right) title_div,
    div(class = "hdr-pill-right",
      shiny::HTML('<svg style = "fill:', x[1], ';height:30px;width:45px;">
           <use xlink:href="svg/sprites.svg#lcars-svg-endcap_right" height="30" width="45"></use>
           </svg>')
    )
  )
}

#' @name lcarsHeader
#' @export
lcarsh1 <- function(title = NULL, color = "atomic-tangerine", title_color = color,
                    background_color = "#000000",
                    title_align = c("center", "right", "left"),
                    title_invert = FALSE, width = "auto"){
  title_align <- match.arg(title_align)
  .lcarshx(title, "h1", color, title_color, background_color,
           title_align, title_invert, width)
}

#' @name lcarsHeader
#' @export
lcarsh2 <- function(title = NULL, color = "atomic-tangerine", title_color = color,
                    background_color = "#000000",
                    title_align = c("center", "right", "left"),
                    title_invert = FALSE, width = "auto"){
  title_align <- match.arg(title_align)
  .lcarshx(title, "h2", color, title_color, background_color,
           title_align, title_invert, width)
}

#' @name lcarsHeader
#' @export
lcarsh3 <- function(title = NULL, color = "atomic-tangerine", title_color = color,
                    background_color = "#000000",
                    title_align = c("center", "right", "left"),
                    title_invert = FALSE, width = "auto"){
  title_align <- match.arg(title_align)
  .lcarshx(title, "h3", color, title_color, background_color,
           title_align, title_invert, width)
}

#' @name lcarsHeader
#' @export
lcarsh4 <- function(title = NULL, color = "atomic-tangerine", title_color = color,
                    background_color = "#000000",
                    title_align = c("center", "right", "left"),
                    title_invert = FALSE, width = "auto"){
  title_align <- match.arg(title_align)
  .lcarshx(title, "h4", color, title_color, background_color,
           title_align, title_invert, width)
}

#' @name lcarsHeader
#' @export
lcarsh5 <- function(title = NULL, color = "atomic-tangerine", title_color = color,
                    background_color = "#000000",
                    title_align = c("center", "right", "left"),
                    title_invert = FALSE, width = "auto"){
  title_align <- match.arg(title_align)
  .lcarshx(title, "h5", color, title_color, background_color,
           title_align, title_invert, width)
}

#' @name lcarsHeader
#' @export
lcarsh6 <- function(title = NULL, color = "atomic-tangerine", title_color = color,
                    background_color = "#000000",
                    title_align = c("center", "right", "left"),
                    title_invert = FALSE, width = "auto"){
  title_align <- match.arg(title_align)
  .lcarshx(title, "h6", color, title_color, background_color,
           title_align, title_invert, width)
}

.lcarshx <- function(title, heading, color, title_color, background_color,
                     title_align, title_invert, width){
  width <- shiny::validateCssUnit(width)
  if(is.null(width)) width <- "100%"
  x <- .lcars_color_check(c(color, title_color, background_color))
  h <- .lcars_hdr_ht(heading, 30)
  w <- round(1.5 * h)
  title_div <- div(class = "lcars-hx-title",
                   style = paste0("color:", if(title_invert) x[3] else x[2],
                                  ";background-color:", if(title_invert) x[2] else x[3],
                                  ";font-size: ", h, "px;line-height:", h - 1,
                                  "px;text-align:", title_align, ";"), title)

  div(style = "margin: 4px 0px;",
    div(class = "lcars-hx",
        style = paste0("width:", width, ";",
                       "grid-template-columns: ", w, "px auto ", w, "px;",
                       "grid-template-rows: ", h, "px;"),
        div(class = "hx-pill-left", style = paste0("height:", h, "px;width:", w, "px;line-height:", h, "px;"),
            shiny::HTML(paste0('<svg style = "fill:', x[1], ';height:', h, 'px;width:', w, 'px;">
             <use xlink:href="svg/sprites.svg#lcars-svg-endcap_left" height="', h, '" width="', w, '"></use>
             </svg>'))
        ),
        title_div,
        div(class = "hx-pill-right", style = paste0("height:", h, "px;width:", w, "px;line-height:", h, "px;"),
            shiny::HTML(paste0('<svg style = "fill:', x[1], ';height:', h, 'px;width:', w, 'px;">
             <use xlink:href="svg/sprites.svg#lcars-svg-endcap_right" height="', h, '" width="', w, '"></use>
             </svg>'))
        )
    )
  )
}

.lcars_hdr_ht <- function(heading, x = 30){
  x <- round(seq(1, 0.5, length.out = 6) * x)
  switch(heading, "h1" = x[1], "h2" = x[2], "h3" = x[3],
         "h4" = x[4], "h5" = x[5], "h6" = x[6])
}

#' LCARS well
#'
#' A simple LCARS well panel wrapper that takes color and background color arguments and understands LCARS color names.
#'
#' @param ... panel contents.
#' @param color border color. Any hex color or a named LCARS color.
#' @param background_color background color. Any hex color or a named LCARS color.
#'
#' @return a div
#' @export
lcarsWell <- function(..., color = "atomic-tangerine", background_color = "#111111"){
  x <- .lcars_color_check(c(color, background_color))
  div(class = "well",
      style = paste0("border-color:", x[1], ";background-color:", x[2], ";"), ...)
}

#' Launch LCARS demo apps.
#'
#' Currently available apps include: \code{demo}, \code{box}, \code{toggle}.
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
  if(id == "box"){
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
  y <- match(x[!idx], lcars::lcarsdata$name)
  if(any(is.na(y))) stop("Invalid LCARS color name. Provide non-LCARS colors in hex format.")
  x[!idx] <- lcars::lcarsdata$value[y]
  x
}
