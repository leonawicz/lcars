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
#' Similarly, set \code{lcars_font*} arguments to \code{FALSE} to use a more readbale sans serif font as desired.
#' See examples for recommendations.
#'
#' @param ... The contents of the document body.
#' @param title The browser window title (defaults to the host URL of the page).
#' @param force_uppercase logical, see details.
#' @param label_uppercase logical, also make widget labels uppercase globally.
#' @param lcars_font_headers use LCARS-style font famliy for major heading text. See details.
#' @param lcars_font_labels use LCARS-style font famliy for LCARS widget labels/titles.
#' @param lcars_font_text use LCARS-style font famliy for general text (paragraphs, lists, etc.).

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

#' LCARS button
#'
#' An LCARS wrapper around \code{shiny::actionButton} with additional color control.
#'
#' @param inputId The input slot that will be used to access the value.
#' @param label The contents of the button or link–usually a text label, but you could also use any other HTML, like an image.
#' @param icon An optional icon to appear on the button.
#' @param color button color. Can be any color given in hex format. Named colors must be LCARS colors. See \code{\link{lcarsColors}} for options.
#' @param hover_color Named colors must be LCARS colors. Other arbitrary colors are not supported for hovering. If \code{hover_color = NULL}, the button will darken on hover.
#' @param ... Named attributes to be applied to the button.
#'
#' @seealso \code{\link{lcarsColors}}
#' @export
lcarsButton <- function(inputId, label, icon = NULL, color = "atomic-tangerine",
                        hover_color = "red-damask", ...){
  cl <- if(!is.null(hover_color) && hover_color %in% lcars::lcarsColors$name)
    paste0("lcars-btn lcars-btn-", hover_color) else "lcars-btn lcars-btn-filtered"
  x <- .lcars_color_check(c(color, hover_color))
  style <- paste0("background-color:", x[1], ";")
  shiny::actionButton(inputId, label, icon, NULL, class = cl, style = style, ...)
}

#' LCARS button column
#'
#' An LCARS button column is a container for \code{lcarsButton} and \code{lcarsToggle} buttons that can be passed to \code{lcarsBox}.
#' The buttons occur vertically in the left side panel of the box. Toggle button width should be 150px.
#'
#'
#' @param ... \code{lcarsButton} objects.
#'
#' @export
buttonColumn <- function(...){
  div(class = "lcars-button-col", ...)
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
  color <- .lcars_color_check(color)
  div(class = if(is.null(title)) "lcars-hdr2" else "lcars-hdr",
    div(class = "hdr-pill-left",
      shiny::HTML('<svg style = "fill:', color, ';">
           <use xlink:href="svg/sprites.svg#lcars-svg-endcap_left" height="30" width="45"></use>
           </svg>')
    ),
    div(class ="blocktext_black lcars-hdr-rect"),
    if(!is.null(title)) div(class = "lcars-hdr-title", title),
    div(class = "hdr-pill-right",
      shiny::HTML('<svg style = "fill:', color[1], ';">
           <use xlink:href="svg/sprites.svg#lcars-svg-endcap_right" height="30" width="45"></use>
           </svg>')
    )
  )
}

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
  value <- restoreInput(id = inputId, default = value)
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
#' @param true_color Color for \code{TRUE} position. Can be any color given in hex format. Named colors must be LCARS colors. See \code{\link{lcarsColors}} for options.
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

#' LCARS box
#'
#' An LCARS box.
#'
#' Box color can be any color given in hex format. Named colors must be LCARS colors. See \code{\link{lcarsColors}} for options.
#' Use a vector of three colors for the top, left side, and bottom box panels, respectively.
#'
#' @param ... box contents.
#' @param title character, box title at top right.
#' @param subtitle character, box subtitle at bottom right.
#' @param buttonColumn optional button column made with \code{buttonColumn}.
#' @param color box color. See details.
#' @param title_color text title color.
#' @param subtitle_color text subtitle color.
#'
#' @export
lcarsBox <- function(..., title = NULL, subtitle = NULL, buttonColumn = NULL,
                     color = "atomic-tangerine", title_color = color, subtitle_color = color){
  color <- rep(color, length = 3)
  x <- .lcars_color_check(c(color, title_color[1], subtitle_color[1]))
  div(class = "lcars-box",
    div(class = if(is.null(title)) "lcars-box-top2" else "lcars-box-top",
      div(class = "lcars-box-top-elbow-left",
        shiny::HTML('<svg style = "fill:', x[1], ';">
          <use xlink:href="svg/sprites.svg#lcars-svg-elbow_left_top" height="90" width="300"></use>
          </svg>')
      ),
      div(class = "lcars-rect-top", style = paste0("background-color:", x[1], ";")),
      if(!is.null(title)) div(class = "lcars-box-title", title, style = paste0("color:", x[4], ";")),
      div(class = "lcars-box-top-pill-right",
        shiny::HTML('<svg style = "fill:', x[1], ';">
          <use xlink:href="svg/sprites.svg#lcars-svg-endcap_right" height="30" width="45"></use>
          </svg>')
      )
    ),
    div(class = "lcars-box-center",
      div(class = "lcars-box-buttons",
        if(!is.null(buttonColumn)) buttonColumn,
        div(class = "lcars-rect-left", style = paste0("background-color:", x[2], ";"))
      ),
      div(class = "lcars-box-main", class = "text", ...)
    ),
    div(class = if(is.null(subtitle)) "lcars-box-bot" else "lcars-box-bot2",
      div(class = "lcars-box-bot-elbow-left",
        shiny::HTML('<svg style = "fill:', x[3], ';">
          <use xlink:href="svg/sprites.svg#lcars-svg-elbow_left_bottom" height="90" width="300"></use>
          </svg>')
      ),
      div(class = "lcars-rect-bot", style = paste0("background-color:", x[3], ";")),
      if(!is.null(subtitle)) div(class = "lcars-box-subtitle", subtitle, style = paste0("color:", x[5], ";")),
      div(class = "lcars-box-bot-pill-right",
        shiny::HTML('<svg style = "fill:', x[3], ';">
          <use xlink:href="svg/sprites.svg#lcars-svg-endcap_right" height="30" width="45"></use>
          </svg>')
      )
    )
  )
}

.lcars_color_check <- function(x){
  idx <- grepl("^#", x)
  if(all(idx)) return(x)
  y <- match(x[!idx], lcars::lcarsColors$name)
  if(any(is.na(y))) stop("Invalid LCARS color name. Provide non-LCARS colors in hex format.")
  x[!idx] <- lcars::lcarsColors$value[y]
  x
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
