#' LCARS Shiny UI
#'
#' Create a Shiny UI page with an LCARS theme.
#'
#' The LCARS style heavily emphasizes uppercase text. Set \code{force_uppercase = TRUE} to force this standard via CSS. This does not make everything uppercase; things like input labels are left alone. However, text in general in uppercased.
#' Although it deviates from traditional LCARS, the default is \code{FALSE}.
#' This makes it easier for R developers to exert sensible judgment and control over how to balance the tension between
#' making something that conforms well to the familiar LCARS aesthetic and making something that communicates information with a lower cognitive load for the user.
#'
#' @param ... The contents of the document body.
#' @param title The browser window title (defaults to the host URL of the page).
#' @param force_uppercase logical, see details.
#'
#' @return A UI definition that can be passed to the shinyUI function.
#' @export
lcarsPage <- function (..., title = NULL, force_uppercase = FALSE){
  shiny::bootstrapPage(div(class = "container-fluid", lcars_init(force_uppercase), ...), title = title)
}

lcars_init <- function(force_uppercase = FALSE){
  shiny::addResourcePath("svg", system.file("www/svg", package = "lcars"))
  shiny::tagList(
    shiny::includeCSS(system.file("www/css/lcars.css", package = "lcars")),
    if(force_uppercase)
      shiny::includeCSS(system.file("www/css/uppercase.css", package = "lcars"))
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

#' LCARS toggle button
#'
#' An LCARS styled toggle button that can be used in place of \code{checkboxInput}.
#'
#' @param inputId The input slot that will be used to access the value.
#' @param label Display label for the control, or NULL for no label.
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
#' @param width character, valid CSS unit, e.g. \code{"100px"}. Using a percentage works, but not as well. Fixed widths recommended.
#'
#' @return a toggle button control that can be added to a UI
#' @export
#'
#' @examples
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
                        label_color = "#FFFFFF", label_right = FALSE, width = "150px"){
  value <- shiny::restoreInput(id = inputId, default = value)
  x <- c(true_color, false_color, background_color, border_color, outer_color, label_color)
  x <- .lcars_color_check(x)
  cl <- if(inverse) "inverse" else "default"
  cl <- paste0("lcars-toggle-", cl, " lcars-toggle-tf")
  if(is.null(width)) width <- "100px"
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
        div(class = "lcars-toggle",
          tags$input(type = "checkbox", name = "lcars-toggle", class = "lcars-toggle-checkbox",
                     id = inputId, checked = if(value) "" else NULL),
          tags$label(class = "lcars-toggle-label",
          style = paste0("text-align:left;border: 2px solid ", x[4], ";width:", width, ";--tog-w:", width, ";",
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
