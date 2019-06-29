#' LCARS Shiny UI
#'
#' Create a Shiny UI page with an LCARS theme.
#'
#' @param ... The contents of the document body.
#' @param title The browser window title (defaults to the host URL of the page).
#'
#' @return A UI definition that can be passed to the shinyUI function.
#' @export
lcarsPage <- function (..., title = NULL){
  shiny::bootstrapPage(div(class = "container-fluid", lcars_init(), ...), title = title)
}

lcars_init <- function(){
  shiny::addResourcePath("svg", system.file("www/svg", package = "lcars"))
  shiny::includeCSS(system.file("www/css/lcars.css", package = "lcars"))
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
#' An LCARS button column is a container for \code{lcarsButton} that can be passed to \code{lcarsBox}. The buttons occur vertically in the left side panel of the box.
#'
#' @param ... \code{lcarsBUtton} objects.
#'
#' @export
buttonColumn <- function(...){
  div(class = "main_button_column", ...)
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
      HTML('<svg style = "fill:', color, ';">
           <use xlink:href="svg/sprites.svg#lcars-svg-endcap_left" height="30" width="45"></use>
           </svg>')
    ),
    div(class ="blocktext_black lcars-hdr-rect"),
    if(!is.null(title)) div(class = "lcars-hdr-title", title),
    div(class = "hdr-pill-right",
      HTML('<svg style = "fill:', color[1], ';">
           <use xlink:href="svg/sprites.svg#lcars-svg-endcap_right" height="30" width="45"></use>
           </svg>')
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
  color <- .lcars_color_check(color)
  div(class = "lcars-box",
      div(class = if(is.null(title)) "lcars-box-top2" else "lcars-box-top",
          div(class = "lcars-box-top-elbow-left",
              HTML('<svg style = "fill:', color[1], ';">
                   <use xlink:href="svg/sprites.svg#lcars-svg-elbow_left_top" height="90" width="300"></use>
                   </svg>')
          ),
          div(class = "lcars-rect-top", style = paste0("background-color:", color[1], ";")),
          if(!is.null(title)) div(class = "lcars-box-title", title, style = paste0("color:", title_color[1], ";")),
          div(class = "lcars-box-top-pill-right",
              HTML('<svg style = "fill:', color[1], ';">
                   <use xlink:href="svg/sprites.svg#lcars-svg-endcap_right" height="30" width="45"></use>
                   </svg>')
          )
      ),
      div(class = "lcars-box-center",
          div(class = "lcars-box-buttons",
              if(!is.null(buttonColumn)) buttonColumn,
              div(class = "lcars-rect-left", style = paste0("background-color:", color[2], ";"))
          ),
          div(class = "lcars-box-main", class = "text", ...)
      ),
      div(class = if(is.null(subtitle)) "lcars-box-bot" else "lcars-box-bot2",
          div(class = "lcars-box-bot-elbow-left",
              HTML('<svg style = "fill:', color[3], ';">
                   <use xlink:href="svg/sprites.svg#lcars-svg-elbow_left_bottom" height="90" width="300"></use>
                   </svg>')
          ),
          div(class = "lcars-rect-bot", style = paste0("background-color:", color[3], ";")),
          if(!is.null(subtitle)) div(class = "lcars-box-subtitle", subtitle, style = paste0("color:", subtitle_color[1], ";")),
          div(class = "lcars-box-bot-pill-right",
              HTML('<svg style = "fill:', color[3], ';">
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
#' Currently available apps include: \code{demo}.
#'
#' @param id character, app id. Currently only \code{demo}.

#' @export
#' @examples
#' \dontrun{lcarsApp("demo")}
lcarsApp <- function(id = "demo"){
  if(id == "demo"){
    if(!requireNamespace("ggrepel", quietly = TRUE)){
      message("This app requires the `ggrepel` package. Install and rerun.")
      return(invisible())
    }
  }
  app <- system.file(file.path("shiny", id), package = "lcars")
  shiny::runApp(app)
}
