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
