#' LCARS box
#'
#' An LCARS box.
#'
#' Box color can be any color given in hex format. Named colors must be LCARS colors. See \code{\link{lcarsColors}} for options.
#' Use a vector of four colors for the top, right side, bottom, and left side box panels, respectively.
#'
#' @param ... box contents.
#' @param title character, box title at top right.
#' @param subtitle character, box subtitle at bottom right.
#' @param corners integer, \code{1:4}, a vector specifying which corner elbows to include: top left, top right, bottom right, bottom left.
#' @param buttonColumn optional button column made with \code{buttonColumn}.
#' @param color box color. See details.
#' @param title_color text title color.
#' @param subtitle_color text subtitle color.
#'
#' @export
lcarsBox <- function(..., title = NULL, subtitle = NULL,
                     corners = c(1, 4),
                     buttonColumn = NULL,
                     color = "atomic-tangerine", title_color = color, subtitle_color = color){
  if(!is.null(corners)){
    if(any(!corners %in% 1:4)) stop("`corners` must be values in 1:4 or NULL.")
  }
  color <- rep(color, length = 4)
  x <- .lcars_color_check(c(color, title_color[1], subtitle_color[1]))
  topclass <- .box_top_class(title, corners)
  botclass <- .box_bot_class(subtitle, corners)
  div(class = "lcars-box",
      style = paste0("grid-template-areas: ", topclass, " lcars-box-centerleft ", botclass, ";"),
      if(grepl("none", topclass)){
        if(!is.null(title)) lcarsHeader(title, x[1], x[5])
      } else {
        div(class = topclass,
            .box_tl_div(corners, x[1]),
            div(class = "lcars-rect-top", style = paste0("background-color:", x[1], ";")),
            if(!is.null(title)) div(class = "lcars-box-title", title, style = paste0("color:", x[5], ";")),
            .box_tr_div(corners, x[1])
        )
      },
      div(class = "lcars-box-centerleft",
          div(class = "lcars-box-buttons",
              if(!is.null(buttonColumn)) buttonColumn,
              div(class = "lcars-rect-left", style = paste0("background-color:", x[4], ";"))
          ),
          div(class = "lcars-box-main", class = "text", ...)
      ),
      if(grepl("none", botclass)){
        if(!is.null(subtitle)) lcarsHeader(subtitle, x[3], x[6])
      } else {
        div(class = botclass,
            .box_bl_div(corners, x[3]),
            div(class = "lcars-rect-bot", style = paste0("background-color:", x[3], ";")),
            if(!is.null(subtitle)) div(class = "lcars-box-subtitle", subtitle, style = paste0("color:", x[6], ";")),
            .box_br_div(corners, x[3])
        )
      }
  )
}

.box_top_class <- function(title, corners){
  if(all(1:2 %in% corners)){
    x <- "lcars-box-top"
  } else if(1 %in% corners){
    x <- "lcars-box-topleft"
  } else if(2 %in% corners){
    x <- "lcars-box-topright"
  } else {
    x <- "lcars-box-topnone"
  }
  if(is.null(title)) x <- paste0(x, 2)
  x
}

.box_tl_div <- function(corners, color){
  if(1 %in% corners){
    div(class = "lcars-box-top-elbow-left",
        shiny::HTML('<svg style = "fill:', color[1], ';">
                    <use xlink:href="svg/sprites.svg#lcars-svg-elbow_left_top" height="90" width="300"></use>
                    </svg>')
    )
  } else {
    div(class = "lcars-box-top-pill-left",
        shiny::HTML('<svg style = "fill:', color[1], ';"">
                    <use xlink:href="svg/sprites.svg#lcars-svg-endcap_right" height="30" width="45" transform="rotate(180 22.5 15)"></use>
                    </svg>')
    )
  }
}

.box_tr_div <- function(corners, color){
  if(2 %in% corners){
    div(class = "lcars-box-top-elbow-right",
        shiny::HTML('<svg style = "fill:', color[1], ';">
                    <use xlink:href="svg/sprites.svg#lcars-svg-elbow_left_bottom" height="90" width="300" transform="rotate(180, 150, 45)"></use>
                    </svg>')
    )
  } else {
    div(class = "lcars-box-top-pill-right",
        shiny::HTML('<svg style = "fill:', color[1], ';"">
                    <use xlink:href="svg/sprites.svg#lcars-svg-endcap_right" height="30" width="45"></use>
                    </svg>')
    )
  }
}

.box_bot_class <- function(title, corners){
  if(all(3:4 %in% corners)){
    x <- "lcars-box-bot"
  } else if(4 %in% corners){
    x <- "lcars-box-botleft"
  } else if(3 %in% corners){
    x <- "lcars-box-botright"
  } else {
    x <- "lcars-box-botnone"
  }
  if(!is.null(title)) x <- paste0(x, 2)
  x
}

.box_bl_div <- function(corners, color){
  if(4 %in% corners){
    div(class = "lcars-box-bot-elbow-left",
        shiny::HTML('<svg style = "fill:', color[1], ';">
                    <use xlink:href="svg/sprites.svg#lcars-svg-elbow_left_bottom" height="90" width="300"></use>
                    </svg>')
    )
  } else {
    div(class = "lcars-box-bot-pill-left",
        shiny::HTML('<svg style = "fill:', color[1], ';"">
                    <use xlink:href="svg/sprites.svg#lcars-svg-endcap_right" height="30" width="45" transform="rotate(180 22.5 15)"></use>
                    </svg>')
    )
  }
}

.box_br_div <- function(corners, color){
  if(3 %in% corners){
    div(class = "lcars-box-bot-elbow-right",
        shiny::HTML('<svg style = "fill:', color[1], ';">
                    <use xlink:href="svg/sprites.svg#lcars-svg-elbow_left_top" height="90" width="300" transform="rotate(180, 150, 45)"></use>
                    </svg>')
    )
  } else {
    div(class = "lcars-box-bot-pill-right",
        shiny::HTML('<svg style = "fill:', color[1], ';"">
                    <use xlink:href="svg/sprites.svg#lcars-svg-endcap_right" height="30" width="45"></use>
                    </svg>')
    )
  }
}
