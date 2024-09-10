#' LCARS box
#'
#' Create a configurable LCARS box.
#'
#' @details
#' This function allows you to customize the inclusion and colors of specific
#' border components of the box. The defaults are closer to standard LCARS
#' style.
#' You can turn on or off specific corner elbows, connecting side panels,
#' control colors of each, as well as title and subtitle inclusion, color and
#' alignment.
#'
#' @section Corner elbows:
#' Control which corners of the box display the characteristic LCARS elbow,
#' clockwise from top left. The top and bottom borders are independent of one
#' another. Each work in the same manner. For each, you can have a left elbow
#' (default), a right elbow, or both.
#'
#' When only one corner is present (on top or on bottom), the bar extends to
#' the other corner and terminates with the characteristic LCARS half pill if
#' the panel border is included (see side panel section below). If the side
#' between the elbow areas is excluded, only the elbows are displayed.
#'
#' If both elbows are excluded from the top or from the bottom, a simple,
#' straight `lcarsHeader()` element is placed above or below the main
#' content area instead, but this can be controlled via `sides`.
#'
#' @section Side panels:
#' Control which sides of the box include an LCARS-styled border, clockwise
#' from top left. Sides connect elbows using straight bars. The top and bottom
#' sides are where title and subtitle text are placed. The title for the top
#' and subtitle for the bottom are included in the bar with standard LCARS
#' right alignment, which can be switched to left. If the top or bottom side
#' panel is excluded, the vertical space remains if `title` or `subtitle` are
#' included, respectively, retaining the text labels; otherwise the space is
#' removed.
#'
#' By default, left and right sides are 150 pixels wide; top and bottom sides
#' are 30 pixels tall. The top and bottom are fixed, but the widths of the left
#' and right side panels can be adjusted using `width_left` and `width_right`,
#' respectively. They can only be adjusted down to smaller widths. This is to
#' ensure proper scaling for connected corners. The side panels are not meant to
#' accommodate wider inputs and should primarily be used for small buttons and
#' short text.
#'
#' @section Side inputs columns:
#' Input columns are different from left and right sides. The latter refers to
#' whether or not there are vertical connecting bars from elbow to elbow.
#' An input column represents a separate element that is placed in the left or
#' right side panel area above the plain side panel bar itself.
#'
#' If the side is included and a column of inputs is provided, they combine
#' vertically to form the side panel. Some amount of plain sidebar will pad the
#' bottom beneath any input column, however tall. If the side is excluded, the
#' input column will take up the entire vertical space.
#'
#' If the side is excluded and no input column is provided, the side panel area
#' is blank. The main content area extends left or right to fill any completely
#' missing left or right side panel. To restrict this, use a black side panel
#' to match the background.
#'
#' Since the inputs contained in an input column are defined separately and
#' passed to `lcarsBox()`, they should be defined to have widths that match
#' the box side panel widths.
#'
#' @section Colors:
#' Box color can be any color given in hex format. Named colors must be LCARS
#' colors. See [lcarsdata] for options. By default, all border colors inherit
#' from a single color passed to `color`.
#'
#' `color` is recycled to length four as needed. `color` actually defines all
#' four corner elbow colors. For corner elbows, use a vector of four colors for
#' the top left, top right, bottom right, and bottom left, respectively.
#'
#' Similarly for the bars between elbows with `side_colors`, use a vector
#' of four colors for the top, right side, bottom, and left side. This is also
#' recycled to length four. If not provided, it inherits from `color`.
#'
#' `title_color` and `subtitle_color` are scalar. They inherit from the first
#' color in `color`.
#'
#' @section Margin space:
#' When at least one corner elbow is present on a top or bottom side, that side
#' will include empty margin space to the inside of the elbow. This space is
#' part of the grid area of the side panel. This is why main panel content does
#' not extend into it. You can override this and make use of this space by
#' setting `clip = FALSE`.
#'
#' Note that this should only be done when both side panels are present so that
#' the main panel content is not directly under or above the elbow near the
#' extreme edge of the box. If you do not want a side panel, you can include
#' it, but set its color to match the background.
#'
#' @section Sizing:
#' There are limitations to the container responsiveness of the LCARS box and
#' sweep. In some cases, using percentage width, e.g., `width = "100%"` will
#' work, but it may respond sluggishly or may not work at all. Fixed pixel
#' width is recommended for `lcarsBox()` and `lcarsSweep()`. Regardless of
#' responsiveness, these widgets are also not intended to fit very small displays.
#'
#' @param ... box contents.
#' @param title character, box title at top right.
#' @param subtitle character, box subtitle at bottom right.
#' @param corners integer, `1:4`, a vector specifying which corner elbows
#' to include: top left, top right, bottom right, bottom left. See details.
#' @param sides integer, `1:4`, a vector specifying which sides to include
#' panels: top, right, bottom, left. See details.
#' @param left_inputs optional input column for left side, for example a column
#' of buttons made with `inputColumn()`. See details.
#' @param right_inputs optional input column for right side, for example a
#' column of buttons made with `inputColumn()`. See details.
#' @param color box border colors. See details.
#' @param side_color box border colors. See details.
#' @param title_color text title color.
#' @param subtitle_color text subtitle color.
#' @param title_right logical, right align title.
#' @param subtitle_right logical, right align subtitle.
#' @param clip logical, use empty margin space. See details.
#' @param width_left numeric, the width of the left side panel in pixels. This
#' also adjusts associated corner elbows to match. Defaults to the maximum
#' allowed: 150.
#' @param width_right numeric, the width of the right side panel in pixels.
#' This also adjusts associated corner elbows to match. Defaults to the maximum
#' allowed: 150.
#' @param width a valid CSS unit, the width of the entire box. Fixed pixel
#' width recommended. See details.
#'
#' @return an HTML widget
#' @export
#' @seealso [lcarsSweep()]
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#'   ui <- lcarsPage(
#'     lcarsBox(
#'       fluidRow(
#'         column(3,
#'           h4("Main panel area"),
#'           HTML("<p>Some paragraph text and <a href='#'>a link</a>
#'           with LCARS styling.</p>
#'           <p>Use <code>lcarsPage</code>
#'           to apply the LCARS theme and <code>lcarsBox</code>
#'           to draw a characteristic box for framing content.</p>
#'           <p>Many of the <code>lcarsBox</code>
#'           properties are configurable.
#'           See <code>lcars::lcarsApp(\"box\")</code> for a demo</p>")
#'         ),
#'         column(9, plotOutput("plot1"))
#'       ),
#'       title = "box title",
#'       left_inputs = inputColumn(lcarsButton("btn1", "A button"))
#'     )
#'   )
#'
#'   server <- function(input, output) {
#'     output$plot1 <- renderPlot({
#'       hist(rnorm(500))
#'     })
#'   }
#'
#'   shinyApp(ui, server)
#' }
lcarsBox <- function(..., title = NULL, subtitle = NULL, corners = c(1, 4),
                     sides = c(1, 3, 4),
                     left_inputs = NULL, right_inputs = NULL,
                     color = "atomic-tangerine", side_color = color,
                     title_color = color, subtitle_color = color,
                     title_right = TRUE, subtitle_right = TRUE, clip = TRUE,
                     width_left = 150, width_right = 150, width = "100%"){
  width <- shiny::validateCssUnit(width)
  width_left <- as.numeric(width_left)
  width_right <- as.numeric(width_right)
  wrn <- "Side panel widths must each be <= 150 pixels to ensure proper corner scaling."
  if(width_left > 150){
    warning(wrn)
    width_left <- 150
  }
  if(width_right > 150){
    warning(wrn)
    width_right <- 150
  }
  if(is.null(width)) width <- "100%"

  if(is.null(corners)){
    corners <- 0
  } else {
    if(any(!corners %in% 1:4)) stop("`corners` must be values in 1:4 or NULL.")
  }
  if(is.null(sides)){
    sides <- 0
  } else {
    if(any(!sides %in% 1:4)) stop("`sides` must be values in 1:4 or NULL.")
  }

  color <- c(rep(color, length = 4), rep(side_color, length = 4))
  x <- .lcars_color_check(c(color, title_color[1], subtitle_color[1]))
  topclass <- .box_top_class(corners, title, title_right)
  botclass <- .box_bot_class(corners, subtitle, subtitle_right)
  left_input <- !is.null(left_inputs)
  right_input <- !is.null(right_inputs)
  centerclass <- .box_center_class(sides, left_input, right_input)
  centerstyle <- .box_center_style(centerclass, width_left, width_right)
  left_bar <- !is.null(sides) && 4 %in% sides
  right_bar <- !is.null(sides) && 2 %in% sides
  title_div <- div(class = "lcars-box-title", title,
                   style = paste0("color:", x[9], ";"))
  subtitle_div <- div(class = "lcars-box-subtitle", subtitle,
                      style = paste0("color:", x[10], ";"))

  div(class = "lcars-box",
      style = paste0("grid-template-areas: ", topclass, " ", centerclass, " ",
                     botclass, ";", "width:", width, ";"),
      if(grepl("none", topclass)){
        if(is.null(title)){
          if(1 %in% sides) lcarsHeader(NULL, x[5], x[9])
        } else {
          lcarsHeader(title, if(1 %in% sides) x[5] else "#000000", x[9],
                      title_right = title_right)
        }
      } else {
        div(class = topclass,
            style = paste0("--corner-width-left: ", 2 * width_left,
                           "px; --corner-width-right: ", 2* width_right, "px;"),
            .box_tl_div(corners, x[1], width_left),
            if(!is.null(title) & !title_right) title_div,
            div(class = "lcars-rect-top",
                style = paste0("background-color:",
                               if(1 %in% sides) x[5] else "#000000", ";")),
            if(!is.null(title) & title_right) title_div,
            .box_tr_div(corners, x[2], width_right)
        )
      },
      div(class = centerclass,
          style = centerstyle,
          if(left_bar | left_input){
            div(class  = "lcars-box-buttons-left",
                style = if(left_bar){
                  "grid-template-rows: auto minmax(60px, 1fr); grid-template-areas: 'lcars-button-col' 'lcars-rect-side';"
                } else {
                  "grid-template-rows: auto; grid-template-areas: 'lcars-button-col';"
                },
                if(left_input) left_inputs,
                if(left_bar) div(class = "lcars-rect-side",
                                 style = paste0("background-color:", x[8], ";"))
            )
          },
          div(class = "lcars-box-main text",
              style = if(!clip) "margin: -45px 0 -45px 0;", ...),
          if(right_bar | right_input){
            div(class = "lcars-box-buttons-right",
                style = if(right_bar){
                  "grid-template-rows: auto minmax(60px, 1fr); grid-template-areas: 'lcars-button-col' 'lcars-rect-side';"
                } else {
                  "grid-template-rows: auto; grid-template-areas: 'lcars-button-col';"
                },
                if(right_input) right_inputs,
                if(right_bar) div(class = "lcars-rect-side",
                                  style = paste0("background-color:", x[6], ";"))
            )
          }
      ),
      if(grepl("none", botclass)){
        if(is.null(subtitle)){
          if(3 %in% sides) lcarsHeader(NULL, x[7], x[10])
        } else {
          lcarsHeader(subtitle, if(3 %in% sides) x[7] else "#000000", x[10],
                      title_right = subtitle_right)
        }
      } else {
        div(class = botclass,
            style = paste0("--corner-width-left: ", 2 * width_left,
                           "px; --corner-width-right: ", 2* width_right, "px;"),
            .box_bl_div(corners, x[4], width_left),
            if(!is.null(subtitle) & !subtitle_right) subtitle_div,
            div(class = "lcars-rect-bot",
                style = paste0("background-color:",
                               if(3 %in% sides) x[7] else "#000000", ";")),
            if(!is.null(subtitle) & subtitle_right) subtitle_div,
            .box_br_div(corners, x[3], width_right)
        )
      }
  )
}

.box_center_class <- function(sides, left, right){
  if((all(c(2, 4) %in% sides)) | (left & right) | (2 %in% sides & left) | (4 %in% sides & right)){
    x <- "lcars-box-center"
  } else if(4 %in% sides | left){
    x <- "lcars-box-centerleft"
  } else if(2 %in% sides | right){
    x <- "lcars-box-centerright"
  } else {
    x <- "lcars-box-centernone"
  }
  x
}

.box_center_style <- function(x, wl, wr){
  if(x == "lcars-box-centernone") return("")
  switch(x,
    "lcars-box-center" = paste0("grid-template-columns: ", wl, "px auto ", wr, "px;"),
    "lcars-box-centerleft" = paste0("grid-template-columns: ", wl, "px auto;"),
    "lcars-box-centerright" = paste0("grid-template-columns: auto ", wr, "px;"))
}

.box_top_class <- function(corners, title, right){
  if(all(1:2 %in% corners)){
    x <- "lcars-box-top"
  } else if(1 %in% corners){
    x <- "lcars-box-topleft"
  } else if(2 %in% corners){
    x <- "lcars-box-topright"
  } else {
    x <- "lcars-box-topnone"
  }
  if(is.null(title)){
    x <- paste0(x, 2)
  } else if(!right){
    x <- paste0(x, "-ljust")
  }
  x
}

.box_tl_div <- function(corners, color, w){
  w <- 2 * w
  r <-  300 / w
  if(1 %in% corners){
    div(class = "lcars-box-top-elbow-left",
        shiny::HTML(paste0('<svg style = "fill:', color, ';height:90px;width:', w, 'px;">
                    <use xlink:href="svg/sprites.svg#lcars-svg-elbow_left_top" height="90" width="', w, '" transform="translate(0 ', -(r - 1) * 45, ') scale(1, ', r, ')"></use>
                    </svg>'))
    )
  } else {
    div(class = "lcars-box-top-pill-left",
        shiny::HTML('<svg style = "fill:', color, ';height:30px;width:45px;">
                    <use xlink:href="svg/sprites.svg#lcars-svg-endcap_right" height="30" width="45" transform="rotate(180 22.5 15)"></use>
                    </svg>')
    )
  }
}

.box_tr_div <- function(corners, color, w){
  w <- 2 * w
  r <-  300 / w
  if(2 %in% corners){
    div(class = "lcars-box-top-elbow-right",
        shiny::HTML(paste0('<svg style = "fill:', color, ';height:90px;width:', w, 'px;">
                    <use xlink:href="svg/sprites.svg#lcars-svg-elbow_left_bottom" height="90" width="', w, '" transform="rotate(180, 150, 45) translate(', 300 - w, ' ', -(r - 1) * 45, ') scale(1, ', r, ')"></use>
                    </svg>'))
    )
  } else {
    div(class = "lcars-box-top-pill-right",
        shiny::HTML('<svg style = "fill:', color, ';height:30px;width:45px;">
                    <use xlink:href="svg/sprites.svg#lcars-svg-endcap_right" height="30" width="45"></use>
                    </svg>')
    )
  }
}

.box_bot_class <- function(corners, title, right){
  if(all(3:4 %in% corners)){
    x <- "lcars-box-bot"
  } else if(4 %in% corners){
    x <- "lcars-box-botleft"
  } else if(3 %in% corners){
    x <- "lcars-box-botright"
  } else {
    x <- "lcars-box-botnone"
  }
  if(!is.null(title)){
    x <- paste0(x, 2)
    if(!right) x <- paste0(x, "-ljust")
  }
  x
}

.box_bl_div <- function(corners, color, w){
  w <- 2 * w
  r <-  300 / w
  if(4 %in% corners){
    div(class = "lcars-box-bot-elbow-left",
        shiny::HTML(paste0('<svg style = "fill:', color, ';height:90px;width:', w, 'px;">
                    <use xlink:href="svg/sprites.svg#lcars-svg-elbow_left_bottom" height="90" width="', w,
                    '" transform="translate(0 ', -(r - 1) * 45, ') scale(1, ', r, ')"></use>
                    </svg>'))
    )
  } else {
    div(class = "lcars-box-bot-pill-left",
        shiny::HTML('<svg style = "fill:', color, ';height:30px;width:45px;">
                    <use xlink:href="svg/sprites.svg#lcars-svg-endcap_right" height="30" width="45" transform="rotate(180 22.5 15)"></use>
                    </svg>')
    )
  }
}

.box_br_div <- function(corners, color, w){
  w <- 2 * w
  r <-  300 / w
  if(3 %in% corners){
    div(class = "lcars-box-bot-elbow-right",
        shiny::HTML(paste0('<svg style = "fill:', color, ';height:90px;width:', w, 'px;">
                    <use xlink:href="svg/sprites.svg#lcars-svg-elbow_left_top" height="90" width="', w,
                    '" transform="rotate(180, 150, 45) translate(', 300 - w, ' ', -(r - 1) * 45, ') scale(1, ', r, ')"></use>
                    </svg>'))
    )
  } else {
    div(class = "lcars-box-bot-pill-right",
        shiny::HTML('<svg style = "fill:', color, ';height:30px;width:45px;">
                    <use xlink:href="svg/sprites.svg#lcars-svg-endcap_right" height="30" width="45"></use>
                    </svg>')
    )
  }
}
