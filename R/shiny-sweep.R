#' LCARS sweep
#'
#' Create an LCARS sweep; the 'S' or reverse-'S' shape comprised of two LCARS elbows pointing in opposite directions.
#' The sweep is effectively two adjacent LCARS boxes separated by an input column and some specific styling to achieve the sweep display.
#'
#' There are limitations to the container responsiveness of the LCARS box and sweep. In some cases, using percentage width, e.g., \code{width = "100\%"} will work, but it may respond sluggishly or may not work at all.
#' Fixed pixel width is recommended for \code{lcarsBox} and \code{lcarsSweep}. Regardless of responsiveness, these widgets are also not intended to fit very small displays.
#'
#' @param column_inputs optional input column for right side, for example a column of buttons made with \code{inputColumn}. See details.
#' @param left_inputs content on the left side of the sweep.
#' @param right_inputs content on the right side of the sweep.
#' @param title character, title for box with header.
#' @param subtitle character, subtitle for box with footer.
#' @param color sweep elbow colors. Any hex color or a named LCARS color.
#' @param reverse logical, create a reverse sweep.
#' @param expand integer, length-2 vector, the number of pixels to expand the left and right content containers above or below the implicit border; the top or bottom border where no sweep is present. See example.
#' @param column_width integer, width of the sweep column section in pixels. Must be in pixels, 150 maximum. Smaller is permitted but will not conform as well to LCARS style.
#' @param left_width numeric, number between 0 and 1 giving the proportional width of the left content section. The right section is 1 - \code{left_width}.
#' @param width a valid CSS unit, the width of the entire sweep. Fixed pixel width recommended. See details.
#'
#' @return an HTML widget
#' @export
#' @seealso \code{\link{lcarsBox}}
#'
#' @examples
#' ## Only run examples in interactive R sessions
#' if (interactive()) {
#'
#'   library(ggplot2)
#'   d <- data.frame(x = rnorm(500))
#'   g <- ggplot(d, aes(x)) + theme_lcars_dark()
#'   g1 <- g + geom_histogram(color = "black", fill = lcarsdata$value[11], bins = 20) +
#'     ggtitle("Plot 1")
#'   left <- div(h4("Some text"), p("The fine print."))
#'
#'   ui <- lcarsPage(
#'     lcarsHeader("LCARS sweep"),
#'     h4("Change colors and relative widths of content sections"),
#'     h4("Add title and subtitle, input column padding, and content"),
#'     lcarsSweep(
#'       inputColumn(
#'         lcarsButton("x1", "Button"),
#'         lcarsRect(color = "hopbush", height = 80)
#'       ),
#'       left, plotOutput("plot1", height = 650), # plot taller than sweep box
#'       title = "Title", subtitle = "Subtitle",
#'       color = "pale-canary", left_width = 0.3, width = 900,
#'       expand = c(0, 350) # negative bottom margin added to right side div
#'     ),
#'     # content from the sweep box above extends into the sweep box below
#'     lcarsSweep(
#'       inputColumn(
#'         lcarsButton("x2", "Button A"),
#'         lcarsButton("x3", "Button B"),
#'         lcarsRect(color = "lilac")
#'       ),
#'       left, title = "Title 2", subtitle = "Subtitle 2",
#'       color = "anakiwa", reverse = TRUE, left_width = 0.3, width = 900
#'     )
#'   )
#'
#'   server <- function(input, output) {
#'     output$plot1 <- renderPlot(g1)
#'   }
#'
#'   shinyApp(ui, server)
#' }
lcarsSweep <- function(column_inputs = NULL, left_inputs = NULL, right_inputs = NULL,
                       title = NULL, subtitle = NULL,
                       color = "atomic-tangerine", reverse = FALSE, expand = c(0, 0),
                       column_width = 150, left_width = 0.5, width = "100%"){
  width <- shiny::validateCssUnit(width)
  if(is.null(width)) width <- "100%"
  column_width <- as.numeric(column_width)
  if(left_width < 0 | left_width > 1) stop("`left_width` must be between 0 and 1.")
  wl <- round(100 * left_width)
  wr <- 100 - wl
  wrn <- "Column width must be <= 150 pixels to ensure proper corner scaling."
  if(column_width > 150){
    warning(wrn)
    column_width <- 150
  }
  x <- .lcars_color_check(color)
  col_input <- !is.null(column_inputs)
  if(reverse){
    cl <- c("lcars-sweep-top-rev", "lcars-sweep-center", "lcars-sweep-bot-rev")
  } else {
    cl <- c("lcars-sweep-top", "lcars-sweep-center", "lcars-sweep-bot")
  }
  w <- column_width * ((190 / 150) - 1)
  style <- paste0("--width: ", w, "px;--width0: ", w - 20, "px;")
  expand <- as.numeric(expand)
  if(length(expand) == 1) expand <- rep(expand, 2)

  sweep_div <- div(class = "lcars-sweep",
    style = paste0("grid-template-areas: ", paste(cl, collapse = " "), ";"),
    div(class = cl[1], style = style, .sweep_top_div(reverse, x, column_width)),
    div(class = cl[2],
        style = style,
        if(col_input){
          div(class  = "lcars-box-buttons-left",
              style = "grid-template-rows: auto; grid-template-areas: 'lcars-button-col';",
              column_inputs
          )
        }
    ),
    div(class = cl[3], style = style, .sweep_bottom_div(reverse, x, column_width))
  )

  r <- c("right", "left")
  title_right <- TRUE
  if(reverse){
    r <- rev(r)
    title_right <- !title_right
  }
  headers <- list(lcarsHeader(title, x, title_right = title_right, round = r[1]),
                  lcarsHeader(subtitle, x, title_right = !title_right, round = r[2]))
  cl <- c("lcars-sweep-box-left", "lcars-sweep-box-right")
  style <- c("margin:10px 0 15px;", "margin:0 0 15px;")
  if(reverse){
    headers <- rev(headers)
    cl <- rev(cl)
    style <- rev(style)
  }

  div(class = if(reverse) "lcars-sweep-box-rev" else "lcars-sweep-box",
    style = paste0("width:", width, "; --width1: ", wl, "fr; --width2: ", column_width * 230 / 150,
                   "px; --width3: ", wr, "fr;"),
    div(class = cl[1], style = style[1],
      if(reverse){
        shiny::tagList(div(headers[[2]]), div(left_inputs))
      } else {
        shiny::tagList(div(left_inputs), div(headers[[2]]))
      }
    ),
    sweep_div,
    div(class = cl[2], style = style[2],
      if(reverse){
        shiny::tagList(
          div(style = paste0("margin-top:-", expand[1], "px;"), right_inputs),
          div(headers[[1]])
        )
      } else {
        shiny::tagList(
          div(headers[[1]]),
          div(style = paste0("margin-bottom:-", expand[2], "px;"), right_inputs)
        )
      }
    )
  )
}

.sweep_top_div <- function(reverse, color, w){
  w <- (190 / 150) * w
  r <-  190 / w
  if(reverse){
    div(class = "lcars-box-top-elbow-right",
        shiny::HTML(paste0('<svg style = "fill:', color, ';height:90px;width:', w, 'px;">
                    <use href="svg/elbow-narrow-sprites.svg#elbow-narrow-bl" height="90" width="', w,
                    '" transform="rotate(180, 150, 45) translate(', 300 - w, ' ', -(r - 1) * 45, ') scale(1, ', r, ')"></use>
                    </svg>'))
    )
  } else {
    div(class = "lcars-box-top-elbow-left",
        shiny::HTML(paste0('<svg style = "fill:', color, ';height:90px;width:', w, 'px;">
                    <use href="svg/elbow-narrow-sprites.svg#elbow-narrow-tl" height="90" width="', w,
                    '" transform="translate(0 ', -(r - 1) * 45, ') scale(1, ', r, ')"></use>
                    </svg>'))
    )
  }
}

.sweep_bottom_div <- function(reverse, color, w){
  w <- (190 / 150) * w
  r <-  190 / w
  if(reverse){
    div(class = "lcars-box-bot-elbow-left", style = "margin-top:-5px;",
        shiny::HTML(paste0('<svg style = "fill:', color, ';height:90px;width:', w, 'px;">
                    <use href="svg/elbow-narrow-sprites.svg#elbow-narrow-bl" height="90" width="', w,
                           '" transform="translate(0 ', -(r - 1) * 45, ') scale(1, ', r, ')"></use>
                    </svg>'))
    )
  } else {
    div(class = "lcars-box-bot-elbow-right", style = "margin-top:-5px;",
        shiny::HTML(paste0('<svg style = "fill:', color, ';height:90px;width:', w, 'px;">
                    <use href="svg/elbow-narrow-sprites.svg#elbow-narrow-tl" height="90" width="', w,
                           '" transform="rotate(180, 150, 45) translate(', 300 - w, ' ', -(r - 1) * 45, ') scale(1, ', r, ')"></use>
                    </svg>'))
    )
  }
}
