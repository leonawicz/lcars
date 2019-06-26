#' LCARS border plot
#'
#' Wrap a ggplot object with an LCARS-themed border or only plot the border.
#'
#' This function draws a plot. It does not return a new ggplot object.
#'
#' For \code{length_frac}, a vector of eight values from 0 to 1 is required. Starting from the top side, clockwise around to the left side,
#' they refer to fraction of that side's length over which the relevant corner bend extends.
#' For example, the first value refers to the top left corner bend's rightward horizontal segment.
#' The second value refers to the top right corner bend's leftward horizontal segment. This takes care of the top side.
#' Finally, the last value refers to the downward vertical arm of the top left corner bend.
#'
#' All arguments that take vectors or lists of length four are in clockwise order from either the top left corner for corner-related arguments or
#' the top side for side-related arguments.
#'
#' @param x optional inset ggplot object.
#' @param width full plot width in inches.
#' @param height full plot height in inches.
#' @param corners integer, \code{1:4}, specifying which corners to include: top left, top right, bottom right, bottom left.
#' @param length_frac numeric, the fraction of a side that a corner extends over. See details.
#' @param corner_color vector of corner colors, clockwise from top left.
#' @param ro vector of corner outer radii, clockwise from top left.
#' @param ri vector of inner outer radii, clockwise from top left.
#' @param side_width width of each side, clockwise from top left.
#' @param side_n_segments for each side clockwise from top left, the number of rectangle segments used to evenly fill the space between corner bends.
#' @param side_color list of color vectors for side segments. Each vector must have the same number of colors as the number of segments for a given side.
#' @param side_label list of label vectors for side segments. Each vector must have the same number of colors as the number of segments for a given side.
#' @param label_size numeric, global label text size.
#' @param side_label_adj list of four vectors, each giving the \code{adj} argument to \code{text} for each side.
#' @param gap vector of two values giving the gap fraction from 0 to 1, based on full plot width and height, for the gap between horizontal and vertical segments, respectively.
#' @param bg background color, should be left black for LCARS standard.
#' @param n integer, number of points used to define inner radii quarter circles for corner bends.
#'
#' @return draws a plot
#' @export
#'
#' @examples
#' layout(matrix(1:4), 2)
#' lcars_border()
#'
#' sw <- seq(0.2, 2, length = 4)
#' lcars_border(width = 5, height = 5, ro = sw, ri = sw / 2, side_width = sw)
#'
#' len_frac <- c(0.3, 0.5, 0.2, 0.4, 0.3, 0.2, 0.1, 0.3)
#' n_seg <- c(1, 2, 0, 8)
#' lcars_border(corners = 1:3, length_frac = len_frac, side_n_segments = n_seg)
#'
#' library(ggplot2)
#' g <- ggplot(iris, aes(Sepal.Width, Sepal.Length, color = Species)) +
#'   geom_point() + facet_wrap(~Species, 2) + theme_lcars_light()
#' lcars_border(g, corners = 1:3, length_frac = len_frac, side_n_segments = n_seg)
lcars_border <- function(x = NULL, width = 10, height = 6, corners = 1:4,
  length_frac = rep(0.5, 8), corner_color = c("#CD6363", "#CC99CC", "#FF9E63", "#FFFF9C"),
  ro = width / 20, ri = height / 60, side_width = c(1, 2, 1, 5) / 5,
  side_n_segments = rep(0, 4), side_color = list("#99CCFF", "#99CCFF", "#99CCFF", "#99CCFF"),
  side_label = rep(NA, 4), label_size = 1,
  side_label_adj = list(c(0.5, 0.5), c(-0.2, -0.2), c(0.5, 0.5), c(1.1, -0.2)),
  gap = c(0.02, 0.01), bg = "black", n = 10){

  inset <- x
  op <- graphics::par(mar = rep(0, 4), font = 2, bg = bg)
  w <- side_width
  l <- length_frac
  f <- function(x) x %in% corners
  cc <- rep(corner_color, length = 4)
  ro <- rep(ro, length = 4)
  ri <- rep(ri, length = 4)

  graphics::plot(0, 0, type = "n", axes = FALSE, xlim = c(0, width), ylim = c(0, height))
  if(f(1)) lcars_bend(0, l[1] * width, height * (1 - l[8]), height,
                      "tl", w[4], w[1], ro[1], ri[1], n, cc[1])
  if(f(2)) lcars_bend(width * (1 - l[2]), width, height * (1 - l[3]), height,
                      "tr", w[2], w[1], ro[2], ri[2], n, cc[2])
  if(f(3)) lcars_bend(width * (1 - l[5]), width, 0, l[4] * height,
                      "br", w[2], w[3], ro[3], ri[3], n, cc[3])
  if(f(4)) lcars_bend(0, l[6] * width, 0, l[7] * height,
                      "bl", w[4], w[3], ro[4], ri[4], n, cc[4])

  lim_top <- c(if(f(1)) width * l[1] else 0, if(f(2)) width * (1 - l[2]) else width)
  lim_right <- c(if(f(3)) height * l[4] else 0, if(f(2)) height * (1 - l[3]) else height)
  lim_bottom <- c(if(f(4)) width * l[6] else 0, if(f(3)) width * (1 - l[5]) else width)
  lim_left <- c(if(f(4)) height * l[7] else 0, if(f(1)) height * (1 - l[8]) else height)

  gx <- width * gap[1] / 2
  gy <- height * gap[2] / 2
  gp <- function(i, s, g, mx){
    x <- c(s[i - 1], s[i])
    if(x[1] > 0 & i == 2) x[1] <- x[1] + 2 * g
    if(x[1] > 0 & i > 2) x[1] <- x[1] + g
    if(x[2] < mx & i == length(s)) x[2] <- x[2] - 2 * g
    if(x[2] < mx & i < length(s)) x[2] <- x[2] - g
    x
  }
  f2 <- function(i, s) i %in% c(2, length(s))

  side_color[[2]] <- rev(side_color[[2]])
  side_color[[3]] <- rev(side_color[[3]])
  if(side_n_segments[1] > 0){
    sc <- rep(side_color[[1]], length = side_n_segments[1])
    sl <- side_label[[1]]
    s <- seq(lim_top[1], lim_top[2], length.out = side_n_segments[1] + 1)
    for(i in 2:length(s)){
      x <- gp(i, s, gx, width)
      lcars_rect(x[1], x[2], height - w[1], height, sc[i - 1])
      if(length(sl) == side_n_segments[1])
        graphics::text(mean(x), height - w[1] / 2, sl[i - 1], cex = label_size, adj = side_label_adj[[1]])
    }
  }
  if(side_n_segments[2] > 0){
    sc <- rep(side_color[[2]], length = side_n_segments[2])
    sl <- side_label[[2]]
    s <- seq(lim_right[1], lim_right[2], length.out = side_n_segments[2] + 1)
    for(i in 2:length(s)){
      x <- gp(i, s, gy, height)
      lcars_rect(width - w[2], width, x[1], x[2], sc[i - 1])
      if(length(sl) == side_n_segments[2])
        graphics::text(width - w[2], x[1], sl[i - 1], cex = label_size, adj = side_label_adj[[2]])
    }
  }
  if(side_n_segments[3] > 0){
    sc <- rep(side_color[[3]], length = side_n_segments[3])
    sl <- side_label[[3]]
    s <- seq(lim_bottom[1], lim_bottom[2], length.out = side_n_segments[3] + 1)
    for(i in 2:length(s)){
      x <- gp(i, s, gx, width)
      lcars_rect(x[1], x[2], 0, w[3], sc[i - 1])
      if(length(sl) == side_n_segments[3])
        graphics::text(mean(x), w[3] / 2, sl[i - 1], cex = label_size, adj = side_label_adj[[3]])
    }
  }
  if(side_n_segments[4] > 0){
    sc <- rep(side_color[[4]], length = side_n_segments[4])
    sl <- side_label[[4]]
    s <- seq(lim_left[1], lim_left[2], length.out = side_n_segments[4] + 1)
    for(i in 2:length(s)){
      x <- gp(i, s, gy, height)
      lcars_rect(0, w[4], x[1], x[2], sc[i - 1])
      if(length(sl) == side_n_segments[4])
        graphics::text(w[4], x[1], sl[i - 1], cex = label_size, adj = side_label_adj[[4]])
    }
  }
  graphics::par(op)

  if(!is.null(inset)){
    mdpt <- c(mean(c(width - w[2], w[4])) / width, mean(c(height - w[1], w[3])) / height)
    vp <- grid::viewport(x = mdpt[1], y = mdpt[2],
                         width = 0.9 * (width - w[2] - w[4]) / width,
                         height = 0.9 * (height - w[1] - w[3]) / height)
    print(inset, vp = vp)
  }
  invisible()
}
