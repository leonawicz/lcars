#' LCARS rectangle
#'
#' A simple wrapper around `rect()`.
#'
#' @param xmin numeric, left x positions.
#' @param xmax numeric, right x positions.
#' @param ymin numeric, bottom y positions.
#' @param ymax numeric, top y positions.
#' @param color fill and border color. Can be any color given in hex format.
#' Named colors must be LCARS colors. See [lcarsdata] for options.
#'
#' @return draws a rectangle
#' @export
#'
#' @examples
#' plot(0:1, 0:1)
#' lcars_rect(0.1, 0.9, 0.6, 0.9)
lcars_rect <- function(xmin, xmax, ymin, ymax, color = "atomic-tangerine"){
  color <- .lcars_color_check(color)
  graphics::rect(xmin, ymin, xmax, ymax, col = color, border = color)
}


#' LCARS pill
#'
#' Wrappers around `lcars_rect()` that add rounded edges on one side or two
#' opposing sides to make an LCARS pill.
#'
#' @param xmin numeric, scalar left x position.
#' @param xmax numeric, scalar right x position.
#' @param ymin numeric, scalar bottom y position.
#' @param ymax numeric, scalar top y position.
#' @param x numeric, x position for edge of horizontal half pill or midpoint of
#' vertical half pill.
#' @param y numeric, y position for edge of vertical half pill or midpoint of
#' horizontal half pill.
#' @param r numeric, radius of half pill.
#' @param color pill color. Can be any color given in hex format.
#' Named colors must be LCARS colors. See [lcarsdata] for options.
#' @param direction integer `1:4` or character: `"topleft"`, `"topright"`, `"bottomleft"`,
#' `"bottomright"`. May be abbreviated as `"tl"`, `"tr"`, `"br"`, `"bl"`.
#' @param vertical logical, vertical pill.
#' @param gap numeric or `"auto"`, the gap between the pill half circle
#' edge and pill rectangle edge.
#' @param n integer, number of points to define rounded edge.
#' @param asp numeric, aspect ratio. This is useful for preventing distortion
#' of pill half circle for plots with different width and height.
#' @param gap_color the color of gaps if present.
#' This is likely black, but because of the way the pill is drawn,
#' it must be specified to match if the plot background color is not black.
#' Can be any color given in hex format. Named colors must be LCARS colors.
#' See [lcarsdata] for options.
#'
#' @return draws to plot
#' @export
#' @name lcars_pill
#'
#' @examples
#' op <- par(bg = "black")
#' plot(0:1, 0:1, asp = 1)
#' lcars_pill(0.05, 0.45, 0.7, 0.9, "chestnut-rose", "left")
#' lcars_pill(0.05, 0.45, 0.4, 0.6, "lilac", "both")
#' lcars_pill(0.05, 0.45, 0.1, 0.3, "orange-peel", "right")
#' lcars_pill(0.55, 0.65, 0.1, 0.9, "chestnut-rose", "left", vertical = TRUE)
#' lcars_pill(0.7, 0.8, 0.1, 0.9, "lilac", "both", vertical = TRUE)
#' lcars_pill(0.85, 0.95, 0.1, 0.9, "orange-peel", "right", vertical = TRUE)
#' par(op)
lcars_pill <- function(xmin, xmax, ymin, ymax, color = "atomic-tangerine",
                       direction = c("both", "left", "right"), vertical = FALSE,
                       gap = "auto", n = 50, asp = 1, gap_color = "#000000"){
  if(gap == "auto") gap <- min(c(xmax - xmin, ymax - ymin)) / 10
  direction <- match.arg(direction)
  if(direction == "both") direction <- c("left", "right")
  gap <- rep(gap, 2)
  if(!"left" %in% direction) gap[1] <- 0
  if(!"right" %in% direction) gap[2] <- 0
  color <- .lcars_color_check(color)
  gap_color <- .lcars_color_check(gap_color)
  if(vertical){
    r <- (xmax - xmin) / 2
    m <- xmax - r
    ymin <- ymin + r
    ymax <- ymax - r
    lcars_rect(xmin, xmax, ymin, ymax, color)
    lcars_rect(xmin, xmax, ymin + gap[1], ymax - gap[2], gap_color)
    lcars_rect(xmin, xmax, ymin + 2 * gap[1], ymax - 2 * gap[2], color)
  } else {
    r <- (ymax - ymin) / 2
    m <- ymax - r
    xmin <- xmin + r
    xmax <- xmax - r
    lcars_rect(xmin, xmax, ymin, ymax, color)
    lcars_rect(xmin + gap[1], xmax - gap[2], ymin, ymax, gap_color)
    lcars_rect(xmin + 2 * gap[1], xmax - 2 * gap[2], ymin, ymax, color)
  }
  if("left" %in% direction){
    if(vertical){
      lcars_half_pill(m, ymin, r, 3, color, n, asp)
    } else {
      lcars_half_pill(xmin, m, r, 4, color, n, asp)
    }
  }
  if("right" %in% direction){
    if(vertical){
      lcars_half_pill(m, ymax, r, 1, color, n, asp)
    } else {
      lcars_half_pill(xmax, m, r, 2, color, n, asp)
    }
  }
  invisible()
}

#' @export
#' @rdname lcars_pill
lcars_half_pill <- function(x, y, r, direction, color = "atomic-tangerine",
                            n = 50, asp = 1){
  direction <- .lcars_direction(direction, "direction")
  f <- switch(direction, "1" = lcars_top_pill, "2" = lcars_right_pill,
              "3" = lcars_bottom_pill, "4" = lcars_left_pill)
  f(x, y, r, color, n, asp)
}

#' @export
#' @rdname lcars_pill
lcars_left_pill <- function(x, y, r, color = "atomic-tangerine", n = 50,
                            asp = 1){
  if(n %% 2 == 0) n <- n + 1
  a <- pi * seq(1.5, 0.5, length.out = n)
  m <- y
  x <- r * cos(a) / asp + x
  y <- r * sin(a) + y
  n2 <- (n + 1) / 2
  p1 <- list(x = x[c(1:n2, (n2 - 1):1)], y = c(rep(m, n2), y[(n2 - 1):1]))
  p2 <- list(x = x[c(n2:n, n:n2)], y = c(rep(m, length(n2:n)), y[n:n2]))
  color <- .lcars_color_check(color)
  graphics::polygon(p1$x, p1$y, border = color, col = color)
  graphics::polygon(p2$x, p2$y, border = color, col = color)
}

#' @export
#' @rdname lcars_pill
lcars_right_pill <- function(x, y, r, color = "atomic-tangerine", n = 50,
                             asp = 1){
  if(n %% 2 == 0) n <- n + 1
  a <- pi * seq(1.5, 0.5, length.out = n)
  m <- y
  x <- -r * cos(a) / asp + x
  y <- r * sin(a) + y
  n2 <- (n + 1) / 2
  p1 <- list(x = x[c(1:n2, (n2 - 1):1)], y = c(rep(m, n2), y[(n2 - 1):1]))
  p2 <- list(x = x[c(n2:n, n:n2)], y = c(rep(m, length(n2:n)), y[n:n2]))
  color <- .lcars_color_check(color)
  graphics::polygon(p1$x, p1$y, border = color, col = color)
  graphics::polygon(p2$x, p2$y, border = color, col = color)
}

#' @export
#' @rdname lcars_pill
lcars_bottom_pill <- function(x, y, r, color = "atomic-tangerine", n = 50,
                              asp = 1){
  if(n %% 2 == 0) n <- n + 1
  a <- pi * seq(1, 2, length.out = n)
  x <- r * cos(a) + x
  y <- r * sin(a) * asp + y
  color <- .lcars_color_check(color)
  graphics::polygon(x, y, border = color, col = color)
}

#' @export
#' @rdname lcars_pill
lcars_top_pill <- function(x, y, r, color = "atomic-tangerine", n = 50,
                           asp = 1){
  if(n %% 2 == 0) n <- n + 1
  a <- pi * seq(0, 1, length.out = n)
  x <- r * cos(a) + x
  y <- r * sin(a) * asp + y
  color <- .lcars_color_check(color)
  graphics::polygon(x, y, border = color, col = color)
}

#' LCARS corner elbow
#'
#' Draw a, LCARS elbow polygon. This is a 90-degree rounded corner bend for top
#' left, top right, bottom right and bottom left LCARS corner panels.
#'
#' @param xmin numeric, scalar left x position.
#' @param xmax numeric, scalar right x position.
#' @param ymin numeric, scalar bottom y position.
#' @param ymax numeric, scalar top y position.
#' @param corner integer `1:4` or character: `"topleft"`, `"topright"`,`"bottomleft"`,
#' `"bottomright"`. May be abbreviated as `"tl"`, `"tr"`, `"br"`, `"bl"`.
#' @param width numeric, the width of the vertical segment of the bend.
#' @param height numeric, the height of the horizontal segment of the bend.
#' @param ro radius of the outer rounded corner.
#' @param ri radius of the inner rounded corner.
#' @param n number of points to define the curve of the inner radial quarter
#' circle. The number of points then used to define the outer curve and
#' extensions of the segments are scaled respectively based on this.
#' @param color ignored if `draw = FALSE`. Can be any color given in hex
#' format. Named colors must be LCARS colors. See [lcarsdata] for
#' options.
#' @param draw draw the corner. Return values if `FALSE`.
#'
#' @return draws a polygon
#' @export
#'
#' @examples
#' plot(0:1, 0:1)
#' lcars_elbow(0.1, 0.9, 0.6, 0.9, "tl", 0.2, 0.05)
lcars_elbow <- function(xmin, xmax, ymin, ymax, corner, width, height,
                        ro = width / 2, ri = height / 2, n = 20,
                        color = "atomic-tangerine", draw = TRUE){
  corner <- .lcars_direction(corner, "corner")
  f <- switch(corner, "1" = .tlc, "2" = .trc, "3" = .brc, "4" = .blc)
  f2 <- function(d) list(x = c(d$xo, rev(d$xi)), y = c(d$yo, rev(d$yi)))
  d <- f2(f(xmin, xmax, ymin, ymax, width, height, ro, ri, n))
  if(draw){
    color <- .lcars_color_check(color)
    graphics::polygon(d$x, d$y, col = color[1], border = color[1])
    invisible(d)
  } else d
}

.lcars_direction <- function(x, argname){
  id <- c("topleft", "topright", "bottomright", "bottomleft", "tl", "tr", "br",
          "bl")
  if(length(x) > 1)
    stop(paste0("`", argname, "` must be a single value."), call. = FALSE)
  valid <- paste0("`", argname, "` must be an 1:4 or one of '",
                  paste(id, collapse = "', '"), "'.")
  if(is.character(x) & !x %in% id){
    stop(valid, call. = FALSE)
  } else if(is.character(x)){
    x <- switch(x, topleft = 1, topright = 2, bottomright = 3, bottomleft = 4,
                tl = 1, tr = 2, br = 3, bl = 4)
  } else if(!x %in% 1:4) stop(valid, call. = FALSE)
  x
}

.tlc <- function(xmin, xmax, ymin, ymax, w = 2, h = 0.4, ro = w / 2, ri = h / 2,
                 n = 10){
  n2 <- round(n * ro / ri)
  ao <- pi * seq(1, 0.5, length.out = n2)
  ai <- pi * seq(1, 0.5, length.out = n)

  xo <- ro * cos(ao)
  yo <- ro * sin(ao)
  xi <- ri * cos(ai)
  yi <- ri * sin(ai)

  xi <- xi + w + min(xo) - min(xi)
  yi <- yi - h + max(yo) - max(yi)

  mx1 <- max(xo)
  mx2 <- max(xi)
  nmx <- max(2, round(n * abs(mx2 - mx1) / (2 * pi * ri)))
  if(mx1 < mx2){
    xo <- c(xo, seq(mx1, mx2, length.out = nmx)[-1])
    yo <- c(yo, rep(yo[n2], nmx - 1))
  }
  if(mx1 > mx2){
    xi <- c(xi, seq(mx2, mx1, length.out = nmx)[-1])
    yi <- c(yi, rep(yi[n], nmx - 1))
  }

  mn1 <- min(yo)
  mn2 <- min(yi)
  nmn <- max(2, round(n * abs(mn2 - mn1) / (2 * pi * ri)))
  if(mn1 < mn2){
    yi <- c(seq(mn1, mn2, length.out = nmn)[-nmn], yi)
    xi <- c(rep(xi[1], nmn - 1), xi)
  }
  if(mn1 > mn2){
    yo <- c(seq(mn2, mn1, length.out = nmn)[-nmn], yo)
    xo <- c(rep(xo[1], nmn - 1), xo)
  }

  x_shift <- xmin - min(xo)
  xo <- xo + x_shift
  xi <- xi + x_shift
  y_shift <- ymax - max(yo)
  yo <- yo + y_shift
  yi <- yi + y_shift

  mx <- max(xo)
  if(xmax > mx){
    nmx <- max(2, round(n * (xmax - mx) / (2 * pi * ri)))
    s <- seq(mx, xmax, length.out = nmx)[-1]
    xo <- c(xo, s)
    yo <- c(yo, rep(yo[length(yo)], nmx - 1))
    xi <- c(xi, s)
    yi <- c(yi, rep(yi[length(yi)], nmx - 1))
  }
  mn <- min(yo)
  if(ymin < mn){
    nmn <- max(2, round(n * (mn - ymin) / (2 * pi * ri)))
    s <- seq(ymin, mn, length.out = nmn)[-nmn]
    xo <- c(rep(xo[1], nmn - 1), xo)
    yo <- c(s, yo)
    xi <- c(rep(xi[1], nmn - 1), xi)
    yi <- c(s, yi)
  }
  list(xo = xo, yo = yo, xi = xi, yi = yi)
}

.blc <- function(xmin, xmax, ymin, ymax, w = 2, h = 0.4, ro = w / 2, ri = h / 2,
                 n = 10){
  d <- .tlc(xmin, xmax, ymin, ymax, w, h, ro, ri, n = 10)
  r <- range(d$yo)
  d$yo <- -(d$yo - r[1]) + r[2]
  d$yi <- -(d$yi - r[1]) + r[2]
  d
}

.trc <- function(xmin, xmax, ymin, ymax, w = 2, h = 0.4, ro = w / 2, ri = h / 2,
                 n = 10){
  d <- .tlc(xmin, xmax, ymin, ymax, w, h, ro, ri, n = 10)
  r <- range(d$xo)
  d$xo <- -(d$xo - r[1]) + r[2]
  d$xi <- -(d$xi - r[1]) + r[2]
  d
}

.brc <- function(xmin, xmax, ymin, ymax, w = 2, h = 0.4, ro = w / 2, ri = h / 2,
                 n = 10){
  d <- .tlc(xmin, xmax, ymin, ymax, w, h, ro, ri, n = 10)
  r <- range(d$xo)
  d$xo <- -(d$xo - r[1]) + r[2]
  d$xi <- -(d$xi - r[1]) + r[2]
  r <- range(d$yo)
  d$yo <- -(d$yo - r[1]) + r[2]
  d$yi <- -(d$yi - r[1]) + r[2]
  d
}
