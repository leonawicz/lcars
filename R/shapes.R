#' LCARS rectangle
#'
#' A simple wrapper around \code{rect}.
#'
#' @param xmin numeric, left x positions.
#' @param xmax numeric, right x positions.
#' @param ymin numeric, bottom y positions.
#' @param ymax numeric, top y positions.
#' @param color fill and border color.
#'
#' @return draws a rectangle
#' @export
#'
#' @examples
#' plot(0:1, 0:1)
#' lcars_rect(0.1, 0.9, 0.6, 0.9, "red")
lcars_rect <- function(xmin, xmax, ymin, ymax, color){
  graphics::rect(xmin, ymin, xmax, ymax, col = color, border = color)
}


lcars_pill <- function(xmin, xmax, ymin, ymax, color, direction = c("both", "left", "right"), vertical = FALSE, n = 10){
  if(n %% 2 == 1) n <- n + 1
  plot(0:6, 0:6)
  xmin <- 2; xmax <- 4; ymin <- 2.5; ymax <- 3
  lcars_rect(xmin, xmax, ymin, ymax, "red")

  direction <- match.arg(direction)
  if(direction == "both") direction <- c("left", "right")
  if(vertical){
    r <- (xmax - xmin) / 2
    m <- xmax - r
  } else {
    r <- (ymax - ymin) / 2
    m <- ymax - r
  }

  if("left" %in% direction){
    if(vertical){
      a <- pi * seq(1, 2, length.out = n)
      x <- r * cos(a) + m
      y <- r * sin(a) + ymin
      points(x, y)
    } else {
      a <- pi * seq(1.5, 0.5, length.out = n)
      x <- r * cos(a) + xmin
      y <- r * sin(a) + m
      points(x, y)
    }
  }

  if("right" %in% direction){
    if(vertical){
      a <- pi * seq(0, 1, length.out = n)
      x <- r * cos(a) + m
      y <- r * sin(a) + ymin
      points(x, y)
    } else {
      a <- pi * seq(1.5, 0.5, length.out = n)
      x <- -r * cos(a) + xmax
      y <- r * sin(a) + m
      points(x, y)
      n2 <- n / 2
      p1 <- list(x = x[c(1:n2, (n2 - 1):1)], y = c(rep(m, n2), y[(n2 - 1):1]))
      p2 <- list(x = x[c(n2:n, n:n2)], y = c(rep(m, length(n2:n)), y[n2:n]))
      polygon(p1$x, p1$y, border = "blue", col = "blue")
      polygon(p2$x, p2$y, border = "blue", col = "green")
    }
  }


  xi <- xi + w + min(xo) - min(xi)
  yi <- yi - h + max(yo) - max(yi)
}


#' LCARS corner bend
#'
#' Draw a 90-degree rounded corner sweep for top left, top right, bottom right and bottom left LCARS corner panels.
#'
#' @param xmin numeric, scalar left x position.
#' @param xmax numeric, scalar right x position.
#' @param ymin numeric, scalar bottom y position.
#' @param ymax numeric, scalar top y position.
#' @param corner integer 1:4 or character: \code{topleft}, \code{topright}, \code{bottomleft}, \code{bottomright}. May be abbreviated as \code{tl}, \code{tr}, \code{br}, \code{bl}.
#' @param width numeric, the width of the vertical segment of the bend.
#' @param height numeric, the height of the horizontal segment of the bend.
#' @param ro radius of the outer rounded corner.
#' @param ri radius of the inner rounded corner.
#' @param n number of points to define the curve of the inner radial quarter circle. The number of points then used to define the outer curve and extensions of the segments are scaled respectively based on this.
#' @param color ignored if \code{draw = FALSE}.
#' @param draw draw the corner. Return values if \code{FALSE}.
#'
#' @return draws a polygon
#' @export
#'
#' @examples
#' plot(0:1, 0:1)
#' lcars_bend(0.1, 0.9, 0.6, 0.9, "tl", 0.2, 0.05)
lcars_bend <- function(xmin, xmax, ymin, ymax, corner, width, height, ro = width / 2, ri = height / 2, n = 10, color, draw = TRUE){
  id <- c("topleft", "topright", "bottomright", "bottomleft", "tl", "tr", "br", "bl")
  if(length(corner) > 1) stop("`corner` must be a single value.", call. = FALSE)
  valid_corners <- paste0("`corner` must be an 1:4 or one of '", paste(id, collapse = "', '"), "'.")
  if(is.character(corner) & !corner %in% id){
    stop(valid_corners, call. = FALSE)
  } else if(is.character(corner)){
    corner <- switch(corner,
                     topleft = 1, topright = 2, bottomright = 3, bottomleft = 4,
                     tl = 1, tr = 2, br = 3, bl = 4)
  } else if(!corner %in% 1:4) stop(valid_corners, call. = FALSE)
  f <- switch(corner, "1" = .tlc, "2" = .trc, "3" = .brc, "4" = .blc)
  f2 <- function(d) list(x = c(d$xo, rev(d$xi)), y = c(d$yo, rev(d$yi)))
  d <- f2(f(xmin, xmax, ymin, ymax, width, height, ro, ri, n))
  if(draw){
    if(missing(color)) color <- "#CD6363"
    graphics::polygon(d$x, d$y, col = color[1], border = color[1])
    invisible(d)
  } else d
}

.tlc <- function(xmin, xmax, ymin, ymax, w = 2, h = 0.4, ro = w / 2, ri = h / 2, n = 10){
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

.blc <- function(xmin, xmax, ymin, ymax, w = 2, h = 0.4, ro = w / 2, ri = h / 2, n = 10){
  d <- .tlc(xmin, xmax, ymin, ymax, w, h, ro, ri, n = 10)
  r <- range(d$yo)
  d$yo <- -(d$yo - r[1]) + r[2]
  d$yi <- -(d$yi - r[1]) + r[2]
  d
}

.trc <- function(xmin, xmax, ymin, ymax, w = 2, h = 0.4, ro = w / 2, ri = h / 2, n = 10){
  d <- .tlc(xmin, xmax, ymin, ymax, w, h, ro, ri, n = 10)
  r <- range(d$xo)
  d$xo <- -(d$xo - r[1]) + r[2]
  d$xi <- -(d$xi - r[1]) + r[2]
  d
}

.brc <- function(xmin, xmax, ymin, ymax, w = 2, h = 0.4, ro = w / 2, ri = h / 2, n = 10){
  d <- .tlc(xmin, xmax, ymin, ymax, w, h, ro, ri, n = 10)
  r <- range(d$xo)
  d$xo <- -(d$xo - r[1]) + r[2]
  d$xi <- -(d$xi - r[1]) + r[2]
  r <- range(d$yo)
  d$yo <- -(d$yo - r[1]) + r[2]
  d$yi <- -(d$yi - r[1]) + r[2]
  d
}
