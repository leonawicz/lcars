% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shapes.R
\name{lcars_pill}
\alias{lcars_pill}
\alias{lcars_half_pill}
\alias{lcars_left_pill}
\alias{lcars_right_pill}
\alias{lcars_bottom_pill}
\alias{lcars_top_pill}
\title{LCARS pill}
\usage{
lcars_pill(
  xmin,
  xmax,
  ymin,
  ymax,
  color = "atomic-tangerine",
  direction = c("both", "left", "right"),
  vertical = FALSE,
  gap = "auto",
  n = 50,
  asp = 1,
  gap_color = "#000000"
)

lcars_half_pill(
  x,
  y,
  r,
  direction,
  color = "atomic-tangerine",
  n = 50,
  asp = 1
)

lcars_left_pill(x, y, r, color = "atomic-tangerine", n = 50, asp = 1)

lcars_right_pill(x, y, r, color = "atomic-tangerine", n = 50, asp = 1)

lcars_bottom_pill(x, y, r, color = "atomic-tangerine", n = 50, asp = 1)

lcars_top_pill(x, y, r, color = "atomic-tangerine", n = 50, asp = 1)
}
\arguments{
\item{xmin}{numeric, scalar left x position.}

\item{xmax}{numeric, scalar right x position.}

\item{ymin}{numeric, scalar bottom y position.}

\item{ymax}{numeric, scalar top y position.}

\item{color}{pill color. Can be any color given in hex format.
Named colors must be LCARS colors. See \link{lcarsdata} for options.}

\item{direction}{integer \code{1:4} or character: \code{"topleft"}, \code{"topright"}, \code{"bottomleft"},
\code{"bottomright"}. May be abbreviated as \code{"tl"}, \code{"tr"}, \code{"br"}, \code{"bl"}.}

\item{vertical}{logical, vertical pill.}

\item{gap}{numeric or \code{"auto"}, the gap between the pill half circle
edge and pill rectangle edge.}

\item{n}{integer, number of points to define rounded edge.}

\item{asp}{numeric, aspect ratio. This is useful for preventing distortion
of pill half circle for plots with different width and height.}

\item{gap_color}{the color of gaps if present.
This is likely black, but because of the way the pill is drawn,
it must be specified to match if the plot background color is not black.
Can be any color given in hex format. Named colors must be LCARS colors.
See \link{lcarsdata} for options.}

\item{x}{numeric, x position for edge of horizontal half pill or midpoint of
vertical half pill.}

\item{y}{numeric, y position for edge of vertical half pill or midpoint of
horizontal half pill.}

\item{r}{numeric, radius of half pill.}
}
\value{
draws to plot
}
\description{
Wrappers around \code{lcars_rect()} that add rounded edges on one side or two
opposing sides to make an LCARS pill.
}
\examples{
op <- par(bg = "black")
plot(0:1, 0:1, asp = 1)
lcars_pill(0.05, 0.45, 0.7, 0.9, "chestnut-rose", "left")
lcars_pill(0.05, 0.45, 0.4, 0.6, "lilac", "both")
lcars_pill(0.05, 0.45, 0.1, 0.3, "orange-peel", "right")
lcars_pill(0.55, 0.65, 0.1, 0.9, "chestnut-rose", "left", vertical = TRUE)
lcars_pill(0.7, 0.8, 0.1, 0.9, "lilac", "both", vertical = TRUE)
lcars_pill(0.85, 0.95, 0.1, 0.9, "orange-peel", "right", vertical = TRUE)
par(op)
}
