#' LCARS ggplot themes
#'
#' A collection of ggplot2 themes that go well with LCARS styles and colors.
#'
#' @param base_size base font size.
#' @param base_family base font family.
#' @param base_line_size base size for line elements.
#' @param base_rect_size base size for rect elements.
#'
#' @name theme_lcars
#' @export
theme_lcars_light <- function(base_size = 11, base_family = "", base_line_size = base_size/22,
                         base_rect_size = base_size/22){
  clr <- c("#FFFFCC", "#FFFFCC", "#000000", "#000000", "#99CCFF", "#AAAAAA")
  ggplot2::theme_gray(base_size, base_family, base_line_size, base_rect_size) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(color = clr[1], fill = clr[1]),
      panel.background = ggplot2::element_rect(color = clr[1], fill = clr[1]),
      panel.border = ggplot2::element_rect(color = clr[4], size = 1, fill = "transparent"),
      panel.grid = ggplot2::element_line(color = clr[6]),
      legend.background = ggplot2::element_rect(color = clr[1], fill = clr[1]),
      text = ggplot2::element_text(color = clr[4], face = "bold"),
      legend.key = ggplot2::element_rect(fill = clr[2], color = clr[1]),
      strip.background = ggplot2::element_rect(color = clr[1], fill = clr[5]),
      strip.text = ggplot2::element_text(color = clr[4]),
      axis.text = ggplot2::element_text(color = clr[3])
    )
}

#' @rdname theme_lcars
#' @export
theme_lcars_dark <- function(base_size = 11, base_family = "", base_line_size = base_size/22,
                              base_rect_size = base_size/22){
  clr <- c("#000000", "#000000", "#FFFFCC", "#FFFFCC", "#000000", "#AAAAAA")
  ggplot2::theme_gray(base_size, base_family, base_line_size, base_rect_size) +
    ggplot2::theme(
      plot.background = ggplot2::element_rect(color = clr[1], fill = clr[1]),
      panel.background = ggplot2::element_rect(color = clr[1], fill = clr[1]),
      panel.border = ggplot2::element_rect(color = clr[4], size = 1, fill = "transparent"),
      panel.grid = ggplot2::element_line(color = clr[6]),
      legend.background = ggplot2::element_rect(color = clr[1], fill = clr[1]),
      text = ggplot2::element_text(color = clr[4], face = "bold"),
      legend.key = ggplot2::element_rect(fill = clr[2], color = clr[1]),
      strip.background = ggplot2::element_rect(color = clr[1], fill = clr[5]),
      strip.text = ggplot2::element_text(color = clr[4]),
      axis.text = ggplot2::element_text(color = clr[3]),
      axis.ticks = ggplot2::element_line(color = clr[3])
    )
}
