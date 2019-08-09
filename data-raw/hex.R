library(hexSticker)
library(ggplot2)
library(png)
library(grid)

pkg <- basename(getwd())
user <- "leonawicz"
account <- "github"
url <- paste0(user, ".", account, ".io/", pkg)
out <- paste0("man/figures/logo.png")

len_frac <- c(0.55, 0.25, 0.2, 0.4, 0.6, 0.1, 0.1, 0.1)
n_seg <- c(1, 2, 1, 6)
cc = c("#CC6699", "#CC99CC", "#CC6699", "#FFFF99")
sc <- list("#FFFF99", c("#99CCFF", "#FFFF99"), "#FFFF99", rep(c("#FF9900", "#99CCFF", "#FFCC66"), each = 2))
sw <- c(1, 5, 1, 5) / 5

file <- "data-raw/lcars-0.png"
png(file, 2400, 1800, type = "cairo", res = 300)
lcars_border(width = 8, corners = 1:4, length_frac = len_frac, side_n_segments = n_seg,
             corner_color = cc, side_color = sc, side_width = sw)
dev.off()

p <- rasterGrob(readPNG(file))

hex_plot <- function(out, mult = 1){
  sticker(p, package = "lcars", p_y = 1, p_color = "#FFFF99", p_size = 36,
          s_x = 1, s_y = 1, s_width = 1.5, s_height = 1.5,
          h_color = "#FFFF99", h_fill = "#000000", h_size =  1.6,
          url = url, u_color = "#FFFF99", u_size = 3, filename = out)
}

hex_plot(out)
