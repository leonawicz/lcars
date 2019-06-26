#install.packages("hexSticker")
library(hexSticker)
library(ggplot2)
pkg <- basename(getwd())
user <- "username"
account <- "github"

url <- paste0(user, ".", account, ".io/", pkg)
out <- paste0("man/figures/logo.png")
dir.create("man/figures", showWarnings = FALSE)

hex_plot <- function(out, mult = 1){
  g <- ggplot() + theme_void() + theme_transparent()
  sticker(g, package = pkg, p_y = 1, p_color = "gray20", p_size = 20,
          h_color = "gray20", h_fill = "burlywood1", h_size =  1.4,
          url = url, u_color = "gray20", u_size = 3, filename = out)
}

hex_plot(out)
