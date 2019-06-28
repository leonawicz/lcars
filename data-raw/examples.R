library(lcars)
library(ggplot2)

logo <- grid::rasterGrob(png::readPNG(system.file("stlogo-white.png", package = "lcars")))
clrs <- c("firebrick1", "#FFFF33", "lightseagreen")

g <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
  geom_point() +
  rtrek::theme_rtrek_dark() + theme(plot.background = element_rect(color = "black")) +
  coord_cartesian(clip = "off") +
  annotation_custom(logo, xmin = 8.9, xmax = Inf, ymin = 3.6, ymax = Inf) +
  scale_color_manual(values = clrs)

len_frac <- c(0.55, 0.25, 0.2, 0.4, 0.1, 0.2, 0.1, 0.1)
n_seg <- c(1, 2, 0, 8)
corner_colors = c("#CD6363", "#CC99CC", "#FFFF9C", "#FFFF9C")
side_colors <- list("#FFFF9C", c("#99CCFF", "#CC99CC"), NA,
                    c("#000000", rep("#99CCFF", 2), rep("#FF9E63", 5)))
side_labels <- list(
  "R 47.0.2", c("47", "B4"), NA,
  c(NA, "..", "47174-A", "O'Brien", "Lefler", "La Forge", "Barclay", "Argyle"))

png("data-raw/lcars-ex1.png", 3000, 1800, type = "cairo", res = 300)
lcars_border(g, corners = 1:3, ro = 0.35, length_frac = len_frac, side_n_segments = n_seg,
             corner_color = corner_colors, side_color = side_colors,
             side_label = side_labels, label_size = 1)

lcars_pill(0.95, 1.3, 0, 0.2, "#99CCFF", "left", gap = 0.05)
lcars_pill(4.8, 5.15, 0, 0.2, "#99CCFF", "right", gap = 0.05)
lcars_pill(8.8, 9.5, 0, 0.2, "#FFFF9C", "right", gap = 0.05)
lcars_pill(5.75, 6.10, 0, 0.2, "#FFFF9C", "left", gap = 0.05)
text(3.05, 0.08, "Iris dataset :: classic Terran flora sample", col = "#99CCFF", font = 2)
text(7.45, 0.1, "LCARS BETA R Interface v0.0.1", col = "#FFFF9C", font = 2)

lcars_bottom_pill(0.5, 0.64, 0.5, "#99CCFF")
text(0.5, 0.45, "LCARS", col = "#000000", font = 2, cex = 1)

lcars_pill(8.3, 9.4, 3.25, 3.45, clrs[1])
lcars_pill(8.3, 9.4, 3.0, 3.2, clrs[2])
lcars_pill(8.3, 9.4, 2.75, 2.95, clrs[3])
text(9.225, 3.28, "setosa", cex = 0.8, adj = c(1, 0), font = 2)
text(9.225, 3.03, "versicolor", cex = 0.8, adj = c(1, 0), font = 2)
text(9.225, 2.78, "virginica", cex = 0.8, adj = c(1, 0), font = 2)
dev.off()

