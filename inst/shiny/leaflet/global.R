library(shiny)
library(leaflet)
library(leaflet.extras)

d <- readRDS("data.rds")
info_colors <- c("rgba(45, 45, 45, 0.9)", "#888888", "#ffffff")
label_opts <- list(
  "background-color" = info_colors[1],
  "font-size" = "16px", "font-style" = "bold", "color" = info_colors[3], "border-color" = info_colors[2],
  "border-size" = "10px", "border-radius" = "4px")

eq_popup <- function(x){
  logo <- paste0("<img src=\"", x$avatar, "\" style=\"float:right;max-height:250px\"/>")
  paste0("<div style=\"width:500px;height:270px;\">", logo, strong("Identification: "), x$loc, "<br/>",
         strong("Geopolitical domain: "), x$zone, "<br/>", strong("Primary species: "), x$species,
         "<br/>", strong("Stellar body: "), x$body, "<br/>", strong("Classification: "), x$category, "</div>")
}
pop_opts <- popupOptions(closeButton = FALSE)

img_style <- "position:absolute; top:10px; right:10px; height:100px; width:65px; padding:5px; z-index:500; background-color:rgba(255, 185, 15, 0.75); border-radius: 4px;"
sf_logo <- "stlogo-white.png"
tiles <- paste0("https://leonawicz.github.io/tiles/st", 1:2, "/tiles/{z}/{x}/{y}.png")

ic1 <- inputColumn(
  lcarsRect("Focus Homeworld", color = "tanoi", height = 30), div(style = "height:5px;"),
  lcarsButton("Earth", "Earth", color = "pale-canary", hover_color = "atomic-tangerine"),
  lcarsButton("Romulus", "Romulus", color = "pale-canary", hover_color = "atomic-tangerine"),
  lcarsButton("Qo'noS", "Qo'noS", color = "pale-canary", hover_color = "atomic-tangerine"),
  lcarsButton("Breen", "Breen", color = "pale-canary", hover_color = "atomic-tangerine"),
  lcarsButton("Ferenginar", "Ferenginar", color = "pale-canary", hover_color = "atomic-tangerine"),
  lcarsButton("Cardassia", "Cardassia", color = "pale-canary", hover_color = "atomic-tangerine"),
  lcarsButton("Tholia", "Tholia", color = "pale-canary", hover_color = "atomic-tangerine"),
  lcarsButton("Tzenketh", "Tzenketh", color = "pale-canary", hover_color = "atomic-tangerine"),
  lcarsButton("Talar", "Talar", color = "pale-canary", hover_color = "atomic-tangerine")
)

ic2 <- inputColumn(
  lcarsButton("zoomin", "Magnify"),
  lcarsButton("zoomout", "Reverse"),
  lcarsButton("reset", "Reset", color = "anakiwa", hover_color = "danub"),
  lcarsToggle("maptoggle", "Galaxy View", FALSE, pill = FALSE, inverse = TRUE, "Dark", "Standard",
              label_color = "atomic-tangerine", label_right = TRUE, width = "150px")
)

gap <- div(style = "height:10px;")
