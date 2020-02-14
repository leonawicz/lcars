shinyUI(
  lcarsPage(
    tags$style(".leaflet-container { background: #000000 !important; } .leaflet-popup-content-wrapper { border-radius: 4px !important; color: #ffffff !important; background-color: rgba(45, 45, 45, 0.9) !important; width: 540px; font-size: 16px; } .leaflet-popup { border-color: #888888 !important; }"),
    lcarsHeader("Shiny :: Star Trek Computer Console"), gap, gap,
    lcarsBox(
      div(
        style = "position: relative;",
        img(src = knitr::image_uri(sf_logo), alt = "logo", height = 100, width = 65, style = img_style),
        conditionalPanel("input.maptoggle === false", leafletOutput("map1", height = "800px")),
        conditionalPanel("input.maptoggle === true", leafletOutput("map2", height = "800px"))
      ),
      title = "Stellar Cartography", subtitle = "Galaxy Map",
      corners = 1:4, sides = 1:4,
      left_inputs = ic1, right_inputs = ic2,
      color = c("tanoi", "anakiwa", "tanoi", "tanoi"),
      title_color = "atomic-tangerine", subtitle_color = "anakiwa", clip = FALSE
    ),
    gap, gap,
    lcarsSweep(
      inputColumn(
        lcarsRect("LCARS", width = 150, height = 30),
        div(style = "height:5px;"),
        lcarsRect(width = 150, height = 100)
      ),
      left_inputs = tagList(
        lcarsRect(HTML("<p style='color:#000000'>This Shiny app is built using the <code>lcars</code> package.</p>"), "both", color = "lilac", width = "100%", height = 60), gap,
        lcarsRect(HTML("<p style='color:#000000'>Simple CRS/non-geographic map tiles were generated using the <code>tiler</code> package.</p>"), "both", color = "pale-canary", width = "100%", height = 60), gap,
        lcarsRect(HTML("<p style='color:#000000'>Map data attributes and georeferencing is part of the <code>rtrek</code> package.</p>"), "both", color = "lilac", width = "100%", height = 60), gap,
        lcarsRect(HTML("<p style='color:#000000'>Related color palettes and fonts can be found in the <code>trekcolors</code> and <code>trekfont</code> packages.</p>"), "both", color = "pale-canary", width = "100%", height = 60)
      ),
      right_inputs = tagList(
        gap, gap, gap,
        lcarsBracket(
          gap,
          lcarsRect(HTML("<p><p>Base map tiles generated from source map <a href='https://archerxx.deviantart.com/art/Star-Trek-Star-Chart-316982311'  style='background-color: #882211'>Star trek Star Chart</a> by <a href='https://archerxx.deviantart.com/' style='background-color: #882211'>Rob Archer</a>.</em></p>"), "both", color = "tamarillo", width = "100%", height = 80), gap,
          lcarsRect(HTML("<p>Base map tiles generated from source map <a href='http://sttff.net/AST_MAP.html' style='background-color: #882211';>AST MAP</a>.</em></p>"), "both", color = "tamarillo", width = "100%", height = 80),
          gap,
          color = "anakiwa"
        )
      ),
      title = "Resources",
      color = "golden-tanoi",
    )
  )
)
