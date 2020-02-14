shinyServer(function(input, output, session){
  output$map1 <- renderLeaflet({
    leaflet(options = leafletOptions(crs = leafletCRS("L.CRS.Simple"), minZoom = 0, maxZoom = 7, attributionControl = FALSE), width = "100%", height = "800px") %>%
      addTiles(urlTemplate = tiles[1]) %>%
      setView(112, -75, 3) %>%
      addFullscreenControl() %>%
      addMiniMap(tiles = tiles[1], toggleDisplay = TRUE) %>%
      addPulseMarkers(data = d$coord1, lng = ~x, lat = ~y, icon = makePulseIcon(heartbeat = 1, color = ~color, iconSize = 16),
                      popup = eq_popup(d$coord1), popupOptions = pop_opts,
                      label = ~loc, labelOptions = labelOptions(style = label_opts))
  })

  output$map2 <- renderLeaflet({
    leaflet(options = leafletOptions(crs = leafletCRS("L.CRS.Simple"), minZoom = 0, maxZoom = 7, attributionControl = FALSE), width = "100%", height = "800px") %>%
      addTiles(urlTemplate = tiles[2]) %>%
      setView(78, -60, 3) %>%
      addFullscreenControl() %>%
      addMiniMap(tiles = tiles[2], toggleDisplay = TRUE) %>%
      addPulseMarkers(data = d$coord2, lng = ~x, lat = ~y, icon = makePulseIcon(heartbeat = 1, color = ~color, iconSize = 16),
                      popup = eq_popup(d$coord2), popupOptions = pop_opts,
                      label = ~loc, labelOptions = labelOptions(style = label_opts))
  })

  observeEvent(input$zoomin, {
    if(!input$maptoggle){
      z <- min(7, input$map1_zoom + 1)
      leafletProxy("map1") %>% setView(input$map1_center$lng, input$map1_center$lat, z)
    } else {
      z <- min(7, input$map2_zoom + 1)
      leafletProxy("map2") %>% setView(input$map2_center$lng, input$map2_center$lat, z)
    }
  })

  observeEvent(input$zoomout, {
    if(!input$maptoggle){
      z <- max(0, input$map1_zoom - 1)
      leafletProxy("map1") %>% setView(input$map1_center$lng, input$map1_center$lat, z)
    } else {
      z <- min(7, input$map2_zoom - 1)
      leafletProxy("map2") %>% setView(input$map2_center$lng, input$map2_center$lat, z)
    }
  })

  observeEvent(input$reset, {
    if(!input$maptoggle){
      z <- max(0, input$map1_zoom - 1)
      leafletProxy("map1") %>% setView(112, -75, 3)
    } else {
      z <- min(7, input$map2_zoom - 1)
      leafletProxy("map2") %>% setView(78, -60, 3)
    }
  })

  lapply(d$coord1$loc, function(i){
    observeEvent(input[[i]], {
      if(!input$maptoggle){
        l <- d$coord1[d$coord1$loc == i, c("x", "y")]
        leafletProxy("map1") %>% setView(l$x, l$y, input$map1_zoom)
      } else {
        l <- d$coord2[d$coord2$loc == i, c("x", "y")]
        leafletProxy("map2") %>% setView(l$x, l$y, input$map2_zoom)
      }
    })
  })
})
