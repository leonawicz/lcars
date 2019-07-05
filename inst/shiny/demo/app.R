library(shiny)
library(ggplot2)
library(ggrepel)
library(showtext)
font_add_google("Oswald", "Oswald")
showtext_auto()
load("demoApp.RData")

bc1 <- buttonColumn(
  lcarsButton("btn_totals", "Total words"),
  lcarsButton("btn_best", "Top episodes"),
  lcarsToggle("bars", "Bar values", FALSE, pill = FALSE, inverse = FALSE, "Yes", "No",
              label_color = "atomic-tangerine", label_right = TRUE, width = "150px"),
  lcarsToggle("uniforms", "Uniforms", FALSE, pill = FALSE, inverse = FALSE, "Yes", "No",
              false_color = "lilac", label_color = "lilac", label_right = TRUE, width = "150px"),
  lcarsToggle("dark", "Dark theme", FALSE, pill = FALSE, inverse = FALSE, "Yes", "No",
              false_color = "tamarillo", outer_border = TRUE, outer_color = "tamarillo", label_color = "tamarillo")
)

bc2 <- buttonColumn(
  lcarsButton("btn_picard", "PICARD"),
  lcarsButton("btn_data", "DATA"),
  lcarsButton("btn_riker", "RIKER"),
  lcarsButton("btn_laforge", "LA FORGE"),
  lcarsButton("btn_crusher", "CRUSHER"),
  lcarsButton("btn_troi", "TROI"),
  lcarsButton("btn_worf", "WORF"),
  lcarsButton("btn_wesley", "WESLEY")
)

ui <- lcarsPage(
  lcarsHeader("Enterprise NCC-1701-D"),
  lcarsBox(uiOutput("topPanel"),
    title = "Crew summary", left_inputs = bc1, color = "tanoi", title_color = "atomic-tangerine"
  ),
  lcarsBox(uiOutput("bottomPanel"),
    title = "Crew members", left_inputs = bc2, color = "tanoi", title_color = "atomic-tangerine"
  ),
  lcarsHeader()
)

server <- function(input, output) {
  rv <- reactiveValues(cur1 = "totals", cur2 = "PICARD")

  observeEvent(input$btn_totals, { rv$cur1 <- "totals" })
  observeEvent(input$btn_best, { rv$cur1 <- "best" })

  observeEvent(input$btn_picard, { rv$cur2 <- "PICARD" })
  observeEvent(input$btn_data, { rv$cur2 <- "DATA" })
  observeEvent(input$btn_riker, { rv$cur2 <- "RIKER" })
  observeEvent(input$btn_laforge, { rv$cur2 <- "LA FORGE" })
  observeEvent(input$btn_crusher, { rv$cur2 <- "CRUSHER" })
  observeEvent(input$btn_troi, { rv$cur2 <- "TROI" })
  observeEvent(input$btn_worf, { rv$cur2 <- "WORF" })
  observeEvent(input$btn_wesley, { rv$cur2 <- "WESLEY" })

  ptheme <- reactive({
    if(input$dark) theme_lcars_dark(9, "Oswald", 0.1) else theme_lcars_light(9, "Oswald", 0.1)
  })
  clr <- reactive(if(input$dark) "#FFFFCC" else "#000000")
  clr2 <- reactive(if(input$dark) "#99ccff" else "#664466")

  p_totals <- reactive({
    g <- ggplot(totals, aes(character, words, fill = if(input$uniforms) uniform else NULL))
    g <- g + if(input$uniforms)
      geom_col(color = NA, show.legend = FALSE) else
        geom_col(color = NA, fill = clr2(), show.legend = FALSE)
    g <- g + scale_fill_manual(values = uniform_colors) +
      scale_x_discrete(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      ptheme() + theme(text = element_text(size = 18)) +
      labs(x = NULL, y = "TOTAL WORDS")
    if(input$bars){
      g <- g + geom_text(aes(label = paste0(round(words / 1000), "K")),
        size = 10, color = "white", vjust = 1.3, family = "Oswald")
    }
    g
  })

  p_biggest <- reactive({
    g <- ggplot(biggest, aes(season, words)) +
      geom_linerange(aes(ymin = ymn, ymax = ymx), color = clr(), size = 0.25)
    g <- g + if(input$uniforms)
      geom_point(aes(color = uniform), size = 1, show.legend = FALSE) else
        geom_point(size = 1, color = clr(), show.legend = FALSE)
    g + scale_color_manual(values = uniform_colors) +
      geom_text_repel(aes(label = paste0(title, " (", character, ")")), color = clr(),
                      size = 3, hjust = -0.1, direction = "y", segment.size = 0.15, min.segment.length = 0.65) +
      scale_x_continuous(breaks = 1:7, labels = 1:7, expand = expand_scale(0, c(0.1, 0.8))) +
      ptheme() + theme(panel.grid.minor = element_blank(), text = element_text(size = 18)) +
      labs(x = "SEASON", y = "TOTAL WORDS")
  })

  topPanelPlot <- reactive({
    switch(rv$cur1, totals = p_totals(), best = p_biggest())
  })

  output$topPanel <- renderUI({
    totals <- rv$cur1 == "totals"
    fluidRow(
      column(3,
        if(totals) h3("Total spoken words") else h3("Prominent roles"),
        if(totals) p("Estimated spoken word count for each member of the main bridge crew.") else
            p("A summary of each character's biggest episode per season in terms of estimated spoken words"),
        br(), br(),
        HTML("<p>Side note: Notice the customization of the toggle buttons in the side panel. See <code>lcarsApp(\"toggle\")</code> for many more examples.</p>")
      ),
      column(9,
        renderPlot(topPanelPlot(), res = 200, height = 600)
      )
    )
  })

  bottomPanelPlot <- reactive({
    d <- biggest[biggest$character == rv$cur2, ]
    clrs <- ifelse(rv$cur2 %in% c("CRUSHER", "TROI", "WESLEY"), uniform_colors[3],
                   ifelse(rv$cur2 %in% c("DATA", "LA FORGE", "WORF"), uniform_colors[2], uniform_colors[1]))
    g <- ggplot(d, aes(season, words)) +
      geom_linerange(aes(ymin = ymn, ymax = ymx), color = clr(), size = 0.25)
    g <- g + if(input$uniforms)
      geom_point(aes(color = uniform), size = 1, show.legend = FALSE) else
        geom_point(size = 1, color = clr(), show.legend = FALSE)
    g + scale_color_manual(values = clrs) +
      geom_text_repel(aes(label = paste0(title, " (", character, ")")), color = clr(),
                      size = 4, hjust = -0.1, direction = "y", segment.size = 0.15, min.segment.length = 0.65) +
      scale_x_continuous(breaks = 1:7, labels = 1:7, expand = expand_scale(0, c(0.1, 0.8))) +
      ptheme() + theme(panel.grid.minor = element_blank(), text = element_text(size = 18)) +
      labs(x = "SEASON", y = "TOTAL WORDS")
  })

  output$bottomPanel <- renderUI({
      fluidRow(column(12, renderPlot(bottomPanelPlot(), res = 200, height = 600)))
  })

}

shinyApp(ui = ui, server = server)
