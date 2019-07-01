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
              label_color = "atomic-tangerine", , label_right = TRUE, width = "150px"),
  lcarsToggle("uniforms", "Uniforms", FALSE, pill = FALSE, inverse = FALSE, "Yes", "No",
              false_color = "lilac", label_color = "lilac", , label_right = TRUE, width = "150px"),
  lcarsToggle("dark", "Dark theme", FALSE, pill = FALSE, inverse = FALSE, "Yes", "No",
              false_color = "tamarillo", outer_border = TRUE, outer_color = "tamarillo", label_color = "tamarillo")
)

bc2 <- buttonColumn(
  lcarsButton("btn_picard", "Picard"),
  lcarsButton("btn_data", "Data"),
  lcarsButton("btn_riker", "Riker"),
  lcarsButton("btn_laforge", "La Forge"),
  lcarsButton("btn_crusher", "Crusher"),
  lcarsButton("btn_troi", "Troi"),
  lcarsButton("btn_worf", "Worf"),
  lcarsButton("btn_wesley", "Wesley")
)

ui <- lcarsPage(
  br(),
  lcarsHeader("Enterprise NCC-1701-D"),
  br(),
  lcarsBox(uiOutput("topPanel"),
    title = "Crew summary", buttonColumn = bc1, color = c("#ffcc99", "#ffcc66"), title_color = "atomic-tangerine"
  ),
  br(),
  lcarsBox(uiOutput("bottomPanel"),
    title = "Crew members", buttonColumn = bc2, color = c("#ffcc99", "#ffcc66"), title_color = "atomic-tangerine"
  ),
  br(),
  lcarsHeader(),
  br()
)

server <- function(input, output) {
  rv <- reactiveValues(cur1 = "totals", cur2 = "Picard")

  observeEvent(input$btn_totals, { rv$cur1 <- "totals" })
  observeEvent(input$btn_best, { rv$cur1 <- "best" })

  observeEvent(input$btn_picard, { rv$cur2 <- "Picard" })
  observeEvent(input$btn_data, { rv$cur2 <- "Data" })
  observeEvent(input$btn_riker, { rv$cur2 <- "Riker" })
  observeEvent(input$btn_laforge, { rv$cur2 <- "La Forge" })
  observeEvent(input$btn_crusher, { rv$cur2 <- "Crusher" })
  observeEvent(input$btn_troi, { rv$cur2 <- "Troi" })
  observeEvent(input$btn_worf, { rv$cur2 <- "Worf" })
  observeEvent(input$btn_wesley, { rv$cur2 <- "Wesley" })

  ptheme <- reactive({
    if(input$dark) theme_lcars_dark(9, "Oswald") else theme_lcars_light(9, "Oswald")
  })
  clr <- reactive(if(input$dark) "#FFFFCC" else "#000000")

  p_totals <- reactive({
    g <- ggplot(totals, aes(character, words, fill = if(input$uniforms) uniform else NULL)) +
      geom_col(color = NA, show.legend = FALSE) +
      scale_fill_manual(values = uniform_colors) +
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
    ggplot(biggest, aes(season, words)) +
      geom_linerange(aes(ymin = ymn, ymax = ymx), color = clr()) +
      geom_point(aes(fill = if(input$uniforms) uniform else NULL), shape = 21, size = 2, color = clr(), show.legend = FALSE) +
      scale_fill_manual(values = uniform_colors) +
      geom_text_repel(aes(label = paste0(title, " (", character, ")")), color = clr(),
                      size = 3, hjust = -0.1, direction = "y", min.segment.length = 0.65, family = "Oswald") +
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
    clrs <- ifelse(rv$cur2 %in% c("Crusher", "Troi", "Wesley"), uniform_colors[3],
                   ifelse(rv$cur2 %in% c("Data", "La Forge", "Worf"), uniform_colors[2], uniform_colors[1]))
    ggplot(d, aes(season, words)) +
      geom_linerange(aes(ymin = ymn, ymax = ymx), color = clr()) +
      geom_point(aes(fill = if(input$uniforms) uniform else NULL), shape = 21, size = 2, color = clr(), show.legend = FALSE) +
      scale_fill_manual(values = clrs) +
      geom_text_repel(aes(label = paste0(title, " (", character, ")")), color = clr(),
                      size = 4, hjust = -0.1, direction = "y", min.segment.length = 0.65, family = "Oswald") +
      scale_x_continuous(breaks = 1:7, labels = 1:7, expand = expand_scale(0, c(0.1, 0.8))) +
      ptheme() + theme(panel.grid.minor = element_blank(), text = element_text(size = 18)) +
      labs(x = "SEASON", y = "TOTAL WORDS")
  })

  output$bottomPanel <- renderUI({
      fluidRow(column(12, renderPlot(bottomPanelPlot(), res = 200, height = 600)))
  })

}

shinyApp(ui = ui, server = server)
