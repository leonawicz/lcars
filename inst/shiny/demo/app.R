library(shiny)
library(ggplot2)
library(ggrepel)
load("demoApp.RData")

p_totals <- ggplot(totals, aes(character, words, fill = uniform)) +
    geom_col(color = NA, show.legend = FALSE) +
    scale_fill_manual(values = uniform_colors) +
    geom_text(aes(label = paste0(round(words / 1000), "K")),
              size = 3, color = "white", vjust = 1.3) +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    theme_lcars_dark(9) + labs(x = NULL, y = "Total words")

p_biggest <- ggplot(biggest, aes(season, words)) +
    geom_linerange(aes(ymin = ymn, ymax = ymx), color = "#FFFFCC") +
    geom_point(aes(fill = uniform), shape = 21, size = 2, color = "#FFFFCC", show.legend = FALSE) +
    scale_fill_manual(values = uniform_colors) +
    geom_text_repel(aes(label = paste0(title, " (", character, ")")), color = "#FFFFCC",
                    size = 1, hjust = -0.1, direction = "y", min.segment.length = 0.65) +
    scale_x_continuous(breaks = 1:7, labels = 1:7, expand = expand_scale(0, c(0.1, 0.8))) +
    theme_lcars_dark(9) + theme(panel.grid.minor = element_blank()) +
    labs(y = "Total words")

blurb <- HTML('<h1>LCARS CSS Grid</h1>
            <p>Based on the Star Trek: The Next Generation LCARS (Library Catalog and Retrieval System) computer
                interface created by Michael Okuda. I also got the specifications from <a
                        href="https://www.lcars47.com/" target="_blank">LCARS47.com</a> for the shape specifications and
                color palettes, modifying the dimensions to be a bit more rounded. The <a
                        href="http://www.lcarssdk.org/" target="_blank">LCARS SDK</a> is also helpful.</p>
            <p>This design makes use of <a href="https://gridbyexample.com/" target="_blank">CSS Grid</a> and SVG to
                recreate the designs in an HMTL document. The odd shapes (endcaps, elbows, etc.) are created as SVGs
                using Adobe Illustrator. The responsive layout and design is possible through the flexibility of CSS
                Grid. Resizing the window will show that the horizontal and vertical block elements resize to fit the
                space. </p>
            <p>There is still some work to be done on device sizing, and eventually I hope to make a WordPress template
                out of it, But for now, this will do.</p>
            <p>This project is stored on <a href="https://github.com/hmt3design/LCARS_CSS_Grid"
                                            target="_blank">GitHub</a> if you wish to examine it. If you wish to access
                the Illustrator file with the complete collection of LCARS shapes that I created, it is also on <a
                        href="https://github.com/hmt3design/Illustrator-templates/blob/master/LCARS%20Shapes.ai"
                        target="_blank">GitHub</a>.</p>')

bc1 <- buttonColumn(
    lcarsButton("btn_totals", "Total words"),
    lcarsButton("btn_best", "Top episodes")
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
    title = "Crew summary", buttonColumn = bc1, color = c("#ffcc99", "#ffcc66"), title_color = "firebrick1", subtitle_color = "purple"
  ),
  br(),
  lcarsBox(uiOutput("bottomPanel"),
    title = "Crew members", buttonColumn = bc2, color = c("#ffcc99", "#ffcc66"), title_color = "firebrick1", subtitle_color = "purple"
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

  topPanelPlot <- reactive({
    switch(rv$cur1, totals = p_totals, best = p_biggest)
  })

  output$topPanel <- renderUI({
    totals <- rv$cur1 == "totals"
    fluidRow(
      column(3,
        if(totals) h3("Total spoken words") else h3("Prominent roles"),
        if(totals) p("Estimated spoken word count for each member of the main bridge crew.") else
            p("A summary of each character's biggest episode per season in terms of estimated spoken words")
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
      geom_linerange(aes(ymin = ymn, ymax = ymx), color = "#FFFFCC") +
      geom_point(aes(fill = uniform), shape = 21, size = 2, color = "#FFFFCC", show.legend = FALSE) +
      scale_fill_manual(values = clrs) +
      geom_text_repel(aes(label = paste0(title, " (", character, ")")), color = "#FFFFCC",
                      size = 2, hjust = -0.1, direction = "y", min.segment.length = 0.65) +
      scale_x_continuous(breaks = 1:7, labels = 1:7, expand = expand_scale(0, c(0.1, 0.8))) +
      theme_lcars_dark(9) + theme(panel.grid.minor = element_blank()) +
      labs(y = "Total words")
  })

  output$bottomPanel <- renderUI({
      fluidRow(column(12, renderPlot(bottomPanelPlot(), res = 200, height = 600)))
  })

}

shinyApp(ui = ui, server = server)
