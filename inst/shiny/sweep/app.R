library(shiny)
library(ggplot2)
d <- data.frame(x = rnorm(500))
g <- ggplot(d, aes(x)) + theme_lcars_dark()
g1 <- g + geom_histogram(color = "black", fill = lcarsdata$value[11], bins = 20) +
  ggtitle("Plot 1")
g2 <- g + geom_histogram(color ="black", fill = lcarsdata$value[4], bins = 20) +
  ggtitle("Plot 2")

left <- div(
  h4("Some text"),
  p("The fine print.")
)
note <- p("Note: using 100% width is not recommended due to some current limitations in responsiveness.")
note2 <- HTML("<p>Note: you can use <code>expand</code> to extend the negative top or bottom margin of a left or right content panel, e.g., <code>expand = c(0, 350)</code>.</p>
              <p>This expands the available vertical space for the left and right content boxes, respectively, in the direction where there is no sweep formation.</p>")
note3 <- HTML("<br/><p>This is useful if you want to fill the entire perceptual space formed by a stacked sweep and reverse sweep with a single content div rather than be foreced to split content into two pieces aligned to each sweep.</p>
              <p>See the <code>lcarsSweep</code> example in the help docs.</p>")

ui <- lcarsPage(
  lcarsHeader("LCARS sweep"),
  fluidRow(
    column(4, lcarsWell(h4("Default sweep"), lcarsSweep(width = 400))),
    column(8,
      lcarsWell(
        h4("Decrease column width"),
        lcarsSweep(p("Column inputs go here.", style = "width:100px;"),
          note, p("Right side content here."),
          column_width = 100)
      )
    )
  ),
  fluidRow(
    column(4,
      lcarsWell(
        h4("Include column inputs"),
        h4("Vertically stack sweep and reverse sweep"),
        lcarsSweep(
          inputColumn(lcarsButton("x1", "Button", width = 80)),
          column_width = 80
        ),
        lcarsSweep(
          inputColumn(
            lcarsButton("x2", "Button A", width = 80),
            lcarsButton("x3", "Button B", width = 80)
          ),
          column_width = 80,
          reverse = TRUE
        )
      )
    ),
    column(8,
      lcarsWell(
        h4("Change colors and relative widths of content sections"),
        h4("Add title and subtitle, input column padding, and content"),
        lcarsSweep(
          inputColumn(
            lcarsButton("x1", "Button"),
            lcarsRect(color = "hopbush", height = 80)
          ),
          note2, plotOutput("plot1", height = 300),
          title = "Title", subtitle = "Subtitle",
          color = "pale-canary", left_width = 0.3, width = 900
        ),
        lcarsSweep(
          inputColumn(
            lcarsButton("x2", "Button A"),
            lcarsButton("x3", "Button B"),
            lcarsRect(color = "lilac")
          ),
          note3, plotOutput("plot2", height = 300, width = "100%"),
          title = "Title 2", subtitle = "Subtitle 2",
          color = "anakiwa", reverse = TRUE, left_width = 0.3, width = 900
        )
      )
    )
  )
)

server <- function(input, output) {
  output$plot1 <- renderPlot(g1)
  output$plot2 <- renderPlot(g2)
}

shinyApp(ui, server)
