library(shiny)
library(ggplot2)
library(showtext)
font_add_google("Oswald", "Oswald")
showtext_auto()

g <- ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) + geom_point(size = 3)
g1 <- g + theme_lcars_dark(28, "Oswald")
g2 <- g + theme_lcars_light(28, "Oswald")

bc1 <- inputColumn(
    lcarsButton("btn1", "A button"),
    lcarsButton("btn2", "Another button"),
    lcarsToggle("tog1", "Toggle 1", FALSE, pill = FALSE, inverse = FALSE, "Yes", "No",
                label_color = "atomic-tangerine", label_right = TRUE, width = "150px"),
    lcarsToggle("tog2", "Toggle 2", FALSE, pill = FALSE, inverse = FALSE, "Yes", "No",
                false_color = "lilac", label_color = "lilac", label_right = TRUE, width = "150px"),
    lcarsToggle("tog3", "Toggle 3", FALSE, pill = FALSE, inverse = FALSE, "Yes", "No",
                false_color = "tamarillo", outer_border = TRUE, outer_color = "tamarillo", label_color = "tamarillo")
)

bc2 <- inputColumn(
    lcarsButton("btn4", "Right side")
)
corners <- c("Top left", "Top right", "Bottom right", "Bottom left")
sides <- c("Top", "Right", "Bottom", "Left")
clrs <- c("black" = "#000000", unique(lcarsdata$name))
clr <- "atomic-tangerine"
txt <- c("Title", "Subtitle")
w <- 240
lab_clr <- "dodger-pale"

ui <- lcarsPage(
    lcarsHeader("LCARS Box properties"),
    fluidRow(
      column(3,
        lcarsh4("Customize box", color = lab_clr, width = 220),
        HTML("<p>Make selections in this box to configure the layout and style of the <code>lcarsBox</code> widget below.</p>"),
        lcarsWell(
          lcarsh4("Border elements", color = lab_clr, title_invert = TRUE, width = w),
          checkboxGroupInput("corners", "Corner elbows", corners, corners[c(1, 4)], inline = TRUE),
          checkboxGroupInput("sides", "Side panels", sides, sides[c(1, 3, 4)], inline = TRUE),
          fluidRow(
              column(6,
                     checkboxGroupInput("incols", "Example input columns", c("Left side", "Right side"), inline = TRUE)
              ),
              column(6,
                     lcarsToggle("clip", "Main panel clipping", TRUE, true = "Clipping on", false = "Clipping off")
              )
          )
        ),
        lcarsWell(lcarsh4("Elbow colors", color = lab_clr, title_invert = TRUE, width = w),
        fluidRow(
          column(6,
            selectInput("clr1", "Top left", clrs, clr),
            selectInput("clr4", "Bottom left", clrs, clr)
          ),
          column(6,
            selectInput("clr2", "Top right", clrs, clr),
            selectInput("clr3", "Bottom right", clrs, clr)
          )
        )),
        lcarsWell(
            lcarsh4("Sides", color = lab_clr, title_invert = TRUE, width = w),
            fluidRow(
              column(6,
                selectInput("clr5", "Top", clrs, clr),
                selectInput("clr8", "Left", clrs, clr),
                selectInput("wl", "Left width", c(30, 60, 100, 150, 200), 150)
              ),
              column(6,
                selectInput("clr7", "Bottom", clrs, clr),
                selectInput("clr6", "Right", clrs, clr),
                selectInput("wr", "Right width", c(30, 60, 100, 150, 200), 150)
              )
            )
        ),
        lcarsWell(
            lcarsh4("Title text", color = lab_clr, title_invert = TRUE, width = w),
            fluidRow(
              column(6, checkboxGroupInput("titles", "Include titles", txt, "Title", inline = TRUE)),
              column(6, checkboxGroupInput("rightjust", "Right align", txt, txt, inline = TRUE))
            ),
            selectInput("clr9", "Title color", clrs, clr),
            selectInput("clr10", "Subtitle color", clrs, clr)
        )
      ),
      column(9,
        uiOutput("box2a"),
        uiOutput("box2b")
      )
    )
)

server <- function(input, output) {
  elbows <- reactive({
    x <- match(input$corners, corners)
    if(!length(x) || is.na(x[1])) NULL else x
  })
  sidepanels <- reactive({
    x <- match(input$sides, sides)
    if(!length(x) || is.na(x[1])) NULL else x
  })
  main <- reactive("Title" %in% input$titles)
  subt <- reactive("Subtitle" %in% input$titles)
  incol1 <- reactive("Left side" %in% input$incols)
  incol2 <- reactive("Right side" %in% input$incols)

  output$box2a <- renderLcarsBox({
      lcarsBox(
          fluidRow(
              column(3,
                     p("This box is updated based on the input selection in the box above. This text and the plot to the right represent some example main panel content."),
                     p("Note: The example sidebar inputs have already been styled. This is done directly in functions like", code("lcarsButton"), "."),
                     p("Hint: If you want to remove the left and or right side panel (no side and no side input column),
            but you do not want the main panel content to fill the space, then keep the side but set its color to black.")
              ),
              column(9, renderPlot(g1))
          ),
          title = if(main()) "Main title" else NULL,
          subtitle = if(subt()) "Subtitle" else NULL,
          corners = elbows(), sides = sidepanels(),
          left_inputs = if(incol1()) bc1 else NULL,
          right_inputs = if(incol2()) bc2 else NULL,
          color = c(input$clr1, input$clr2, input$clr3, input$clr4),
          side_color = c(input$clr5, input$clr6, input$clr7, input$clr8),
          title_color = input$clr9,
          subtitle_color = input$clr9,
          title_right = "Title" %in% input$rightjust,
          subtitle_right = "Subtitle" %in% input$rightjust,
          clip = input$clip,
          width_left = input$wl,
          width_right = input$wr
      )
  })

  output$box2b <- renderLcarsBox({
      lcarsBox(
          fluidRow(
              column(3,
                     HTML("<p>Same as above, but a preview of <code>lcars_theme_light</code> in place of <code>lcars_theme_dark</code>."),
                     p("Note: The LCARS box allows you to make use of the extra black space that occurs above and below the main content area when corner elbows are present."),
                     p("This is done by using negative margins. It should only be used when you set box properties such that the main content is not already extended to either outer left or right edge of the box."),
                     p("See the clipping toggle button in the top left control panel.")
              ),
              column(9, renderPlot(g2))
          ),
          title = if(main()) "Main title" else NULL,
          subtitle = if(subt()) "Subtitle" else NULL,
          corners = elbows(), sides = sidepanels(),
          left_inputs = if(incol1()) bc1 else NULL,
          right_inputs = if(incol2()) bc2 else NULL,
          color = c(input$clr1, input$clr2, input$clr3, input$clr4),
          side_color = c(input$clr5, input$clr6, input$clr7, input$clr8),
          title_color = input$clr9,
          subtitle_color = input$clr10,
          title_right = "Title" %in% input$rightjust,
          subtitle_right = "Subtitle" %in% input$rightjust,
          clip = input$clip,
          width_left = input$wl,
          width_right = input$wr
      )
  })

}

shinyApp(ui = ui, server = server)
