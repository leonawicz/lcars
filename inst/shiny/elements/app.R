library(shiny)

clrs <- unique(lcarsdata$name)

ui <- lcarsPage(
  lcarsHeader("LCARS basic elements"),
  lcarsWell(
    fluidRow(
      column(3,
        h3("Rectangle/Pill indicators"),
        selectInput("w", "Width", seq(100, 600, by = 100), 100),
        selectInput("h", "Height", seq(30, 300, by = 30), 30),
        selectInput("r", "Round sides (pill or half-pill)", c("none", "both", "left", "right"), "none"),
        selectInput("dec", "Decorate rounded sides", c("none", "both", "left", "right"), "none"),
        selectInput("clr", "Color", clrs, "golden-tanoi"),
        sliderInput("textsize", "Text size", 16, 60, 16, 4),
        checkboxInput("heading", "Optional header title")
      ),
      column(3,
        h3("Brackets"),
        selectInput("clr2", "Color", clrs, "golden-tanoi"),
        checkboxInput("hollow", "Hollow brackets"),
        uiOutput("bracket")
      )
    )
  )
)

server <- function(input, output) {
  output$rect <- renderUI({
    lcarsRect("Some text", input$r, input$dec, color = input$clr,
              text_size = input$textsize, title = if(input$heading) "A title",
              width = as.numeric(input$w), height = as.numeric(input$h))
  })

  output$bracket <- renderUI({
      lcarsBracket(
          div(style = "padding-bottom:5px;"),
          HTML("<p>The <code>lcarsRect</code> function creates basic rectangle widgets with the standard bottom-right-aligned LCARS text. It offers discrete sizing and rounding options that fit LCARS style.</p>
               <p>The functions <code>lcarsPill</code>, <code>lcarsLeftPill</code> and <code>lcarsRightPill</code> are wrappers around lcarsRect with default pill rounding.</p>
               <p>In LCARS widgets, to decorate means to add the sliced half pill, which is a rounded edge separated from the main widget by a narrow gap.</p>"),
          uiOutput("rect"), div(style = "padding-bottom:10px;"),
          color = input$clr2, hollow = input$hollow, width = "100%"
      )
  })
}

shinyApp(ui = ui, server = server)
