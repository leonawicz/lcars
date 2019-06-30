library(shiny)

ui <- lcarsPage(force_uppercase = TRUE,
  br(), lcarsHeader("LCARS Demo: lcarsToggle"), br(),
  fluidRow(
    column(2, style = "padding-left:30px;",
      p("You can control several aspects of the LCARS toggle button with the available arguments."),
      HTML(
        "<ul>
        <li>Default filled color or inverted</li>
        <li>Default rectangle or pill (rounded)</li>
        <li>Control <code>TRUE</code> and <code>FALSE</code> text labels and colors</li>
        <li>Background color and border color</li>
        <li>Custom width.</li>
        </ul>"),
      HTML("<p>Fixed widths, e.g., <code>\"100px\"</code> recommended. <code>\"100%\"</code> can be used but button movement is not smooth.</p>")
    ),
    column(5,
      lcarsToggle("tog1", "Default", FALSE, pill = FALSE, inverse = FALSE, "Yes", "No", width = "100px"),
      lcarsToggle("tog2", "Inverse", FALSE, pill = FALSE, inverse = TRUE, "True", "False", width = "100px"),
      lcarsToggle("tog3", "Widths/color", FALSE, pill = FALSE, inverse = FALSE, "On", "Off", "periwinkle", "red-damask", width = "150px"),
      lcarsToggle("tog4", "Border color", FALSE, pill = FALSE, inverse = TRUE, "Affirmative", "Negative", "periwinkle", "red-damask", border_color =  "#333333", width = "200px"),
      lcarsToggle("tog5", "Longer text", FALSE, pill = FALSE, inverse = FALSE, "Include variables", "Exclude variables", "pale-canary", "bourbon", width = "250px"),
      lcarsToggle("tog6", "100% width/border color", FALSE, pill = FALSE, inverse = FALSE, "Include variables", "Exclude variables", "pale-canary", "bourbon", border_color = "#FFFFFF", width = "100%"),
      lcarsToggle("tog7", "Border/outer/label colors", FALSE, pill = FALSE, inverse = FALSE, "Include variables", "Exclude variables", "pale-canary", "bourbon",
                  outer_border = TRUE, outer_color  = "atomic-tangerine", label_color = "lilac", width = "250px")
    ),
    column(5,
      lcarsToggle("tog8", "Pill", FALSE, pill = TRUE, inverse = FALSE, "Yes", "No", width = "100px"),
      lcarsToggle("tog9", "Pill inverse", FALSE, pill = TRUE, inverse = TRUE, "True", "False", width = "100px"),
      lcarsToggle("tog10", "Widths/color", FALSE, pill = TRUE, inverse = FALSE, "On", "Off", "periwinkle", "red-damask", width = "150px"),
      lcarsToggle("tog11","Background color", FALSE, pill = TRUE, inverse = TRUE, "Affirmative", "Negative", "periwinkle", "red-damask", background_color =  "#333333", width = "200px"),
      lcarsToggle("tog12", "Border color", FALSE, pill = TRUE, inverse = FALSE, "Include variables", "Exclude variables", "pale-canary", "bourbon", border_color = "#FFFFFF", width = "250px"),
      lcarsToggle("tog13","100% width", FALSE, pill = TRUE, inverse = FALSE, "Include variables", "Exclude variables", "pale-canary", "bourbon", width = "100%"),
      lcarsToggle("tog14", "100% width/border color/right align label", FALSE, pill = FALSE, inverse = FALSE, "Include variables", "Exclude variables", "pale-canary", "bourbon", border_color = "#FFFFFF",
                  outer_border = TRUE, outer_color  = "atomic-tangerine", label_right = TRUE, width = "100%")
    )
  ),
  br(), lcarsHeader(), br()
)

server <- function(input, output) {}
shinyApp(ui = ui, server = server)
