library(shiny)

ui <- fluidPage(
  navbarPage(
    title = "Socioeconomic Impact on Communities of Color During Covid-19",
    tabPanel("Introduction",
             fluidRow(
               column(12,
                      h2("Introduction"),
                      p("This tab contains an introduction.")
               )
             )
    ),
    tabPanel("Income and Employment",
             fluidRow(
               column(12,
                      h2("Income and Employment"),
                      p("This tab contains information about income and employment.")
               )
             )
    ),
    tabPanel("Healthcare Access",
             fluidRow(
               column(12,
                      h2("Healthcare Access"),
                      p("This tab contains information about healthcare access.")
               )
             )
    ),
    tabPanel("Summary",
             fluidRow(
               column(12,
                      h2("Summary"),
                      p("This tab contains a summary of the information.")
               )
             )
    ),
    tabPanel("Conclusion",
             fluidRow(
               column(12,
                      h2("Conclusion"),
                      p("This tab contains a conclusion.")
               )
             )
    )
  )
)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)
