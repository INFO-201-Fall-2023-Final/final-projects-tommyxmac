library(shiny)
library(ggplot2)
library(plotly)
library(shinyanimate)
library(gganimate)
source("GroupBD_App.r") 


ui <- fluidPage(
  navbarPage(
    title = "Socioeconomic Impact on Communities of Color During Covid-19",
    tabPanel("Introduction",
             fluidRow(
               column(12,
                      h2("Introduction"),
                      p("This report dives into how socio-economic factors influence how specific communities of color (Black, Hispanic and Latino, American Indian) are impacted by the COVID-19 pandemic. Our report compares data of these specific communities in correlation to other racial groups under the context of each of our three factors: education, employment and income, and healthcare access. 
                        Our report also provides context on how these disparities intersect together and possible solutions to address them. Ultimately, connecting all these factors together to showcase the inequities these communities face and how more needs to be done to address the needs of communities of color."),
                      img(src = "intro_image.jpg", height = "400px", width = "600px")
                      #I TRIED TO UPLOAD AN IMAGE BUT IT WONT WORK, if anybody else can upload an image for me instead thatd be great! just ANY relevant image like... search up communities of color and covid-19 and boom!
               )
             )
    ),
    tabPanel("Education",
             fluidRow(
               column(12,
                      h2("Education"),
                      p("This graph compares data on the participation rates of academic programs available in high schools to graduation rates in college and high school by race. Low participation in academic programs could indicate that these communities may not be given opportunities to graduate from high school or college. As a result, they might face barriers to accessing higher education or obtaining well-paying jobs compared to other groups. This disparity in educational attainment can contribute to a cycle of limited economic opportunities, potentially leading to lower expected incomes and perpetuating socioeconomic inequalities that can impact the accessibility to how these communities receive healthcare. "),
                      h3("Key Takeaways"),
                      p("The most evident underrepresentation in proportion to their population is observed among Hispanic and Latino individuals, American Indians, Blacks, and Pacific Islanders for academic programs. 
                        These communities also share lower rates of graduation in high school aside from Blacks and Native Hawaiians. However, all of these communities have a significantly lower percentage (at least a 10% difference) of holding a bachelor's degree compared to their Asian and White counterparts. This is also reflective of these communities likely not having access to college preparatory programs such as Dual Enrollment or Advanced Placement classes offered. Understanding these disparities is important as not only does it impact education, but it also impacts social mobility and the economic stability of the communities at hand as a result of previous historical segregation."),
                      
                      sidebarLayout(
                        sidebarPanel(
                          conditionalPanel(
                            condition = "input.plotType === 'Enrollment in Academic Programs Percentage by Race'",
                            sliderInput("category", "Select Category:",
                                        min = 1, max = ncol(df) - 1, value = 1, step = 1)
                          ),
                          radioButtons("plotType", "Choose Plot Type:",
                                       choices = c("Enrollment in Academic Programs Percentage by Race",
                                                   "High School and Bachelor Degree Graduation by Race"),
                                       selected = "Enrollment in Academic Programs Percentage by Race"
                          ),
                          style = "margin-top: 20px;" 
                        ),
                        mainPanel(
                          fluidRow(
                            conditionalPanel(
                              condition = "input.plotType === 'Enrollment in Academic Programs Percentage by Race'",
                              plotlyOutput("donutChart")
                            ),
                            conditionalPanel(
                              condition = "input.plotType === 'High School and Bachelor Degree Graduation by Race'",
                              plotOutput("educationPlot")
                            )
                          )
                        )
                      ),
                        h3("Possible Solutions"),
                      tags$b("Funding Schools That Primarily Serve Communities Of Color:"),
                      p("As evident in the graph above, students of color are generally underrepresented in academic programs that assist them in preparation for college. By investing in these communities and their opportunities to have access to these programs, we enable them to have higher rates of success in college and help bolster their college application as well. Learn more through the following link:",
                        tags$a("https://www.vox.com/22266219/biden-eduation-school-funding-segregation-antiracist-policy", "Vox")),
                      tags$b("Re-instate Affirmative Action Policies:"),
                      p("Although many existing laws exist to protect against the discrimination of women and people of color, these laws are not enough to pave equity as a result of past historical racism. By implementing affirmative action policies, we can ensure that equal opportunities exist for people regardless of race and gender. We can also elevate communities of color to be on par with their Asian and White counterparts in college attendance as well through affirmative action programs such as diversity outreach and first-generation support. Learn more through the following link:",
                        tags$a("https://www.aclu.org/documents/striving-equal-opportunity-why-aclu-supports-affirmative-action", "ACLU"))
               
                         )
             )
    ),
    tabPanel("Income and Employment",
             fluidRow(
               column(12,
                      h2("Income and Employment"),
                      p("This graph compares data on the median income and unemployment rates by race. A lower median income and a higher rate of unemployment could indicate that a racial group has fewer access to healthcare sources. Thus, making it challenging for individuals to seek medical assistance or afford preventive measures. While a higher rate of employment may indicate to a lack of health insurance and force individuals into jobs with greater exposure risks, increasing their chances of contracting the virus.") 
                      library(shiny)
library(shinyanimate)
library(gganimate)
library(ggplot2)
library(tidyr)


data <- data.frame(
  group = c("American Indian or Alaska Native", "Asian", "Black or African American", 
            "Hispanic or Latino of any race", "Native Hawaiian or Other Pacific Islander", "White"),
  values = c(52810, 108700, 52860, 62800, 69973, 81060),
  frame = c(1, 2, 3, 4, 5, 6)
)


unemployment_data <- data.frame(
  Racial.Groups = c("American Indian or Alaska Native", "Asian", "Black or African American", 
                    "Hispanic or Latino of any race", "Native Hawaiian or Other Pacific Islander", 
                    "Two or more races", "White"),
  x2020_unemployment_percentage = c(11.7, 8.7, 11.4, 10.4, 8.9, 11.6, 7.3),
  x2021_unemployment_percentage = c(8.2, 5, 8.6, 5, 6.9, 8.2, 4.7)
)


unemployment_data_long <- gather(unemployment_data, key = "Year", value = "UnemploymentRate", -Racial.Groups)


ui <- fluidPage(
  titlePanel("Combined Visualization"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("selected_races", "Select Race(s):", choices = data$group, selected = ""),
      actionButton("update_button", "Update Plot")
    ),
    mainPanel(
      plotOutput("animated_bar_plot"),
      plotOutput("unemployment_bar_plot")
    )
  )
)


server <- function(input, output, session) {
  
  filtered_data <- reactive({
    selected_races <- input$selected_races
    data[data$group %in% selected_races, ]
  })
  
  
  output$animated_bar_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = group, y = values, group = group, fill = group)) +
      geom_col() +
      labs(title = "Income by Race", 
           x = "Race", y = "Income in $") + 
      theme_bw()
  })
  
  observeEvent(input$update_button, {
    animate("animated_bar_plot", nframes = nrow(filtered_data()))
  })
  

  output$unemployment_bar_plot <- renderPlot({
    ggplot(unemployment_data_long, aes(x = Racial.Groups, y = UnemploymentRate, fill = Year)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7) +
      coord_polar(theta = "y") +
      labs(title = "Unemployment Rate by Race (2020 and 2021)",
           y = "Unemployment Rate (%)",
           x = NULL,
           fill = "Year") +
      theme_minimal()
  })
}


shinyApp(ui, server)

                        )
             )
    ),
    tabPanel("Healthcare Access",
             fluidRow(
               column(12,
                      h2("Healthcare Access"),
                      p("This graph presents data on uninsured rates and the percentage of individuals avoiding care due to cost, categorized by racial groups. 
                        A higher rate of uninsured individuals may indicate a barrier to attaining care, leading to neglected medical care and increasing the risk of COVID-19. A higher rate of individuals avoiding care due to cost, on the otherhand, might indicate a lack of detection or treatment of COVID-19. Resulting in potentially contributing to increased transmission and severity of the virus within these communities.")
                      #INSERT GRAPH HERE  
                        )
             )
    ),
    tabPanel("Conclusion",
             fluidRow(
               column(12,
                      h2("Project Title: Socioeconomic Impact on Communities of Color During Covid-19"),
                      p("Authors: Sarah Coulibaly, Ndeye Diop, Tommy Mac"),
                      p("Affiliation: INFO 201"),
                      p("Date: 11/23/2023"),
                      p("Key Words: Healthcare, Race, Social-Justice, Disparities, Inequities"),
                      h3("Introduction"),
                      p("In 2020, the first case of Covid-19 was documented in the U.S. in Washington state. Unbeknownst to the general public, this case would further mark the beginnings of a pandemic as the world was taken overhaul by the Coronavirus.
  Nonetheless, researchers quickly gathered to document data and determine possible responses to the pandemic at hand and discovered a finding crucial to understanding public health: Black, Hispanic, Latino, and Indigenous communities were most at risk of COVID-19. 
  Our project aims to examine how three key factors—education, employment and income, and healthcare access—interconnect and impact these communities.")
               )
             )
    )
  )
)

server <- function(input, output) {
  output$donutChart <- renderPlotly({
    selected_category <- df[, input$category + 1]
    labels <- df$Racial.Groups
    values <- selected_category
    
    donut_chart <- plot_ly(labels = ~labels, values = ~values, type = 'pie', hole = 0.6)
    
    donut_chart %>% layout(title = paste(" College Prep Enrollment by Racial Group -", names(df)[input$category + 1]))
  })
  output$educationPlot <- renderPlot({
    ggplot(data, aes(y = Racial_Groups)) +
      geom_col(aes(x = High_School_Grad_Percentage, fill = "High School Graduation"),
               position = "dodge", width = 0.4) +
      geom_col(aes(x = Bachelor_Degree_Percentage, fill = "Bachelor's Degree"),
               position = position_dodge(width = 0.5), width = 0.4) +
      labs(title = "High School Graduation and Bachelor's Degree Percentage by Race",
           y = "Racial Groups", x = "Percentage") +
      scale_x_continuous(labels = scales::percent_format(scale = 1), limits = c(0,100)) + 
      scale_fill_manual(values = c("High School Graduation" = "skyblue", "Bachelor's Degree" = "hotpink"),
                        name = "Education Level") +
      theme_minimal()
  })
}

shinyApp(ui = ui, server = server)
