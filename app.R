library(shiny)

ui <- fluidPage(
  navbarPage(
    title = "Socioeconomic Impact on Communities of Color During Covid-19",
    tabPanel("Introduction",
             fluidRow(
               column(12,
                      h2("Introduction"),
                      p("This report dives into how socio-economic factors influence how specific communities of color (Black, Hispanic and Latino, American Indian) are impacted by the COVID-19 pandemic. Our report compares data of these specific communities in correlation to other racial groups under the context of each of our three factors: education, employment and income, and healthcare access. 
                        Our report also provides context on how these disparities intersect together and possible solutions to address them. Ultimately, showcasing the inequities these communities face and how more needs to be done to address the needs of communities of color."),
                      img(src = "intro_image.jpg", height = "400px", width = "600px")
                      #I TRIED TO UPLOAD AN IMAGE BUT IT WONT, if anybody else can upload an image for me instead thatd be great! just ANY relevant image.
               )
             )
    ),
    tabPanel("Education",
             fluidRow(
               column(12,
                      h2("Education"),
                      p("This graph compares data on the participation rates of academic programs available in high schools to graduation rates in college and high school by race. Low participation of academic programs could indicate that these communities may not be given opportunities to graduate in high school or college. As a result, they might face barriers to accessing higher education or obtaining well-paying jobs compared to other groups.
                        This disparity in educational attainment can contribute to a cycle of limited economic opportunities, potentially leading to lower expected incomes and perpetuating socioeconomic inequalities that can impact the accessibility to how these communities receive healthcare. ")
               )
             )
    ),
    tabPanel("Income and Employment",
             fluidRow(
               column(12,
                      h2("Income and Employment"),
                      p("This graph compares data on the median income and unemployment rates by race. A lower median income and a higher rate of unemployment could indicate that a racial group has fewer access to healthcare sources. Thus, making it challenging for individuals to seek medical assistance or afford preventive measures. While a higher rate of employment may indicate to a lack of health insurance and force individuals into jobs with greater exposure risks, increasing their chances of contracting the virus.
")
               )
             )
    ),
    tabPanel("Healthcare Access",
             fluidRow(
               column(12,
                      h2("Healthcare Access"),
                      p("This graph presents data on uninsured rates and the percentage of individuals avoiding care due to cost, categorized by racial groups. 
                        A higher rate of uninsured individuals may indicate a barrier to attaining care, leading to neglected medical care and increasing the risk of COVID-19. A higher rate of individuals avoiding care due to cost, on the otherhand, might indicate a lack of detection or treatment of COVID-19. Resulting in potentially contributing to increased transmission and severity of the virus within these communities.")
               )
             )
    ),
    tabPanel("Conclusion",
             fluidRow(
               column(12,
                      h2("Conclusion"),
                      p("In 2020, the first case of Covid-19 was documented in the U.S. in Washington state. Unbeknownst to the general public, this case would further mark the beginnings of a pandemic as the world was taken overhaul by the Coronavirus.
                        Nonetheless, researchers quickly gathered to document data and determine possible responses to the pandemic at hand and discovered a finding crucial to understanding public health: Black, Hispanic, Latino, and Indigenous communities were most at risk of COVID-19. 
                        Our project aims to examine how three key factors—education, employment and income, and healthcare access—interconnect and impact these communities.")
               )
             )
    )
  )
)

server <- function(input, output) {}

shinyApp(ui = ui, server = server)
