#library(shiny)
library(ggplot2)
library(plotly)

source("GroupBD_App.r") 
source("covid19_impact.R")


ui <- fluidPage(
  navbarPage(
    title = "Socioeconomic Impact on Communities of Color During Covid-19",
    tabPanel("Introduction",
             fluidRow(
               column(12,
                      h2("Introduction"),
                      p("This report dives into how socio-economic factors influence how specific communities of color (Black, Hispanic and Latino, American Indian) are impacted by the COVID-19 pandemic. Our report compares data of these specific communities in correlation to other racial groups under the context of each of our three factors: education, employment and income, and healthcare access. 
                        Our report also provides context on how these disparities intersect together and possible solutions to address them. Ultimately, connecting all these factors together to showcase the inequities these communities face and how more needs to be done to address the needs of communities of color."),
                      img(src = "test.jpg", height = "500px", width = "800px")
                      #I TRIED TO UPLOAD AN IMAGE BUT IT WONT WORK, if anybody else can upload an image for me instead thatd be great! just ANY relevant image like... search up communities of color and covid-19 and boom!
               )
             )
    ),
    tabPanel("Education",
             fluidRow(
               column(12,
                      h2("Education"),
                      p("This graph compares data on the participation rates of academic programs available in high schools to graduation rates in college and high school by race. Low participation in academic programs could indicate that these communities may not be given opportunities to graduate from high school or college. As a result, they might face barriers to accessing higher education or obtaining well-paying jobs compared to other groups. This disparity in educational attainment can contribute to a cycle of limited economic opportunities, potentially leading to lower expected incomes and perpetuating socioeconomic inequalities that can impact the accessibility to how these communities receive healthcare."),
                      h3("Key Takeaways"),
                      p("The most evident underrepresentation in proportion to their population is observed among Hispanic and Latino individuals, American Indians, Blacks, and Pacific Islanders for academic programs. These communities also share lower rates of graduation in high school aside from Blacks and Native Hawaiians. However, all of these communities have a significantly lower percentage (at least a 10% difference) of holding a bachelor's degree compared to their Asian and White counterparts. This is also reflective of these communities likely not having access to college preparatory programs such as Dual Enrollment or Advanced Placement classes offered. Understanding these disparities is important as not only does it impact education, but it also impacts social mobility and the economic stability of the communities at hand as a result of previous historical segregation."),
                      h3("Possible Solutions"),
                      tags$b("Funding Schools That Primarily Serve Communities Of Color:"),
                      p("As evident in the graph above, students of color are generally underrepresented in academic programs that assist them in preparation for college. By investing in these communities and their opportunities to have access to these programs, we enable them to have higher rates of success in college and help bolster their college application as well. Learn more through the following link:",
                        tags$a("https://www.vox.com/22266219/biden-eduation-school-funding-segregation-antiracist-policy", "Vox")),
                      tags$b("Re-instate Affirmative Action Policies:"),
                      p("Although many existing laws exist to protect against the discrimination of women and people of color, these laws are not enough to pave equity as a result of past historical racism. By implementing affirmative action policies, we can ensure that equal opportunities exist for people regardless of race and gender. We can also elevate communities of color to be on par with their Asian and White counterparts in college attendance as well through affirmative action programs such as diversity outreach and first-generation support. Learn more through the following link:",
                        tags$a("https://www.aclu.org/documents/striving-equal-opportunity-why-aclu-supports-affirmative-action", "ACLU")), 
                      sidebarLayout(
                        sidebarPanel(
                          sliderInput("category", "Select Category:",
                                      min = 1, max = ncol(df) - 1, value = 1, step = 1,)
                        ),
                        mainPanel(
                          plotlyOutput("donutChart"),
                          plotOutput("educationPlot")
                        )
                      )
               )
             )
    ),
    tabPanel("Income and Employment",
             fluidRow(
               column(12,
                      h2("Income and Employment"),
                      p("This graph compares data on the median income and unemployment rates by race. A lower median income and a higher rate of unemployment could indicate that a racial group has fewer access to healthcare sources. Thus, making it challenging for individuals to seek medical assistance or afford preventive measures. While a higher rate of employment may indicate to a lack of health insurance and force individuals into jobs with greater exposure risks, increasing their chances of contracting the virus."), 
                      h3("Key Takeaways"),
                      p("As evident on the graph, rates of unemployment and income seem to correlate with levels of education. This can be seen in practically every group but one notable example is Asians having a average yearly income of 108,700 and being the second least less likely to be unemployed compared to their peers as reflective of their higher level of education. One outlier, however, is the Native Hawaiian or Other Pacific Islander population as this community has a slightly higher rate of income despite its rate of education. This may be because this particular group has a tendency to live in areas that have a higher cost of living and as such require a higher income as a result. Nonetheless, the income and employment of these communities rely heavily on their level of education as through education, these communities have higher access to jobs and are more likely to earn more as well."),
                      h3("Possible Solutions:"),
                      tags$b("Promote Unionization:"),
                      p("Through unionization, workers of color are able to work together in efforts to pave towards living wages, health coverage, and other necessary benefits. These unions help provide a platform for workers to advocate for their rights and also to advocate for increased representation and awareness of their struggles. By promoting unionization in communities of color, more can be done to address the gaps in yearly salary and unemployment rates as shown in the graph. Learn more here:",
                        tags$a("https://www.americanprogress.org/article/the-importance-of-unions-for-workers-of-color/", "American Progress")
                      ),
                      tags$b("Investment in Community Programs:"),
                      p("Investing in community programs is necessary to bridging the notable income and unemployment gap between Whites and people of color. Currently, Whites make at minimum $10,000 more than their counterparts. By investing in community programs dedicated towards skill-building and career preparation, individuals of color are able to be prepared for the jobs they take in the near future. This will also help support economic development as well as more communities of color are able to participate in the workforce alongside their counterparts. Learn more here:",
                        tags$a("https://www.citigroup.com/global/news/perspective/2021/investing-in-positive-change-with-our-us-pathways-to-progress-partners", "CitiGroup")
                      ),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("plot_type", "Select Plot:",
                                      choices = c("Unemployment", "Income"),
                                      selected = "Unemployment")  # Default selection
                        ),
                        mainPanel(
                          conditionalPanel(
                            condition = "input.plot_type === 'Unemployment'",
                            plotOutput("unemployment_bar_plot")
                          ),
                          conditionalPanel(
                            condition = "input.plot_type === 'Income'",
                            plotOutput("income_comparison_plot")
                          )
                        )
                      )
               )
             )
    ),
    tabPanel("Healthcare Access",
             fluidRow(
               column(12,
                      h2("Healthcare Access"),
                      p("This graph presents data on uninsured rates and the percentage of individuals avoiding care due to cost, categorized by racial groups. A higher rate of uninsured individuals may indicate a barrier to attaining care, leading to neglected medical care and increasing the risk of COVID-19.
                        A higher rate of individuals avoiding care due to cost, on the otherhand, might indicate a lack of detection or treatment of COVID-19. Resulting in potentially contributing to increased transmission and severity of the virus within these communities. Uninsured rate data for American Indian or Alaska Native, Pacific Islander or Native Hawaiian, and Multi-racial groups could not be found and have been omitted. "),
                      h3("Key Takeaways"),
                      p("Rates of uninsurance generally align with rates of avoiding cost due to care. Although there is an outlier with the Hispanic and Latino having a percentage of 4.3% of the population avoiding care due to cost despite the communities uninsured rate being nearly twice as much as everyone else. The rate of uninsurance still correlates with rates of avoiding care nonetheless as one example shows African Americans being the second most likely to avoid care, reflecting the populations lower income and being the second largest population to be uninsured. These rates of uninsurance also seem to align with income as well, indiciating that a lack of income could also mean a lack of ability to afford insurance."),
                      h3("Possible Solutions"),
                      tags$b("Universal Healthcare:"),
                      p("With 61% of Americans living paycheck to paycheck, many people are unable to afford proper medical treatment in a case of emergency. But by implementing universal healthcare, we can ensure that the needs of communities of color are met without leaving them in debt. We also help prevent health disparities in general as communities are able to receive routine check-ups and pre-screening appointments, stopping issues from occurring further down the line. Learn more here:",
                        tags$a("https://www.populardemocracy.org/news-and-publications/medicare-all-racial-justice-issue", "Popular Democracy")
                      ),
                      
                      tags$b("Public Health Centers:"),
                      p("By implementing public health centers that seek to serve local communities without searching for profit, we can ensure that those who are most marginalized by the pandemic are kept safe. These centers help provide care at an affordable cost, often being created to help target certain populations and serve as testing locations for areas that may be hard to traverse. Creating these centers will help improve community public health overall and reduce long-term risk not just from COVID-19, but other diseases as well. Learn more here:",
                        tags$a("https://publichealthinsider.com/2019/11/08/community-health-workers-increase-access-to-health-care-for-south-seattle-communities/", "Public Health Insider"),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("race1", "Select Race 1", choices = insurance_data$RacialGroup),
                          selectInput("race2", "Select Race 2", choices = insurance_data$RacialGroup),
                          radioButtons("plotType", "Choose Plot:",
                                       choices = c("Uninsured Rates Comparison", "Avoided Care Cost Percentage"),
                                       selected = "Uninsured Rates Comparison")
                        ),
                        mainPanel(
                          conditionalPanel(
                            condition = "input.plotType === 'Avoided Care Cost Percentage'",
                            plotlyOutput("avoidedCarePlot")
                          ),
                          conditionalPanel(
                            condition = "input.plotType === 'Uninsured Rates Comparison'",
                            plotlyOutput("comparisonPlot")
                          ),
                          
                         
                          )
                        )
                      )
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
                      p("In 2020, the first case of Covid-19 was documented in the U.S. in Washington state. Unbeknownst to the general public, this case would further mark the beginnings of a pandemic as the world was taken overhaul by the Coronavirus. Nonetheless, researchers quickly gathered to document data and determine possible responses to the pandemic at hand and discovered a finding crucial to understanding public health: Black, Hispanic, Latino, and Indigenous communities were most at risk of COVID-19. Our project aims to examine how three key factors—education, employment and income, and healthcare access—interconnect and impact these communities."),
                      h3("Project Core and Design"),
                      p(""),
                      tags$b("Project Framing: "),
                      p("We want our project to be a call to action to the ongoing disparities that these communities of color still face as a result of historical oppression. We also hope to illustrate the situation using community-resources and statistical data to highlight the factors that connect to these disparities."),
                      tags$b("Racial Groups: "),
                      p("As our project is centered upon race and ethnicity, we had collected data based on the U.S. Census but also included multi-racial and Hispanics and Latinos communities as well. We will be focusing on Black, Hispanic and Latino, and American Indian communities as we found those three to be the most impacted and backed up in evidence based on research papers and academia journals."),
                      tags$b("Education: "),
                      p("Chosen as 1 one of the 3 factors we wanted to focus on. Although education may not directly interact with health disparities, education however can indirectly influence one’s risk of Covid through impacting insurance rate and likelihood of being able to afford care."),
                      tags$b("Employment and Income: "),
                      p("Chosen as 1 one of the 3 factors we wanted to focus on. Employment and Income interacts with health disparities indirectly, influencing one’s likelihood of being insured and having the proper funds to afford care."),
                      tags$b("Healthcare Access: "),
                      p("Chosen as 1 one of the 3 factors we wanted to focus on. Healthcare Access interacts with health disparities directly through determining who gets access to treatment and who does not."),
                      tags$b("COVID-19 Impact: "),
                      p("Numbers are expected to be skewed slightly higher for Whites and Asians due to outliers appearing in the data and not enough documentation on African Americans, Latinos and Hispanics, and American Indians."),
                      tags$b("Data Collection and Wrangling: "),
                      p("All data was collected by public or government organizations likely to assess need and see if certain groups were more at risk of COVID-19. Data has been validated through it coming from a public institution and having been proof-read and curated by experts in the field. Data was found through government databases including the CDC and the U.S. Department of Health or through community organizations."),
                      h3("Research Questions: How did COVID-19 disproportionately impact African Americans, Latinos and Hispanics, and American Indians?"),
                      HTML(paste(
                        "<span style='font-size: small; font-weight: bold; font-style: italic;'>",
                        "Which community is most impacted by COVID-19? Why?<br><br>",
                        "How do these factors intersect and impact communities of color?<br><br>",
                        "What can possibly be done to address these health disparities?",
                        "</span>"
                      )),
                      p(" "),
                      p("Thus, creating a more equitable response and ensuring that the communities are getting the support they need. Although we may not necessarily be in the front lines and providing medical treatment, our data is still important nonetheless through assessing need and socio-economic factors. We hope that our research can be used to pivot towards proper culturally-sensitive training and equity-based healthcare treatment that involves proper knowledge of the communities that will be served. We also wish that our research is used to pave systemic change within communities through legislation and the creation of public health centers made to address the needs of communities of color. Finally, ensuring more is done to protect the rights and well-being of these communities by addressing the systemic issues that have perpetuated these disparities."),
                      p("This data set consists of 7 different rows, each corresponding with a race or ethnicity. We also feature 23 columns that go with our three factors: education, income and employment, healthcare access. Our columns also include rates of COVID-19 hospitalization, tests, and deaths."),
                      mainPanel(
                        plotlyOutput("deathsGraph"),
                        plotlyOutput("hospitalizationGraph"),
                        plotlyOutput("testsGraph"),
                      h3("Findings"),
                      p("Although data may show that Asians and Whites are more likely to be impacted by COVID-19, we found this to be quite the contrary due to how the data was sampled. For example, because of how the data was collected by state, this may negatively impact the data as some states may have a higher proportion of certain racial groups than others. It's also important to note the percentages were also calculated based on cases, indicating that the data is more based on understanding the severity of the disease among those diagnosed then how it might impact a general population. 
                        Thus, we expect there to be a slightly higher number for Black and African Americans, Hispanics and Latinos, and American Indians in the graphs shown as a result of under-sampling. Nonetheless, we found the higher numbers in these communities to correlate with the data found in the other factors as well. Although one's unemployment rate or the education level may not directly interact with one's health disparities, they still indirectly influence each other in different ways. As shown in the data, we can see the rates of being more likely to be uninsured influencing the higher amount of deaths by Covid-19 in Black communities. In conclusion, this demonstrates the multi-faceted complexity that comes with dealing with health disparities. These disparities don't simply occur just because, but because they are a result of decades of oppression against education, income and employment, and healthcare access for Black and African Americans, Hispanics and Latinos, and American Indians communities."),
                      h3("Discussion"),
                      p("When discussing our data, it's important to keep in mind the concept of 
                      'intersectionality.' Coined by Kimberlie Crenshaw, intersectionality is the idea of how different identities interact together and create impact. In context of our data, we had gathered four factors: race and ethnicity, education, income and employment, and healthcare access. Although seemingly separate from one another, these factors come together in either preventing certain communities from harm or making them more vulnerable in the process. An example could be that because some Black and African Americans are less likely to graduate from university, they also are more likely to have a lower income and be unemployed. Thus, resulting in them being unable to afford treatment when necessary and be more at risk of diseases. This concept of intersectionality was used as the framework of our project, as we also used it to help create possible solutions to address these issues. By implementing comprehensive ideas that took in account of the previous factors, we sought to address these factors directly at the root. For example, implementing policies that address educational disparities could also positively influence income levels and, subsequently, healthcare access for marginalized communities. Ultimately, revealing the complexity that comes with addressing these social issues and the just as complex solutions needed to address them at hand."),
                      h3("Conclusion"),
                      p("Though the pandemic may arguably be over, these issues heightened by the pandemic all remain present today and have not been addressed despite existing for decades. We hope to provide possible solutions to these issues, sharing ideas regarding what policies need to be established, what we as students can do, etc. The data we gathered can be utilized to train future health providers, educating them on how to better provide to different racial communities and address disparities more effectively. We understand that we may not be able to do all the work that is needed to undo these past centuries of racism. But through our story, we seek to shed light on these issues that impact those close to us and push towards systemic changes and initiatives towards the Black, Latine, and Indigenous communities most impacted, especially in light recent rulings made towards minoritized groups. So eventually, a foundation can exist where communities of color can have equitable access to resources and opportunities, regardless of their background."),
                      img(src = "who.jpg", height = "400px", width = "700px")
                      )
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
    ggplot(grad_df, aes(y = Racial_Groups)) +
      geom_col(aes(x = High_School_Grad_Percentage, fill = "High School Graduation"),
               position = "dodge", width = 0.4) +
      geom_col(aes(x = Bachelor_Degree_Percentage, fill = "Bachelor's Degree"),
               position = position_dodge(width = 0.5), width = 0.4) +
      labs(title = "High School Graduation and Bachelor's Degree Percentage by Race",
           y = "Racial Groups", x = "Percentage") +
      scale_x_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) + 
      scale_fill_manual(values = c("High School Graduation" = "skyblue", "Bachelor's Degree" = "hotpink"),
                        name = "Education Level") +
      theme_minimal()
  })
  output$comparisonPlot <- renderPlotly({
    race1_data <- insurance_data[insurance_data$RacialGroup == input$race1, ]
    race2_data <- insurance_data[insurance_data$RacialGroup == input$race2, ]
    
    plot <- plot_ly()
    
    plot <- add_trace(plot, x = ~paste0(input$race1, " (2020)"), y = ~race1_data$X2020_uninsured_rates, type = 'bar', name = paste0(input$race1, " (2020)"), marker = list(color = '#1f77b4'))
    plot <- add_trace(plot, x = ~paste0(input$race2, " (2020)"), y = ~race2_data$X2020_uninsured_rates, type = 'bar', name = paste0(input$race2, " (2020)"), marker = list(color = '#1f77b4'))
    plot <- add_trace(plot, x = ~paste0(input$race1, " (2021)"), y = ~race1_data$X2021_uninsured_rates, type = 'bar', name = paste0(input$race1, " (2021)"), marker = list(color = '#d62728'))
    plot <- add_trace(plot, x = ~paste0(input$race2, " (2021)"), y = ~race2_data$X2021_uninsured_rates, type = 'bar', name = paste0(input$race2, " (2021)"), marker = list(color = '#d62728'))
    
    plot <- layout(plot, 
                   title = "Uninsured Rates Comparison by Race",
                   xaxis = list(title = "Racial Groups and Year"),
                   yaxis = list(title = "Uninsured Rates per Year (%)"),
                   barmode = 'group'
    )
  })
  output$avoidedCarePlot <- renderPlotly({
    race1_data <- insurance_data[insurance_data$RacialGroup == input$race1, ]
    race2_data <- insurance_data[insurance_data$RacialGroup == input$race2, ]
    
    plot <- plot_ly()
    
    plot <- add_trace(plot, data = race1_data, x = ~RacialGroup, y = ~avoided_care_cost_percentage, type = 'bar', name = input$race1, marker = list(color = input$race1))
    plot <- add_trace(plot, data = race2_data, x = ~RacialGroup, y = ~avoided_care_cost_percentage, type = 'bar', name = input$race2, marker = list(color = input$race2))
    
    plot <- layout(plot, 
                   title = "Avoided Care Cost Percentage Comparison",
                   xaxis = list(title = "Racial Groups"),
                   yaxis = list(title = "Avoided Care Cost Percentage"),
                   barmode = 'group'
    )
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
  output$income_comparison_plot <- renderPlot({
    ggplot(bd_df, aes(x = Racial.Groups, y = median_income_us_2022_race_ethnicity_usd, fill = Racial.Groups)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7) +
      labs(title = "Median Income Comparison by Race",
           y = "Median U.S. Income in 2022",
           x = "Racial Group") +
      theme_minimal()
  })
  output$deathsGraph <- renderPlotly({
    plot_ly(Covid_impact_on_communities_df, x = ~Racial.Groups, y = ~Deaths_2020_percentage, type = 'bar', name = '2020') %>%
      add_trace(y = ~Deaths_2021_percentage, name = '2021') %>%
      layout(
        title = "Deaths Comparison by Racial Group",
        xaxis = list(title = ""),
        yaxis = list(title = "")
      )
  })
  
  output$hospitalizationGraph <- renderPlotly({
    plot_ly(Covid_impact_on_communities_df, x = ~Racial.Groups, y = ~Hospitalization_2020_percentage, type = 'bar', name = '2020') %>%
      add_trace(y = ~Hospitalization_2021_percentage, name = '2021') %>%
      layout(
        title = "Hospitalization Comparison by Racial Group",
        xaxis = list(title = ""),
        yaxis = list(title = "")
      )
  })
  
  output$testsGraph <- renderPlotly({
    plot_ly(Covid_impact_on_communities_df, x = ~Racial.Groups, y = ~Tests_2020_percentage, type = 'bar', name = '2020') %>%
      add_trace(y = ~Tests_2021_percentage, name = '2021') %>%
      layout(
        title = "Tests Comparison by Racial Group",
        xaxis = list(title = ""),
        yaxis = list(title = "")
      )
  })
}
shinyApp(ui = ui, server = server)