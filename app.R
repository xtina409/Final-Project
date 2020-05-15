#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyr)
library(ggplot2)
library(dplyr)
library(haven)


Pew_data <- read_sav("Pathways Mar 2020 (ATP W63.5).sav", user_na = TRUE) %>%
    as_factor()

source("projectdata.R")

ui <- fluidPage(
    # App title 
    titlePanel("Americans Perceptions of Covid-19 Response"),
    
    sidebarLayout(
        sidebarPanel(
            helpText("Party ID"),
            
            selectInput("F_PARTY_FINAL",
                        label = "Partyid",
                        choices = c("Democrat", "Republican", "Independent", "Something else")),
            selectInput("subgroup",
                        label = "Demographics",
                        choices = c("18-29", "30-49", "50-64", "65+", "Black non-Hispanic", "Hispanic", "Other", "White non-Hispanic")),
        ),
        mainPanel(
            plotOutput(outputId = "bar")
        )
 )
    

        )



# Define server logic required to draw a histogram
server <- function(input, output, session) { 
    
    filtered_data <- reactive({
        dplyr::filter(Trump_estimates, F_PARTY_FINAL == input$F_PARTY_FINAL, subgroup == input$subgroup)
    })
    
    
    output$bar <- renderPlot({
            ggplot(filtered_data(), aes(x = COVIDCONF_a_W63.5, y = weighted_estimate)) +
            geom_bar(aes(fill = NEWS_MOST_W57), stat = "identity")+
            facet_grid( cols = vars(input$F_PARTY_FINAL),
                        rows = vars(input$subgroup),
                        scales = "free_y",
                        space = "free")+
            labs(title = "Confidence by News Source",
                 y = "% estimates",
                 x = "Confidence Level")+
            theme_bw()
 
    
        

            
    })
    
    }   


    




shinyApp(ui = ui, server = server)
