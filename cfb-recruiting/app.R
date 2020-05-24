#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readr)
library(rvest)
library(tidyverse)
library(data.table)
library(DT)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("state", "State:",
                                    c("MD" = "MD",
                                      "AL" = "AL",
                                      "CA" = "CA")),
            selectInput("pos", "Position:",
                        c("WR" = "WR",
                          "RB" = "RB",
                          "DT" = "DT")),
            selectInput("yr", "Class:",
                        c("2002" = "2002",
                          "2003" = "2003",
                          "2004" = "2004",
                          "2005" = "2005",
                          "2006" = "2006",
                          "2007" = "2007",
                          "2008" = "2008",
                          "2009" = "2009",
                          "2010" = "2010",
                          "2011" = "2011",
                          "2012" = "2012",
                          "2013" = "2013",
                          "2014" = "2014",
                          "2015" = "2015",
                          "2016" = "2016",
                          "2017" = "2017",
                          "2018" = "2018",
                          "2019" = "2019",
                          "2020" = "2020"))
        ),
    

        # Show a plot of the generated distribution
        mainPanel(
            #DT::dataTableOutput("recruits")
           textOutput("txtOutput")
        )
    )
)
)

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
    output$txtOutput = renderText({
        recruits<-reactive({
            csv_add<-paste(input$yr,".csv",sep = "")
            str<-paste("data/", csv_add, sep="")
            read_csv((str)) %>%
                filter(stateProvince==input$state) %>% 
                filter(position==input$pos)
            
        })
        paste("The area of the circle is: ", recruits()[1,4])
    })
    
})

# Run the application 
shinyApp(ui = ui, server = server)
