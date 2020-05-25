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
    titlePanel("College Football Recruiting War!"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("state", "State:",
                                    c("MD" = "MD",
                                      "AL" = "AL",
                                      "CA" = "CA", "NC" = "NC")),
            selectInput("str", "Star Rating (according to 247Sports):",
                        c(
                          "4 stars and higher" = "four",
                          "3 stars and higher" = "three", "All" = "All")),
            selectInput("pos", "Position:",
                        c("All Positions" = "all", "WR" = "WR",
                          "RB" = "RB",
                          "DT" = "DT")),
            selectInput("yr", "Class:",
                        c("All Years(2002-2020):" = "allYears", "2002" = "2002",
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
           h3("#1 Recruiting School:"),
           textOutput("moreOutput"),
           h3("#2 Recruiting School:"),
           textOutput("moreOutput2"),
           h3("#3 Recruiting School:"),
           textOutput("moreOutput3")
        )
    )
)
)

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
    temp<-reactive({
        if(input$pos == "all"){
            csv_add<-paste(input$yr,".csv",sep = "")
            str<-paste("data/", csv_add, sep="")
            read_csv((str)) %>%
                filter(stateProvince==input$state)
        }else{
        csv_add<-paste(input$yr,".csv",sep = "")
        str<-paste("data/", csv_add, sep="")
        read_csv((str)) %>%
            filter(stateProvince==input$state) %>% 
            filter(position==input$pos) 
        }
        
        
    })
    
    recruits<-reactive({
        if(input$str == "four"){
            temp() %>% filter(stars>=4 )
        }
        else if(input$str == "three"){
            temp() %>% filter(stars >= 3)
        }
        else{
            temp()
        }
    })
     output$txtOutput = renderText({
        
        #paste("The area of the circle is: ", recruits()[1,4])
      
    })
    
     vect<-reactive({
         recruits()$committedTo
     })
     
     vecFinal<-reactive({
         na.omit(recruits()$committedTo)
     })
     
    var<-reactive({
        names(sort(table(vecFinal()),decreasing=TRUE))[1]
    })
    
    varnum<-reactive({
        sort(table(vecFinal()),decreasing=TRUE)[1]
    })
    
    name1<-reactive({
        tempor<-recruits()%>%filter(committedTo == var())
        tempor[order(-tempor$rating),]
    })
    
    percent1<-reactive({
        round(varnum()/length(vecFinal())*100,digits = 2)
    })
    
    var2<-reactive({
        names(sort(table(vecFinal()),decreasing=TRUE))[2]
    })
    
    varnum2<-reactive({
        sort(table(vecFinal()),decreasing=TRUE)[2]
    })
    
    name2<-reactive({
        tempor<-recruits()%>%filter(committedTo == var2())
        tempor[order(-tempor$rating),]
    })
    
    percent2<-reactive({
        round(varnum2()/length(vecFinal())*100,digits = 2)
    })
    
    var3<-reactive({
        names(sort(table(vecFinal()),decreasing=TRUE))[3]
    })
    
    varnum3<-reactive({
        sort(table(vecFinal()),decreasing=TRUE)[3]
    })
    
    name3<-reactive({
        tempor<-recruits()%>%filter(committedTo == var3())
        tempor[order(-tempor$rating),]
    })
    
    percent3<-reactive({
        round(varnum3()/length(vecFinal())*100,digits = 2)
    })
    
    output$moreOutput = renderText({
        if(!is.na(varnum())){
            paste(var(), "-> ",percent1(),"% (",varnum(),"/",length(vecFinal()),")","---- Best Recruit from category: ", name1()[1,4])
        }else{
            paste("No committed recruits in this category")
        }
        })
    
    output$moreOutput2 = renderText({
        if(!is.na(varnum2())){
            paste(var2(), "-> ",percent2(),"% (",varnum2(),"/",length(vecFinal()),")","---- Best Recruit from category: ", name2()[1,4])
        }else{
            paste("No committed recruits in this category")
        }
    })
    output$moreOutput3 = renderText({
        if(!is.na(varnum3())){
            paste(var3(), "-> ",percent3(),"% (",varnum3(),"/",length(vecFinal()),")","---- Best Recruit from category: ", name3()[1,4])
        }else{
            paste("No committed recruits in this category")
        }
    })
})

    

    
    

# Run the application 
shinyApp(ui = ui, server = server)
