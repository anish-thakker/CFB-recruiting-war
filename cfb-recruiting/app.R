#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(readr)
library(dplyr)
library(shinyWidgets)
# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
    
    setBackgroundImage(
       src = "background.jpg"
    ),
    
    
    headerPanel(
        "College Football Recruiting War!",
    ),
   
  
    # Sidebar for inputs
    sidebarLayout(
        
        sidebarPanel(
            
          #Recruiting state input
            selectInput("state", "Recruiting State:",
                                    c("Alabama" = "AL",
                                      "Alaska" = "AK",
                                      "Arizona" = "AZ",
                                      "Arkansas" = "AK",
                                      "California" = "CA",
                                      "Colorado" = "CO",
                                      "Connecticut" = "CT",
                                      "Delaware" = "DE",
                                      "Distict of Columbia (DC)" = "DC",
                                      "Florida" = "FL",
                                      "Georgia" = "GA",
                                      "Hawaii" = "HI",
                                      "Idaho" = "ID",
                                      "Illinois" = "IL",
                                      "Indiana" = "IN",
                                      "Iowa" = "IA",
                                      "Kansas" = "KS",
                                      "Kentucky" = "KY",
                                      "Louisiana" = "LA",
                                      "Maine" = "ME",
                                      "Maryland" = "MD",
                                      "Massachusetts" = "MA",
                                      "Michigan" = "MI",
                                      "Minnesota" = "MN",
                                      "Mississippi" = "MS",
                                      "Missouri" = "MO",
                                      "Montana" = "MT",
                                      "Nebraska" = "NE",
                                      "Nevada" = "NV",
                                      "New Hampshire" = "NH",
                                      "New Jersey" = "NJ",
                                      "New Mexico" = "NM",
                                      "New York" = "NY",
                                      "North Carolina" = "NC",
                                      "North Dakota" = "ND",
                                      "Ohio" = "OH",
                                      "Oklahoma" = "OK",
                                      "Oregon" = "OR",
                                      "Pennsylvania" = "PA",
                                      "Rhode Island" = "RI",
                                      "South Carolina" = "SC",
                                      "South Dakota" = "SD",
                                      "Tennessee" = "TN",
                                      "Texas" = "TX",
                                      "Utah" = "UT",
                                      "Vermont" = "VT",
                                      "Virginia" = "VA",
                                      "Washington" = "WA",
                                      "West Virginia" = "WV",
                                      "Wisconsin" = "WI",
                                      "Wyoming" = "WY"
                                      )),
            
            #Star Rating Input
            selectInput("str", "Star Rating (according to 247Sports):",
                        c(
                          "All" = "All",
                          "5 stars only" = "five",
                          "4 stars and higher" = "four",
                          "3 stars and higher" = "three")),
            
            #Position Input
            selectInput("pos", "Position:",
                        c("All Positions" = "all", 
                          "Quarterbacks" = "QB",
                          "Running Backs" = "RB",
                          "Wide Receivers" = "WR",
                          "Tight Ends" = "TE",
                          "Offensive Lineman" = "OL",
                          "Defensive Lineman" = "DL",
                          "Linebackers" = "LB",
                          "Cornerbacks" = "CB",
                          "Safeties" = "S",
                          "Athletes(ATH)" = "ATH",
                          "Kickers" = "K",
                          "Punters" = "P")),
            
            #Startin Class Input
            selectInput("yr", "Starting Class:",
                        c("2020"=2020,
                          "2019"=2019,
                          "2018"=2018,
                          "2017"=2017,
                          "2016"=2016,
                          "2015"=2015,
                          "2014"=2014,
                          "2013"=2013,
                          "2012"=2012,
                          "2011"=2011,
                          "2010"=2010,
                          "2009"=2009,
                          "2008"=2008,
                          "2007"=2007,
                          "2006"=2006,
                          "2005"=2005,
                          "2004"=2004,
                          "2003"=2003,
                          "2002"=2002
                        )),
       
          #Ending Class Input
        selectInput("yr2", "Ending Class:",
                    c("2020"=2020,
                      "2019"=2019,
                      "2018"=2018,
                      "2017"=2017,
                      "2016"=2016,
                      "2015"=2015,
                      "2014"=2014,
                      "2013"=2013,
                      "2012"=2012,
                      "2011"=2011,
                      "2010"=2010,
                      "2009"=2009,
                      "2008"=2008,
                      "2007"=2007,
                      "2006"=2006,
                      "2005"=2005,
                      "2004"=2004,
                      "2003"=2003,
                      "2002"=2002
                      ))
    
        ),
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

# Define server logic
server <- shinyServer(function(input, output) {
  
  #Loads the csv file with data from all the years
    temp<-reactive({
        csv_add<-paste(input$yr,".csv",sep = "")
        str<-paste("data/", csv_add, sep="")
        read_csv(("data/allYears.csv")) %>%
            filter(stateProvince==input$state) %>% #Filters by input state
            filter(year>=input$yr & year <= input$yr2) #Filters by years greater than start year and less than end year
        
        
    })
    
    #Handles star rating filtering
    recruitsTemp<-reactive({
        if(input$str == "four"){
            temp() %>% filter(stars>=4 )
        }
        else if(input$str == "three"){
            temp() %>% filter(stars >= 3)
        }
      else if(input$str == "five"){
        temp() %>% filter(stars == 5)
      }
        else{
            temp()
        }
    })
    
    #Handled position filtering
    recruits<-reactive({
        if(input$pos == "QB"){
            recruitsTemp()%>%filter(position == "PRO" | position == "DUAL")
        }
        else if(input$pos == "RB"){
            recruitsTemp()%>%filter(position == "APB" | position == "RB" | position == "FB")
        }
        else if(input$pos == "WR"){
            recruitsTemp()%>%filter(position == "WR")
        }
        else if(input$pos == "TE"){
            recruitsTemp()%>%filter(position == "TE")
        }
        else if(input$pos == "OL"){
            recruitsTemp()%>%filter(position == "OT" | position == "OG" | position == "OC")
        }
        else if(input$pos == "DL"){
            recruitsTemp()%>%filter(position == "WDE" | position == "SDE" | position == "DT")
        }
        else if(input$pos == "LB"){
            recruitsTemp()%>%filter(position == "ILB" | position == "OLB")
        }
        else if(input$pos == "CB"){
            recruitsTemp()%>%filter(position == "CB")
        }
        else if(input$pos == "S"){
            recruitsTemp()%>%filter(position == "S")
        }
        else if(input$pos == "ATH"){
            recruitsTemp()%>%filter(position == "ATH")
        }
        else if(input$pos == "K"){
            recruitsTemp()%>%filter(position == "K")
        }
        else if(input$pos == "P"){
            recruitsTemp()%>%filter(position == "P")
        }
        else{
            recruitsTemp()
        }
    })
     
    #A vector containing the commmitted schools, excluding NA entries(recruits that didn't commit)
     vecFinal<-reactive({
         na.omit(recruits()$committedTo)
     })
     
     #Retrieves the name of the top 3 schools with the most commits for the query
    var<-reactive({
        names(sort(table(vecFinal()),decreasing=TRUE))[1]
    })
    
    var2<-reactive({
      names(sort(table(vecFinal()),decreasing=TRUE))[2]
    })
    
    var3<-reactive({
      names(sort(table(vecFinal()),decreasing=TRUE))[3]
    })
    
    #Retrievs the number of commits for the top 3 schools retrieved above
    varnum<-reactive({
        sort(table(vecFinal()),decreasing=TRUE)[1]
    })
    
    varnum2<-reactive({
      sort(table(vecFinal()),decreasing=TRUE)[2]
    })
    
    varnum3<-reactive({
      sort(table(vecFinal()),decreasing=TRUE)[3]
    })
    
    #Obtains the name of the best recruit from the best 3 recruiting schools in this query
    name1<-reactive({
        tempor<-recruits()%>%filter(committedTo == var())
        tempor[order(-tempor$rating),]
    })
    
    name2<-reactive({
      tempor<-recruits()%>%filter(committedTo == var2())
      tempor[order(-tempor$rating),]
    })
    
    name3<-reactive({
      tempor<-recruits()%>%filter(committedTo == var3())
      tempor[order(-tempor$rating),]
    })
    
    #Obtains the percentage of recruits committed to the top 3 schools in the specified category
    percent1<-reactive({
        round(varnum()/length(vecFinal())*100,digits = 2)
    })
    
    percent2<-reactive({
        round(varnum2()/length(vecFinal())*100,digits = 2)
    })
    
    percent3<-reactive({
        round(varnum3()/length(vecFinal())*100,digits = 2)
    })
    
    
    output$moreOutput = renderText({
      #Invalid input
        if(input$yr > input$yr2){
            paste("Starting Class must be less than or equal to Ending Class")
        }else{
        if(!is.na(varnum())){
            paste(var(), "-> ",percent1(),"% (",varnum(),"/",length(vecFinal()),")","---- Best Recruit from ",input$state, " in this category: ", name1()[1,4])
        }else{
            paste("No committed recruits in this category from the state of ", input$state)
        }
        }
        })
    
    output$moreOutput2 = renderText({
      #Invalid input
        if(input$yr > input$yr2){
            paste("Starting Class must be less than or equal to Ending Class")
        }else{
        if(!is.na(varnum2())){
            paste(var2(), "-> ",percent2(),"% (",varnum2(),"/",length(vecFinal()),")","---- Best Recruit from ",input$state, " in this category: ", name2()[1,4])
        }else{
          paste("No committed recruits in this category from the state of ", input$state)
        }}
    })
    output$moreOutput3 = renderText({
      #Invalid input
        if(input$yr > input$yr2){
            paste("Starting Class must be less than or equal to Ending Class")
        }else{
        if(!is.na(varnum3())){
            paste(var3(), "-> ",percent3(),"% (",varnum3(),"/",length(vecFinal()),")","---- Best Recruit from ",input$state, " in this category: ", name3()[1,4])
        }else{
          paste("No committed recruits in this category from the state of ", input$state)
        }}
    })
})

    

    
    

# Run the application 
shinyApp(ui = ui, server = server)
