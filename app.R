#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)

Data3 <- read.csv('./Data_TP_PD_CO2_AFL_C_Inner.csv',
                  header=TRUE)

Data_Test <- Data3 %>% group_by(.,Region,IncomeGroup,Year) %>%  
  mutate(.,Agriculture_Land_Ratio=Agriculture.Land/Total.Land) %>% 
  summarize(.,Total_POP=sum(Population), average_POP=mean(Population),sd_POP=sd(Population),
            Total_CO2=sum(CO2Emission), average_CO2=mean(CO2Emission),sd_CO2=sd(CO2Emission),
            average_ALR=mean(Agriculture_Land_Ratio),average_FLR=mean(Forest.Area)) %>% 
  mutate(.,POPperMIN=((average_POP/min(average_POP)-1.0)*100),CO2perMIN=((average_CO2/min(average_CO2)-1)*100))

Data_Test2<-na.omit(Data3)

Data_Test3 <-  Data3 %>% group_by(.,Region,IncomeGroup,Year,Country.Name)  
#%>%  
#  mutate(.,Agriculture_Land_Ratio=(Agriculture.Land/Total.Land), Forest_Area=(Forest.Area*100*Total.Land)) %>% 
#  summarize(.,Total_POP=sum(Population), average_POP=mean(Population),
#            Total_CO2=sum(CO2Emission), average_CO2=mean(CO2Emission),
#            Total_AL=sum(Agriculture.Land),average_ALR=mean(Agriculture_Land_Ratio),
#            Total_FL=sum(Forest_Area),average_FLR=mean(Forest.Area)) %>% 
#  mutate(.,POPperMIN=((average_POP/min(average_POP)-1.0)*100),CO2perMIN=((average_CO2/min(average_CO2)-1)*100),
#         ALperMIN=((Total_AL/min(Total_AL)-1)*100), FRperMIN=((Total_FL/min(Total_FL)-1)*100))

# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidPage(
    titlePanel("Global Warming - CO2 Emission by Countries"),
    sidebarLayout(
      sidebarPanel(
        selectizeInput(inputId = "x1",
                       label = "Select Region",
                       choices = Data3$Region),
        uiOutput("Income.GR"),
        uiOutput("CON.Name"),
        uiOutput("Time.Year")),
      mainPanel(
       fluidRow(
          column(6, plotOutput("PopulationPlot")),
          column(6, plotOutput("avg_CO2Plot"))),
       fluidRow(
         column(6, plotOutput("AgricultureLandPercentage")),
         column(6, plotOutput("ForestLandPercentage"))),
       fluidRow(
         column(12, verbatimTextOutput("Nation")),
         column(12, verbatimTextOutput("Location")),
         column(12, verbatimTextOutput("Type")),
         column(12, verbatimTextOutput("World")))
      )
    )
  )
)
  
# Define server logic required to draw a histogram
server <- function(input, output) {

  output$Income.GR <- renderUI({
    if (length(input$x1)==0) return()
    else {
      Data6 <- Data3 %>%  filter(Region==input$x1)
      selectizeInput(inputId = "incomeGR", label = "Select Incrome Group", choices = Data6$IncomeGroup)    
    }
  })
  
  output$CON.Name <- renderUI({
    if (length(input$x1)==0 & length(input$incomeGR)==0) return()
    else {
    Data7 <- Data3 %>%  filter(Region==input$x1) %>% filter(IncomeGroup==input$incomeGR)    
    selectizeInput(inputId="conname", label="Select Country Name", choices = Data7$Country.Name)  }      
  })
  
  output$Time.Year <- renderUI({
    if (length(input$x1)==0 & length(input$incomeGR) & length(input$conname)) return() 
    else{
    Data8 <- Data3 %>% filter(Region==input$x1) %>% filter(IncomeGroup==input$incomeGR & Country.Name==input$conname) 
    selectizeInput(inputId="time", label='Select Year', choices = Data8$Year)}
  })
  selectedData2 <- reactive({
    Data8<-Data_Test3[Data_Test3$Region==input$x1,]
    Data9<-Data8[Data8$IncomeGroup==input$incomeGR,]
    Data10<-Data9[Data9$Country.Name==input$conname,]
  })
  selectedData3 <- reactive({
    Data_Test15 <- Data_Test3[Data_Test3$Region==input$time,] %>% 
      mutate(.,Agriculture_Land_Ratio=(Agriculture.Land/Total.Land)*100, Forest_Area=(Forest.Area*100*Total.Land)) %>% 
      summarize(.,TotalPopulation=formatC(sum(as.numeric(Population)),format='e',digit=4), 
                TotalCO2Emision=round(sum(CO2Emission),2),
                ALR=round(mean(Agriculture_Land_Ratio),2), FLR=round(mean(Forest.Area),2))
  })
  
 output$PopulationPlot <- renderPlot(
     ggplot(selectedData2(),aes(x=Year,y=CO2Emission))+geom_line() +
        ggtitle("CO2 Emission vs. Year") + 
       labs(x = "Year", y="CO2 Emission (metric tons per capita)") 
)
 
 output$avg_CO2Plot <- renderPlot(

   ggplot(selectedData2(),aes(x=Population,y=CO2Emission))+geom_point() + geom_smooth(method=lm) + 
     ggtitle("CO2 Emission vs. Total Population") + 
     labs(x = "Poluation (N)", y="CO2 Emission (metric tons per capita)") 
 )
 output$AgricultureLandPercentage <- renderPlot(
   ggplot(selectedData2(),aes(x=Agriculture.Land/Total.Land,y=CO2Emission))+geom_point() + geom_smooth(method=lm) +
     ggtitle("CO2 Emission vs. Agriculture Land Ratio") +
     labs(x = "ALR=Agriculture Land Ratio(%)", y="CO2 Emission (metric tons per capita)") 
 )
 output$ForestLandPercentage <- renderPlot(
   ggplot(selectedData2(),aes(x=Forest.Area,y=CO2Emission))+geom_point() + geom_smooth(method=lm) +
     ggtitle("CO2 Emission vs. Forest Land Ratio") +
     labs(x = "FLR=Forest Land Ratio(%)", y="CO2 Emission (metric tons per capita)") 
 )

 output$Nation <- renderPrint({
   Data_Test7 <- Data3 %>% filter(Country.Name==input$conname,Year==input$time) %>% 
     mutate(.,Agriculture_Land_Ratio=(Agriculture.Land/Total.Land)*100, Forest_Area=(Forest.Area*100*Total.Land)) %>% 
     summarize(.,"Population(N)"=formatC(sum(Population),format='e',digit=4), "CO2 Emission (mtpc)"=round(sum(CO2Emission),2),
               "ALR (%)"=round(mean(Agriculture_Land_Ratio),2), "FLR (%)"=round(mean(Forest.Area),2)) 
     Data_Test7})
 
 output$Location <- renderPrint({
     Data_Test6 <-  Data3 %>% filter(Region==input$x1,Year==input$time) %>% 
     mutate(.,Agriculture_Land_Ratio=(Agriculture.Land/Total.Land)*100, Forest_Area=(Forest.Area*100*Total.Land)) %>% 
       summarize(.,"Population(N)"=formatC(sum(Population),format='e',digit=4), "CO2 Emission (mtpc)"=round(sum(CO2Emission),2),
                 "ALR (%)"=round(mean(Agriculture_Land_Ratio),2), "FLR (%)"=round(mean(Forest.Area),2)) 
     Data_Test6})
 output$Type <- renderPrint({
   Data_Test6 <-  Data3 %>% filter(IncomeGroup==input$incomeGR,Year==input$time) %>% 
     mutate(.,Agriculture_Land_Ratio=(Agriculture.Land/Total.Land)*100, Forest_Area=(Forest.Area*100*Total.Land)) %>% 
     summarize(.,"Population(N)"=formatC(sum(Population),format='e',digit=4), 
               "CO2 Emission (mtpc)"=round(sum(CO2Emission),2),
               "ALR (%)"=round(mean(Agriculture_Land_Ratio),2), "FLR (%)"=round(mean(Forest.Area),2)) 
   Data_Test6})

 output$World <- renderPrint({
   Data_Test15 <-  Data3 %>% filter(Year==input$time) %>% 
     mutate(.,Agriculture_Land_Ratio=(Agriculture.Land/Total.Land)*100, Forest_Area=(Forest.Area*100*Total.Land)) %>% 
     summarize(.,"Population(N)"=formatC(sum(as.numeric(Population)),format='e',digit=4), 
               "CO2 Emiision (mtpc)"=round(sum(CO2Emission),2),
               "ALR (%)"=round(mean(Agriculture_Land_Ratio),2), "FLR (%)"=round(mean(Forest.Area),2))
   Data_Test15})

}

# Run the application 
shinyApp(ui = ui,server = server)
