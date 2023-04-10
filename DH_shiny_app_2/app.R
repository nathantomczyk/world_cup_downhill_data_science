#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
options(scipen = 999)
library(rsconnect)

rd=read.csv("https://raw.githubusercontent.com/nathantomczyk/world_cup_downhill_data_science/main/Data/cleaned_downhill_data.csv")
Encoding(rd$Name)<-"UTF-8"
rd$year=as.numeric(substring(rd$Date,nchar(rd$Date)-3,nchar(rd$Date)))
rd$adj_pct_time_back=rd$percent_time_back+0.008
effects=read.csv("https://raw.githubusercontent.com/nathantomczyk/world_cup_downhill_data_science/main/Data/rider_home_race_effects.csv")
Encoding(effects$rider)<-"UTF-8"
effects$good_result_effect=round(effects$good_result_effect*100,0)
effects$bad_result_effect=round(effects$bad_result_effect*100,0)

rider_country_map=unique(data.frame(rider=rd$Name,country=rd$rider_country_new))
effects2=merge(effects,rider_country_map,by='rider')
effects2=effects2[effects2$country!="",]



bad_results=c('DNF','DNQ','DNS','DSQ','')
plot_data=subset(rd, !(rd$Run.1 %in% bad_results))
home_races=plot_data[plot_data$home_race==1,]
plot_data2=plot_data[plot_data$home_race==0,]

years=sort(as.numeric(unique(rd$year)))
countries=sort(unique(effects2$country))

library("scales")
library("ggplot2")

library(viridis)
library(shiny)
library(shinyWidgets)

# Define UI for application that draws a histogram
ui = fluidPage(
  titlePanel(
    h1("Effect of racing at home on World Cup riders", align = "center")
  ),
  
  sidebarPanel(
    multiInput("Year", label="choose years:",
               years,selected=2022),
    multiInput("Country", label="choose countries:",
               countries,selected=countries),
    
    
  ),
  
  mainPanel(
    plotOutput("rider_graph",click = "plot_click"),
    verbatimTextOutput("info"),
    p("By default riders from every country in the year 2022 are selected, use the menus at the left to add riders that competed in other years or limit the graph to only have riders of certain nationalities. If you click points on the graph you can see the name of the rider and the values displayed above this paragraph. The graph is broken up into four quadrants. The riders on the bottom right have a higher probability of good results when racing in their home country and a lower probability of bad results (‘better at home’). Riders in the top left have a lower probability of good results and a higher probability of bad results (‘Worse at home’). Riders in the top right have a higher probability of both good and bad results (‘Checkers or wreckers’). And riders in the bottom left have a lower probability of both good and bad results at home, which implies there results are average (‘Average at home’). Note that the data from each rider models their results from their whole career regardless of the years selected, the year menu just filters for riders who raced in a given year. The code and data that underlie this app can be found at: https://github.com/nathantomczyk/world_cup_downhill_data_science"),
    
    
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
    output$rider_graph <- renderPlot({ 
    riders_to_select=unique(rd[rd$year %in% as.numeric(input$Year) & rd$rider_country_new %in% input$Country, 'Name'])
    plotting_data=effects2[effects2$rider %in% riders_to_select,]
    ggplot(plotting_data,aes(x=good_result_effect,y=bad_result_effect,color=country))+np.random.normal(loc=5, scale=2, size=200)
      theme_classic()+geom_vline(xintercept = 0,linetype='dashed')+
      geom_hline(yintercept=0,linetype='dashed')+geom_point(size=4)+
      xlab('Increase in probability of good result at home')+ylab('Increase in probability of bad result at home')+theme(text=element_text(size=18))+
      annotate('text',x=0.85*max(plotting_data$good_result_effect),y=0.85*max(plotting_data$bad_result_effect),label="Checkers 
  or Wreckers",size=6)+
      annotate('text',x=0.85*max(plotting_data$good_result_effect),y=0.85*min(plotting_data$bad_result_effect),label="Better 
  at Home",size=6)+
      annotate('text',x=0.85*min(plotting_data$good_result_effect),y=0.85*max(plotting_data$bad_result_effect),label="Worse 
  at Home",size=6)+
      annotate('text',x=0.85*min(plotting_data$good_result_effect),y=0.85*min(plotting_data$bad_result_effect),label="Average 
  at Home",size=6)+
      scale_color_viridis(discrete=TRUE)
      
  })
    
    output$info <- renderPrint({
      nearPoints(effects2, input$plot_click, allRows = FALSE)
    })

  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
