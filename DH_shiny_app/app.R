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
rd$adj_pct_time_back=rd$percent_time_back+0.008
effects=read.csv("https://raw.githubusercontent.com/nathantomczyk/world_cup_downhill_data_science/main/Data/rider_home_race_effects.csv")
Encoding(effects$rider)<-"UTF-8"
effects2=data.frame('Rider'=effects$rider,"Effect_of_racing_at_home_on_probability_of_good_result"=round((effects$good_result_effect*100),0),
                    "Effect_of_racing_at_home_on_probability_of_bad_result"=round((effects$bad_result_effect*100),0))


rd$year=substring(rd$Date,nchar(rd$Date)-4,nchar(rd$Date))
bad_results=c('DNF','DNQ','DNS','DSQ','')


riders=sort(unique(rd$Name))


library("scales")
library("ggplot2")
reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv, 
            log_breaks(base = base), 
            domain = c(1e-100, Inf))
}

library(viridis)
library(shiny)
library(shinyWidgets)

# Define UI for application that draws a histogram
ui = fluidPage(
  titlePanel(
    h1("World Cup Race Results", align = "center")
  ),
  
  sidebarPanel(
    multiInput("Rider", label="choose riders:",
               riders,selected='Aaron GWIN'),
    p("Select riders to see their race results with the home races displayed in black on the graph to the right. Triangles on the left represent results where a rider did not qualify, did not finish, did not start, or was disqualified. The table below the graph populates with the estimated effects of the rider racing at home. These are interpreted as the effect of a rider racing at home on the probability of the rider having a good or bad result, with positive values indicating an increased probability. This is not estimated when a rider never competed at home and the table should display 0 in this case. Code and underlying data for this app can be found at: https://github.com/nathantomczyk/world_cup_downhill_data_science"),
  ),
  
  mainPanel(
    plotOutput("rider_graph",click = "plot_click"),
    tableOutput("rider_trends")
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$rider_graph <- renderPlot({ 
    selected_data=rd[rd$Name %in% input$Rider,]
    selected_data$DNF='no'
    selected_data[selected_data$Run.1 %in% bad_results,'adj_pct_time_back']=rnorm(nrow(selected_data[selected_data$Run.1 %in% bad_results,]),max(selected_data$adj_pct_time_back,na.rm=TRUE),4)
    selected_data[selected_data$Run.1 %in% bad_results,'DNF']='YES'
    
    home_races=selected_data[selected_data$home_race==1,]
    plot_data2=selected_data[selected_data$home_race==0,]
    
    ggplot(plot_data2,aes(x=adj_pct_time_back,y=Name,color=Name,shape=DNF))+
      theme_classic()+geom_jitter(width=0,size=3,alpha=0.6)+scale_x_continuous(trans=reverselog_trans(10))+geom_vline(xintercept=0.008)+
      xlab('Percent slower than fastest time')+ylab('')+theme(text=element_text(size=18))+
      geom_jitter(data=home_races[home_races$Name %in% input$Rider,],aes(x=adj_pct_time_back,y=Name),color='black',width=0,size=3)+
      theme(legend.position = "none")+annotate("text",x=max(selected_data$adj_pct_time_back,na.rm=TRUE)*0.7,y=0.7,label='DNF',size=10)+
      scale_color_viridis(discrete=TRUE)
  })
  output$rider_trends <-renderTable(effects2[effects2$Rider %in% input$Rider,])
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)