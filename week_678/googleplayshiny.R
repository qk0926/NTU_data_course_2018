#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)

ui <- fluidPage( navlistPanel(
  
  titlePanel("GooglePlay Apps Analysis"),
  
#  mainPanel(radioButtons("radio", label = h3("Radio buttons"),
#                         choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
#                         selected = 1)),
  tabPanel("Introduction", fluidRow(verbatimTextOutput("introtext"),
                                    tags$head(tags$style(HTML("#introtext {
                                                              font-size: 20px;
                                                              }"))))
           ,  mainPanel(DT::dataTableOutput("tablegoogle"))),

  tabPanel("App Types", fluidRow(verbatimTextOutput("typetext"),
                                 tags$head(tags$style(HTML("#typetext {
                                                           font-size: 20px;
                                                           }"))))
           , sliderInput("n","rankings:",
                                min = 1,
                                max = 25,
                                value = c(1,5)),
                    mainPanel(plotOutput("plota"))),
  tabPanel("Rating Distribution", fluidRow(verbatimTextOutput("ratingtext"),
                               tags$head(tags$style(HTML("#typetext {
                                                         font-size: 20px;
                                                         }"))))
           , selectInput("type","type",
                         choices=c("ALL"="ALL",levels(googleplay$Category)), selected = "All")
                         ,
         mainPanel(plotOutput("plotb")),
         fluidRow(textOutput("mean")))

))

server <- function(input, output) {
  library(ggplot2)
  library(dplyr)
  library(kableExtra)
  
  output$introtext <- renderText({
    string <- "This is a project that analyzes a dataset downloaded from Kaggle.com.\nIt shows the information of googleplaystore apps. Here's a part of the dataset."
    string
  })
  output$typetext <- renderText({
    string <- "The following plot shows the categories which have the most apps."
    string
  })
  output$ratingtext <- renderText({
    string <- "The following plot shows rating distribution of apps of different categories."
    string
  })
  

  output$tablegoogle = DT::renderDataTable({
    googleplay
  })
  
  output$plota <- renderPlot({
    ggplot(googleplay, aes(x = Category)) + scale_x_discrete(limits = names(sort(table(googleplay$Category), decreasing = TRUE)[input$n[1]:input$n[2]])) + layer(
      geom = "bar",  stat = "count",  position = "identity",
      params = list(
        fill = "steelblue",  na.rm = FALSE
      )
    )  + labs(title = "App Types") + theme(
      plot.background = element_rect(colour = "black",size = 3, linetype = 4, fill = "lightblue"), 
      plot.title = element_text(colour = "black", face = "bold", size = 30, vjust = 1, hjust = 0.5),
      plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "inches"),
      axis.text.x = element_text(angle = 90, family = "Calibri", hjust = 1, vjust = 0.5)
    ) 
    
  })
  output$plotb <- renderPlot({
    a <- c()
    if(input$type == "ALL"){
      a <- c(1:length(googleplay))
    }
    else{
      a <- which(googleplay$Category==input$type)
    }
    ggplot(googleplay[a,], aes(x = Rating))+ xlim(0, 5) + layer(
      geom = "density",  stat = "bin",  position = "identity",  params = list(
        fill = "steelblue",
        binwidth = 0.2,
        na.rm = FALSE
      )
    )  + labs(title = "Rating distribution") + theme(
      plot.background = element_rect(colour = "black",size = 3, linetype = 4, fill = "lightblue"), 
      plot.title = element_text(colour = "black", face = "bold", size = 30, vjust = 1, hjust = 0.5),
      plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "inches")
    )
    
  })
  output$mean <- renderText({
    a <- c()
    if(input$type == "ALL"){
      a <- c(1:length(googleplay))
    }
    else{
      a <- which(googleplay$Category==input$type)
    }
    paste("mean = ", mean(googleplay[a,"Rating"],na.rm = T))
    
  })
  output$plotc<- renderPlot({
    my.plot_c
  })
}



shinyApp(ui = ui, server = server)