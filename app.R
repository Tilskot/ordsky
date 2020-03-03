
library(shiny)
library(dplyr)
library(highcharter)
library(tidytext)

ord <- read.table("gruppert.csv",sep=";", header = TRUE, stringsAsFactors = FALSE)

ui <-fluidPage(
  
  titlePanel("Ordsky momskompensasjon"),
  
  sidebarPanel(
    selectInput("kat","Vel kategori:", c(Choose=NULL, c("Alle",unique(ord$kategori1))),selectize=TRUE),
    selectInput("vekt","Vekt:", c(Choose=NULL, c("Frekvens","Sqrt_tf_idf")),selectize=TRUE),
    sliderInput("slider", label = "Tal ord", min = 0,max = 500, value = 200)
  ),
                       
   # Viser orskya
  mainPanel(
    highchartOutput("ordsky", height = "600px"))
)

server <- function(input, output) {
  
output$ordsky <- renderHighchart({
  
    if (input$kat=="Alle"){
      klarKat <- ord
    } else {
      klarKat <- filter(ord,kategori1==input$kat)
    }
  
    if (input$vekt=="Frekvens"){
      klarKatVekt <- klarKat %>%
        mutate(vekt=Frekvens)
    } else {
      klarKatVekt <- klarKat %>%
        mutate(vekt=Sqrt_tf_idf)
    }
  
  tekst_ordsky <- klarKatVekt %>%
    top_n(input$slider,Frekvens) %>%
    rename(name=word,
           weight=vekt)

  highchart() %>%
    hc_chart(type = 'wordcloud') %>%
    hc_add_series(data=tekst_ordsky )
  })
  
 }

# Run the application 
shinyApp(ui = ui, server = server)