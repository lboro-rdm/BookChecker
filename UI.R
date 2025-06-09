library(shiny)
library(tidyverse)
library(janitor)
library(DT)

ui <- fluidPage(
  titlePanel("Accessed Books Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV", accept = ".csv")
    ),
    
    mainPanel(
      h3("Matched Accessed Content Types"),
      DTOutput("matched_table"),
      
      h3("Unmatched Accessed Books"),
      DTOutput("unmatched_table")
    )
  )
)