library(shiny)
library(tidyverse)
library(janitor)
library(DT)
library(dadjoke)


ui <- fluidPage(
  titlePanel("Accessed Books Analysis"),
  p("Here is today's dad joke for today:"),
  paste(capture.output(dadjoke::dadjoke()), collapse = "\n"),
  p(),
  p(),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "platform",
        label = "Step 1: select platform",
        choices = c("T&F"),  # can expand later
        selected = "T&F"
      ),
      fileInput("holdings", "Step 2: upload your holdings file", accept = ".csv"),

      fileInput("file", "Step 3: upload the monthly usage report", accept = ".csv")
    ),
    
    mainPanel(
      h3("Matched Accessed Content Types"),
      DTOutput("matched_table"),
      
      h3("Unmatched Accessed Books"),
      DTOutput("unmatched_table")
    )
  )
)