library(shiny)
library(shinyjs)
library(tidyverse)
library(janitor)
library(DT)
library(dadjoke)
library(bslib)


ui <- fluidPage(
  useShinyjs(),
  titlePanel("Accessed Books Analysis"),
  
  theme = bs_theme(
    version      = 5,
    bg           = "#0f1117",
    fg           = "#e8eaf0",
    primary      = "#004C4C",
    secondary    = "#66B2B2",
    base_font    = font_google("Lato"),
    heading_font = font_google("Arimo"),
    font_scale   = 0.92
  ),
  
  p("Here is today's dad joke:"),
  paste(capture.output(dadjoke::dadjoke()), collapse = "\n"),
  p(),
  p(),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "platform",
        label = "Step 1: select platform",
        choices = c("EBC", "Springer", "T&F"),
        selected = "EBC"
      ),
      fileInput("holdings", "Step 2: upload your holdings file", accept = ".csv"),
      
      fileInput("file", "Step 3: upload the monthly usage report", accept = ".csv"),
      selectInput(
        inputId = "source",
        label = "Step 4: select source of usage report",
        choices = c("Publisher", "JUSP"),
        selected = "JUSP"
      ),
      actionButton("run", "Run Analysis"),
      p(),
      p("Wait for the unmatched table to populate before downloading. You have been warned."),
      downloadButton("download_unmatched", "Download Unmatched Books (.csv)",
                     disabled = "disabled")
      
    ),
    
    mainPanel(
      h3("Matched Accessed Content Types"),
      DTOutput("matched_table"),
      
      h3("Unmatched Accessed Books"),
      DTOutput("unmatched_table"),
      
      
      
    )
  )
)