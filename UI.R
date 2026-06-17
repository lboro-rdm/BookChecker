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
        choices = c("ACM", "ASCE", "BibliU", "Bloomsbury", "Bloomsbury Fashion", "Brill", "Cambridge", "De Gruyter", "Drama Online", "Duke Uni Press", "EBC", "Springer", "T&F")
      ),
      fileInput("holdings", "Step 2: upload your holdings file", accept = ".csv"),
      selectizeInput("isbn_col", "Step 3: What is the ISBN column called? (eg for eISBN, select e_isbn).",
                     choices  = c("e_isbn", "print_isbn", "isbn_e_isbn", "e_book_isbn", "isbn"),
                     selected = NULL,
                     multiple = TRUE,
                     options  = list(maxItems = 2)
      ),
      fileInput("file", "Step 4: upload the monthly usage report", accept = ".csv"),
      selectInput(
        inputId = "source",
        label = "Step 5: select source of usage report",
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