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
    primary      = "#b299cc",
    secondary    = "#66B2B2",
    base_font    = font_google("Lato"),
    heading_font = font_google("Arimo"),
    font_scale   = 0.92
  ),
  
  p("Here is today's dad joke:"),
  paste(capture.output(dadjoke::dadjoke()), collapse = "\n"),
  p(),
  p(),
  
  tabsetPanel(
    
    # ── Main analysis tab ──────────────────────────────────────────────────────
    tabPanel(
      "Analysis",
      br(),
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "platform",
            label = "Step 1: select platform",
            choices = c("ACM", "ASCE", "BibliU", "Bloomsbury", "Bloomsbury Fashion",
                        "Brill", "Cambridge", "De Gruyter", "Drama Online",
                        "Duke Uni Press", "EBC", "EBSCO", "Elsevier", "Emerald",
                        "Gale", "Human Kinetics", "IEEE", "ICE", "IGI", "JSTOR",
                        "Kortext", "Oxford", "Project Muse", "RSC", "Sage",
                        "Springer", "T&F", "VLeBooks", "Wiley")
          ),
          fileInput("holdings", "Step 2: upload your holdings file", accept = ".csv"),
          selectizeInput(
            "isbn_col",
            "Step 3: What is the ISBN column called? (eg for eISBN, select e_isbn).",
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
          DTOutput("unmatched_table")
        )
      )
    ),
    
    # ── About tab ──────────────────────────────────────────────────────────────
    tabPanel(
      "About",
      br(),
      fluidRow(
        column(
          width = 8, offset = 1,
          
          h2("About this tool"),
          p("This tool compares a platform's monthly usage report against your library's
            holdings to identify which accessed books are owned and which are not. It
            supports both publisher-supplied COUNTER reports and JUSP-formatted files,
            and works across a wide range of e-book platforms."),
          
          hr(),
          
          h3("How it works"),
          tags$ol(
            tags$li("Select the platform your usage report comes from."),
            tags$li("Upload your ", tags$strong("holdings file"), " — a CSV export of your e-book holdings
                    for that platform, including at least one ISBN column."),
            tags$li("Tell the tool which column contains the ISBN(s). You can select up to
                    two columns if your holdings file contains both print and electronic ISBNs."),
            tags$li("Upload the ", tags$strong("monthly usage report"), " from the platform or JUSP."),
            tags$li("Select whether the report came directly from the publisher or via JUSP."),
            tags$li("Click ", tags$strong("Run Analysis"), ".")
          ),
          p("The tool will match accessed titles to your holdings by ISBN and display:"),
          tags$ul(
            tags$li(tags$strong("Matched content"), " — accessed titles found in your holdings, broken down by content type."),
            tags$li(tags$strong("Unmatched titles"), " — accessed titles not found in your holdings, which may indicate
                    demand for titles you do not own.")
          ),
          
          hr(),
          
          h3("Input file requirements"),
          tags$ul(
            tags$li("All files must be in ", tags$strong(".csv format"), "."),
            tags$li("Holdings files must have these columns: title, at least one but not more than two ISBN columns, and content type. The latter should be 's' for subscription, 'p' for purchased, 'eba' for evidence eased aquisition, and 'c' for complementary access."),
            tags$li("Usage reports should be standard COUNTER 5 Book Report (BR1/BR2) files
                    if coming from a publisher, or JUSP Title Report exports if using JUSP.")
            
          ),
          
          hr(),
          
          h3("Interpreting unmatched titles"),
          p("A title appearing in the unmatched table does not necessarily mean your library
            has no access to it. Common reasons for a failed match include:"),
          tags$ul(
            tags$li("The platform uses a different ISBN from the one in your holdings (e.g. print
                    ISBN in the usage report vs. electronic ISBN in holdings). Try selecting both
                    ISBN columns in Step 3 to improve match rates."),
            tags$li("The title was accessed under a consortial or trial agreement not reflected
                    in your local holdings file."),
            tags$li("The ISBN in the usage report is malformed or missing.")
          ),
          p("You can download the full unmatched list as a CSV for further investigation once
            the table has populated."),
          
          hr(),
          
          h3("Supported platforms"),
          p("The following platforms are currently supported:"),
          p(tags$em("ACM, ASCE, BibliU, Bloomsbury, Bloomsbury Fashion, Brill, Cambridge,
            De Gruyter, Drama Online, Duke University Press, EBC, EBSCO, Elsevier,
            Emerald, Gale, Human Kinetics, IEEE, ICE, IGI, JSTOR, Kortext, Oxford,
            Project MUSE, RSC, Sage, Springer, Taylor & Francis, VLeBooks, Wiley.")),
          p("If a platform you need is not listed, please contact the tool maintainer to
            request support."),
          
          hr(),
          
          h3("Data privacy"),
          p("All processing happens locally within your session. Usage reports and holdings files are not stored beyond the current session. This app does not make use of cookies."),
          
          hr(),
          

          h3("Attribution"),
          
          h4("App development"),
          p("This app was developed by Lara Skelly, with the input of Kerry O'Brien, for Loughborough University. The source code is publicly available on GitHub:"),
          tags$p(
            tags$a(
              href   = "https://github.com/lboro-rdm/BookChecker",
              target = "_blank",
              "https://github.com/lboro-rdm/BookChecker"
            )
          ),
          
          h4("R and open-source libraries"),
          p("This app is built in R and relies on the following open-source packages:"),
          tags$ul(
            tags$li(
              tags$a(href = "https://rstudio.github.io/bslib/", target = "_blank", "bslib"), " — ",
              "Sievert C, Cheng J, Aden-Buie G (2024). ",
              tags$em("bslib: Custom 'Bootstrap' 'Sass' Themes for 'shiny' and 'rmarkdown'."),
              " R package version 0.8.0, ",
              tags$a(href = "https://CRAN.R-project.org/package=bslib", target = "_blank",
                     "https://CRAN.R-project.org/package=bslib"), "."
            ),
            tags$li(
              tags$a(href = "https://github.com/jhollist/dadjoke", target = "_blank", "dadjoke"), " — ",
              "Priyam S (2020). ",
              tags$em("dadjoke: Displays a Dad Joke."),
              " R package version 1.0, ",
              tags$a(href = "https://CRAN.R-project.org/package=dadjoke", target = "_blank",
                     "https://CRAN.R-project.org/package=dadjoke"), "."
            ),
            tags$li(
              tags$a(href = "https://rstudio.github.io/DT/", target = "_blank", "DT"), " — ",
              "Xie Y, Cheng J, Tan X (2024). ",
              tags$em("DT: A Wrapper of the JavaScript Library 'DataTables'."),
              " R package version 0.33, ",
              tags$a(href = "https://CRAN.R-project.org/package=DT", target = "_blank",
                     "https://CRAN.R-project.org/package=DT"), "."
            ),
            tags$li(
              tags$a(href = "https://sfirke.github.io/janitor/", target = "_blank", "janitor"), " — ",
              "Firke S (2024). ",
              tags$em("janitor: Simple Tools for Examining and Cleaning Dirty Data."),
              " R package version 2.2.1, ",
              tags$a(href = "https://CRAN.R-project.org/package=janitor", target = "_blank",
                     "https://CRAN.R-project.org/package=janitor"), "."
            ),
            tags$li(
              tags$a(href = "https://shiny.posit.co/", target = "_blank", "shiny"), " — ",
              "Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Xie Y, Allen J, McPherson J,
    Dipert A, Borges B (2024). ",
              tags$em("shiny: Web Application Framework for R."),
              " R package version 1.9.1, ",
              tags$a(href = "https://CRAN.R-project.org/package=shiny", target = "_blank",
                     "https://CRAN.R-project.org/package=shiny"), "."
            ),
            tags$li(
              tags$a(href = "https://deanattali.com/shinyjs/", target = "_blank", "shinyjs"), " — ",
              "Attali D (2021). ",
              tags$em("shinyjs: Easily Improve the User Experience of Your Shiny Apps in Seconds."),
              " R package version 2.1.0, ",
              tags$a(href = "https://CRAN.R-project.org/package=shinyjs", target = "_blank",
                     "https://CRAN.R-project.org/package=shinyjs"), "."
            ),
            tags$li(
              tags$a(href = "https://www.tidyverse.org/", target = "_blank", "tidyverse"), " — ",
              "Wickham H, Averick M, Bryan J, Chang W, McGowan LD, François R, Grolemund G,
    Hayes A, Henry L, Hester J, Kuhn M, Pedersen TL, Miller E, Bache SM, Müller K,
    Ooms J, Robinson D, Seidel DP, Spinu V, Takahashi K, Vaughan D, Wilke C, Woo K,
    Yutani H (2019). \"Welcome to the tidyverse.\" ",
              tags$em("Journal of Open Source Software,"),
              tags$b(" 4"), "(43), 1686. doi:10.21105/joss.01686 ",
              tags$a(href = "https://doi.org/10.21105/joss.01686", target = "_blank",
                     "https://doi.org/10.21105/joss.01686"), "."
            )
          ),

          h3("Contact & feedback"),
          p("For bug reports, feature requests, or questions about the tool, please contact Lara <RDM at lboro.ac.uk>.")
        )
      )
    )
    
  ),
  tags$div(class = "footer", 
           fluidRow(
             column(12, 
                    tags$a(href = 'https://doi.org/10.17028/rd.lboro.28525481', 
                           "Accessibility Statement"),
                    style = "text-align: right;"
             )
           )
  )
)