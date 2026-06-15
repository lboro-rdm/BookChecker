server <- function(input, output, session) {
  
  options(shiny.maxRequestSize = 30 * 1024^2)
  
  files_ready <- reactiveVal(FALSE)
  observeEvent(c(input$holdings, input$file), {
    if (!is.null(input$holdings) && !is.null(input$file)) {
      files_ready(TRUE)
    }
  })
  
  df_processed <- eventReactive(input$run, {
    req(input$holdings, input$file)
    
    # Read and clean holdings file
    df_main <- read_csv(input$holdings$datapath,
                        col_types = cols(.default = col_character())) %>%
      clean_names() %>%
      mutate(across(all_of(input$isbn_col),
                    ~ str_trim(str_replace_all(., "[-\\s]", "")))) %>%
      filter(!is.na(content_type) & content_type != "")
    
    skip_rows <- dplyr::case_when(
      input$source   == "JUSP"    ~ 15,
      input$platform == "ASCE" ~ 14,
      input$platform == "BibliU" ~ 14,
      input$platform == "Bloomsbury" ~ 14,
      input$platform == "Bloomsbury Fashion" ~ 14,
      input$platform == "EBC"      ~ 14,
      input$platform == "Springer" ~ 15,
      .default = 14
    )
    
    # Read and clean usage file (ISBN column is always "isbn" in COUNTER reports)
    df_accessed <- read_csv(input$file$datapath, skip = skip_rows) %>%
      clean_names() %>%
      mutate(
        isbn = str_replace_all(isbn, "[-\\s]", ""),
        isbn = str_trim(isbn)
      ) %>%
      pivot_wider(
        names_from = metric_type,
        values_from = reporting_period_total
      )
    
    isbn_lookup <- bind_rows(
      lapply(seq_along(input$isbn_col), function(i) {
        df_main %>%
          filter(!is.na(.data[[input$isbn_col[i]]]) & .data[[input$isbn_col[i]]] != "") %>%
          select(content_type, isbn = all_of(input$isbn_col[i])) %>%
          mutate(priority = i)
      })
    )
    
    accessed_with_type <- df_accessed %>%
      left_join(isbn_lookup, by = "isbn") %>%
      group_by(isbn) %>%
      slice_min(priority, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      mutate(accessed_content_type = recode(content_type,
                                            "p" = "Purchased",
                                            "s" = "Subscribed"))
    
    # Output list
    list(
      matched_counts = accessed_with_type %>%
        filter(!is.na(accessed_content_type)) %>%
        group_by(accessed_content_type) %>%
        summarise(
          row_count = n(),
          total_item_requests = sum(Total_Item_Requests, na.rm = TRUE),
          total_unique_title_requests = sum(Unique_Title_Requests, na.rm = TRUE),
          .groups = "drop"
        ),
      
      unmatched_accessed = accessed_with_type %>%
        filter(is.na(accessed_content_type)) %>%
        select(title, isbn, yop) %>%
        distinct(isbn, yop, .keep_all = TRUE)
    )
  })
  
  output$matched_table <- renderDT({
    req(df_processed())
    datatable(df_processed()$matched_counts)
  })
  
  output$unmatched_table <- renderDT({
    req(df_processed())
    datatable(df_processed()$unmatched_accessed)
  })
  
  observe({
    if (!is.null(df_processed()$unmatched_accessed)) {
      shinyjs::enable("download_unmatched")
    } else {
      shinyjs::disable("download_unmatched")
    }
  })
  
  output$download_unmatched <- downloadHandler(
    filename = function() {
      paste0("unmatched_books_", input$platform, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(df_processed()$unmatched_accessed, file, row.names = FALSE)
    }
  )
}