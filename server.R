server <- function(input, output, session) {
  
  files_ready <- reactiveVal(FALSE)

  observeEvent(c(input$holdings, input$file), {
    if (!is.null(input$holdings) && !is.null(input$file)) {
      files_ready(TRUE)
    }
  })
  
  df_processed <- eventReactive(input$run, {
    req(input$holdings, input$file)

    isbn_col <- if (input$platform == "Springer") "e_isbn" else "isbn_e_isbn"
    
    # Read and clean holdings file
    df_main <- read_csv(input$holdings$datapath) %>%
      clean_names() %>%
      mutate(
        !!isbn_col := str_replace_all(.data[[isbn_col]], "[-\\s]", ""),
        !!isbn_col := str_trim(.data[[isbn_col]])
      ) %>%
      filter(!is.na(content_type) & content_type != "")
    
    skip_rows <- if (input$platform == "Springer") 15 else 13
    
    # Read and clean usage file
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
    
    accessed_with_type <- df_accessed %>%
      left_join(df_main %>% select(all_of(isbn_col), content_type),
                by = c("isbn" = isbn_col)) %>%
      rename(accessed_content_type = content_type) %>%
      mutate(accessed_content_type = recode(accessed_content_type,
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