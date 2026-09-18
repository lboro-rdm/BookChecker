server <- function(input, output, session) {
  
  options(shiny.maxRequestSize = 30 * 1024^2)
  
  files_ready <- reactiveVal(FALSE)
  observeEvent(c(input$holdings, input$file), {
    if (!is.null(input$holdings) && !is.null(input$file)) {
      files_ready(TRUE)
    }
  })
  
  # Preview holdings column headings as soon as the file is picked —
  # n_max = 0 reads only the header row, so this is instant even on a big file
  holdings_headers <- eventReactive(input$holdings, {
    req(input$holdings)
    tryCatch(
      read_csv(input$holdings$datapath, n_max = 0, col_types = cols(.default = col_character())) %>%
        clean_names() %>%
        names(),
      error = function(e) {
        showNotification(
          paste("Could not read holdings file headers:", conditionMessage(e)),
          type = "error", duration = NULL
        )
        NULL
      }
    )
  })
  
  output$holdings_preview <- renderTable({
    req(holdings_headers())
    data.frame(column = holdings_headers())
  })
  
  df_processed <- eventReactive(input$run, {
    req(input$holdings, input$file)
    
    # Read and clean holdings file
    df_main <- tryCatch(
      read_csv(input$holdings$datapath, col_types = cols(.default = col_character())) %>%
        clean_names() %>%
        mutate(across(all_of(input$isbn_col),
                      ~ str_trim(str_replace_all(., "[-\\s]", "")))) %>%
        filter(!is.na(content_type) & content_type != ""),
      error = function(e) {
        showNotification(
          paste("Error reading holdings file:", conditionMessage(e)),
          type = "error", duration = NULL
        )
        NULL
      }
    )
    req(!is.null(df_main), cancelOutput = TRUE)
    
    skip_rows <- dplyr::case_when(
      input$source   == "JUSP"    ~ 15,
      input$platform == "ASCE" ~ 14,
      input$platform == "BibliU" ~ 14,
      input$platform == "Bloomsbury" ~ 14,
      input$platform == "Bloomsbury Fashion" ~ 14,
      input$platform == "Brill" ~ 14,
      input$platform == "Cambridge" ~ 14,
      input$platform == "De Gruyter" ~ 14,
      input$platform == "Drama Online" ~ 14,
      input$platform == "Duke Uni Press" ~ 14,
      input$platform == "EBC"      ~ 14,
      input$platform == "EBSCO"      ~ 14,
      input$platform == "Gale"      ~ 14,
      input$platform == "Human Kinetics"      ~ 14,
      input$platform == "IGI"      ~ 14,
      input$platform == "Kortext"      ~ 14,
      input$platform == "Oxford"      ~ 14,
      input$platform == "Project Muse"      ~ 13,
      input$platform == "RSC"      ~ 14,
      input$platform == "Springer" ~ 15,
      input$platform == "VLeBooks"      ~ 14,
      .default = 14
    )
    
    # Read and clean usage file (kept in long format — one row per isbn/metric_type)
    df_accessed <- tryCatch(
      read_csv(input$file$datapath, skip = skip_rows) %>%
        clean_names() %>%
        mutate(
          isbn = str_replace_all(isbn, "[-\\s]", ""),
          isbn = str_trim(isbn)
        ),
      error = function(e) {
        showNotification(
          paste("Error reading usage file — check platform/source selection is correct:", conditionMessage(e)),
          type = "error", duration = NULL
        )
        NULL
      }
    )
    req(!is.null(df_accessed), cancelOutput = TRUE)
    
    # Validate that expected columns exist after clean_names()
    validate(
      need("isbn" %in% names(df_accessed),
           "Usage file has no 'isbn' column after parsing — check the platform/source selection and that the file is a valid COUNTER report."),
      need("metric_type" %in% names(df_accessed),
           "Usage file has no 'metric_type' column after parsing — check the platform/source selection and that the file is a valid COUNTER report."),
      need(any(input$isbn_col %in% names(df_main)),
           paste("Holdings file does not contain the selected ISBN column(s):", paste(input$isbn_col, collapse = ", ")))
    )
    
    isbn_lookup <- bind_rows(
      lapply(seq_along(input$isbn_col), function(i) {
        df_main %>%
          filter(!is.na(.data[[input$isbn_col[i]]]) & .data[[input$isbn_col[i]]] != "") %>%
          select(content_type, isbn = all_of(input$isbn_col[i])) %>%
          mutate(priority = i)
      })
    ) %>%
      group_by(isbn) %>%
      slice_min(priority, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      distinct(isbn, .keep_all = TRUE)
    
    # Join every usage row (all metric types) to its content_type
    df_accessed_typed <- df_accessed %>%
      left_join(isbn_lookup %>% select(isbn, content_type), by = "isbn") %>%
      mutate(accessed_content_type = recode(content_type,
                                            "p" = "Purchased",
                                            "s" = "Subscribed",
                                            "eba" = "Evidence Based Aquisition",
                                            "c" = "Complementary access"),
             accessed_content_type = if_else(
               is.na(accessed_content_type) | accessed_content_type == "",
               "Unmatched",
               accessed_content_type
             ))
    
    # Warn if nothing matched at all — likely an ISBN format mismatch
    if (all(df_accessed_typed$accessed_content_type == "Unmatched")) {
      showNotification(
        "No ISBNs from the usage file matched the holdings file. Check the ISBN zeros. Down with Excel!",
        type = "warning", duration = NULL
      )
    }
    
    list(
      matched_counts = df_accessed_typed %>%
        group_by(accessed_content_type) %>%
        summarise(
          unique_title_requests     = sum(metric_type == "Unique_Title_Requests"),
          sum_unique_title_requests = sum(reporting_period_total[metric_type == "Unique_Title_Requests"], na.rm = TRUE),
          sum_total_item_requests   = sum(reporting_period_total[metric_type == "Total_Item_Requests"], na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(accessed_content_type == "Unmatched", accessed_content_type),
      
      unmatched_accessed = df_accessed_typed %>%
        filter(accessed_content_type == "Unmatched",
               metric_type %in% c("Unique_Title_Requests", "Total_Item_Requests")) %>%
        select(title, isbn, yop, doi, metric_type, reporting_period_total) %>%
        distinct(title, isbn, yop, doi, metric_type, .keep_all = TRUE) %>%
        pivot_wider(
          names_from  = metric_type,
          values_from = reporting_period_total,
          values_fn   = ~ sum(.x, na.rm = TRUE)
        ) %>%
        mutate(
          doi = if_else(!is.na(doi) & doi != "", paste0("https://doi.org/", doi), doi)
        ) %>%
        select(title, isbn, yop, doi, any_of(c("Unique_Title_Requests", "Total_Item_Requests")))
    )
  })
  
  output$matched_table <- renderDT({
    req(df_processed())
    datatable(df_processed()$matched_counts)
  })
  
  output$unmatched_table <- renderDT({
    req(df_processed())
    display_df <- df_processed()$unmatched_accessed %>%
      mutate(
        doi = if_else(
          !is.na(doi) & doi != "",
          paste0('<a href="', doi, '" target="_blank" rel="noopener noreferrer">', doi, '</a>'),
          doi
        )
      )
    datatable(display_df, escape = FALSE)
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