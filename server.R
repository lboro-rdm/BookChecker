server <- function(input, output, session) {
  
  df_processed <- reactive({
    req(input$holdings, input$file)
    
    # Read and clean holdings file
    df_main <- read_csv(input$holdings$datapath) %>%
      clean_names() %>%
      mutate(
        isbn_e_isbn = str_replace_all(isbn_e_isbn, "[-\\s]", ""),
        isbn_e_isbn = str_trim(isbn_e_isbn)
      ) %>%
      filter(!is.na(content_type) & content_type != "")

    # Read and clean usage file
    df_accessed <- read_csv(input$file$datapath, skip = 13) %>%
      clean_names() %>%
      mutate(
        isbn = str_replace_all(isbn, "[-\\s]", ""),
        isbn = str_trim(isbn)
      ) %>%
      distinct(isbn, .keep_all = TRUE)  # 🧹 Deduplicate on ISBN
    
    
    accessed_with_type <- df_accessed %>%
      left_join(df_main %>% select(isbn_e_isbn, content_type),
                by = c("isbn" = "isbn_e_isbn")) %>%
      rename(accessed_content_type = content_type) %>%
      mutate(accessed_content_type = recode(accessed_content_type,
                                            "p" = "Purchased",
                                            "s" = "Subscribed"))

    # Output list
    list(
      matched_counts = accessed_with_type %>%
        filter(!is.na(accessed_content_type)) %>%
        count(accessed_content_type),
      
      unmatched_accessed = accessed_with_type %>%
        filter(is.na(accessed_content_type)) %>%
        select(title, isbn)
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
}
