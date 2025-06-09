server <- function(input, output) {
  
  df_processed <- reactive({
    req(input$file)
    
    df <- read_csv(input$file$datapath) %>%
      clean_names()
    
    # Split into main and accessed
    df_main <- df %>% filter(!is.na(content_type) & content_type != "") %>%
      mutate(isbn_e_isbn = str_trim(isbn_e_isbn))
    
    df_accessed <- df %>% filter(is.na(content_type) | content_type == "") %>%
      mutate(isbn_e_isbn = str_trim(isbn_e_isbn))
    
    # Join
    accessed_with_type <- df_accessed %>%
      left_join(df_main %>% select(isbn_e_isbn, content_type), by = "isbn_e_isbn") %>%
      rename(accessed_content_type = content_type.y)
    
    # Output list
    list(
      matched_counts = accessed_with_type %>%
        filter(!is.na(accessed_content_type)) %>%
        count(accessed_content_type),
      
      unmatched_accessed = accessed_with_type %>%
        filter(is.na(accessed_content_type)) %>%
        select(title, isbn_e_isbn)
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

