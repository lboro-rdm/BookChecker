library(tidyverse)
library(janitor)

# 1. Load data and clean column names
df <- read_csv("tfholdings.csv") %>%
  clean_names()

# 2. Split into main list and accessed list
df_main <- df %>% filter(!is.na(content_type) & content_type != "")
df_accessed <- df %>% filter(is.na(content_type) | content_type == "")

# 3. Clean ISBN fields
df_main <- df_main %>% mutate(isbn_e_isbn = str_trim(isbn_e_isbn))
df_accessed <- df_accessed %>% mutate(isbn_e_isbn = str_trim(isbn_e_isbn))

# 4. Match accessed books to main list
accessed_with_type <- df_accessed %>%
  left_join(df_main %>% select(isbn_e_isbn, content_type), by = "isbn_e_isbn") %>%
  rename(accessed_content_type = content_type.y)

# Now count only on the matched accessed books
accessed_with_type %>%
  filter(!is.na(accessed_content_type)) %>%
  count(accessed_content_type)

# Find accessed books that did not match to the main list
unmatched_accessed <- accessed_with_type %>%
  filter(is.na(accessed_content_type)) %>%
  select(title, isbn_e_isbn)

# Print the table
unmatched_accessed %>% print(n = Inf)

