# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 01 Load libraries ----

library(vroom)
library(tidyverse)

# 02 Wrangling Data ----
col_types <- list(
  id = col_character(),
  date = col_date("%Y-%m-%d"),
  num_claims = col_double()
)

patent_tbl <- vroom(
  file       = "content/01_journal/Patent_data_reduced/patent.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

col_types <- list(
  id = col_character(),
  type = col_character(),
  organization = col_character()
)

assignee_tbl <- vroom(
  file       = "content/01_journal/Patent_data_reduced/assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

col_types <- list(
  patent_id = col_character(),
  assignee_id = col_character()
)

patent_assignee_tbl <- vroom(
  file       = "content/01_journal/Patent_data_reduced/patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

col_types <- list(
  patent_id = col_character(),
  mainclass_id = col_character(),
  sequence = col_integer()
)

uspc_tbl <- vroom(
  file       = "content/01_journal/Patent_data_reduced/uspc.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

US_top_10 <- patent_assignee_tbl %>%
  group_by(assignee_id) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  left_join(assignee_tbl, c("assignee_id" = "id"))

Challange_1 <- US_top_10$organization %>%
  head(n=50)
Challange_1





S_2014_05 <- patent_tbl %>%
  filter(date >= "2014-05-01" & date < "2014-06-01") %>%
  left_join(patent_assignee_tbl, c("id" = "patent_id")) %>%
  left_join(assignee_tbl, c("assignee_id" = "id")) %>%
  drop_na() %>%
  group_by(organization) %>%
  summarize(count =n()) %>%
  arrange(desc(count))

Challange_2 <-S_2014_05$organization %>%
  head(n=10)
Challange_2




world_top_10 <- uspc_tbl %>%
  inner_join(patent_assignee_tbl, c("patent_id" = "patent_id")) %>%
  inner_join(assignee_tbl, c("assignee_id"= "id")) %>%
  group_by(organization) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head(n=10)


Top_mainclasses <- uspc_tbl %>%
  inner_join(patent_assignee_tbl, c("patent_id" = "patent_id")) %>%
  inner_join(assignee_tbl, c("assignee_id"= "id")) %>%
  subset(organization %in% world_top_10$organization) %>%
  group_by(mainclass_id) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  head(n=5)
Challange_3 <- Top_mainclasses$mainclass_id %>%
  head(n=5)
Challange_3
