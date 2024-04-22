# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library(tidyverse)
library(readxl)

# 2.0 Importing Files ----
bike_orderlines_wrangled_tbl <- read_rds("C:/Programming/R/data science basics/00_data/02_wrangled_data/bike_orderlines.rds")

# 3.0 Examining Data ----
bike_orderlines_wrangled_tbl %>%
  head(n=5)

# 4.0 Wrangling Data ----
bike_orderlines_wrangled_tbl <- bike_orderlines_wrangled_tbl %>%
  separate(col = location,
           into = c("city", "state"),
           sep = ", ")

# 5.0 Business insight ----
#5.1 Challange.1 ----
sales_by_state <- bike_orderlines_wrangled_tbl %>%
  select(state, total_price) %>%
  group_by(state) %>%
  summarize(sales = sum(total_price))
sales_by_state %>% head(n=5)

plot_1 <- sales_by_state %>%
  ggplot(aes(x = state, y = sales)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("C:/Programming/R/data science basics/00_data/02_wrangled_data/Sales_by_state.png", height = 7 , width = 10)


# 5.2 Challange.2
sales_by_state_by_year <- bike_orderlines_wrangled_tbl %>%
  select(state, total_price, order_date) %>%
  mutate(year = year(order_date)) %>%
  group_by(year) %>%
  summarize(sales = sum(total_price), state = state)
sales_by_state_by_year %>% head(n=5)

plot2 <- sales_by_state_by_year %>%
  ggplot(aes(x = year, y = sales, fill = state)) +
  facet_wrap(~state) +
  geom_col(fill = "#2DC6D6") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

ggsave("C:/Programming/R/data science basics/00_data/02_wrangled_data/Sales_by_state_by_year.png", height = 7 , width = 10)
  