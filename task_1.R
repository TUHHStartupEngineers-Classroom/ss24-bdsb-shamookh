---
  title: "01 Intro to the tidyverse"
author: "Bálint Szikszai"
date: "2021-04"
output:
  html_document:
  toc: true
toc_float: true
df_print: paged
collapsed: false
number_sections: true
toc_depth: 3
code_folding: hide
---
  
  ```{r setup, include=FALSE}
knitr::opts_chunk$set(message=FALSE,warning=FALSE, cache=TRUE)
```

# Tidyverse

Last compiled: `r Sys.Date()`

I learned the basics of the tidyverse library throughout the chapter. This includes basic data wrangling and data visualization.

# Challange Nr. 1

Using the bike_orderlines_wrangled_tbl and the knowledge, that I acquired during the exercise, the 'state' and 'city' columns were created, so that the data could be grouped by the state variable only. Sales by location were plotted on a bar plot to gain more insight in their relation.

## Sales by State

```{r}
## 1.0 Load libraries ----
# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library(tidyverse)
library(readxl)

# 2.0 Importing Files ----
bike_orderlines_wrangled_tbl <- read_rds("./01_tidyverse_files/bike_orderlines.rds")

# 3.0 Examining Data ----
#bike_orderlines_wrangled_tbl %>%
#  head(n=5)

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

plot_1 <- sales_by_state %>%
  ggplot(aes(x = state, y = sales)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_1
ggsave("./01_tidyverse_files/figure-html/Sales_by_state.png", height = 7 , width = 10)

```
Figure 1: Sales by State


According to Figure 1, North Rhine-Westphalia has the largest revenue.

# Challange Nr. 2

Usng the wrangled bike_orderlines_wrangled_tbl, the data frame was grouped by the state and year columns, then the sales in each state were plotted against the year (facet_wrap(\~state)), on a bar plot.

## Sales by State by Year

```{r}

# 5.2 Challange.2
sales_by_state_by_year <- bike_orderlines_wrangled_tbl %>%
  select(state, total_price, order_date) %>%
  mutate(year = format(order_date, format = "%Y")) %>%
  group_by(year) %>%
  summarize(sales = sum(total_price), state = state)

plot2 <- sales_by_state_by_year %>%
  ggplot(aes(x = year, y = sales, fill = state)) +
  facet_wrap(~state) +
  geom_col(fill = "#2DC6D6") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

plot2
ggsave("./01_tidyverse_files/figure-html/Sales_by_state_by_year.png", height = 7 , width = 10)

```
Figure 2: Sales by State by Year