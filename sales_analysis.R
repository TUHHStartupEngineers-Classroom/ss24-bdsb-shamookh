# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library(tidyverse)
library(readxl)

#systemfonts::register_font("Times New Roman",plain = "Times New Roman", bold = "Times New Roman")

# 2.0 Importing Files ----
bike_orderlines_wrangled_tbl <- read_rds("01_tidyverse_files/bike_orderlines.rds")

# 3.0 Examining Data ----
bike_orderlines_wrangled_tbl %>%
head(bike_orderlines_wrangled_tbl, n=7)


# 4.0 Wrangling Data ----
bike_orderlines_wrangled_tbl <- bike_orderlines_wrangled_tbl %>%
  separate(col = location,
           into = c("city", "state"),
           sep = ", ")

# 5.0 Business insight ----
#5.1 Challenge No. 1 - Sales by State ----

#Step 1 - Manipulate----
sales_by_state <- bike_orderlines_wrangled_tbl %>%
  select(state, total_price) %>%
  group_by(state) %>%
  summarize(sales = sum(total_price))
sales_by_state %>% head(n=10)

#Step 2 - Visulaize----
plot_1 <- sales_by_state %>%
  ggplot(aes(x = state, y = sales),)+
  geom_col(fill = "blue")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Sales By State")+
  labs(y = "Sales")+labs(x = "States")+
  theme(plot.title = element_text(size = 34))+
  theme(axis.title.x = element_text(size = 28))+
  theme(axis.title.y = element_text(size = 28))+
  theme(axis.text.y = element_text(size = 14))+
  theme(axis.text.x = element_text(size = 14))+
  theme(text = element_text(family = "Times New Roman"))+
  theme(axis.title = element_text(face="bold"))+
  theme(axis.text = element_text(face="bold"))+
  theme(plot.title = element_text(face="bold"))+
  theme(axis.title.x=element_text(colour="black"))+
  theme(axis.title.y=element_text(colour="black"))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(plot.title=element_text(hjust=0.5))
ggsave("content/01_journal/01_tidyverse_files/figure-html/sales_by_state.png", height = 7 , width = 12)


# 5.2 Challenge No. 2 - sales by location and year (facet_wrap) ----

#Step 1 - Manipulate----
sales_by_state_by_year <- bike_orderlines_wrangled_tbl %>%
  select(state, total_price, order_date) %>%
  mutate(year = year(order_date)) %>%
  group_by(year) %>%
  summarize(sales = sum(total_price), state = state)
sales_by_state_by_year %>% head(n=5)


#Step 2 - Visulaize----
plot2 <- sales_by_state_by_year %>%
  ggplot(aes(x = year, y = sales, fill = state)) +
  facet_wrap(~state) +
  geom_col(fill = "chocolate2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(title = "Sales By Location and Year")+
  labs(x = "Year")+
  labs(y = "Sales")+
  theme(plot.title = element_text(size = 34))+
  theme(axis.title.x = element_text(size = 28))+
  theme(axis.title.y = element_text(size = 28))+
  theme(axis.text.y = element_text(size = 14))+
  theme(axis.text.x = element_text(size = 14))+
  theme(text = element_text(family = "Times New Roman"))+
  theme(axis.title = element_text(face="bold"))+
  theme(axis.text = element_text(face="bold"))+
  theme(plot.title = element_text(face="bold"))+
  theme(axis.title.x=element_text(colour="black"))+
  theme(axis.title.y=element_text(colour="black"))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y=element_text(colour="black"))+
  theme(plot.title=element_text(hjust=0.5))+
  theme(strip.text = element_text(size = 14, color = "black"))+
  theme(strip.text = element_text(face = "bold"))
ggsave("content/01_journal/01_tidyverse_files/figure-html/Sales_by_State_und_Location.png", height = 8 , width = 15)


