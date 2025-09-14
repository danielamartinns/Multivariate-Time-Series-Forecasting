#Data Import and Data Understanding


library(dplyr)
library(lubridate)
library(readxl)
library(ggplot2)
library(tidyverse)

#Working directory

#Set working directory in local computer to import files

#Import data

spain_sales <- read_excel("sales_cannibalization_final.xlsx", sheet = "MASTER COR")
spain_macro_factors <- read_excel("spain_macro_factors.xlsx")
spain_competitor <- read_excel("spain_competitor.xlsx")
spain_category_interest <- read_excel("spain_category_interest.xlsx")
spain_invis_searches <- read_excel("spain_invis_searches.xlsx")
spain_nps <- read_excel("spain_nps.xlsx")

#Data understanding and pre-processing

#Spain Sales
#View(spain_sales)

#spain_sales$Month <- as.Date(spain_sales$Month)

lapply(spain_macro_factors, class)

# Unpivot data

spain_sales_format <- spain_sales %>% pivot_longer(-c(Month)) 

spain_sales_format %>%
  ggplot()+
  geom_line(aes(y = value, x = Month, color = name))+
  theme_minimal() +
  ggtitle("Sales Evolution per Month - Product A (Original), Product B (New) and Total Sales (Prod. A + B)") + 
  xlab("Month") + ylab("Sales") +
  theme_bw() + theme(legend.position = "bottom") + 
  theme(plot.title = element_text(hjust = 0.5))

##External Factors

#Macroeconomic Factor
#View(spain_macro_factors)

lapply(spain_macro_factors, class)

spain_macro_factors$Month <- as.Date(spain_macro_factors$Month)

# Unpivot data

spain_macro_factors_index <- spain_macro_factors[c(1, 2, 4, 6, 8)]

spain_macro_factors_annual_change <- spain_macro_factors[c(1, 3, 5, 7, 9)]
  
spain_macro_factors_index_format <- spain_macro_factors_index %>% pivot_longer(-c(Month)) 

spain_macro_factors_index_format %>%
  ggplot()+
  geom_line(aes(y = value, x = Month, color = name))+
  theme_minimal() +
  ggtitle("Harmonised Index of Consumer Prices - All Items, Health, Recreation & Restaurants/Hotels") + 
  xlab("Month") + ylab("HICP") +
  theme_bw() + theme(legend.position = "bottom") + 
  theme(plot.title = element_text(hjust = 0.5))

spain_macro_factors_annual_change_format <- spain_macro_factors_annual_change %>% pivot_longer(-c(Month)) 

spain_macro_factors_annual_change_format %>%
  ggplot()+
  geom_line(aes(y = value, x = Month, color = name))+
  theme_minimal() +
  ggtitle("Harmonised Index of Consumer Prices - All Items, Health, Recreation & Restaurants/Hotels (Annual change of rate)") + 
  xlab("Month") + ylab("HICP") +
  theme_bw() + theme(legend.position = "bottom") + 
  theme(plot.title = element_text(hjust = 0.5))

#Health HICP / All items

spain_macro_factors$hicp_health_per_all_items <- spain_macro_factors$hicp_health / spain_macro_factors$hicp_all_items

spain_macro_factors$hicp_health_per_recreation <- spain_macro_factors$hicp_health / spain_macro_factors$hicp_recreation

spain_macro_factors$hicp_health_per_restaurants <- spain_macro_factors$hicp_health / spain_macro_factors$hicp_restaurants

spain_macro_factors %>%
  ggplot()+
  geom_line(aes(y = hicp_health_per_all_items, x = Month))+
  theme_minimal() +
  ggtitle("Harmonised Index of Consumer Prices - Health vs. All Items Ratio") + 
  xlab("Month") + ylab("HICP") +
  theme_bw() + theme(legend.position = "bottom") + 
  theme(plot.title = element_text(hjust = 0.5))

spain_macro_factors %>%
  ggplot()+
  geom_line(aes(y = hicp_health_per_recreation, x = Month))+
  theme_minimal() +
  ggtitle("Harmonised Index of Consumer Prices - Health vs. All Items Ratio") + 
  xlab("Month") + ylab("HICP") +
  theme_bw() + theme(legend.position = "bottom") + 
  theme(plot.title = element_text(hjust = 0.5))

spain_macro_factors %>%
  ggplot()+
  geom_line(aes(y = hicp_health_per_restaurants, x = Month))+
  theme_minimal() +
  ggtitle("Harmonised Index of Consumer Prices - Health vs. All Items Ratio") + 
  xlab("Month") + ylab("HICP") +
  theme_bw() + theme(legend.position = "bottom") + 
  theme(plot.title = element_text(hjust = 0.5))

#Competitor Searches
#View(spain_competitor)
lapply(spain_competitor, class)

spain_competitor$Month <- as.Date(spain_competitor$Month)

spain_competitor %>%
  ggplot()+
  geom_line(aes(y = `dr smile + straumann + angel aligner: (Spain)`, x = Month))+
  theme_minimal() +
  ggtitle("Competitor Searches per Month") + 
  xlab("Month") + ylab("Competitor Searches") +
  theme_bw() + theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5))

#Invisalign Searches
#View(spain_invis_searches)
lapply(spain_invis_searches, class)

spain_invis_searches$Month <- as.Date(spain_invis_searches$Month)

spain_invis_searches %>%
  ggplot()+
  geom_line(aes(y = invisalign_searches, x = Month))+
  theme_minimal() +
  ggtitle("Invisalign Searches per Month") + 
  xlab("Month") + ylab("Invisalign Searches Searches") +
  theme_bw() + theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5))

#Category Interest 
#View(spain_category_interest)
lapply(spain_category_interest, class)

spain_category_interest$Month <- as.Date(spain_category_interest$Month)

spain_category_interest %>%
  ggplot()+
  geom_line(aes(y = `ortodoncia invisible + alineadores: (Spain)`, x = Month))+
  theme_minimal() +
  ggtitle("Category Interest per Month") + 
  xlab("Month") + ylab("Category Searches") +
  theme_bw() + theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5))

#Invisalign vs Competitor Searches

#merge dataframes

spain_impressions <- spain_invis_searches %>% 
  left_join(spain_competitor, by = "Month") %>% 
  left_join(spain_category_interest, by = "Month")

colnames(spain_impressions) <- c("Month", "Invisalign", "Competitor", "Category Interest")

spain_impressions$invis_competitor <- spain_impressions$Invisalign / spain_impressions$Competitor

spain_impressions$invis_category <- spain_impressions$Invisalign / spain_impressions$`Category Interest`

spain_impressions$comp_category <- spain_impressions$Competitor / spain_impressions$`Category Interest`

spain_impressions %>%
  ggplot()+
  geom_line(aes(y = invis_competitor, x = Month))+
  theme_minimal() +
  ggtitle("Invisalign vs Competitor Searches Searches per Month") + 
  xlab("Month") + ylab("Invisalign vs Comp Searches") +
  theme_bw() + theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5))

spain_impressions %>%
  ggplot()+
  geom_line(aes(y = invis_category, x = Month))+
  theme_minimal() +
  ggtitle("Invisalign vs Category Interest per Month") + 
  xlab("Month") + ylab("Invisalign vs Category Interest") +
  theme_bw() + theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5))

spain_impressions %>%
  ggplot()+
  geom_line(aes(y = comp_category, x = Month))+
  theme_minimal() +
  ggtitle("Competitor vs Category Interest per Month") + 
  xlab("Month") + ylab("Competitor vs Category Interest") +
  theme_bw() + theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5))

##Internal Factors

#NPS
#View(spain_nps)

lapply(spain_nps, class)

spain_nps$Month <- as.Date(spain_nps$Month)

spain_nps %>%
  ggplot()+
  geom_line(aes(y = NPS, x = Month))+
  theme_minimal() +
  ggtitle("NPS per Month") + 
  xlab("Month") + ylab("NPS") +
  theme_bw() + theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5))

data_full <- spain_sales %>% 
  left_join(spain_macro_factors, by = "Month") %>%
  left_join(spain_impressions, by = "Month") %>%
  left_join(spain_flights, by = "Month") %>%
  left_join(spain_nps, by = "Month")
