#Exploratory Data Analysis

library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyverse)

#Data pre-processing

#SALES DATASET
#View(spain_sales)

spain_sales$Month <- as.Date(spain_sales$Month)
summary(spain_sales)

# Unpivot data to visualize

spain_sales_format <- spain_sales %>% pivot_longer(-c(Month)) 

spain_sales_format$name <- factor(spain_sales_format$name, 
                                  levels = c("Total_sales", "Product_A", "Product_B"))  # this will plot C first (on top), then A, then B

spain_sales_format %>%
  ggplot()+
  geom_line(aes(y = value, x = Month, color = name))+
  theme_minimal() +
  ggtitle("Sales Evolution per Month - Product A (Original), Product B (New) and Total Sales (Prod. A + B)") + 
  xlab("Month") + ylab("Sales") +
  theme_bw() + theme(legend.position = "bottom") + 
  theme(plot.title = element_text(hjust = 0.5))

# convert to time series

sales_ts <- ts(spain_sales$Product_A, 
               start= c(2018,1),
               end = c(2025,3), frequency = 12) 

plot(decompose(sales_ts))


##EXTERNAL FACTORS DATASET

#Macroeconomic Factor
#View(spain_macro_factors)

spain_macro_factors$Month <- as.Date(spain_macro_factors$Month)

# Divide in two datasets - HICP index and annual change, to be visualized separately

spain_macro_factors_index <- spain_macro_factors[c(1, 2, 4, 6, 8)]

#spain_macro_factors_annual_change <- spain_macro_factors[c(1, 3, 5, 7, 9)]

# HICP All items - Unpivot data to visualize

spain_macro_factors_index_format <- spain_macro_factors_index %>% pivot_longer(-c(Month)) 

spain_macro_factors_index_format %>%
  ggplot()+
  geom_line(aes(y = value, x = Month, color = name))+
  theme_minimal() +
  ggtitle("Harmonised Index of Consumer Prices - All Items, Health, Recreation & Restaurants/Hotels") + 
  xlab("Month") + ylab("HICP") +
  theme_bw() + theme(legend.position = "bottom") + 
  theme(plot.title = element_text(hjust = 0.5))

hicp_all_items_ts <- ts(spain_macro_factors_index$hicp_all_items, 
               start= c(2018,1),
               end = c(2025,3), frequency = 12) 


plot(decompose(hicp_all_items_ts))


hicp_health_ts <- ts(spain_macro_factors_index$hicp_health, 
                        start= c(2018,1),
                        end = c(2025,3), frequency = 12) 


plot(decompose(hicp_health_ts))

hicp_recreation_ts <- ts(spain_macro_factors_index$hicp_recreation, 
                     start= c(2018,1),
                     end = c(2025,3), frequency = 12) 


plot(decompose(hicp_recreation_ts))

hicp_restaurants_ts <- ts(spain_macro_factors_index$hicp_restaurants, 
                         start= c(2018,1),
                         end = c(2025,3), frequency = 12) 


plot(decompose(hicp_restaurants_ts))


# HICP Annual rate of change - Unpivot data to visualize

#spain_macro_factors_annual_change_format <- spain_macro_factors_annual_change %>% pivot_longer(-c(Month)) 

# spain_macro_factors_annual_change_format %>%
#   ggplot()+
#   geom_line(aes(y = value, x = Month, color = name))+
#   theme_minimal() +
#   ggtitle("Harmonised Index of Consumer Prices - All Items, Health, Recreation & Restaurants/Hotels (Annual change of rate)") + 
#   xlab("Month") + ylab("HICP") +
#   theme_bw() + theme(legend.position = "bottom") + 
#   theme(plot.title = element_text(hjust = 0.5))


##create new variables (ratios): 
##health/recreation/restaurants HICP per All Items ratio

spain_macro_factors_ratios <- spain_macro_factors

spain_macro_factors_ratios$hicp_health_per_all_items <- spain_macro_factors_ratios$hicp_health / spain_macro_factors_ratios$hicp_all_items

spain_macro_factors_ratios$hicp_health_per_recreation <- spain_macro_factors_ratios$hicp_health / spain_macro_factors_ratios$hicp_recreation

spain_macro_factors_ratios$hicp_health_per_restaurants <- spain_macro_factors_ratios$hicp_health / spain_macro_factors$hicp_restaurants

spain_macro_factors_ratios <- spain_macro_factors_ratios[c(1, 10:12)]

#Unpivot data to visualize

spain_macro_factors_ratios_format <- spain_macro_factors_ratios %>% pivot_longer(-c(Month)) 

spain_macro_factors_ratios_format %>%
  ggplot()+
  geom_line(aes(y = value, x = Month, color = name))+
  theme_minimal() +
  ggtitle("Harmonised Index of Consumer Prices - HICP Health in relation to All Items, Recreation & Restaurants") + 
  xlab("Month") + ylab("HICP") +
  theme_bw() + theme(legend.position = "bottom") + 
  theme(plot.title = element_text(hjust = 0.5))


# ##TRAVEL DEMAND DATASET
# #View(spain_flights)
# spain_flights$Month <- as.Date(spain_flights$Month)
# 
# spain_flights %>%
#   ggplot()+
#   geom_line(aes(y = flight_searches, x = Month), color = "#87cefa")+
#   theme_minimal() + 
#   ggtitle("Travel searches per Month") + 
#   xlab("Month") + ylab("Flight Searches") +
#   theme_bw() + theme(legend.position = "none") + 
#   theme(plot.title = element_text(hjust = 0.5))

##MARKET DYNAMICS DATASETS

#Invisalign Searches
#View(spain_invis_searches)

spain_invis_searches$Month <- as.Date(spain_invis_searches$Month)

spain_invis_searches %>%
  ggplot()+
  geom_line(aes(y = invisalign_searches, x = Month), color = "#87cefa")+
  theme_minimal() +
  ggtitle("Company X searches per Month (based on Google data)") + 
  xlab("Month") + ylab("Company X searches") +
  theme_bw() + theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5))

spain_invis_ts <- ts(spain_invis_searches$invisalign_searches, 
                          start= c(2018,1),
                          end = c(2025,3), frequency = 12) 

plot(decompose(spain_invis_ts))

#Competitor Searches
#View(spain_competitor)

spain_competitor$Month <- as.Date(spain_competitor$Month)

spain_competitor %>%
  ggplot()+
  geom_line(aes(y = `dr smile + straumann + angel aligner: (Spain)`, x = Month), color = "#87cefa")+
  theme_minimal() +
  ggtitle("Competitor Searches per Month (based on Google Trends data)") + 
  xlab("Month") + ylab("Competitor Searches") +
  theme_bw() + theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5))

spain_competitor_ts <- ts(spain_competitor$`dr smile + straumann + angel aligner: (Spain)`, 
                          start= c(2018,1),
                          end = c(2025,3), frequency = 12) 

plot(decompose(spain_competitor_ts))

#Category Interest 
#View(spain_category_interest)

spain_category_interest$Month <- as.Date(spain_category_interest$Month)

spain_category_interest %>%
  ggplot()+
  geom_line(aes(y = `ortodoncia invisible + alineadores: (Spain)`, x = Month), color = "#87cefa")+
  theme_minimal() +
  ggtitle("Category Interest per Month (based on Google Trends data)") + 
  xlab("Month") + ylab("Category Searches") +
  theme_bw() + theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5))

spain_category_interest_ts <- ts(spain_category_interest$`ortodoncia invisible + alineadores: (Spain)`, 
                          start= c(2018,1),
                          end = c(2025,3), frequency = 12) 

plot(decompose(spain_category_interest_ts))

##create new variables (ratios):
##Invisalign vs. competitor searches - to determine how much invisalign interest has been growing/declining vs. competitor interest
##Invisalign vs. category interest - to determine how much much invisalign interest has been growing/declining vs. overall clear aligners market
##Competitor searches vs. category interest - to determine much competitor interest has been growing/declining vs. competitor interest

#Invisalign vs Competitor Searches

#merge all dataframes to build ratios

spain_market_dynamics <- spain_invis_searches %>% 
  left_join(spain_competitor, by = "Month") %>% 
  left_join(spain_category_interest, by = "Month")

colnames(spain_market_dynamics) <- c("Month", "invisalign", "competitor", "category_interest")

#write.csv(spain_market_dynamics, "spain_market_dynamics.csv")

spain_market_dynamics$companyx_vs_competitor <- spain_market_dynamics$invisalign / spain_market_dynamics$competitor

spain_market_dynamics$companyx_vs_category_interest <- spain_market_dynamics$invisalign / spain_market_dynamics$category_interest

spain_market_dynamics$competitor_vs_category_interest <- spain_market_dynamics$competitor / spain_market_dynamics$category_interest

#visualize all together
#unpivot

spain_market_dynamics_format <- spain_market_dynamics[c(1, 5:6)] %>% pivot_longer(-c(Month)) 

spain_market_dynamics_format %>%
  ggplot()+
  geom_line(aes(y = value, x = Month, color = name))+
  theme_minimal() +
  ggtitle("Relative search ratios between Company X searches, Competitor searches and Category interest (based on Google Trends data)") + 
  xlab("Month") +
  theme_bw() + theme(legend.position = "bottom") + 
  theme(plot.title = element_text(hjust = 0.5))

##INTERNAL FACTORS DATASET - NPS
#View(spain_nps)

spain_nps$Month <- as.Date(spain_nps$Month)

spain_nps %>%
  ggplot()+
  geom_line(aes(y = NPS, x = Month), , color = "#87cefa")+
  theme_minimal() +
  ggtitle("Net Promoter Score development per Month") + 
  xlab("Month") + ylab("NPS") +
  theme_bw() + theme(legend.position = "none") + 
  theme(plot.title = element_text(hjust = 0.5))

spain_nps_ts <- ts(spain_nps$NPS, 
                          start= c(2018,1),
                          end = c(2025,3), frequency = 12) 

plot(decompose(spain_nps_ts))


#Merge all data sets

data_full <- spain_sales %>% 
  left_join(spain_macro_factors_index, by = "Month") %>%
  left_join(spain_macro_factors_ratios, by = "Month") %>%
  left_join(spain_market_dynamics, by = "Month") %>%
  #left_join(spain_travel_demand, by = "Month") %>%
  left_join(spain_nps, by = "Month")

