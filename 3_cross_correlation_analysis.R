#Cross correlation Analysis

#using data_full dataset

##Product cannibalization analysis should be done until the period that the new product was introduced, in this case Sep'24

data_full_original <- data_full %>% filter(Month <= "2024-09-01")

#Using ccf function

#function to test

N <- length(data_full_original$Total_sales)  
# Number of observations in the time series
threshold <- 2 / sqrt(N)  
# 95% confidence interval threshold

#####EXTERNAL FACTORS######

###INFLATION

###All items
sales_hicp_index <- ccf(data_full_original$hicp_all_items, 
                        data_full_original$Total_sales,
                        type = "correlation")
print(sales_hicp_index)
sales_hicp_index$lag[abs(sales_hicp_index$acf) > threshold]
#0 time lag


###Health items
sales_hicp_health_index <- ccf(data_full_original$hicp_health, 
                               data_full_original$Total_sales)
print(sales_hicp_health_index)
sales_hicp_health_index$lag[abs(sales_hicp_health_index$acf) > threshold]
#2 months positive lag: changes in inflation today are strongly correlated with changes in sales 2 months later.


###Recreation items
sales_hicp_recreation_index <- ccf(data_full_original$hicp_recreation,
                                   data_full_original$Total_sales)
print(sales_hicp_recreation_index)
sales_hicp_recreation_index$lag[abs(sales_hicp_recreation_index$acf) > threshold]
#3 months positive lag: 

###Restaurants items
sales_hicp_restaurants_index <- ccf(data_full_original$hicp_restaurants,
                                   data_full_original$Total_sales)
print(sales_hicp_restaurants_index)
sales_hicp_restaurants_index$lag[abs(sales_hicp_restaurants_index$acf) > threshold]
#3 months positive lag: 

#Health HICP / All items

sales_hicp_health_per_all_items <- ccf(data_full_original$hicp_health_per_all_items,
                                       data_full_original$Total_sales)
print(sales_hicp_health_per_all_items)
sales_hicp_health_per_all_items$lag[abs(sales_hicp_health_per_all_items$acf) > threshold]
#0 months lag - simultaneous correlation
#3 months positive lag: changes in inflation today are strongly correlated with changes in sales 3 months later.

#Health HICP / Recreation

sales_hicp_health_per_rec <- ccf(data_full_original$hicp_health_per_recreation,
                                       data_full_original$Total_sales)
print(sales_hicp_health_per_rec)
sales_hicp_health_per_rec$lag[abs(sales_hicp_health_per_rec$acf) > threshold]
#3 months positive lag: changes in inflation today are strongly correlated with changes in sales 3 months later.

#Health HICP / Restaurants

sales_hicp_health_per_res <- ccf(data_full_original$hicp_health_per_restaurants,
                                 data_full_original$Total_sales)
print(sales_hicp_health_per_res)
sales_hicp_health_per_res$lag[abs(sales_hicp_health_per_res$acf) > threshold]
#3 months positive lag: changes in inflation today are strongly correlated with changes in sales 3 months later.


### IMPRESSIONS

#Invisalign Searches
sales_invis <- ccf(data_full_original$invisalign,
                   data_full_original$Total_sales)
print(sales_invis)
sales_invis$lag[abs(sales_invis$acf) > threshold]
#0 months lag: Indicates simultaneous correlation.

#Competitor Searches
sales_competitor_searches <- ccf(data_full_original$competitor,
                                 data_full_original$Total_sales)
print(sales_competitor_searches)
sales_competitor_searches$lag[abs(sales_competitor_searches$acf) > threshold]
#3 months lag: Indicate that the comp searches leads the sales 3 months later.


#Category Interest 
sales_category_interest <- ccf(data_full_original$category_interest,
                               data_full_original$Total_sales)
print(sales_category_interest)
sales_category_interest$lag[abs(sales_category_interest$acf) > threshold]
#3 months lag: Indicate that the category searches leads the sales 3 months later.


#Invis per Competitor Interest 
sales_impressions_invis_comp <- ccf(data_full_original$companyx_vs_competitor,
                               data_full_original$Total_sales)
print(sales_impressions_invis_comp)
sales_impressions_invis_comp$lag[abs(sales_impressions_invis_comp$acf) > threshold]
#15 months lag, negative


#Invis per Category Interest 
sales_impressions_invis_category <- ccf(data_full_original$companyx_vs_category_interest,
                                    data_full_original$Total_sales)
print(sales_impressions_invis_category)
sales_impressions_invis_category$lag[abs(sales_impressions_invis_category$acf) > threshold]
#16 months lag, negative

#Competitor per Category Interest 
sales_impressions_comp_category <- ccf(data_full_original$competitor_vs_category_interest,
                                        data_full_original$Total_sales)
print(sales_impressions_comp_category)
sales_impressions_comp_category$lag[abs(sales_impressions_comp_category$acf) > threshold]
#12 months lag, positive 


####INTERNAL FACTORS

#NPS
sales_nps <- ccf(data_full_original$NPS, 
                 data_full_original$Total_sales)
print(sales_nps)
sales_nps$lag[abs(sales_nps$acf) > threshold]
#no significant correlation

#Choose variables

colnames(data_full)

data_with_lag <- data_full %>% 
  laging("hicp_all_items",0,"hicp_all_items_0") %>%
  laging("hicp_health", 2,"hicp_health_2") %>%
  laging("hicp_recreation",2,"hicp_recreation_2") %>%
  laging("hicp_restaurants", 3,"hicp_restaurants_3") %>%
  laging("hicp_health_per_all_items", 3,"hicp_health_per_all_items_3") %>%
  laging("hicp_health_per_recreation", 3,"hicp_health_per_recreation_3") %>%
  laging("hicp_health_per_restaurants", 3,"hicp_health_per_restaurants_3") %>%
  laging("invisalign",0,"invisalign_0") %>%
  laging("competitor", 0,"Competitor_0") %>%
  laging("category_interest", 3,"category_interest_3") %>%
  laging("companyx_vs_competitor", 16,"companyx_vs_competitor_16") %>%
  laging("companyx_vs_category_interest", 16,"companyx_vs_category_interest_16") %>%
  laging("NPS", 13,"NPS_13")

#View(data_with_lag)

len <- length(data_with_lag)

data_multi <- data_with_lag[c(1:4, 19:len)]

names(data_multi)[names(data_multi) == 'invisalign_0'] <- 'companyx_0'

