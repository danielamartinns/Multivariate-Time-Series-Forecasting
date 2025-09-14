#Correlation between best variables

library(PerformanceAnalytics)
library(ggcorrplot)
library(htmltools)
library(Hmisc)
library(car)

#dataset to be used (from cross correlation)
data_multi #dataset

#create numeric dataframe
data_multi_test <- data_multi[c(2, 5:17)]

#variance influence factors 

model_multi <- lm(Product_A ~ ., data = data_multi_test)

vif_values <- vif(model_multi)

vif_values

# Visualizing the model
plot(model_multi, which = 1, main = "Model Fit")

barplot(vif_values, col = "skyblue", main = "Variance Inflation Factor (VIF)")


#correlation analysis

lcor <- length(data_multi_test)

cor_matrix <- cor(data_multi_test[2:lcor], use = "pairwise.complete.obs", method = "pearson")

View (cor_matrix)

#write.csv(cor_matrix, "cor_matrix.csv")

chart.Correlation(data_multi_test, histogram = TRUE, pch = 19)

ggcorrplot(cor_matrix, type = "lower", lab = TRUE)

#test correlation and significance 

result <- rcorr(as.matrix(data_multi_test))
result$r  # Correlation matrix
result$P  # P-value matrix

View(result$P)

#test only with hicp_health_items and compax x vs competitor searches

colnames(data_multi_test)

model_multi_clean <- lm(Product_A ~ hicp_health_per_all_items_3 + companyx_vs_competitor_16, 
                        data = data_multi_test)

vif_values_clean <- vif(model_multi_clean)

vif_values_clean

cor_matrix_clean <- cor(data_multi_test[c(1,6,12)], use = "pairwise.complete.obs", method = "pearson")

ggcorrplot(cor_matrix_clean, type = "lower", lab = TRUE)

#choose best variables

data_final <- data_multi[c(1:4, 9, 14, 15)]

#data_final <- data_multi[c(1:4, 14)]
