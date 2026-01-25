# ---------------------------------------------------------
# 1. LOAD LIBRARIES
# ---------------------------------------------------------
library(tidyverse)      # Data manipulation + ggplot
library(dplyr)          # Data wrangling
library(ggplot2)        # Visualization
library(readr)          # Reading files
library(corrplot)       # Correlation plot


# ---------------------------------------------------------
# 2. LOAD DATASET
# ---------------------------------------------------------
df <- read_csv("C:/Users/Dell/Downloads/macrotrends.csv")

# View first rows
head(df)

# Check structure
str(df)


# ---------------------------------------------------------
# 3. DATA CLEANING
# ---------------------------------------------------------
# Remove duplicate rows
df <- df %>% distinct()

# Remove rows with many missing values
df <- df %>% drop_na()

# Check summary
summary(df)


# ---------------------------------------------------------
# 4. SUMMARY STATISTICS
# ---------------------------------------------------------
# Mean of all numeric columns
sapply(df, function(x) if(is.numeric(x)) mean(x) else NA)

# Standard deviation
sapply(df, function(x) if(is.numeric(x)) sd(x) else NA)


# ---------------------------------------------------------
# 5. DATA VISUALIZATION
# ---------------------------------------------------------

# ---- Histogram of first numeric column
num_cols <- df %>% select(where(is.numeric))
first_col <- names(num_cols)[1]

ggplot(df, aes_string(first_col)) +
  geom_histogram(bins = 30) +
  ggtitle(paste("Histogram of", first_col))


# ---- Scatter plot between first two numeric columns
if(length(num_cols) >= 2){
  ggplot(df,aes_string(num_cols %>% names() %>% .[1],
                        num_cols %>% names() %>% .[2])) +
    geom_point() +
    ggtitle("Scatter Plot of Two Variables")
}


# ---------------------------------------------------------
# 6. CORRELATION ANALYSIS
# ---------------------------------------------------------
if(ncol(num_cols) > 1){
  corr_matrix <- cor(num_cols)
  print(corr_matrix)
  corrplot(corr_matrix, method = "circle")
}


# ---------------------------------------------------------
# 7. LINEAR REGRESSION MODEL
# ---------------------------------------------------------

# Choose first variable as target, second as predictor
if(ncol(num_cols) >= 2){
  target <- names(num_cols)[1]
  predictor <- names(num_cols)[2]
  
  model <- lm(df[[target]] ~ df[[predictor]])
  summary(model)
}


# ---------------------------------------------------------
# 8. EXPORT CLEANED DATA + MODEL OUTPUT
# ---------------------------------------------------------

# Save cleaned dataset
write_csv(df, "C:/Users/Dell/Downloads/cleaned_macrotrends.csv")

# Save regression model summary
sink("/mnt/data/model_summary.txt")
print(summary(model))
sink()

# ---------------------------------------------------------
# END OF PROJECT
# ---------------------------------------------------------
