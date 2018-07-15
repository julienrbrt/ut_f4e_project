# Project Part 2
# BIT Group 1
# Filter data from Euronext to corresponding Yahoo Finance names

if (!require(data.table)) install.packages("data.table")
if (!require(stringr)) install.packages("stringr")
if (!require(tidyr)) install.packages("tidyr")
library(data.table)
library(stringr)
library(tidyr)

# Import datasets of value from Euronext (15/10/2017)
euronext <- fread("preprocess/Euronext_Equities_EU_2017-10-15.csv")

# Keep EUR values only
euronext <- subset(euronext, euronext$`Trading Currency` == "EUR")

# Filter symbols to correspond to Yahoo Finance!
euronext$Market <- str_sub(euronext$Market, nchar("Euronext") + 2, -1)
euronext$Market <- gsub("^(.*?),.*", "\\1", euronext$Market)

# A function should be made but quicker that way
euronext$Market <- gsub("Paris", "PA", euronext$Market)
euronext$Market <- gsub("Amsterdam", "AS", euronext$Market)
euronext$Market <- gsub("Brussels", "BR", euronext$Market)
euronext$Market <- gsub("Lisbon", "LS", euronext$Market)

# Separate Access / Growth and normal stock markets
euronext$Market <- gsub("(.*?) (.*?)$", "\\2 \\1", euronext$Market)
euronext <- separate(euronext, Market, c("Market", "Type"), sep = " ")

euronext$Market <- str_to_upper(euronext$Market)

euronext <- unite(euronext, "YSymbols", c("Symbol", "Market"), sep = ".", remove = FALSE)

# Keep only Growth and Normal companies
euronext$Type[is.na(euronext$Type)] <- "Standard"
euronext <- subset(euronext, euronext$Type != "Access")

# Remove unused column
euronext$Market <- NULL