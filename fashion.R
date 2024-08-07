library(dplyr)
library(tidyverse)
library(ggplot2)
library(kableExtra)

#raw_data = read.csv("clean_data_new_columns.csv")
#unique(raw_data$product_color)

df = read.csv("data/processed_clothing_reviews.csv")

library(dplyr)

product_type_keywords <- c(
  "crop", "cami", "kaftan", "off-shoulder", "off shoulder", "one shoulder", "blouse", 
  "bralette", "bustier", "bra", "tie front", "sleeveless", "cape", "tube", "flare", 
  "corset", "asymmetrical", "boxy", "cold shoulder", "wrap", "peplum", "tunic", 
  "halter neck", "tie back", "backless", "belted", "button down", "button-down", 
  "cowl", "tank", "v-neck", "v neck", "tee", "t-shirt", "bow", "bodysuit", 
  "full sleeve", "sweater", "cardigan", "sweatshirt", "pullover", "high low", 
  "high-low", "poncho", "empire", "low waist", "ruffle", "cocktail", "sundress", 
  "shift", "body con", "babydoll", "maxi", "drop waist", "blazer", "apron", 
  "bootcut", "boyfriend", "high rise", "straight", "baggy", "slim", "distressed", 
  "ripped", "flared", "mom", "capri", "skinny","shorts"," pleated", "tassel", "tassle", 
  "bikini", "swimsuit", "one piece", "one-piece", "two piece", "two-piece", "swim", 
  "padded", "push up", "push-up", "cap","hat","visor", "fringe"
  
)

add_product_type <- function(df, product_type_keywords) {
  # Combine relevant columns into a single text string for each row
  combined_columns <- paste(df$Title, df$Review.Text, df$Division.Name, df$Department.Name, df$Class.Name, sep = " ")
  
  # Create an empty Product Type column
  df$product_type <- NA
  
  # Loop through each keyword and check if it exists in the combined columns
  for (keyword in product_type_keywords) {
    matching_indices <- grep(keyword, combined_columns, ignore.case = TRUE)
    df$product_type[matching_indices] <- keyword
  }
  
  # Replace NA values with "unknown"
  df$product_type[is.na(df$product_type)] <- "unknown"
 
  return(df)
}

style_keywords <- c(
  "casual","formal", "vintage", "contemporary", "modern","minnimalist","boho","bohemian",
  "edgy","preppy","sporty","punk","glamorous","glamourous","classic","streetwear",
  "hipster","army","military","retro","eclectic","rustic","chic", "summer","winter",
  "fall","autumn","spring", "holiday","grunge","etheral","avant-garde","avant garde",
  "goth","gothic","nautical","futuristic","western","industrial","romantic","coastal",
  "art deco","mod","androgynous","athletic", "utility", "flower child", "picnic",
  "outdoor","beach","camping", "yoga", "hiking" 
  
)

add_style <- function(df, style_keywords) {
  # Combine relevant columns into a single text string for each row
  combined_columns <- paste(df$Title, df$Review.Text, sep = " ")
  
  # Create an empty Style column
  df$style <- NA
  
  # Loop through each keyword and check if it exists in the combined columns
  for (keyword in style_keywords) {
    matching_indices <- grep(keyword, combined_columns, ignore.case = TRUE)
    df$style[matching_indices] <- keyword
  }
  
  # Replace NA values with "unknown"
  df$style[is.na(df$style)] <- "unknown"
  
  return(df)
}

pattern_keywords <- c(
  "printed", "pattern", "camoflage","flannel", "stripes", "stripe", "polka dot", 
  "plaid", "tartan","paisley","floral","houndstooth","chevron", "argyle", "geometric",
  "gingham", "animal", "toile", "herringbone", "pinstripe", "leopard", "cheetah"
)

add_pattern <- function(df, pattern_keywords) {
  # Combine relevant columns into a single text string for each row
  combined_columns <- paste(df$Title, df$Review.Text, sep = " ")
  
  # Create an empty Pattern column
  df$pattern <- NA
  
  # Loop through each keyword and check if it exists in the combined columns
  for (keyword in pattern_keywords) {
    matching_indices <- grep(keyword, combined_columns, ignore.case = TRUE)
    df$pattern[matching_indices] <- keyword
  }
  
  # Replace NA values with "unknown"
  df$pattern[is.na(df$pattern)] <- "unknown"
  
  return(df)
}

material_keywords <- c(
  "cotton", "linen","silk","wool","leather","denim","polyester","rayon","nylon",
  "spandex","lycra","cashmere","velvet","chiffon","tweed","satin","flannel",
  "hemp","fleece", "nylon", "elastic", "stretch"
)

add_material <- function(df, material_keywords) {
  # Combine relevant columns into a single text string for each row
  combined_columns <- paste(df$Title, df$Review.Text, sep = " ")
  
  # Create an empty Material column
  df$material <- NA
  
  # Loop through each keyword and check if it exists in the combined columns
  for (keyword in material_keywords) {
    matching_indices <- grep(keyword, combined_columns, ignore.case = TRUE)
    df$material[matching_indices] <- keyword
  }
  
  # Replace NA values with "unknown"
  df$material[is.na(df$material)] <- "unknown"
  
  return(df)
}

colour_keywords <- c(
  "silver", "brown", "offwhite", "violet", "rosegold", "gold", "floral", "purple",
  "multi", "khaki", "pink", "unknown", "red", "orange", "grey", "beige", "blue",
  "yellow", "black", "leopard", "green", "white"
)

add_colour <- function(df, colour_keywords) {
  # Combine relevant columns into a single text string for each row
  combined_columns <- paste(df$Title, df$Review.Text, sep = " ")
  
  # Create an empty Material column
  df$colour <- NA
  
  # Loop through each keyword and check if it exists in the combined columns
  for (keyword in colour_keywords) {
    matching_indices <- grep(keyword, combined_columns, ignore.case = TRUE)
    df$colour[matching_indices] <- keyword
  }
  
  # Replace NA values with "unknown"
  df$colour[is.na(df$colour)] <- "unknown"
  
  return(df)
}


# Apply the function to your dataset
df <- add_product_type(df, product_type_keywords)
df <- add_style(df, style_keywords)
df <- add_pattern(df, pattern_keywords)
df <- add_material(df, material_keywords)
df <- add_colour(df, colour_keywords)

# Print the updated dataframe
print(df)

sum(is.na(df))

#Export Clean Data
write.csv(df, file = "review_data_new_columns.csv", row.names = FALSE)



















































