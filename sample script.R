# Install the packages if you haven't
# install.packages("httr")
# install.packages("jsonlite")
# install.packages("tidyverse")
# install.packages("spacyr")

# Load packages
library(httr)
library(jsonlite)
library(tidyverse)
library(spacyr)

# To save your OpenAI API key in the .Renviron file, you can run this in the console:
# file.edit("~/.Renviron")
# Then write this in the .Renviron file:
# OPENAI_API_KEY = XXX

# Import the API key that you saved in the .Renviron file
api_key <- Sys.getenv("OPENAI_API_KEY")

# Read data 
comments <- read.csv("sample data.csv") # Change it to your file path

# Function to mask names mentioned in the data
spacy_initialize(model = "en_core_web_lg")
mask_entities <- function(text) {
  parsed <- spacy_parse(text, entity = TRUE)
  persons <- parsed %>% filter(str_detect(entity, "PERSON")) %>% select(token)
  if (nrow(persons) > 0) {
    masked <- str_replace_all(text, regex(paste0("\\b(", paste(persons$token, collapse="|"), ")\\b")), "[PERSON]")
  } else {
    masked <- text
  }
  return(masked)
}

# Mask names
for (i in 1:nrow(comments)) {
  comments$comment[i] <- mask_entities(comments$comment[i])
}

# Convert the dataframe to text
comments_text <- paste(capture.output(write.csv(comments, row.names = F)), collapse = "\n")

# Prompt
prompt <- paste("You are analyzing open-ended comments from a student satisfaction survey.\n\n", comments_text, 
"\n\n Task 1: Determine the overall perception of each comment toward the college. 
Choose one of the following categories: (a) positive, (b) negative, (c) neutral, or (d) mixed.
Task 2: Identify the topics mentioned in each comment. 
Use clear and descriptive labels such as advising, class availability, faculty, etc. 
Please create new topic labels as needed. 
List all relevant labels, separated by commas, such as ‘advising, class availability’.
Return the results in a table with the following three columns: index, perception, and labels.")

# API call
response <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(
    Authorization = paste("Bearer", api_key),
    "Content-Type" = "application/json"
  ),
  body = toJSON(list(
    model = "gpt-4o", #Change it to the model you want to use.
    messages = list(
      list(role = "user", content = prompt)
    )
  ), auto_unbox = TRUE)
)

# Parse and print response
content <- content(response, "parsed")
cat(content$choices[[1]]$message$content)

# Save the table
text <- content$choices[[1]]$message$content
lines <- unlist(strsplit(text, "\n"))
table_lines <- lines[grepl("^\\|", lines)]

# Remove the first two rows
data_lines <- table_lines[-c(1, 2)]

# Convert to a matrix
max_cols <- max(sapply(str_split(data_lines, "\\|"), length))
data_matrix <- str_split_fixed(data_lines, "\\|", max_cols)

# Convert to a dataframe
data_df <- as.data.frame(data_matrix)

# Clean the dataframe
data_df <- data_df[, c(2:4)] # Modify the columns accordingly
data_df <- data_df %>% mutate(across(everything(), str_trim))
names(data_df) <- c("index", "perception", "labels") # Modify the columns accordingly
data_df$index <- as.numeric(data_df$index)

# Join the results back to the comments data
comments_ai <- comments %>% left_join(data_df, by = "index")

# Save results
write.csv(comments_ai, file = "ai_output.csv", row.names = F)
