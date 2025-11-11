library(httr)
library(jsonlite)
library(tidyverse)
library(readxl)
library(spacyr)

# Get data
rnl <- read_excel("/Users/xia2128737/Library/CloudStorage/GoogleDrive-xia2128737@paradisevalley.edu/Shared drives/PV IE Team/Surveys/Ruffalo Noel Levitz Survey 2025/data/data 2025.xlsx")
rnl <- rnl %>% filter(!if_all(everything(), is.na))
rnl25 <- rnl %>% filter(SurveyAdministrationName == "Paradise Valley Community College - SSI - 4/2025")
comments <- rnl25 %>% mutate(index = c(1:nrow(rnl25))) %>% 
  select(index, OpenEndedComments) %>% 
  filter(!is.na(OpenEndedComments))
sample <- comments %>% 
  filter(index %in% c(5, 15, 140, 141, 179)) %>% 
  mutate(index = c(1:5)) %>% 
  rename(comment = OpenEndedComments)

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
sample_name_masked <- sample
for (i in 1:nrow(sample_name_masked)) {
  sample_name_masked$comment[i] <- mask_entities(sample_name_masked$comment[i])
}

# Convert the dataframe to text
comments_text <- paste(capture.output(write.csv(sample_name_masked, row.names = F)), collapse = "\n")

# Prompt 1 ====
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
    model = "gpt-4o",
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
comments_ai <- sample_name_masked %>% left_join(data_df, by = "index")

# Prompt 2 ====
prompt <- paste("You are analyzing open-ended comments from a student satisfaction survey.\n\n", comments_text,
                "\n\n Your task is to code each comment using none, one, or more of the following standardized labels:
Campus Climate – Comments about the general environment or sense of belonging on campus (e.g., feeling cared for, welcomed, respected, or included).
Advising – Comments about academic advising, advisor helpfulness, or access to advisors.
Classes/Schedules – Comments about class offerings, availability, scheduling, or course registration timing.
Faculty/Instruction – Comments about instructors, teaching quality, or classroom experiences.
Facilities – Comments about physical campus resources such as parking lots, gyms, cafeterias, buildings, or classrooms.
Academic Program – Comments about specific academic programs or majors (e.g., Nursing, Music, Education).
Admissions/Registration – Comments about enrollment, admissions, or registration processes.
Other Services – Comments about campus services other than Advising or Admissions/Registration, such as Veterans Services, Counseling, Athletics, Library, Tutoring, Career Services, Disability Services, or Student Clubs.
Instructions: Please do not create new labels. If a comment fits multiple labels, include all relevant ones separated by commas, such as ‘Campus Climate, Academic Program’.
The output should be a table with two columns: index and labels.")

# API call
response <- POST(
  url = "https://api.openai.com/v1/chat/completions",
  add_headers(
    Authorization = paste("Bearer", api_key),
    "Content-Type" = "application/json"
  ),
  body = toJSON(list(
    model = "gpt-4o",
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
data_df <- data_df[, c(2:3)] # Modify the columns accordingly
data_df <- data_df %>% mutate(across(everything(), str_trim))
names(data_df) <- c("index", "labels") # Modify the columns accordingly
data_df$index <- as.numeric(data_df$index)

# Join the results back to the comments data
comments_ai_2 <- sample_name_masked %>% left_join(data_df, by = "index")

# Save data needed for presentation
write.csv(sample, file = "sample data.csv", row.names = F)
save(sample, sample_name_masked, comments_ai, comments_ai_2, file = "data.RData")
