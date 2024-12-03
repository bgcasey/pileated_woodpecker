# ---
# title: "notes_to_table"
# author: "Brendan Casey"
# created: "2024-05-24"
# description: "code to convert field notes to a table."
# ---


# Install and load the necessary packages
install.packages("readtext")
library(readtext)
library(tidyverse)

# Read the Word document
doc <- readtext(paste0("0_data/external/Simran_Bains/fieldwork_2024/",
                       "field_notes_2024.docx"))$text

# Split the text into lines
lines <- strsplit(doc, "\n")[[1]]

# Split each line at the colon, with or without a space
split_lines <- strsplit(lines, ": ?")

# Convert the list to a data frame
df <- data.frame(
  ss = sapply(split_lines, `[`, 1),
  notes = sapply(split_lines, `[`, 2)
)

# Add the "WOOD-GT-" prefix if the entry is a lone number
df$ss[grepl("^[0-9]+$", df$ss)] <- paste0("WOOD-GT-", 
                                          df$ss[grepl("^[0-9]+$", 
                                                      df$ss)])
field_notes <- df
save(field_notes, file = paste0("0_data/manual/response/",
                                "wood_field_notes_2024.RData"))

