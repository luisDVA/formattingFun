# load all packages and functions from file
library(dplyr)
library(fs)
library(tidyr)
library(stringr)
library(purrr)
library(parallel)
source("R/get_formatting.R")
source("R/summarizefiles.R")
source("R/summarizesheets.R")
source("R/translate_formatting.R")

# multiple xls files
allbins <- dir_ls("exampleinputs/")

# convert, summarize, and save to disk
summ1_25 <- summarize_formatting_files(allbins[1:20])
saveRDS(summ1_25, "output/sum1to25.rds")

# consider restarting R here
# summarize outputs
library(dplyr)
library(fs)
library(readr)
library(tidyr)
library(purrr)

# all summaris
chunks <- dir_ls("output/", glob = "*.rds")
allsummaries <- map(chunks, readRDS) 

fsumm <- allsummaries |> map(1) |> purrr::list_rbind()
fstats <- allsummaries |> map(2) |> purrr::list_rbind()


# Calculate percentage of files that use formatting
files_with_formatting <- fstats %>%
  filter(`metric` == "Percentage of Cells with Formatting") %>%
  filter(value > 0) %>%
  nrow()
files_with_formatting

total_files <- nrow(fstats %>% filter(`metric` == "Percentage of Cells with Formatting"))
total_files
percent_files_formatted <- (files_with_formatting / total_files) * 100
percent_files_formatted

# Calculate average percentage of formatted cells
avg_percent_cells_formatted <- fstats %>%
  filter(`metric` == "Percentage of Cells with Formatting") %>%
  summarize(avg_percent = mean(value, na.rm = TRUE)) %>%
  pull(avg_percent)
avg_percent_cells_formatted
# Find the three most common colors
color_counts <- fstats %>%
  filter(grepl("^Most Common Color", metric)) %>%
  filter(!is.na(color_code)) %>%
  count(color_code, sort = TRUE) %>%
  top_n(3, n)
color_counts
# Create summary
summary <- list(
  percent_files_formatted = percent_files_formatted,
  avg_percent_cells_formatted = avg_percent_cells_formatted,
  top_colors = color_counts$color_code,
  top_color_counts = color_counts$n
)

# bold and italic
total_files <- n_distinct(fsumm$fileid)

# Calculate the number of files using bold, italic, or both
files_with_formatting <- fsumm %>%
  group_by(fileid) %>%
  summarize(
    has_bold = any(grepl("bold", cell_text_unique_values, ignore.case = TRUE), na.rm = TRUE),
    has_italic = any(grepl("italic", cell_text_unique_values, ignore.case = TRUE), na.rm = TRUE)
  ) %>%
  summarize(
    bold_only = sum(has_bold & !has_italic),
    italic_only = sum(!has_bold & has_italic),
    both = sum(has_bold & has_italic),
    any = sum(has_bold | has_italic)
  )
files_with_formatting

# Calculate proportions
proportions <- files_with_formatting %>%
  mutate(
    prop_bold_only = bold_only / total_files,
    prop_italic_only = italic_only / total_files,
    prop_both = both / total_files,
    prop_any = any / total_files
  )

