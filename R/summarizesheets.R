
# summary for a single sheet in a file

summarize_formatting <- function(file_path) {
  
  nsheets <- length(readxl::excel_sheets(file_path))
  sampsheet <- sample(1:nsheets,1)
  
  # vector of 
  
  # Read formatting information from one sheet
  format_long <- get_formatting(file_path, sheet = sampsheet)
  
  # Translate formatting
  translated_format <- forgts::translate_defs(format_long)
  
 
  format_summary <- translated_format %>%
    filter(!is.na(arg_value)) %>%
    group_by(helper, styling_arg) %>%
    summarize(
      count = n(),
      unique_values = str_c(unique(arg_value), collapse = ", "),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = helper,
      values_from = c(count, unique_values),
      names_glue = "{helper}_{.value}"
    )
  
  # Calculate overall stats
  total_cells <- nrow(format_long) / length(unique(format_long$format))
  formatted_cells <- translated_format  %>% filter(!is.na(arg_value)) %>% distinct(rowid,target_var) %>% nrow()
  variables_with_formatting <- length(unique(translated_format$target_var[!is.na(translated_format$arg_value)]))
  total_variables <- length(unique(translated_format$target_var))
  
  # Find most common color codes
  color_counts <- translated_format %>%
    filter(grepl("color", styling_arg, ignore.case = TRUE), !is.na(arg_value)) %>%
    group_by(arg_value) %>%
    summarize(count = n(), .groups = "drop") %>%
    arrange(desc(count))
  
  top_colors <- head(color_counts, 5)
  
  # Create overall stats dataframe
  overall_stats <- data.frame(
    metric = c("Percentage of Cells with Formatting", 
               "Percentage of Variables with Formatting"),
    value = c(
      ((formatted_cells/total_cells) * 100),
      ((variables_with_formatting / total_variables) * 100)
    ),
    color_code = NA,
    occurrences = NA
  )
 
  if (nrow(top_colors) > 0) {
    color_stats <- data.frame(
      metric = paste("Most Common Color", 1:nrow(top_colors)),
      value = NA,
      color_code = top_colors$arg_value,
      occurrences = top_colors$count
    )
    overall_stats <- bind_rows(overall_stats, color_stats)
  } else {
    overall_stats <- bind_rows(overall_stats, 
                               data.frame(metric = "Color Usage",
                                          value = NA,
                                          color_code = NA,
                                          occurrences = NA))
  }
  
  format_summary$fileid <-  paste0(file_path)
  overall_stats$fileid <-  paste0(file_path)
  
  format_summary$sheet <-  paste0(sampsheet," of ",nsheets)
  overall_stats$sheet <-  paste0(sampsheet," of ",nsheets)
  
  return(list(format_summary = format_summary, overall_stats = overall_stats))
}


