# summarize formatting for many files
# hardcoded the parallelization code here!
summarize_formatting_files <- function(file_paths) {
  
  process_file <- function(file_path) {
    tryCatch({
     
      if (!file.exists(file_path)) {
        warning("File does not exist: ", file_path)
        return(NULL)
      }
      
      original_ext <- tolower(fs::path_ext(file_path))
      
      if (original_ext != "xlsx") {
        # directory and filename
        dir_path <- dirname(file_path)
        file_name <- basename(file_path)
        
        #  to xlsx
        system2("libreoffice", 
                args = c("--headless", "--convert-to", "xlsx", file_path, "--outdir", dir_path))
        
        # Update file_path 
        xlsx_file_path <- fs::path_ext_set(file_path, "xlsx")
        
       
        if (!file.exists(xlsx_file_path)) {
          warning("Conversion failed: ", xlsx_file_path)
          return(NULL)
        }
      } else {
        xlsx_file_path <- file_path
      }
      
      # summarize_formatting
      result <- summarize_formatting(xlsx_file_path)
      
      
      result$format_summary$file_path <- file_path
      result$overall_stats$file_path <- file_path
      
      # Remove the temporary xlsx file if created
      if (original_ext != "xlsx" && xlsx_file_path != file_path) {
        file.remove(xlsx_file_path)
      }
      
      return(result)
      
    }, error = function(e) {
      warning("Error processing file: ", file_path, "\nError message: ", e$message)
      return(NULL)
    })
  }
  
  # Process all files
  results <- mclapply(file_paths, process_file,mc.cores = 4)
  
  # Remove failures
  results <- results[!sapply(results, is.null)]
  
  # Combine format summaries
  combined_format_summary <- purrr::map_dfr(results, ~.x$format_summary)
  
  # Combine overall stats
  combined_overall_stats <- as_tibble(purrr::map_dfr(results, ~.x$overall_stats))
  

  return(list(
    combined_format_summary = combined_format_summary,
    combined_overall_stats = combined_overall_stats
  ))
}

