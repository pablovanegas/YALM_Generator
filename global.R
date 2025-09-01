# Global.R - Load libraries and utility functions for YALM_Generator
library(shiny)
library(shinyAce)
library(yaml)

# Global configuration
options(shiny.maxRequestSize = 30*1024^2)  # 30MB max file size

# Utility functions
validate_file_extension <- function(filename, allowed_extensions = c("qmd", "md", "Rmd", "rmd")) {
  file_ext <- tools::file_ext(filename)
  return(file_ext %in% allowed_extensions)
}

# Function to safely read file content
safe_read_file <- function(filepath) {
  tryCatch({
    content <- readLines(filepath, warn = FALSE)
    list(success = TRUE, content = content, error = NULL)
  }, error = function(e) {
    list(success = FALSE, content = NULL, error = e$message)
  })
}

# Function to extract existing YAML from file content
extract_yaml_from_content <- function(content) {
  if (length(content) < 2) return(NULL)
  
  # Look for YAML delimiters
  yaml_start <- which(content == "---")[1]
  if (is.na(yaml_start) || yaml_start != 1) return(NULL)
  
  yaml_end_positions <- which(content == "---")
  if (length(yaml_end_positions) < 2) return(NULL)
  
  yaml_end <- yaml_end_positions[2]
  
  # Extract YAML content (without delimiters)
  yaml_content <- content[(yaml_start + 1):(yaml_end - 1)]
  
  tryCatch({
    yaml::yaml.load(paste(yaml_content, collapse = "\n"))
  }, error = function(e) {
    NULL
  })
}

# Function to remove existing YAML from content
remove_yaml_from_content <- function(content) {
  if (length(content) < 2) return(content)
  
  yaml_start <- which(content == "---")[1]
  if (is.na(yaml_start) || yaml_start != 1) return(content)
  
  yaml_end_positions <- which(content == "---")
  if (length(yaml_end_positions) < 2) return(content)
  
  yaml_end <- yaml_end_positions[2]
  
  # Return content after YAML (skip empty line if present)
  remaining_content <- content[(yaml_end + 1):length(content)]
  if (length(remaining_content) > 0 && remaining_content[1] == "") {
    remaining_content <- remaining_content[-1]
  }
  
  return(remaining_content)
}

# Function to combine YAML and content
combine_yaml_and_content <- function(yaml_string, content = NULL) {
  yaml_lines <- strsplit(yaml_string, "\n")[[1]]
  
  if (is.null(content) || length(content) == 0) {
    # Default content if none provided
    content <- c(
      "",
      "# Your Document Title",
      "",
      "Write your content here...",
      "",
      "## Section 1",
      "",
      "Content for section 1.",
      "",
      "## Section 2", 
      "",
      "Content for section 2."
    )
  }
  
  # Combine with empty line separator
  combined <- c(yaml_lines, "", content)
  return(combined)
}

# Function to generate suggested filename
generate_filename <- function(title = NULL, format = "html") {
  if (!is.null(title) && nchar(trimws(title)) > 0) {
    # Clean title for filename
    clean_title <- gsub("[^A-Za-z0-9_-]", "_", trimws(title))
    clean_title <- gsub("_{2,}", "_", clean_title)  # Replace multiple underscores
    clean_title <- gsub("^_|_$", "", clean_title)   # Remove leading/trailing underscores
  } else {
    clean_title <- "document"
  }
  
  # Add appropriate extension
  extension <- switch(format,
    "html" = ".qmd",
    "pdf" = ".qmd", 
    "docx" = ".qmd",
    "revealjs" = ".qmd",
    "book" = ".qmd",
    ".qmd"
  )
  
  return(paste0(clean_title, extension))
}

# CSS for better styling
app_css <- "
  .alert {
    padding: 10px;
    margin: 10px 0;
    border-radius: 4px;
    border: 1px solid transparent;
  }
  
  .alert-success {
    color: #3c763d;
    background-color: #dff0d8;
    border-color: #d6e9c6;
  }
  
  .alert-danger {
    color: #a94442;
    background-color: #f2dede;
    border-color: #ebccd1;
  }
  
  .alert-info {
    color: #31708f;
    background-color: #d9edf7;
    border-color: #bce8f1;
  }
  
  .module-section {
    background-color: #f8f9fa;
    padding: 15px;
    margin: 10px 0;
    border-radius: 5px;
    border: 1px solid #dee2e6;
  }
  
  .yaml-preview {
    background-color: #f8f9fa;
    border: 1px solid #ced4da;
    border-radius: 4px;
    padding: 10px;
    font-family: 'Monaco', 'Consolas', 'Lucida Console', monospace;
    font-size: 12px;
    white-space: pre-wrap;
    max-height: 400px;
    overflow-y: auto;
  }
"
