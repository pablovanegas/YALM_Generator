# Test script to launch YALM_Generator
# This script will run the enhanced Shiny application

# Check if all required packages are available
required_packages <- c("shiny", "shinyAce", "yaml", "DT", "bslib")
missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]

if (length(missing_packages) > 0) {
  cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
  install.packages(missing_packages)
}

# Load packages
library(shiny)
library(shinyAce)
library(yaml)
library(DT)
library(bslib)

cat("All packages loaded successfully!\n")
cat("Starting YALM_Generator application...\n")

# Run the application
runApp(port = 3838, host = "127.0.0.1")
