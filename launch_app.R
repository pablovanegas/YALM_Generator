# Simple launcher for YALM Generator
# Fixed version to avoid dependency issues

cat("🚀 Starting YALM Generator...\n")

# Load required packages
required_packages <- c("shiny", "shinyAce", "yaml", "bslib")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Installing package:", pkg, "\n")
    install.packages(pkg)
  }
}

cat("✅ All packages ready\n")
cat("🎯 Launching app...\n\n")

# Run the app
shiny::runApp()
