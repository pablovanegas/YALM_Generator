# Alternative launcher using standalone app.R
# This version should work without issues

cat("🔄 YALM Generator - Standalone Launch\n")
cat("====================================\n\n")

# Check and install required packages
required_packages <- c("shiny", "shinyAce", "yaml")

cat("📦 Checking required packages...\n")
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("⬇️  Installing package:", pkg, "\n")
    install.packages(pkg)
  } else {
    cat("✅", pkg, "is available\n")
  }
}

cat("\n🚀 Loading standalone app.R...\n")
source("app.R")

cat("✅ App loaded successfully!\n")
cat("🌐 App should open in your browser at http://127.0.0.1:3838\n")
cat("🔄 If it doesn't open automatically, copy the URL from the console above.\n")
