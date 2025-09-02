# Safe app launcher - tests components first, then runs the app

cat("🔧 YALM Generator - Safe Launch Script\n")
cat("====================================\n\n")

# Step 1: Load packages
cat("Step 1: Loading packages...\n")
required_packages <- c("shiny", "shinyAce", "yaml", "DT", "bslib")

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("Installing", pkg, "...\n")
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}
cat("✅ All packages loaded successfully\n\n")

# Step 2: Test basic structure
cat("Step 2: Testing app structure...\n")
if (file.exists("app.R") && file.exists("ui.R") && file.exists("server.R")) {
  cat("✅ Core app files found\n")
} else {
  cat("⚠️ Some core files missing\n")
}
cat("\n")

# Step 3: Offer options
cat("Step 3: Choose how to run the app:\n")
cat("Option 1: Minimal version (recommended if there were errors above)\n")
cat("Option 2: Full version (if all tests passed)\n\n")

cat("To run minimal version: source('app_minimal.R')\n")
cat("To run full version: runApp()\n")
cat("To test both: source('run_safe.R') then choose\n\n")

cat("🚀 Ready to launch! Choose your option above.\n")
