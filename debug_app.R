# Debug script to test the modular app structure
cat("🔍 Debugging YALM Generator App Structure\n")
cat("==========================================\n\n")

# Test 1: Load global.R
cat("Step 1: Loading global.R...\n")
tryCatch({
  source("global.R")
  cat("✅ global.R loaded successfully\n")
}, error = function(e) {
  cat("❌ Error loading global.R:", e$message, "\n")
})

# Test 2: Check ui.R
cat("\nStep 2: Testing ui.R...\n")
tryCatch({
  source("ui.R")
  cat("✅ ui.R loaded successfully\n")
  cat("✅ UI object class:", class(ui), "\n")
}, error = function(e) {
  cat("❌ Error loading ui.R:", e$message, "\n")
})

# Test 3: Check server.R
cat("\nStep 3: Testing server.R...\n")
tryCatch({
  source("server.R")
  cat("✅ server.R loaded successfully\n")
  cat("✅ Server object class:", class(server), "\n")
}, error = function(e) {
  cat("❌ Error loading server.R:", e$message, "\n")
})

# Test 4: Check function signature
cat("\nStep 4: Checking server function signature...\n")
if (exists("server")) {
  server_args <- formals(server)
  cat("✅ Server function arguments:", names(server_args), "\n")
  if (length(server_args) == 3) {
    cat("✅ Correct number of arguments (3)\n")
  } else {
    cat("❌ Wrong number of arguments. Expected 3, got", length(server_args), "\n")
  }
} else {
  cat("❌ Server function not found\n")
}

cat("\n🎯 RECOMMENDATION:\n")
cat("If errors above, use: source('app.R') instead of runApp()\n")
cat("If no errors, try: shiny::runApp()\n")
