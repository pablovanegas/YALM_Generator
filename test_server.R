# Minimal server test to identify the issue
cat("🔍 Testing server function structure...\n")

# Try to load just the server function
tryCatch({
  # Source the server file
  source("server.R")
  
  # Check if server exists and its properties
  if (exists("server")) {
    cat("✅ Server function exists\n")
    cat("📝 Function class:", class(server), "\n")
    cat("📝 Function formals:", names(formals(server)), "\n")
    cat("📝 Number of arguments:", length(formals(server)), "\n")
    
    # Check if it's a proper function
    if (is.function(server)) {
      cat("✅ Server is a proper function\n")
      
      # Check argument names
      args <- names(formals(server))
      expected_args <- c("input", "output", "session")
      
      if (identical(args, expected_args)) {
        cat("✅ Server arguments are correct:", paste(args, collapse = ", "), "\n")
      } else {
        cat("❌ Server arguments are wrong. Expected:", paste(expected_args, collapse = ", "), "\n")
        cat("❌ Got:", paste(args, collapse = ", "), "\n")
      }
    } else {
      cat("❌ Server is not a function, it's a:", class(server), "\n")
    }
  } else {
    cat("❌ Server function does not exist after sourcing\n")
  }
  
}, error = function(e) {
  cat("❌ Error loading server.R:", e$message, "\n")
})

cat("\n💡 Try the standalone app instead: source('launch_standalone.R')\n")
