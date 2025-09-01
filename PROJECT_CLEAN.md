# 🧹 YALM_Generator - Clean Project Structure

## Files Remaining (Essential Only):

### Core Application Files:
- `app.R` - Complete standalone application (all-in-one)
- `ui.R` - User interface (modular version)
- `server.R` - Server logic (modular version)  
- `global.R` - Global functions and utilities

### Launch Scripts:
- `run_app.R` - Standard launcher (installs packages + runs)
- `run_safe.R` - Safe launcher with diagnostics
- `app_minimal.R` - Minimal working version

### Project Files:
- `YALM_Generator.Rproj` - RStudio project file
- `README.md` - Original project documentation
- `STARTUP_GUIDE.md` - Usage instructions

### Assets & Modules:
- `www/` - Static assets (CSS, images)
- `modules/` - Reusable Shiny modules (for future extensibility)

## Files Deleted:
✅ All backup files (server_old.R, ui_complex.R, etc.)
✅ All test files (test_*.R, quick_*.R)
✅ Temporary files (temp_*.txt)
✅ R history and user settings (.Rhistory, .Rproj.user/)
✅ Old documentation (ENHANCEMENT_SUMMARY.md)

## Recommended Usage:
1. **For production:** `source("app.R")`
2. **For development:** `runApp()` (uses ui.R + server.R)
3. **For testing:** `source("app_minimal.R")`

The project is now clean and organized! 🎉
