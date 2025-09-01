# 🚀 YALM_Generator - Production-Ready Version

## Quick Start Guide

### To run the application:

1. **Open R or RStudio**
2. **Set your working directory:**
   ```r
   setwd("d:/JVANEGASMO/Downloads/YALM_Generator-main/YALM_Generator-main")
   ```

3. **Install required packages (if not already installed):**
   ```r
   install.packages(c("shiny", "shinyAce", "yaml", "DT", "bslib"))
   ```

4. **Test components (optional):**
   ```r
   source("test_components.R")
   ```

5. **Run the application (choose ONE method):**
   
   **Method A - Using run_app.R (recommended):**
   ```r
   source("run_app.R")
   ```
   
   **Method B - Using runApp():**
   ```r
   runApp()
   ```
   
   **Method C - Using standalone app.R:**
   ```r
   source("app.R")
   ```

### What's New in This Version:

✅ **Real-time YAML Preview** - See your YAML header as you type
✅ **Robust Error Handling** - Clear error messages and validation
✅ **Multiple Output Formats** - Support for HTML, PDF, Word, and more
✅ **File Upload/Download** - Import existing documents and export results
✅ **Enhanced UI** - Clean, intuitive interface with tabbed organization
✅ **Production-Ready Code** - Comprehensive error handling and validation

### Features:

- **Metadata Editor**: Fill in document title, author, date, and abstract
- **Format Configuration**: Choose output format with specific options
- **Document Editor**: Edit your document content with syntax highlighting
- **YAML Preview**: Real-time preview of generated YAML header
- **File Operations**: Upload existing files and download complete documents

### File Structure:

- `app.R` - Complete standalone application (recommended for production)
- `ui.R` / `server.R` / `global.R` - Modular version (alternative structure)
- `modules/` - Reusable Shiny modules for future extensibility
- `www/` - Static assets (CSS, images)
- `test_*.R` - Testing scripts for validation

### Troubleshooting:

If you encounter any issues:
1. Check that all required packages are installed
2. Ensure you're in the correct working directory
3. Run one of the test scripts first to verify functionality
4. Check the R console for detailed error messages

### Next Steps:

This version provides a solid foundation. You can now:
- Add custom output formats
- Extend with additional metadata fields
- Integrate with version control systems
- Deploy to Shiny Server or shinyapps.io

---

**Ready to use!** The application is now robust, extensible, and production-ready.
