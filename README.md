# YAMLGen - Advanced YAML Header Generator

## Description
**YAMLGen** is a professional Shiny application designed to generate syntactically perfect YAML headers for Quarto (.qmd) and R Markdown (.md) documents. This enhanced version provides real-time preview, robust validation, and comprehensive format support for modern document creation workflows.

## 🚀 Key Features

### Core Functionality
- **Real-time YAML Generation**: Instant preview of YAML headers as you configure settings
- **Syntactic Validation**: Built-in YAML validation with clear error reporting  
- **Multiple Output Formats**: Support for HTML, PDF, Word, and RevealJS presentations
- **File Integration**: Upload existing documents and preserve content while updating headers
- **Professional UI**: Clean, intuitive interface with tabbed navigation

### Format Support
- **HTML**: Themes, syntax highlighting, TOC options, code folding
- **PDF**: Multiple engines (pdflatex, xelatex, lualatex), document classes, margins
- **Word (DOCX)**: Table of contents, reference document support
- **Presentations (RevealJS)**: Themes, incremental slides, transitions

### Advanced Capabilities
- **Smart YAML Parsing**: Automatically detect and preserve existing YAML from uploaded files
- **Content Preservation**: Maintains document content while updating headers
- **Error Handling**: Comprehensive validation with user-friendly error messages
- **Export Options**: Generate clean, production-ready documents

## 📋 Requirements

### R Packages
```r
# Core packages
library(shiny)        # Shiny web framework
library(shinyAce)     # Code editor integration
library(yaml)         # YAML parsing and generation
library(DT)           # Data table display

# Optional packages for enhanced functionality
library(rmarkdown)    # Document rendering
library(knitr)        # Dynamic report generation
```

### System Requirements
- R version 4.0 or higher
- Modern web browser (Chrome, Firefox, Safari, Edge)
- RStudio (recommended for development)

## 🏃‍♂️ Quick Start

### Option 1: Direct Launch
```r
# From R console or RStudio
shiny::runApp()
```

### Option 2: Using the provided script
```r
# Run the test script
source("run_app.R")
```

### Option 3: RStudio Integration
1. Open the project file `YALM_Generator.Rproj` in RStudio
2. Click "Run App" in the Source pane
3. Application will launch in RStudio Viewer or external browser

## 📖 User Guide

### Basic Workflow
1. **Configure Document Metadata**
   - Enter title, subtitle, author information
   - Choose date format (system date or custom)
   - Add description (optional)

2. **Select Output Format**
   - Choose from HTML, PDF, Word, or Presentation
   - Configure format-specific options

3. **Preview YAML**
   - View real-time YAML generation in the "YAML Preview" tab
   - Check validation status for syntax errors

4. **Upload File (Optional)**
   - Upload existing .qmd, .md, or .Rmd files
   - Existing YAML will be detected and form fields populated
   - Content is preserved and editable

5. **Download Document**
   - Generate final document with YAML header
   - Content is combined automatically

### Format-Specific Options

#### HTML Documents
- **Themes**: Bootstrap-based themes (cerulean, journal, flatly, etc.)
- **Syntax Highlighting**: Code syntax highlighting styles
- **Table of Contents**: Static or floating TOC with customization
- **Code Folding**: Hide/show code blocks in output

#### PDF Documents  
- **PDF Engine**: Choose between pdflatex, xelatex, or lualatex
- **Document Class**: article, report, or book layouts
- **Margins**: Customizable page margins
- **Bibliography**: Citation and reference support

#### Word Documents
- **Table of Contents**: Automatic TOC generation
- **Reference Document**: Use custom .docx templates
- **Styling**: Professional formatting options

#### Presentations (RevealJS)
- **Themes**: Professional presentation themes
- **Incremental**: Progressive slide reveals
- **Transitions**: Slide transition effects

## 🛠️ Technical Architecture

### Modular Design
The application follows a modular architecture with clear separation of concerns:

```
├── global.R              # Global configuration and utility functions
├── ui.R                  # User interface definition  
├── server.R              # Server logic and reactive programming
├── modules/              # Reusable Shiny modules
│   ├── yaml_generator_module.R    # YAML generation logic
│   └── file_input_module.R        # File handling module
└── www/                  # Static web assets
    ├── styles.css        # Custom CSS styling
    ├── portada.png       # Logo and branding
    └── guia.png          # User guide images
```

### Key Components

#### YAML Generation Engine
- **Reactive Programming**: Real-time updates using Shiny reactivity
- **Validation**: Built-in YAML syntax checking with `yaml` package
- **Format Abstraction**: Modular format configuration system

#### File Processing
- **Smart Upload**: Automatic file type detection and validation
- **YAML Extraction**: Parse existing YAML headers from documents
- **Content Preservation**: Maintain document content during header updates

#### Error Handling
- **Graceful Degradation**: Continue operation even with invalid inputs
- **User Feedback**: Clear error messages and validation status
- **Logging**: Comprehensive error tracking for debugging

## 🔧 Customization

### Adding New Output Formats
To add support for additional output formats:

1. **Update UI Options**: Add new format choice in `selectInput('output_format')`
2. **Create Format Configuration**: Add format-specific options in `conditionalPanel`
3. **Implement Server Logic**: Add format handling in `build_format_config()`
4. **Test Integration**: Verify YAML generation and validation

### Extending YAML Features
The modular design allows easy extension of YAML capabilities:

```r
# Example: Adding custom metadata fields
custom_metadata <- list(
  keywords = input$keywords,
  category = input$category,
  license = input$license
)

yaml_list <- c(yaml_list, custom_metadata)
```

## 🐛 Troubleshooting

### Common Issues

#### Package Installation Errors
```r
# Install missing dependencies
install.packages(c("shiny", "shinyAce", "yaml", "DT"))
```

#### YAML Validation Errors
- Check for proper indentation (use spaces, not tabs)
- Verify special characters are properly quoted
- Ensure list items are consistently formatted

#### File Upload Issues
- Verify file extensions (.qmd, .md, .Rmd, .rmd)
- Check file size limits (30MB maximum)
- Ensure proper file encoding (UTF-8 recommended)

### Performance Optimization
- For large files, the application uses streaming file reading
- YAML generation is optimized for real-time updates
- Content validation runs asynchronously to maintain UI responsiveness

## 🤝 Contributing

We welcome contributions to improve YAMLGen! Areas for enhancement include:

- **Additional Output Formats**: Support for more document types
- **Enhanced Validation**: More sophisticated YAML checking
- **UI Improvements**: Better user experience and accessibility
- **Performance**: Optimization for larger documents
- **Documentation**: Expanded user guides and examples

## 📄 License

This project is open source and available under the MIT License. See the LICENSE file for details.

## 🆘 Support

For questions, issues, or feature requests:
- Create an issue on the GitHub repository
- Check the documentation in the "Instructions" tab
- Review the troubleshooting section above

## 🙏 Acknowledgments

- Built with [Shiny](https://shiny.rstudio.com/) web framework
- YAML processing powered by the [yaml](https://cran.r-project.org/package=yaml) package  
- Code editing via [shinyAce](https://github.com/trestletech/shinyAce)
- Inspired by the Quarto and R Markdown ecosystems

