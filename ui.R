# YALM_Generator UI
# Enhanced Shiny application for YAML header generation

ui <- fluidPage(
  theme = bslib::bs_theme(version = 4, bootswatch = "flatly"),
  
  # Custom CSS
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$style(HTML("
      .sidebar-panel { background-color: #f8f9fa; padding: 20px; border-radius: 5px; }
      .main-panel { padding: 20px; }
      .yaml-preview { background-color: #f8f9fa; border: 1px solid #dee2e6; border-radius: 5px; padding: 15px; font-family: 'Courier New', monospace; white-space: pre-wrap; }
      .error-message { color: #dc3545; font-weight: bold; }
      .success-message { color: #28a745; font-weight: bold; }
      .nav-tabs { margin-bottom: 20px; }
    "))
  ),
  
  # Application title
  titlePanel(
    div(
      img(src = "unal.png", height = "50px", style = "float: left; margin-right: 15px;"),
      h1("YALM Generator", style = "margin: 0; line-height: 50px;"),
      style = "overflow: hidden; margin-bottom: 20px;"
    )
  ),
  
  # Sidebar layout
  sidebarLayout(
    # Sidebar panel for inputs
    sidebarPanel(
      width = 4,
      class = "sidebar-panel",
      
      h3("Document Metadata"),
      
      # Basic metadata
      textInput("title", "Title:", value = ""),
      textInput("author", "Author:", value = ""),
      dateInput("date", "Date:", value = Sys.Date()),
      textAreaInput("abstract", "Abstract:", value = "", rows = 3),
      
      hr(),
      
      # Output format selection
      h4("Output Format"),
      selectInput("output_format", "Format:",
                  choices = list(
                    "HTML Document" = "html_document",
                    "PDF Document" = "pdf_document", 
                    "Word Document" = "word_document",
                    "PowerPoint" = "powerpoint_presentation",
                    "Beamer Presentation" = "beamer_presentation",
                    "R Markdown" = "rmarkdown"
                  ),
                  selected = "html_document"),
      
      # Conditional panels for format-specific options
      conditionalPanel(
        condition = "input.output_format == 'html_document'",
        selectInput("html_theme", "Theme:",
                    choices = c("default", "cerulean", "journal", "flatly", "readable", "spacelab", "united", "cosmo"),
                    selected = "flatly"),
        checkboxInput("html_toc", "Table of Contents", value = TRUE),
        checkboxInput("html_toc_float", "Floating TOC", value = TRUE)
      ),
      
      conditionalPanel(
        condition = "input.output_format == 'pdf_document'",
        selectInput("pdf_engine", "PDF Engine:",
                    choices = c("pdflatex", "xelatex", "lualatex"),
                    selected = "pdflatex"),
        checkboxInput("pdf_toc", "Table of Contents", value = TRUE),
        numericInput("pdf_toc_depth", "TOC Depth:", value = 3, min = 1, max = 5)
      ),
      
      hr(),
      
      # File operations
      h4("File Operations"),
      fileInput("file_upload", "Upload Document:",
                accept = c(".md", ".Rmd", ".qmd"),
                multiple = FALSE),
      
      br(),
      downloadButton("download_yaml", "Download YAML", class = "btn-primary"),
      br(), br(),
      downloadButton("download_document", "Download Document", class = "btn-success")
    ),
    
    # Main panel
    mainPanel(
      width = 8,
      class = "main-panel",
      
      # Tabset panel
      tabsetPanel(
        id = "main_tabs",
        
        # Instructions tab
        tabPanel("Instructions",
          div(
            img(src = "portada.png", width = "100%", style = "margin-bottom: 20px;"),
            h3("Welcome to YALM Generator"),
            p("This application helps you generate YAML headers for your R Markdown, Quarto, and Markdown documents."),
            
            h4("How to use:"),
            tags$ol(
              tags$li("Fill in the document metadata in the sidebar"),
              tags$li("Choose your desired output format and configure options"),
              tags$li("Preview the generated YAML in the 'YAML Preview' tab"),
              tags$li("Edit your document content in the 'Document Editor' tab"),
              tags$li("Download the complete document when ready")
            ),
            
            h4("Features:"),
            tags$ul(
              tags$li("Real-time YAML preview"),
              tags$li("Support for multiple output formats"),
              tags$li("File upload and download capabilities"),
              tags$li("Syntax highlighting in the editor"),
              tags$li("Validation and error checking")
            )
          )
        ),
        
        # YAML Preview tab
        tabPanel("YAML Preview",
          h3("Generated YAML Header"),
          verbatimTextOutput("yaml_preview"),
          br(),
          div(id = "yaml_validation", uiOutput("yaml_validation_ui"))
        ),
        
        # Document Editor tab
        tabPanel("Document Editor",
          h3("Document Content"),
          fluidRow(
            column(12,
              aceEditor("document_content", 
                       mode = "markdown",
                       theme = "github",
                       height = "500px",
                       value = "# Your Document Title\n\nStart writing your content here...",
                       fontSize = 14,
                       showLineNumbers = TRUE,
                       wordWrap = TRUE)
            )
          )
        )
      )
    )
  )
)