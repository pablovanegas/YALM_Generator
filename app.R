# app.R - Complete working YAML Generator application
library(shiny)
library(shinyAce)
library(yaml)

# Utility functions
validate_file_extension <- function(filename) {
  file_ext <- tools::file_ext(filename)
  return(file_ext %in% c("qmd", "md", "Rmd", "rmd"))
}

safe_read_file <- function(filepath) {
  tryCatch({
    content <- readLines(filepath, warn = FALSE)
    list(success = TRUE, content = content, error = NULL)
  }, error = function(e) {
    list(success = FALSE, content = NULL, error = e$message)
  })
}

# UI
ui <- fluidPage(
  titlePanel("YAMLGen - YAML Header Generator"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Document Configuration"),
      textInput('title', 'Title', placeholder = "Enter document title"),
      textInput('subtitle', 'Subtitle', placeholder = "Enter subtitle (optional)"),
      textInput('author', 'Author', placeholder = "Enter author name"),
      
      radioButtons('date_type', 'Date', 
                   choices = c('Use system date' = 'system', 'Enter a date' = 'custom')),
      conditionalPanel(
        condition = "input.date_type == 'custom'",
        dateInput('date', 'Date', value = Sys.Date())
      ),
      
      textAreaInput('description', 'Description', 
                    placeholder = "Brief description", rows = 2),
      
      selectInput('output_format', 'Output Format', 
                  choices = c('HTML' = 'html', 'PDF' = 'pdf', 'Word' = 'docx', 'Presentation' = 'revealjs'),
                  selected = 'html'),
      
      # HTML Options
      conditionalPanel(
        condition = "input.output_format == 'html'",
        h4("HTML Options"),
        selectInput('theme', 'Theme', 
                    choices = c('default', 'cerulean', 'journal', 'flatly', 'darkly', 'readable', 'spacelab'),
                    selected = 'default'),
        selectInput('highlight_style', 'Highlight Style', 
                    choices = c('default', 'github', 'tango', 'pygments', 'kate', 'monochrome'),
                    selected = 'default'),
        checkboxInput('toc', 'Table of Contents', value = FALSE),
        checkboxInput('code_fold', 'Code Folding', value = FALSE)
      ),
      
      # PDF Options
      conditionalPanel(
        condition = "input.output_format == 'pdf'",
        h4("PDF Options"),
        selectInput('pdf_engine', 'PDF Engine',
                    choices = c('pdflatex', 'xelatex', 'lualatex'),
                    selected = 'pdflatex'),
        checkboxInput('pdf_toc', 'Table of Contents', value = FALSE)
      ),
      
      hr(),
      fileInput('file_upload', 'Choose .qmd or .md File',
                accept = c('.qmd', '.md', '.Rmd', '.rmd')),
      downloadButton('download_document', 'Download Document', class = "btn-primary")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Instructions",
          h2("YAML Header Generator"),
          div(
            style = "background-color: #f8f9fa; padding: 20px; border-radius: 5px; margin: 10px 0;",
            h3("How to Use"),
            tags$ol(
              tags$li("Fill in your document details in the sidebar"),
              tags$li("Choose your output format and configure options"),
              tags$li("Check the 'YAML Preview' tab to see the generated header"),
              tags$li("Optionally upload an existing file to edit"),
              tags$li("Download your complete document")
            ),
            br(),
            div(style = "color: #007bff; font-weight: bold;", 
                "✨ Real-time YAML generation with validation!")
          )
        ),
        
        tabPanel("YAML Preview",
          h3("Generated YAML Header"),
          div(style = "background-color: #f8f9fa; border: 1px solid #ced4da; border-radius: 4px; padding: 15px; font-family: monospace; font-size: 13px;",
            verbatimTextOutput("yaml_display")
          ),
          br(),
          h4("Validation Status"),
          uiOutput("yaml_validation")
        ),
        
        tabPanel("Document Editor",
          h3("Document Content"),
          p("Edit your document content below:"),
          aceEditor('document_editor', 
                   mode = 'markdown', 
                   theme = 'github', 
                   fontSize = 14, 
                   height = '400px',
                   value = "# Your Document Title\n\nWrite your content here...\n\n## Section 1\n\nContent for section 1.\n\n## Section 2\n\nContent for section 2.")
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  values <- reactiveValues(
    file_content = NULL,
    original_content = NULL
  )
  
  # File upload
  observeEvent(input$file_upload, {
    req(input$file_upload)
    
    if (!validate_file_extension(input$file_upload$name)) {
      showNotification("Please upload a .qmd, .md, or .Rmd file", type = "error", duration = 5)
      return()
    }
    
    read_result <- safe_read_file(input$file_upload$datapath)
    
    if (read_result$success) {
      values$file_content <- read_result$content
      values$original_content <- read_result$content
      
      updateAceEditor(session, "document_editor", 
                     value = paste(read_result$content, collapse = "\n"))
      
      showNotification("File loaded successfully!", type = "message", duration = 3)
    } else {
      showNotification(paste("Error reading file:", read_result$error), type = "error", duration = 5)
    }
  })
  
  # Generate YAML data
  yaml_data <- reactive({
    yaml_list <- list()
    
    # Add title
    if (!is.null(input$title) && nchar(trimws(input$title)) > 0) {
      yaml_list$title <- input$title
    }
    
    # Add subtitle
    if (!is.null(input$subtitle) && nchar(trimws(input$subtitle)) > 0) {
      yaml_list$subtitle <- input$subtitle
    }
    
    # Add author
    if (!is.null(input$author) && nchar(trimws(input$author)) > 0) {
      yaml_list$author <- input$author
    }
    
    # Add date
    if (input$date_type == 'custom' && !is.null(input$date)) {
      yaml_list$date <- as.character(input$date)
    } else {
      yaml_list$date <- "`r Sys.Date()`"
    }
    
    # Add description
    if (!is.null(input$description) && nchar(trimws(input$description)) > 0) {
      yaml_list$description <- input$description
    }
    
    # Format configuration
    format_config <- list()
    
    if (input$output_format == "html") {
      html_config <- list()
      if (!is.null(input$theme) && input$theme != "default") {
        html_config$theme <- input$theme
      }
      if (!is.null(input$highlight_style) && input$highlight_style != "default") {
        html_config$`highlight-style` <- input$highlight_style
      }
      if (!is.null(input$toc) && input$toc) {
        html_config$toc <- TRUE
      }
      if (!is.null(input$code_fold) && input$code_fold) {
        html_config$`code-fold` <- TRUE
      }
      if (length(html_config) > 0) {
        format_config$html <- html_config
      }
    } else if (input$output_format == "pdf") {
      pdf_config <- list()
      if (!is.null(input$pdf_engine)) {
        pdf_config$`pdf-engine` <- input$pdf_engine
      }
      if (!is.null(input$pdf_toc) && input$pdf_toc) {
        pdf_config$toc <- TRUE
      }
      format_config$pdf <- pdf_config
    } else if (input$output_format == "docx") {
      format_config$docx <- list()
    } else if (input$output_format == "revealjs") {
      format_config$revealjs <- list()
    }
    
    if (length(format_config) > 0) {
      yaml_list$format <- format_config
    }
    
    return(yaml_list)
  })
  
  # Generate YAML string
  yaml_string <- reactive({
    tryCatch({
      yaml_data_current <- yaml_data()
      
      # Remove empty elements
      yaml_clean <- yaml_data_current[!sapply(yaml_data_current, function(x) {
        is.null(x) || (is.character(x) && length(x) == 1 && nchar(trimws(x)) == 0)
      })]
      
      if (length(yaml_clean) == 0) {
        return("---\n# No configuration specified\n---")
      }
      
      # Generate YAML
      yaml_str <- yaml::as.yaml(yaml_clean, indent = 2)
      paste0("---\n", yaml_str, "---")
      
    }, error = function(e) {
      paste0("# Error generating YAML: ", e$message)
    })
  })
  
  # Output YAML
  output$yaml_display <- renderText({
    yaml_string()
  })
  
  # Validation status
  output$yaml_validation <- renderUI({
    yaml_str <- yaml_string()
    if (startsWith(yaml_str, "# Error")) {
      div(
        style = "color: #721c24; background-color: #f8d7da; padding: 10px; border-radius: 4px; border: 1px solid #f5c6cb;",
        icon("exclamation-triangle"),
        strong(" YAML Generation Error")
      )
    } else {
      div(
        style = "color: #155724; background-color: #d4edda; padding: 10px; border-radius: 4px; border: 1px solid #c3e6cb;",
        icon("check-circle"),
        strong(" YAML is valid and ready to use!")
      )
    }
  })
  
  # Download handler
  output$download_document <- downloadHandler(
    filename = function() {
      if (!is.null(input$title) && nchar(trimws(input$title)) > 0) {
        title_clean <- gsub("[^A-Za-z0-9_-]", "_", trimws(input$title))
        title_clean <- gsub("_{2,}", "_", title_clean)
        title_clean <- gsub("^_|_$", "", title_clean)
        paste0(title_clean, ".qmd")
      } else {
        "document.qmd"
      }
    },
    
    content = function(file) {
      tryCatch({
        # Get YAML header
        yaml_header <- strsplit(yaml_string(), "\n")[[1]]
        
        # Get content from editor
        doc_content <- input$document_editor
        content_lines <- if (!is.null(doc_content) && nchar(trimws(doc_content)) > 0) {
          strsplit(doc_content, "\n")[[1]]
        } else {
          c("# Your Document", "", "Write your content here...")
        }
        
        # Combine YAML and content
        final_content <- c(yaml_header, "", content_lines)
        
        # Write to file
        writeLines(final_content, file)
        
      }, error = function(e) {
        # Write error information to file
        writeLines(
          c(
            "# Error generating document",
            paste("# Error:", e$message),
            "",
            "# Please check your configuration and try again."
          ), 
          file
        )
      })
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)
