# ==============================================================================
# YALM_Generator - Versión Consolidada
#
# Este archivo único contiene la UI, el servidor y la lógica global
# para evitar conflictos y facilitar el mantenimiento.
# Fase 0: Estabilización de la Aplicación.
# ==============================================================================

# 1. CONTENIDO DE global.R (Librerías y Funciones Globales)
# ==============================================================================
library(shiny)
library(shinyAce)
library(yaml)
library(bslib)

options(shiny.maxRequestSize = 30*1024^2)  # 30MB max file size

validate_file_extension <- function(filename, allowed_extensions = c("qmd", "md", "Rmd", "rmd")) {
  file_ext <- tools::file_ext(filename)
  return(file_ext %in% allowed_extensions)
}

safe_read_file <- function(filepath) {
  tryCatch({
    content <- readLines(filepath, warn = FALSE)
    list(success = TRUE, content = content, error = NULL)
  }, error = function(e) {
    list(success = FALSE, content = NULL, error = e$message)
  })
}

extract_yaml_from_content <- function(content) {
  if (length(content) < 2) return(NULL)
  
  yaml_start <- which(content == "---")[1]
  if (is.na(yaml_start) || yaml_start != 1) return(NULL)
  
  yaml_end_positions <- which(content == "---")
  if (length(yaml_end_positions) < 2) return(NULL)
  
  yaml_end <- yaml_end_positions[2]
  
  yaml_content <- content[(yaml_start + 1):(yaml_end - 1)]
  
  tryCatch({
    yaml::yaml.load(paste(yaml_content, collapse = "\n"))
  }, error = function(e) {
    NULL
  })
}

remove_yaml_from_content <- function(content) {
  if (length(content) < 2) return(content)
  
  yaml_start <- which(content == "---")[1]
  if (is.na(yaml_start) || yaml_start != 1) return(content)
  
  yaml_end_positions <- which(content == "---")
  if (length(yaml_end_positions) < 2) return(content)
  
  yaml_end <- yaml_end_positions[2]
  
  remaining_content <- content[(yaml_end + 1):length(content)]
  if (length(remaining_content) > 0 && remaining_content[1] == "") {
    remaining_content <- remaining_content[-1]
  }
  
  return(remaining_content)
}

combine_yaml_and_content <- function(yaml_string, content = NULL) {
  yaml_lines <- strsplit(yaml_string, "\n")[[1]]
  
  if (is.null(content) || length(content) == 0) {
    content <- c(
      "", "# Your Document Title", "", "Write your content here...",
      "", "## Section 1", "", "Content for section 1.",
      "", "## Section 2", "", "Content for section 2."
    )
  }
  
  combined <- c(yaml_lines, "", content)
  return(combined)
}

generate_filename <- function(title = NULL, format = "html") {
  if (!is.null(title) && nchar(trimws(title)) > 0) {
    clean_title <- gsub("[^A-Za-z0-9_-]", "_", trimws(title))
    clean_title <- gsub("_{2,}", "_", clean_title)
    clean_title <- gsub("^_|_$", "", clean_title)
  } else {
    clean_title <- "document"
  }
  
  extension <- ".qmd"
  return(paste0(clean_title, extension))
}


# 2. CONTENIDO DE ui.R (Definición de la Interfaz de Usuario)
# ==============================================================================
ui <- fluidPage(
  theme = bslib::bs_theme(version = 4, bootswatch = "flatly"),
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  titlePanel(
    div(
      img(src = "unal.png", height = "50px", style = "float: left; margin-right: 15px;"),
      h1("YALM Generator", style = "margin: 0; line-height: 50px;"),
      style = "overflow: hidden; margin-bottom: 20px;"
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h3("Document Metadata"),
      textInput("title", "Title:", value = ""),
      textInput("subtitle", "Subtitle:", value = ""),
      textInput("author", "Author:", value = ""),
      
      radioButtons('date_type', 'Date', 
                   choices = c('Use system date' = 'system', 'Enter a date' = 'custom'),
                   selected = 'system'),
      conditionalPanel(
        condition = "input.date_type == 'custom'",
        dateInput('date', 'Custom Date', value = Sys.Date())
      ),
      
      textAreaInput("description", "Description:", value = "", rows = 2),
      
      hr(),
      
      h4("Output Format"),
      selectInput("output_format", "Format:",
                  choices = c('HTML' = 'html', 'PDF' = 'pdf', 'Word' = 'docx', 'Presentation' = 'revealjs'),
                  selected = 'html'),
      
      # Conditional panels for format-specific options
      conditionalPanel(
        condition = "input.output_format == 'html'",
        selectInput('theme', 'Theme', 
                    choices = c('default', 'cerulean', 'journal', 'flatly', 'darkly', 'readable', 'spacelab'),
                    selected = 'default'),
        selectInput('highlight_style', 'Highlight Style', 
                    choices = c('default', 'github', 'tango', 'pygments', 'kate', 'monochrome'),
                    selected = 'default'),
        checkboxInput('toc', 'Table of Contents', value = FALSE),
        checkboxInput('code_fold', 'Code Folding', value = FALSE),
        checkboxInput('toc_float', 'Floating TOC', value = FALSE),
        conditionalPanel(
          condition = "input.toc_float == true",
          checkboxInput('toc_collapsed', 'Collapse TOC', value = TRUE),
          checkboxInput('smooth_scroll', 'Smooth Scroll', value = TRUE)
        )
      ),
      
      conditionalPanel(
        condition = "input.output_format == 'pdf'",
        selectInput('pdf_engine', 'PDF Engine',
                    choices = c('pdflatex', 'xelatex', 'lualatex'),
                    selected = 'pdflatex'),
        selectInput('documentclass', 'Document Class',
                    choices = c('article', 'report', 'book'),
                    selected = 'article'),
        checkboxInput("pdf_toc", "Table of Contents", value = TRUE)
      ),
      
      conditionalPanel(
        condition = "input.output_format == 'docx'",
        checkboxInput("docx_toc", "Table of Contents", value = TRUE)
      ),
      
      conditionalPanel(
        condition = "input.output_format == 'revealjs'",
        selectInput('revealjs_theme', 'Theme',
                    choices = c('default', 'dark', 'solarized', 'white'),
                    selected = 'default'),
        checkboxInput('incremental', 'Incremental Slides', value = FALSE)
      ),
      
      hr(),
      
      h4("File Operations"),
      fileInput("file_upload", "Upload Document:",
                accept = c(".md", ".Rmd", ".qmd"),
                multiple = FALSE),
      
      br(),
      downloadButton("download_document", "Download Document", class = "btn-success")
    ),
    
    mainPanel(
      width = 8,
      tabsetPanel(
        id = "main_tabs",
        
        tabPanel("Instructions",
                 div(
                   img(src = "portada.png", width = "100%", style = "margin-bottom: 20px; border-radius: 5px;"),
                   h3("Welcome to YALM Generator"),
                   p("This application helps you generate YAML headers for your Quarto (.qmd) and R Markdown (.md) documents."),
                   
                   h4("How to use:"),
                   tags$ol(
                     tags$li("Fill in the document metadata in the sidebar."),
                     tags$li("Choose your desired output format and configure its options."),
                     tags$li("Preview the generated YAML in the 'YAML Preview' tab."),
                     tags$li("Optionally, upload an existing document. The form will populate with its YAML, and the content will appear in the 'Document Editor'."),
                     tags$li("Download the complete document when you're ready.")
                   )
                 )
        ),
        
        tabPanel("YAML Preview",
                 h3("Generated YAML Header"),
                 uiOutput("yaml_validation"),
                 br(),
                 verbatimTextOutput("yaml_display")
        ),
        
        tabPanel("Document Editor",
                 h3("Document Content"),
                 aceEditor("document_editor", 
                           mode = "markdown",
                           theme = "github",
                           height = "500px",
                           value = "# Your Document Title\n\nStart writing your content here...",
                           fontSize = 14)
        )
      )
    )
  )
)

# 3. CONTENIDO DE server.R (Lógica del Servidor)
# ==============================================================================
server <- function(input, output, session) {
  values <- reactiveValues(
    file_content = NULL,
    file_name = NULL,
    original_content = NULL,
    yaml_data = NULL,
    validation_errors = NULL
  )
  
  observeEvent(input$file_upload, {
    req(input$file_upload)
    
    if (!validate_file_extension(input$file_upload$name)) {
      showNotification("Please upload a .qmd, .md, or .Rmd file", type = "error", duration = 5)
      return()
    }
    
    read_result <- safe_read_file(input$file_upload$datapath)
    
    if (read_result$success) {
      values$file_content <- read_result$content
      values$file_name <- input$file_upload$name
      
      existing_yaml <- extract_yaml_from_content(read_result$content)
      values$original_content <- remove_yaml_from_content(read_result$content)
      
      updateAceEditor(session, "document_editor", value = paste(values$original_content, collapse = "\n"))
      
      if (!is.null(existing_yaml)) {
        populate_form_from_yaml(session, existing_yaml)
      }
      
      showNotification(paste("File", input$file_upload$name, "loaded successfully"), type = "message", duration = 3)
      
    } else {
      showNotification(paste("Error reading file:", read_result$error), type = "error", duration = 5)
      values$file_content <- NULL
      values$file_name <- NULL
      values$original_content <- NULL
    }
  })
  
  yaml_data <- reactive({
    yaml_list <- list()
    
    if (!is.null(input$title) && nchar(trimws(input$title)) > 0) yaml_list$title <- input$title
    if (!is.null(input$subtitle) && nchar(trimws(input$subtitle)) > 0) yaml_list$subtitle <- input$subtitle
    if (!is.null(input$author) && nchar(trimws(input$author)) > 0) yaml_list$author <- input$author
    
    if (input$date_type == 'custom' && !is.null(input$date)) {
      yaml_list$date <- as.character(input$date)
    } else {
      yaml_list$date <- "`r Sys.Date()`"
    }
    
    if (!is.null(input$description) && nchar(trimws(input$description)) > 0) yaml_list$description <- input$description
    
    format_config <- build_format_config(input)
    if (length(format_config) > 0) yaml_list$format <- format_config
    
    values$yaml_data <- yaml_list
    return(yaml_list)
  })
  
  yaml_string <- reactive({
    tryCatch({
      yaml_data_clean <- clean_yaml_list(yaml_data())
      
      if (length(yaml_data_clean) == 0) {
        values$validation_errors <- NULL
        return("---\n# No configuration specified\n---")
      }
      
      yaml_str <- yaml::as.yaml(yaml_data_clean, indent = 2)
      yaml_with_delim <- paste0("---\n", yaml_str, "---")
      
      values$validation_errors <- NULL
      return(yaml_with_delim)
      
    }, error = function(e) {
      values$validation_errors <- e$message
      return(paste0("# Error generating YAML: ", e$message))
    })
  })
  
  output$yaml_display <- renderText({
    yaml_string()
  })
  
  output$yaml_validation <- renderUI({
    if (is.null(values$validation_errors)) {
      div(
        style = "color: #28a745; background-color: #d4edda; border: 1px solid #c3e6cb; padding: 10px; border-radius: 4px; margin-bottom: 10px;",
        icon("check-circle"),
        strong(" YAML is valid and ready to use")
      )
    } else {
      div(
        style = "color: #721c24; background-color: #f8d7da; border: 1px solid #f5c6cb; padding: 10px; border-radius: 4px; margin-bottom: 10px;",
        icon("exclamation-triangle"),
        strong(" YAML Validation Error: "),
        br(),
        values$validation_errors
      )
    }
  })
  
  output$download_document <- downloadHandler(
    filename = function() {
      if (!is.null(values$file_name)) {
        return(values$file_name)
      } else {
        return(generate_filename(input$title, input$output_format))
      }
    },
    
    content = function(file) {
      tryCatch({
        current_content <- input$document_editor
        content_lines <- if (!is.null(current_content) && nchar(trimws(current_content)) > 0) {
          strsplit(current_content, "\n")[[1]]
        } else {
          values$original_content
        }
        
        final_content <- combine_yaml_and_content(yaml_string(), content_lines)
        writeLines(final_content, file)
        
      }, error = function(e) {
        writeLines(c("# Error generating document", paste("# Error:", e$message)), file)
      })
    }
  )
}

# Helper functions from server.R
build_format_config <- function(input) {
  config <- list()
  
  switch(input$output_format,
         "html" = {
           html_config <- list()
           if (input$theme != "default") html_config$theme <- input$theme
           if (input$highlight_style != "default") html_config$`highlight-style` <- input$highlight_style
           if (input$toc) html_config$toc <- TRUE
           if (input$code_fold) html_config$`code-fold` <- TRUE
           
           if (input$toc_float) {
             toc_float_config <- list(
               collapsed = input$toc_collapsed,
               smooth_scroll = input$smooth_scroll
             )
             html_config$toc_float <- toc_float_config
           }
           
           config$html <- html_config
         },
         "pdf" = {
           pdf_config <- list()
           pdf_config$`pdf-engine` <- input$pdf_engine
           pdf_config$documentclass <- input$documentclass
           if (input$pdf_toc) pdf_config$toc <- TRUE
           config$pdf <- pdf_config
         },
         "docx" = {
           docx_config <- list()
           if (input$docx_toc) docx_config$toc <- TRUE
           config$docx <- docx_config
         },
         "revealjs" = {
           revealjs_config <- list()
           if (input$revealjs_theme != "default") revealjs_config$theme <- input$revealjs_theme
           if (input$incremental) revealjs_config$incremental <- TRUE
           config$revealjs <- revealjs_config
         }
  )
  
  return(config)
}

clean_yaml_list <- function(yaml_list) {
  if (!is.list(yaml_list)) return(yaml_list)
  
  yaml_list <- yaml_list[!sapply(yaml_list, is.null)]
  yaml_list <- yaml_list[!sapply(yaml_list, function(x) is.character(x) && length(x) == 1 && nchar(trimws(x)) == 0)]
  
  yaml_list <- lapply(yaml_list, function(x) {
    if (is.list(x)) clean_yaml_list(x) else x
  })
  
  yaml_list <- yaml_list[!sapply(yaml_list, function(x) is.list(x) && length(x) == 0)]
  
  return(yaml_list)
}

populate_form_from_yaml <- function(session, yaml_data) {
  if (is.null(yaml_data)) return()
  
  # Basic fields
  if (!is.null(yaml_data$title)) updateTextInput(session, "title", value = yaml_data$title)
  if (!is.null(yaml_data$subtitle)) updateTextInput(session, "subtitle", value = yaml_data$subtitle)
  if (!is.null(yaml_data$author)) updateTextInput(session, "author", value = yaml_data$author)
  if (!is.null(yaml_data$description)) updateTextAreaInput(session, "description", value = yaml_data$description)
  
  # Date
  if (!is.null(yaml_data$date)) {
    if (yaml_data$date == "`r Sys.Date()`") {
      updateRadioButtons(session, "date_type", selected = "system")
    } else {
      updateRadioButtons(session, "date_type", selected = "custom")
      tryCatch({
        updateDateInput(session, "date", value = as.Date(yaml_data$date))
      }, error = function(e) {})
    }
  }
  
  # Format
  if (!is.null(yaml_data$format)) {
    format_name <- names(yaml_data$format)[1]
    updateSelectInput(session, "output_format", selected = format_name)
    
    config <- yaml_data$format[[1]]
    if (format_name == "html" && !is.null(config)) {
      if (!is.null(config$theme)) updateSelectInput(session, "theme", selected = config$theme)
      if (!is.null(config$`highlight-style`)) updateSelectInput(session, "highlight_style", selected = config$`highlight-style`)
      if (!is.null(config$toc)) updateCheckboxInput(session, "toc", value = config$toc)
      if (!is.null(config$`code-fold`)) updateCheckboxInput(session, "code_fold", value = config$`code-fold`)
    }
    # ... (add population for other formats as needed)
  }
}

# 4. LLAMADA A shinyApp
# ==============================================================================
shinyApp(ui = ui, server = server)
