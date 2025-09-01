# YAML Generator Module
# This module handles the core YAML generation logic

yaml_generator_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("YAML Configuration"),
    
    # Basic metadata inputs
    textInput(ns('title'), 'Title', placeholder = "Enter document title"),
    textInput(ns('subtitle'), 'Subtitle', placeholder = "Enter subtitle (optional)"),
    textInput(ns('author'), 'Author', placeholder = "Enter author name"),
    
    # Date configuration
    radioButtons(ns('date_type'), 'Date', 
                 choices = c('Use system date' = 'system', 'Enter a date' = 'custom')),
    conditionalPanel(
      condition = paste0("input['", ns('date_type'), "'] == 'custom'"),
      dateInput(ns('date'), 'Date', value = Sys.Date())
    ),
    
    textAreaInput(ns('description'), 'Description', 
                  placeholder = "Brief description of the document",
                  rows = 2),
    
    # Output format selection
    selectInput(ns('output_format'), 'Output Format', 
                choices = c('HTML' = 'html', 'PDF' = 'pdf', 'Word' = 'docx', 
                           'Presentation' = 'revealjs', 'Book' = 'book'),
                selected = 'html'),
    
    # Dynamic format-specific options
    uiOutput(ns("format_options")),
    
    # Validation feedback
    uiOutput(ns("validation_feedback"))
  )
}

yaml_generator_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values for module state
    values <- reactiveValues(
      yaml_data = NULL,
      validation_errors = NULL
    )
    
    # Dynamic UI for format-specific options
    output$format_options <- renderUI({
      req(input$output_format)
      
      switch(input$output_format,
        "html" = html_format_options(ns),
        "pdf" = pdf_format_options(ns),
        "docx" = docx_format_options(ns),
        "revealjs" = revealjs_format_options(ns),
        "book" = book_format_options(ns),
        div()
      )
    })
    
    # Generate YAML data reactively
    yaml_data <- reactive({
      # Build YAML structure
      yaml_list <- list()
      
      # Add basic metadata
      if (!is.null(input$title) && nchar(trimws(input$title)) > 0) {
        yaml_list$title <- input$title
      }
      
      if (!is.null(input$subtitle) && nchar(trimws(input$subtitle)) > 0) {
        yaml_list$subtitle <- input$subtitle
      }
      
      if (!is.null(input$author) && nchar(trimws(input$author)) > 0) {
        yaml_list$author <- input$author
      }
      
      # Handle date
      if (input$date_type == 'custom' && !is.null(input$date)) {
        yaml_list$date <- as.character(input$date)
      } else {
        yaml_list$date <- "`r Sys.Date()`"
      }
      
      if (!is.null(input$description) && nchar(trimws(input$description)) > 0) {
        yaml_list$description <- input$description
      }
      
      # Add format-specific configuration
      format_config <- get_format_config(input, input$output_format)
      if (length(format_config) > 0) {
        yaml_list$format <- format_config
      }
      
      values$yaml_data <- yaml_list
      return(yaml_list)
    })
    
    # Generate YAML string with validation
    yaml_string <- reactive({
      tryCatch({
        yaml_data_current <- yaml_data()
        
        # Remove empty/null elements
        yaml_data_clean <- clean_yaml_data(yaml_data_current)
        
        # Generate YAML
        if (length(yaml_data_clean) > 0) {
          yaml_str <- yaml::as.yaml(yaml_data_clean, indent = 2)
          yaml_with_delim <- paste0("---\n", yaml_str, "---")
          values$validation_errors <- NULL
          return(yaml_with_delim)
        } else {
          return("---\n# No configuration specified\n---")
        }
        
      }, error = function(e) {
        values$validation_errors <- e$message
        return(paste0("# Error generating YAML: ", e$message))
      })
    })
    
    # Validation feedback UI
    output$validation_feedback <- renderUI({
      if (is.null(values$validation_errors)) {
        tags$div(
          class = "alert alert-success",
          style = "margin-top: 10px;",
          icon("check-circle"),
          " YAML is valid"
        )
      } else {
        tags$div(
          class = "alert alert-danger", 
          style = "margin-top: 10px;",
          icon("exclamation-triangle"),
          " Error: ", values$validation_errors
        )
      }
    })
    
    # Return reactive values and functions for parent module
    return(list(
      yaml_string = yaml_string,
      yaml_data = reactive(values$yaml_data),
      is_valid = reactive(is.null(values$validation_errors))
    ))
  })
}

# Helper functions for format-specific options
html_format_options <- function(ns) {
  tagList(
    h4("HTML Options"),
    selectInput(ns('theme'), 'Theme', 
                choices = c('default', 'cerulean', 'journal', 'flatly', 'darkly', 
                           'readable', 'spacelab', 'united', 'cosmo', 'lumen', 
                           'paper', 'sandstone', 'simplex', 'yeti'),
                selected = 'default'),
    selectInput(ns('highlight_style'), 'Highlight Style', 
                choices = c('default', 'github', 'tango', 'pygments', 'kate', 
                           'monochrome', 'espresso', 'zenburn', 'haddock'),
                selected = 'default'),
    checkboxInput(ns('toc'), 'Table of Contents', value = FALSE),
    checkboxInput(ns('code_fold'), 'Code Folding', value = FALSE),
    conditionalPanel(
      condition = paste0("input['", ns('toc'), "'] == true"),
      checkboxInput(ns('toc_float'), 'Floating TOC', value = FALSE),
      conditionalPanel(
        condition = paste0("input['", ns('toc_float'), "'] == true"),
        checkboxInput(ns('toc_collapsed'), 'Collapsed', value = TRUE),
        checkboxInput(ns('smooth_scroll'), 'Smooth Scroll', value = TRUE)
      )
    )
  )
}

pdf_format_options <- function(ns) {
  tagList(
    h4("PDF Options"),
    selectInput(ns('pdf_engine'), 'PDF Engine',
                choices = c('pdflatex', 'xelatex', 'lualatex'),
                selected = 'pdflatex'),
    selectInput(ns('documentclass'), 'Document Class',
                choices = c('article', 'report', 'book'),
                selected = 'article'),
    checkboxInput(ns('pdf_toc'), 'Table of Contents', value = FALSE),
    numericInput(ns('margin_left'), 'Left Margin (in)', value = 1, min = 0.5, max = 3, step = 0.1),
    numericInput(ns('margin_right'), 'Right Margin (in)', value = 1, min = 0.5, max = 3, step = 0.1)
  )
}

docx_format_options <- function(ns) {
  tagList(
    h4("Word Document Options"),
    checkboxInput(ns('docx_toc'), 'Table of Contents', value = FALSE),
    textInput(ns('reference_doc'), 'Reference Document', 
              placeholder = "Path to reference .docx file (optional)")
  )
}

revealjs_format_options <- function(ns) {
  tagList(
    h4("Presentation Options"),
    selectInput(ns('revealjs_theme'), 'Presentation Theme',
                choices = c('default', 'dark', 'white', 'league', 'beige', 'sky', 
                           'night', 'serif', 'simple', 'solarized'),
                selected = 'default'),
    checkboxInput(ns('incremental'), 'Incremental Slides', value = FALSE),
    checkboxInput(ns('smaller'), 'Smaller Text', value = FALSE)
  )
}

book_format_options <- function(ns) {
  tagList(
    h4("Book Options"),
    checkboxInput(ns('book_toc'), 'Table of Contents', value = TRUE),
    checkboxInput(ns('book_download'), 'Download Options', value = TRUE),
    textInput(ns('book_title'), 'Book Title', placeholder = "Override document title for book")
  )
}

# Helper function to build format configuration
get_format_config <- function(input, format) {
  config <- list()
  
  switch(format,
    "html" = {
      html_config <- list()
      if (!is.null(input$theme) && input$theme != "default") {
        html_config$theme <- input$theme
      }
      if (!is.null(input$highlight_style) && input$highlight_style != "default") {
        html_config$`highlight-style` <- input$highlight_style
      }
      if (!is.null(input$toc)) {
        html_config$toc <- input$toc
      }
      if (!is.null(input$code_fold)) {
        html_config$`code-fold` <- input$code_fold
      }
      if (!is.null(input$toc_float) && input$toc_float) {
        toc_float_config <- list()
        if (!is.null(input$toc_collapsed)) {
          toc_float_config$collapsed <- input$toc_collapsed
        }
        if (!is.null(input$smooth_scroll)) {
          toc_float_config$smooth_scroll <- input$smooth_scroll
        }
        if (length(toc_float_config) > 0) {
          html_config$toc_float <- toc_float_config
        }
      }
      config$html <- html_config
    },
    
    "pdf" = {
      pdf_config <- list()
      if (!is.null(input$pdf_engine)) {
        pdf_config$`pdf-engine` <- input$pdf_engine
      }
      if (!is.null(input$documentclass)) {
        pdf_config$documentclass <- input$documentclass
      }
      if (!is.null(input$pdf_toc)) {
        pdf_config$toc <- input$pdf_toc
      }
      
      # Add margin settings if specified
      geometry <- c()
      if (!is.null(input$margin_left)) {
        geometry <- c(geometry, paste0("left=", input$margin_left, "in"))
      }
      if (!is.null(input$margin_right)) {
        geometry <- c(geometry, paste0("right=", input$margin_right, "in"))
      }
      if (length(geometry) > 0) {
        pdf_config$geometry <- paste(geometry, collapse = ", ")
      }
      
      config$pdf <- pdf_config
    },
    
    "docx" = {
      docx_config <- list()
      if (!is.null(input$docx_toc)) {
        docx_config$toc <- input$docx_toc
      }
      if (!is.null(input$reference_doc) && nchar(trimws(input$reference_doc)) > 0) {
        docx_config$`reference-doc` <- input$reference_doc
      }
      config$docx <- docx_config
    },
    
    "revealjs" = {
      revealjs_config <- list()
      if (!is.null(input$revealjs_theme) && input$revealjs_theme != "default") {
        revealjs_config$theme <- input$revealjs_theme
      }
      if (!is.null(input$incremental)) {
        revealjs_config$incremental <- input$incremental
      }
      if (!is.null(input$smaller)) {
        revealjs_config$smaller <- input$smaller
      }
      config$revealjs <- revealjs_config
    },
    
    "book" = {
      book_config <- list()
      if (!is.null(input$book_toc)) {
        book_config$toc <- input$book_toc
      }
      if (!is.null(input$book_download)) {
        book_config$downloads <- c("pdf", "epub")
      }
      if (!is.null(input$book_title) && nchar(trimws(input$book_title)) > 0) {
        book_config$title <- input$book_title
      }
      config$book <- book_config
    }
  )
  
  return(config)
}

# Helper function to clean YAML data
clean_yaml_data <- function(yaml_data) {
  if (is.list(yaml_data)) {
    # Remove NULL values
    yaml_data <- yaml_data[!sapply(yaml_data, is.null)]
    
    # Remove empty strings
    yaml_data <- yaml_data[!sapply(yaml_data, function(x) {
      is.character(x) && length(x) == 1 && nchar(trimws(x)) == 0
    })]
    
    # Recursively clean nested lists
    yaml_data <- lapply(yaml_data, function(x) {
      if (is.list(x)) {
        clean_yaml_data(x)
      } else {
        x
      }
    })
    
    # Remove empty lists
    yaml_data <- yaml_data[!sapply(yaml_data, function(x) {
      is.list(x) && length(x) == 0
    })]
  }
  
  return(yaml_data)
}
