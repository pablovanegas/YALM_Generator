server <- function(input, output, session) {
  # Reactive values for storing file and YAML data
  values <- reactiveValues(
    file_content = NULL,
    file_name = NULL,
    original_content = NULL,
    yaml_data = NULL,
    validation_errors = NULL
  )
  
  # File upload handler with robust error handling
  observeEvent(input$file_upload, {
    req(input$file_upload)
    
    # Validate file extension
    file_ext <- tools::file_ext(input$file_upload$name)
    if (!validate_file_extension(input$file_upload$name)) {
      showNotification(
        "Please upload a .qmd, .md, or .Rmd file", 
        type = "error", 
        duration = 5
      )
      return()
    }
    
    # Read file safely
    read_result <- safe_read_file(input$file_upload$datapath)
    
    if (read_result$success) {
      values$file_content <- read_result$content
      values$file_name <- input$file_upload$name
      
      # Extract existing YAML if present
      existing_yaml <- extract_yaml_from_content(read_result$content)
      
      # Remove YAML from content to get clean content
      values$original_content <- remove_yaml_from_content(read_result$content)
      
      # Update document editor with clean content
      updateAceEditor(
        session, 
        "document_editor", 
        value = paste(values$original_content, collapse = "\n")
      )
      
      # If existing YAML found, populate form fields
      if (!is.null(existing_yaml)) {
        populate_form_from_yaml(session, existing_yaml)
      }
      
      showNotification(
        paste("File", input$file_upload$name, "loaded successfully"), 
        type = "message", 
        duration = 3
      )
      
    } else {
      showNotification(
        paste("Error reading file:", read_result$error), 
        type = "error", 
        duration = 5
      )
      values$file_content <- NULL
      values$file_name <- NULL
      values$original_content <- NULL
    }
  })
  
  # Reactive YAML generation
  yaml_data <- reactive({
    # Build YAML structure based on inputs
    yaml_list <- list()
    
    # Basic metadata
    if (!is.null(input$title) && nchar(trimws(input$title)) > 0) {
      yaml_list$title <- input$title
    }
    
    if (!is.null(input$subtitle) && nchar(trimws(input$subtitle)) > 0) {
      yaml_list$subtitle <- input$subtitle
    }
    
    if (!is.null(input$author) && nchar(trimws(input$author)) > 0) {
      yaml_list$author <- input$author
    }
    
    # Date handling
    if (input$date_type == 'custom' && !is.null(input$date)) {
      yaml_list$date <- as.character(input$date)
    } else {
      yaml_list$date <- "`r Sys.Date()`"
    }
    
    if (!is.null(input$description) && nchar(trimws(input$description)) > 0) {
      yaml_list$description <- input$description
    }
    
    # Format configuration
    format_config <- build_format_config(input)
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
      
      # Clean the data (remove empty values)
      yaml_data_clean <- clean_yaml_list(yaml_data_current)
      
      if (length(yaml_data_clean) == 0) {
        values$validation_errors <- NULL
        return("---\n# No configuration specified\n---")
      }
      
      # Generate YAML
      yaml_str <- yaml::as.yaml(yaml_data_clean, indent = 2)
      
      # Add delimiters
      yaml_with_delim <- paste0("---\n", yaml_str, "---")
      
      values$validation_errors <- NULL
      return(yaml_with_delim)
      
    }, error = function(e) {
      values$validation_errors <- e$message
      return(paste0("# Error generating YAML: ", e$message))
    })
  })
  
  # Real-time YAML preview output
  output$yaml_display <- renderText({
    yaml_string()
  })
  
  # Validation status output
  output$yaml_validation <- renderUI({
    if (is.null(values$validation_errors)) {
      div(
        style = "color: #28a745; background-color: #d4edda; border: 1px solid #c3e6cb; padding: 10px; border-radius: 4px;",
        icon("check-circle"),
        strong(" YAML is valid and ready to use")
      )
    } else {
      div(
        style = "color: #721c24; background-color: #f8d7da; border: 1px solid #f5c6cb; padding: 10px; border-radius: 4px;",
        icon("exclamation-triangle"),
        strong(" YAML Validation Error: "),
        br(),
        values$validation_errors
      )
    }
  })
  
  # Download handler
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
        # Get current document content
        current_content <- input$document_editor
        content_lines <- if (!is.null(current_content) && nchar(trimws(current_content)) > 0) {
          strsplit(current_content, "\n")[[1]]
        } else if (!is.null(values$original_content)) {
          values$original_content
        } else {
          c(
            "# Your Document Title",
            "",
            "Write your content here...",
            "",
            "## Section 1", 
            "",
            "Content for section 1.",
            "",
            "## Section 2",
            "",
            "Content for section 2."
          )
        }
        
        # Combine YAML and content
        final_content <- combine_yaml_and_content(yaml_string(), content_lines)
        
        # Write to file
        writeLines(final_content, file)
        
      }, error = function(e) {
        # Write error information
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

# Helper function to build format configuration
build_format_config <- function(input) {
  config <- list()
  
  switch(input$output_format,
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
      
      # TOC float configuration
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
      
      config$pdf <- pdf_config
    },
    
    "docx" = {
      docx_config <- list()
      
      if (!is.null(input$docx_toc)) {
        docx_config$toc <- input$docx_toc
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
      
      config$revealjs <- revealjs_config
    }
  )
  
  return(config)
}

# Helper function to clean YAML list
clean_yaml_list <- function(yaml_list) {
  if (!is.list(yaml_list)) return(yaml_list)
  
  # Remove NULL values
  yaml_list <- yaml_list[!sapply(yaml_list, is.null)]
  
  # Remove empty strings
  yaml_list <- yaml_list[!sapply(yaml_list, function(x) {
    is.character(x) && length(x) == 1 && nchar(trimws(x)) == 0
  })]
  
  # Recursively clean nested lists
  yaml_list <- lapply(yaml_list, function(x) {
    if (is.list(x)) {
      clean_yaml_list(x)
    } else {
      x
    }
  })
  
  # Remove empty lists
  yaml_list <- yaml_list[!sapply(yaml_list, function(x) {
    is.list(x) && length(x) == 0
  })]
  
  return(yaml_list)
}

# Helper function to populate form from existing YAML
populate_form_from_yaml <- function(session, yaml_data) {
  if (is.null(yaml_data)) return()
  
  # Update basic fields
  if (!is.null(yaml_data$title)) {
    updateTextInput(session, "title", value = yaml_data$title)
  }
  
  if (!is.null(yaml_data$subtitle)) {
    updateTextInput(session, "subtitle", value = yaml_data$subtitle)
  }
  
  if (!is.null(yaml_data$author)) {
    updateTextInput(session, "author", value = yaml_data$author)
  }
  
  if (!is.null(yaml_data$description)) {
    updateTextAreaInput(session, "description", value = yaml_data$description)
  }
  
  # Handle date
  if (!is.null(yaml_data$date)) {
    if (yaml_data$date == "`r Sys.Date()`") {
      updateRadioButtons(session, "date_type", selected = "system")
    } else {
      updateRadioButtons(session, "date_type", selected = "custom")
      tryCatch({
        date_val <- as.Date(yaml_data$date)
        updateDateInput(session, "date", value = date_val)
      }, error = function(e) {
        # If date parsing fails, keep system date
      })
    }
  }
  
  # Handle format-specific settings
  if (!is.null(yaml_data$format)) {
    if (!is.null(yaml_data$format$html)) {
      updateSelectInput(session, "output_format", selected = "html")
      html_config <- yaml_data$format$html
      
      if (!is.null(html_config$theme)) {
        updateSelectInput(session, "theme", selected = html_config$theme)
      }
      
      if (!is.null(html_config$`highlight-style`)) {
        updateSelectInput(session, "highlight_style", selected = html_config$`highlight-style`)
      }
      
      if (!is.null(html_config$toc)) {
        updateCheckboxInput(session, "toc", value = html_config$toc)
      }
      
      if (!is.null(html_config$`code-fold`)) {
        updateCheckboxInput(session, "code_fold", value = html_config$`code-fold`)
      }
    } else if (!is.null(yaml_data$format$pdf)) {
      updateSelectInput(session, "output_format", selected = "pdf")
      # Add PDF-specific updates here
    } else if (!is.null(yaml_data$format$docx)) {
      updateSelectInput(session, "output_format", selected = "docx")
      # Add DOCX-specific updates here
    } else if (!is.null(yaml_data$format$revealjs)) {
      updateSelectInput(session, "output_format", selected = "revealjs")
      # Add RevealJS-specific updates here
    }
  }
}
