# File Input Module
# This module handles file upload and content management

file_input_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("File Upload"),
    
    fileInput(ns('file_upload'), 
              'Choose .qmd or .md File',
              accept = c('.qmd', '.md', '.Rmd', '.rmd'),
              placeholder = "No file selected"),
    
    # File status
    uiOutput(ns("file_status")),
    
    # File info
    uiOutput(ns("file_info"))
  )
}

file_input_Server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values
    values <- reactiveValues(
      file_content = NULL,
      file_name = NULL,
      file_extension = NULL,
      upload_status = NULL
    )
    
    # Handle file upload
    observeEvent(input$file_upload, {
      req(input$file_upload)
      
      # Extract file information
      file_name <- input$file_upload$name
      file_ext <- tools::file_ext(file_name)
      
      # Validate file extension
      valid_extensions <- c("qmd", "md", "Rmd", "rmd")
      if (!file_ext %in% valid_extensions) {
        values$upload_status <- list(
          type = "error",
          message = paste("Invalid file type. Please upload a file with extension:", 
                         paste(valid_extensions, collapse = ", "))
        )
        values$file_content <- NULL
        values$file_name <- NULL
        values$file_extension <- NULL
        return()
      }
      
      # Try to read file
      tryCatch({
        file_content <- readLines(input$file_upload$datapath, warn = FALSE)
        
        # Store file information
        values$file_content <- file_content
        values$file_name <- file_name
        values$file_extension <- file_ext
        values$upload_status <- list(
          type = "success",
          message = paste("File", file_name, "uploaded successfully")
        )
        
      }, error = function(e) {
        values$upload_status <- list(
          type = "error", 
          message = paste("Error reading file:", e$message)
        )
        values$file_content <- NULL
        values$file_name <- NULL
        values$file_extension <- NULL
      })
    })
    
    # File status UI
    output$file_status <- renderUI({
      if (!is.null(values$upload_status)) {
        status <- values$upload_status
        
        if (status$type == "success") {
          tags$div(
            class = "alert alert-success",
            icon("check-circle"),
            " ", status$message
          )
        } else {
          tags$div(
            class = "alert alert-danger",
            icon("exclamation-triangle"), 
            " ", status$message
          )
        }
      }
    })
    
    # File info UI
    output$file_info <- renderUI({
      if (!is.null(values$file_content) && !is.null(values$file_name)) {
        tagList(
          h4("File Information"),
          tags$ul(
            tags$li("Name: ", tags$strong(values$file_name)),
            tags$li("Type: ", tags$strong(toupper(values$file_extension))),
            tags$li("Lines: ", tags$strong(length(values$file_content))),
            tags$li("Size: ", tags$strong(paste(object.size(values$file_content), "bytes")))
          )
        )
      }
    })
    
    # Return reactive values for parent module
    return(list(
      file_content = reactive(values$file_content),
      file_name = reactive(values$file_name),
      file_extension = reactive(values$file_extension),
      has_file = reactive(!is.null(values$file_content))
    ))
  })
}
