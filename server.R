server <- function(input, output, session) {
  # Reactive variable to store the original file content
  originalFileContent <- reactiveVal()
  
  # Observe file upload
  observeEvent(input$file1, {
    req(input$file1)
    # Read the file content
    fileContent <- readLines(input$file1$datapath)
    # Store the original file content
    originalFileContent(fileContent)
    # Update Ace editor with file content
    updateAceEditor(session, "code", value = paste(fileContent, collapse = "\n"), theme = "monokai")
  })
  
  # Function to generate YAML header
  generate_yaml <- function() {
    c(
      "---",
      paste0("author: ", input$author),
      paste0("date: ", if(input$date_type == 'custom') {input$date} else {'"`r Sys.Date()`"'}),
      paste0("description: ", input$description),
      "format:",
      "  html:",
      paste0("    highlight-style: ", input$highlight_style),
      paste0("    theme: ", input$theme),
      paste0("    toc: ", ifelse(input$toc, "true", "false")),
      "    code-fold: true",
      paste0("    file: ", tools::file_path_sans_ext(input$file1$name), ".qmd"),
      paste0("    title: ", input$title),
      paste0('    subtitle: "', input$subtitle, '"'),
      "---"
    )
  }
  
  # Observe generate button
  observeEvent(input$generate, {
    # Generate YAML header
    yaml_header <- generate_yaml()
    # Combine YAML header and original file content
    new_fileContent <- c(yaml_header, originalFileContent())
    # Update Ace editor with new file content
    updateAceEditor(session, "code", value = paste(new_fileContent, collapse = "\n"))
  })
  
  # Download handler
  output$Download <- downloadHandler(
    
    filename = function() {
      paste(tools::file_path_sans_ext(input$file1$name), ".", input$format, sep = "")
    },
    content = function(file) {
      # Generate YAML header
      yaml_header <- generate_yaml()
      # Combine YAML header and original file content
      new_fileContent <- c(yaml_header, originalFileContent())
      # Write the new file content to the file
      writeLines(new_fileContent, file)
    }
  )
}
