# Server function
server <- function(input, output, session) {
  # Observe file upload
  observeEvent(input$file1, {
    req(input$file1)
    # Read the file content
    fileContent <- readLines(input$file1$datapath)
    
    # Modify the file content based on the user's selections
    fileContent <- gsub("title: .*", paste0("title: ", input$title), fileContent)
    fileContent <- gsub("author: .*", paste0("author: ", input$author), fileContent)
    fileContent <- gsub("date: .*", paste0("date: ", if(input$date_type == 'custom') {input$date} else {'"`r Sys.Date()`"'}), fileContent)
    fileContent <- gsub("description: .*", paste0("description: ", input$description), fileContent)
    fileContent <- gsub("highlight-style: .*", paste0("highlight-style: ", input$highlight_style), fileContent)
    fileContent <- gsub("theme: .*", paste0("theme: ", input$theme), fileContent)
    fileContent <- gsub("toc: .*", paste0("toc: ", ifelse(input$toc, "true", "false")), fileContent)
    
    # Update Ace editor with modified file content
    updateAceEditor(session, "code", value = paste(fileContent, collapse = "\n"))
  })
  
  # Download handler
  output$Download <- downloadHandler(
    filename = function() {
      paste(tools::file_path_sans_ext(input$file1$name), ".", input$format, sep = "")
    },
    content = function(file) {
      writeLines(input$code, file)
    }
  )
}
