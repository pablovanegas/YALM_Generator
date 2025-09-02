# Minimal working YALM Generator
# This version focuses on core functionality without complex UI elements

library(shiny)
library(shinyAce)
library(yaml)

# Simple UI
ui <- fluidPage(
  titlePanel("YALM Generator - Minimal Version"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Document Metadata"),
      textInput("title", "Title:", value = "My Document"),
      textInput("author", "Author:", value = "Author Name"),
      dateInput("date", "Date:", value = Sys.Date()),
      
      selectInput("output_format", "Output Format:",
                  choices = list(
                    "HTML Document" = "html_document",
                    "PDF Document" = "pdf_document", 
                    "Word Document" = "word_document"
                  ),
                  selected = "html_document"),
      
      br(),
      downloadButton("download_yaml", "Download YAML", class = "btn-primary")
    ),
    
    mainPanel(
      h3("YAML Preview"),
      verbatimTextOutput("yaml_output"),
      
      hr(),
      
      h3("Document Editor"),
      aceEditor("content_editor", 
               mode = "markdown",
               value = "# Your Document\n\nStart writing here...",
               height = "300px")
    )
  )
)

# Simple Server
server <- function(input, output, session) {
  
  # Generate YAML
  generate_yaml <- reactive({
    yaml_data <- list()
    
    if (input$title != "") yaml_data$title <- input$title
    if (input$author != "") yaml_data$author <- input$author
    yaml_data$date <- as.character(input$date)
    
    # Add format
    format_name <- input$output_format
    yaml_data$output <- structure(list(), names = format_name)
    
    return(yaml_data)
  })
  
  # Display YAML
  output$yaml_output <- renderText({
    yaml_result <- yaml::as.yaml(generate_yaml(), indent = 2)
    paste0("---\n", yaml_result, "---")
  })
  
  # Download handler
  output$download_yaml <- downloadHandler(
    filename = function() {
      paste0("document_", Sys.Date(), ".yaml")
    },
    content = function(file) {
      yaml_content <- yaml::as.yaml(generate_yaml(), indent = 2)
      yaml_formatted <- paste0("---\n", yaml_content, "---")
      writeLines(yaml_formatted, file)
    }
  )
}

# Run the app
cat("Starting minimal YALM Generator...\n")
shinyApp(ui = ui, server = server)
