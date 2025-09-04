# ==============================================================================
# YALM_Generator - Versión Refactorizada
#
# - Eliminadas las pestañas y la lógica para "Document Editor" e "Import from YAML"
#   para simplificar la interfaz y el flujo de trabajo.
# ==============================================================================

# 1. LIBRERÍAS, FUNCIONES GLOBALES Y MÓDULOS
# ==============================================================================
library(shiny)
library(shinyjs)
library(yaml)
library(bslib)

source("modules/fileUploadUI.R")
source("modules/doc_actions_module.R")

options(shiny.maxRequestSize = 30*1024^2)

# Funciones de utilidad que permanecen globales
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
  }, error = function(e) { NULL })
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
  if (is.null(content) || length(content) == 0 || (length(content) == 1 && content == "")) {
    content <- c("", "# Your Document Title", "", "Write your content here...")
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
  return(paste0(clean_title, ".qmd"))
}


# 2. INTERFAZ DE USUARIO (UI)
# ==============================================================================
ui <- fluidPage(
  theme = bslib::bs_theme(version = 4, bootswatch = "flatly"),
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$style(HTML(".format-options-box { background-color: #f7f7f7; border: 1px solid #e3e3e3; border-radius: 5px; padding: 15px; margin-top: 10px; }"))
  ),
  titlePanel(div(img(src = "unal.png", height = "50px", style = "float: left; margin-right: 15px;"), h1("YALM Generator", style = "margin: 0; line-height: 50px;"), style = "overflow: hidden; margin-bottom: 20px;")),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      div(id = "form",
          h3("Document Metadata"),
          textInput("title", "Title:", value = ""),
          textInput("subtitle", "Subtitle:", value = ""),
          textInput("author", "Author:", value = ""),
          radioButtons('date_type', 'Date', choices = c('Use system date' = 'system', 'Enter a date' = 'custom'), selected = 'system'),
          conditionalPanel(condition = "input.date_type == 'custom'", dateInput('date', 'Custom Date', value = Sys.Date())),
          textAreaInput("description", "Description:", value = "", rows = 2),
          hr(),
          h4("Output Format"),
          selectInput("output_format", "Format:", choices = c('HTML' = 'html', 'PDF' = 'pdf', 'Word' = 'docx', 'Presentation' = 'revealjs'), selected = 'html'),
          conditionalPanel(condition = "input.output_format == 'html'", div(class = "format-options-box", h5("HTML Options"), selectInput('theme', 'Theme', choices = c('default', 'cerulean', 'journal', 'flatly', 'darkly', 'readable', 'spacelab'), selected = 'default'), selectInput('highlight_style', 'Highlight Style', choices = c('default', 'github', 'tango', 'pygments', 'kate', 'monochrome'), selected = 'default'), checkboxInput('toc', 'Table of Contents', value = FALSE), checkboxInput('code_fold', 'Code Folding', value = FALSE), checkboxInput('toc_float', 'Floating TOC', value = FALSE), conditionalPanel(condition = "input.toc_float == true", checkboxInput('toc_collapsed', 'Collapse TOC', value = TRUE), checkboxInput('smooth_scroll', 'Smooth Scroll', value = TRUE) ))),
          conditionalPanel(condition = "input.output_format == 'pdf'", div(class = "format-options-box", h5("PDF Options"), selectInput('pdf_engine', 'PDF Engine', choices = c('pdflatex', 'xelatex', 'lualatex'), selected = 'pdflatex'), selectInput('documentclass', 'Document Class', choices = c('article', 'report', 'book'), selected = 'article'), checkboxInput("pdf_toc", "Table of Contents", value = TRUE))),
          conditionalPanel(condition = "input.output_format == 'docx'", div(class = "format-options-box", h5("Word Options"), checkboxInput("docx_toc", "Table of Contents", value = TRUE))),
          conditionalPanel(condition = "input.output_format == 'revealjs'", div(class = "format-options-box", h5("Presentation Options"), selectInput('revealjs_theme', 'Theme', choices = c('default', 'dark', 'solarized', 'white'), selected = 'default'), checkboxInput('incremental', 'Incremental Slides', value = FALSE)))
      ),
      hr(),
      fileUploadUI("file_loader"),
      docActionsUI("doc_actions")
    ),
    mainPanel(width = 8, tabsetPanel(id = "main_tabs", 
                                     tabPanel("Instructions", div(img(src = "portada.jpg", width = "100%", style = "margin-bottom: 20px; border-radius: 5px;"), h3("Welcome to YALM Generator"), p("This application helps you generate YAML headers for your Quarto and R Markdown documents."), h4("How to use:"), tags$ol(tags$li("Fill in the document metadata."), tags$li("Choose and configure the output format."), tags$li("Preview the YAML in the 'YAML Preview' tab."), tags$li("Optionally, upload an existing document."), tags$li("Download the complete document or clear the form to start over.")))), 
                                     tabPanel("YAML Preview", h3("Generated YAML Header"), verbatimTextOutput("yaml_display"), br(), uiOutput("yaml_validation"))
    ))
  )
)

# 3. LÓGICA DEL SERVIDOR
# ==============================================================================
server <- function(input, output, session) {
  values <- reactiveValues(file_name = NULL, original_content = NULL, loaded_yaml = NULL, validation_errors = NULL)
  
  uploaded_file_data <- fileUploadServer("file_loader")
  
  observeEvent(uploaded_file_data(), {
    data <- uploaded_file_data(); req(data)
    values$file_name <- data$name
    values$original_content <- data$original_content
    values$loaded_yaml <- data$loaded_yaml
    if (!is.null(data$loaded_yaml)) { populate_form_from_yaml(session, input, data$loaded_yaml) }
  })
  
  observeEvent(input$output_format, {
    if (!is.null(values$loaded_yaml)) { populate_form_from_yaml(session, input, values$loaded_yaml) }
  })
  
  yaml_data <- reactive({
    yaml_list <- list()
    if (!is.null(input$title) && nchar(trimws(input$title)) > 0) yaml_list$title <- input$title
    if (!is.null(input$subtitle) && nchar(trimws(input$subtitle)) > 0) yaml_list$subtitle <- input$subtitle
    if (!is.null(input$author) && nchar(trimws(input$author)) > 0) yaml_list$author <- input$author
    if (input$date_type == 'custom' && !is.null(input$date)) { yaml_list$date <- as.character(input$date) } else { yaml_list$date <- "`r Sys.Date()`" }
    if (!is.null(input$description) && nchar(trimws(input$description)) > 0) yaml_list$description <- input$description
    format_config <- build_format_config(input)
    if (length(format_config) > 0) yaml_list$format <- format_config
    return(yaml_list)
  })
  
  yaml_string <- reactive({
    tryCatch({
      yaml_data_clean <- clean_yaml_list(yaml_data())
      if (length(yaml_data_clean) == 0) { values$validation_errors <- NULL; return("---\n# No configuration specified\n---") }
      yaml_str <- yaml::as.yaml(yaml_data_clean, indent = 2)
      values$validation_errors <- NULL; return(paste0("---\n", yaml_str, "---"))
    }, error = function(e) { values$validation_errors <- e$message; return(paste0("# Error generating YAML: ", e$message)) })
  })
  
  output$yaml_display <- renderText({ yaml_string() })
  output$yaml_validation <- renderUI({ if (is.null(values$validation_errors)) { div(style = "color: #28a745; background-color: #d4edda; border: 1px solid #c3e6cb; padding: 10px; border-radius: 4px; margin-top: 10px;", icon("check-circle"), strong(" YAML is valid")) } else { div(style = "color: #721c24; background-color: #f8d7da; border: 1px solid #f5c6cb; padding: 10px; border-radius: 4px; margin-top: 10px;", icon("exclamation-triangle"), strong(" YAML Validation Error: "), br(), values$validation_errors) } })
  
  clear_trigger <- docActionsServer("doc_actions", yaml_string = yaml_string, content_string = reactive(values$original_content), file_name_reactive = reactive(values$file_name), title_reactive = reactive(input$title))
  
  observeEvent(clear_trigger(), {
    if (clear_trigger() > 0) {
      reset("form")
      values$file_name <- NULL
      values$original_content <- NULL
      values$loaded_yaml <- NULL
      showNotification("Form cleared.", type = "message")
    }
  })
}

# 4. FUNCIONES AUXILIARES Y LLAMADA A LA APP
# ==============================================================================
`%||%` <- function(a, b) { if (is.null(a)) b else a }

build_format_config <- function(input) { config <- list(); switch(input$output_format, "html" = { html_config <- list(); if (input$theme != "default") html_config$theme <- input$theme; if (input$highlight_style != "default") html_config$`highlight-style` <- input$highlight_style; if (input$toc) html_config$toc <- TRUE; if (input$code_fold) html_config$`code-fold` <- TRUE; if (input$toc_float) { toc_float_config <- list(collapsed = input$toc_collapsed, smooth_scroll = input$smooth_scroll); html_config$toc_float <- toc_float_config }; config$html <- html_config }, "pdf" = { pdf_config <- list(); pdf_config$`pdf-engine` <- input$pdf_engine; pdf_config$documentclass <- input$documentclass; if (input$pdf_toc) pdf_config$toc <- TRUE; config$pdf <- pdf_config }, "docx" = { docx_config <- list(); if (input$docx_toc) docx_config$toc <- TRUE; config$docx <- docx_config }, "revealjs" = { revealjs_config <- list(); if (input$revealjs_theme != "default") revealjs_config$theme <- input$revealjs_theme; if (input$incremental) revealjs_config$incremental <- TRUE; config$revealjs <- revealjs_config }); return(config) }

clean_yaml_list <- function(yaml_list) { if (!is.list(yaml_list)) return(yaml_list); yaml_list <- yaml_list[!sapply(yaml_list, is.null)]; yaml_list <- yaml_list[!sapply(yaml_list, function(x) is.character(x) && length(x) == 1 && nchar(trimws(x)) == 0)]; yaml_list <- lapply(yaml_list, function(x) { if (is.list(x)) clean_yaml_list(x) else x }); yaml_list <- yaml_list[!sapply(yaml_list, function(x) is.list(x) && length(x) == 0)]; return(yaml_list) }

populate_form_from_yaml <- function(session, input, yaml_data) { if (is.null(yaml_data)) return(); update_input <- function(id, value) { if (!is.null(value)) { if (is.logical(value)) { updateCheckboxInput(session, id, value = value) } else { updateSelectInput(session, id, selected = value) } } }; updateTextInput(session, "title", value = yaml_data$title %||% ""); updateTextInput(session, "subtitle", value = yaml_data$subtitle %||% ""); updateTextInput(session, "author", value = yaml_data$author %||% ""); updateTextAreaInput(session, "description", value = yaml_data$description %||% ""); if (!is.null(yaml_data$date)) { if (yaml_data$date == "`r Sys.Date()`") { updateRadioButtons(session, "date_type", selected = "system") } else { updateRadioButtons(session, "date_type", selected = "custom"); tryCatch({ updateDateInput(session, "date", value = as.Date(yaml_data$date)) }, error = function(e) {}) } }; if (!is.null(yaml_data$format)) { format_name <- names(yaml_data$format)[1]; if (format_name == input$output_format) { config <- yaml_data$format[[1]]; switch(format_name, "html" = { update_input("theme", config$theme); update_input("highlight_style", config$`highlight-style`); update_input("toc", config$toc); update_input("code_fold", config$`code-fold`); update_input("toc_float", !is.null(config$toc_float)); if (!is.null(config$toc_float)) { update_input("toc_collapsed", config$toc_float$collapsed); update_input("smooth_scroll", config$toc_float$smooth_scroll) } }, "pdf" = { update_input("pdf_engine", config$`pdf-engine`); update_input("documentclass", config$documentclass); update_input("pdf_toc", config$toc) }, "docx" = { update_input("docx_toc", config$toc) }, "revealjs" = { update_input("revealjs_theme", config$theme); update_input("incremental", config$incremental) }) } else { updateSelectInput(session, "output_format", selected = format_name) } } }

shinyApp(ui = ui, server = server)
