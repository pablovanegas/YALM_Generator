# ==============================================================================
# YALM_Generator - v2.6 (Copy Fix)
#
# - Corregida la salida de valores lógicos para ser compatibles con YAML 1.2.
# - Corregido el valor de `code-copy` para que sea 'hover' como espera Quarto.
# - Corregida la función de copiar para incluir los delimitadores ---.
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


# 2. INTERFAZ DE USUARIO (UI) - REDISEÑO UX
# ==============================================================================
ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "minty", primary = "#007A33"),
  useShinyjs(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$link(rel = "stylesheet", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css")
  ),
  titlePanel(
    div(
      class = "title-container",
      img(src = "unal.png", height = "50px"),
      h1("YALM Generator")
    )
  ),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      accordion(
        id = "config_accordion",
        open = "step1",
        accordion_panel(
          title = tagList(icon("file-alt"), "Paso 1: Metadatos del Documento"),
          value = "step1",
          textInput("title", "Título:", value = "", placeholder = "Ej: Mi Reporte Anual"),
          textInput("subtitle", "Subtítulo:", value = "", placeholder = "Ej: Resultados Q3"),
          textInput("author", "Autor:", value = "", placeholder = "Ej: Juan Pérez"),
          radioButtons('date_type', 'Fecha:', choices = c('Fecha del sistema' = 'system', 'Elegir fecha' = 'custom'), selected = 'system'),
          conditionalPanel(condition = "input.date_type == 'custom'", dateInput('date', NULL, value = Sys.Date())),
          textAreaInput("description", "Descripción:", value = "", rows = 2, placeholder = "Una breve descripción del documento..."),
          hr(),
          h5("Metadatos Avanzados"),
          textAreaInput("abstract", "Resumen (Abstract):", value = "", rows = 4, placeholder = "Escriba aquí el resumen del documento..."),
          textInput("keywords", "Palabras Clave:", value = "", placeholder = "Ej: R, Shiny, Análisis de Datos"),
          textInput("bibliography", "Archivo de Bibliografía:", value = "", placeholder = "Ej: references.bib")
        ),
        accordion_panel(
          title = tagList(icon("cogs"), "Paso 2: Formato de Salida y Opciones"),
          value = "step2",
          selectInput("output_format", "Formato:", choices = c('HTML' = 'html', 'PDF' = 'pdf', 'Word' = 'docx', 'Presentation' = 'revealjs'), selected = 'html'),
          div(
            class = "format-options-box",
            conditionalPanel(condition = "input.output_format == 'html'", 
                             h5("Opciones HTML"),
                             selectInput('theme', 'Tema', choices = c('default', 'cerulean', 'journal', 'flatly', 'darkly', 'readable', 'spacelab'), selected = 'default'),
                             selectInput('highlight_style', 'Resaltado de Código', choices = c('default', 'github', 'tango', 'pygments', 'kate', 'monochrome'), selected = 'default'),
                             checkboxInput('toc', 'Tabla de Contenidos', value = FALSE),
                             checkboxInput('code_fold', 'Plegar Código', value = FALSE),
                             checkboxInput('toc_float', 'TOC Flotante', value = FALSE),
                             conditionalPanel(condition = "input.toc_float == true", checkboxInput('toc_collapsed', 'Colapsar TOC', value = TRUE), checkboxInput('smooth_scroll', 'Scroll Suave', value = TRUE)),
                             hr(),
                             h6("Opciones Avanzadas de HTML"),
                             checkboxInput('html_number_sections', 'Numerar Secciones', value = FALSE),
                             checkboxInput('html_code_copy', 'Botón para Copiar Código (al pasar mouse)', value = TRUE),
                             checkboxInput('html_code_link', 'Enlace al Código Fuente', value = FALSE)
            ),
            conditionalPanel(condition = "input.output_format == 'pdf'", 
                             h5("Opciones PDF"),
                             selectInput('pdf_engine', 'Motor PDF', choices = c('pdflatex', 'xelatex', 'lualatex'), selected = 'pdflatex'),
                             selectInput('documentclass', 'Clase de Documento', choices = c('article', 'report', 'book'), selected = 'article'),
                             checkboxInput("pdf_toc", "Tabla de Contenidos", value = TRUE),
                             hr(),
                             h6("Tipografía y Diseño (Avanzado)"),
                             selectInput('pdf_papersize', 'Tamaño de Papel', choices = c('a4', 'letter', 'legal', 'a5'), selected = 'a4'),
                             selectInput('pdf_fontsize', 'Tamaño de Fuente', choices = c('10pt', '11pt', '12pt'), selected = '11pt'),
                             textInput('pdf_mainfont', 'Fuente Principal', placeholder = "Ej: Times New Roman"),
                             textInput('pdf_margin', 'Márgenes', placeholder = "Ej: 1in o 2.5cm")
            ),
            conditionalPanel(condition = "input.output_format == 'docx'", 
                             h5("Opciones de Word"),
                             checkboxInput("docx_toc", "Tabla de Contenidos", value = TRUE)
            ),
            conditionalPanel(condition = "input.output_format == 'revealjs'", 
                             h5("Opciones de Presentación"),
                             selectInput('revealjs_theme', 'Tema', choices = c('default', 'dark', 'solarized', 'white'), selected = 'default'),
                             checkboxInput('incremental', 'Diapositivas Incrementales', value = FALSE)
            )
          )
        ),
        accordion_panel(
          title = tagList(icon("code"), "Paso 2.5: Opciones Globales de Ejecución"),
          value = "step2_5",
          p("Controla el comportamiento por defecto de todos los bloques de código.", style = "color: #6c757d; font-size: 0.9em;"),
          checkboxInput('exec_echo', 'Mostrar código fuente (echo)', value = TRUE),
          checkboxInput('exec_eval', 'Ejecutar código (eval)', value = TRUE),
          checkboxInput('exec_warning', 'Mostrar advertencias (warning)', value = TRUE),
          checkboxInput('exec_error', 'Mostrar errores y detener (error)', value = FALSE)
        ),
        accordion_panel(
          title = tagList(icon("tasks"), "Paso 3: Acciones y Archivos"),
          value = "step3",
          fileUploadUI("file_loader"),
          docActionsUI("doc_actions")
        )
      )
    ),
    mainPanel(
      width = 8,
      tabsetPanel(
        id = "main_tabs",
        tabPanel("Instrucciones", 
                 div(img(src = "portada.png", width = "100%", style = "margin-bottom: 20px; border-radius: 5px;"), 
                     h3("Bienvenido a YALM Generator"), 
                     p("Esta aplicación le ayuda a generar cabeceras YAML para sus documentos de Quarto y R Markdown."), 
                     h4("¿Cómo usar?"), 
                     tags$ol(
                       tags$li("Complete los metadatos del documento en el ", strong("Paso 1.")),
                       tags$li("Elija y configure el formato de salida en el ", strong("Paso 2.")),
                       tags$li("Defina el comportamiento global del código en el ", strong("Paso 2.5.")),
                       tags$li("Previsualice el YAML en la pestaña 'YAML Preview'."),
                       tags$li("Opcionalmente, cargue un documento existente usando las opciones del ", strong("Paso 3.")),
                       tags$li("Descargue el documento completo o limpie el formulario para empezar de nuevo.")
                     )
                 )
        ),
        tabPanel("YAML Preview", 
                 div(style = "display: flex; align-items: center; justify-content: space-between;",
                     h3("Cabecera YAML Generada", style = "margin: 0;"),
                     actionButton("copy_yaml_button", "Copiar YAML", icon = icon("copy"), class = "btn-sm")
                 ),
                 verbatimTextOutput("yaml_display"), 
                 br(), 
                 uiOutput("yaml_validation")
        )
      )
    )
  )
)

# 3. LÓGICA DEL SERVIDOR (CON ADICIONES)
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
    # --- Metadatos Básicos ---
    if (!is.null(input$title) && nchar(trimws(input$title)) > 0) yaml_list$title <- input$title
    if (!is.null(input$subtitle) && nchar(trimws(input$subtitle)) > 0) yaml_list$subtitle <- input$subtitle
    if (!is.null(input$author) && nchar(trimws(input$author)) > 0) yaml_list$author <- input$author
    if (input$date_type == 'custom' && !is.null(input$date)) { yaml_list$date <- as.character(input$date) } else { yaml_list$date <- "`r Sys.Date()`" }
    if (!is.null(input$description) && nchar(trimws(input$description)) > 0) yaml_list$description <- input$description
    
    # --- Metadatos Avanzados (FASE 1) ---
    if (!is.null(input$abstract) && nchar(trimws(input$abstract)) > 0) yaml_list$abstract <- input$abstract
    if (!is.null(input$keywords) && nchar(trimws(input$keywords)) > 0) {
      keywords_vec <- trimws(strsplit(input$keywords, ",")[[1]])
      yaml_list$keywords <- keywords_vec
    }
    if (!is.null(input$bibliography) && nchar(trimws(input$bibliography)) > 0) yaml_list$bibliography <- input$bibliography
    
    # --- Configuración de Formato (FASE 2) ---
    format_config <- build_format_config(input)
    if (length(format_config) > 0) yaml_list$format <- format_config
    
    # --- Opciones Globales de Ejecución (FASE 3) ---
    exec_config <- list()
    if (!input$exec_echo) exec_config$echo <- FALSE
    if (!input$exec_eval) exec_config$eval <- FALSE
    if (!input$exec_warning) exec_config$warning <- FALSE
    if (input$exec_error) exec_config$error <- TRUE 
    if (length(exec_config) > 0) yaml_list$execute <- exec_config
    
    return(yaml_list)
  })
  
  yaml_string <- reactive({
    tryCatch({
      yaml_data_clean <- clean_yaml_list(yaml_data())
      if (length(yaml_data_clean) == 0) { values$validation_errors <- NULL; return("---\n# No configuration specified\n---") }
      
      # Handler personalizado para forzar la salida de 'true'/'false' para Quarto
      handlers <- list(
        logical = function(x) {
          result <- ifelse(x, 'true', 'false')
          class(result) <- "verbatim" # Evita que la librería `yaml` lo entrecomille
          return(result)
        },
        character = function(x) {
          if (grepl("\n", x)) {
            return(paste0("|\n", gsub("^", "  ", x)))
          }
          return(x)
        }
      )
      
      yaml_str <- yaml::as.yaml(yaml_data_clean, indent = 2, indent.mapping.sequence = TRUE, handlers = handlers)
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
  
  # Observador para el botón de copiar YAML
  observeEvent(input$copy_yaml_button, {
    text_to_copy <- yaml_string()
    # La línea que eliminaba los '---' ha sido removida.
    
    # Escapamos los caracteres que podrían romper el string de JS (` y \)
    js_sanitized_text <- gsub("\\\\", "\\\\\\\\", text_to_copy)
    js_sanitized_text <- gsub("`", "\\\\`", js_sanitized_text)
    
    js_code <- sprintf("
      const textToCopy = `%s`;
      const el = document.createElement('textarea');
      el.value = textToCopy;
      el.setAttribute('readonly', '');
      el.style.position = 'absolute';
      el.style.left = '-9999px';
      document.body.appendChild(el);
      el.select();
      document.execCommand('copy');
      document.body.removeChild(el);
    ", js_sanitized_text)
    
    shinyjs::runjs(js_code)
    showNotification("YAML copiado al portapapeles!", type = "message", duration = 3)
  })
}

# 4. FUNCIONES AUXILIARES Y LLAMADA A LA APP
# ==============================================================================
`%||%` <- function(a, b) { if (is.null(a)) b else a }

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
             toc_float_config <- list(collapsed = input$toc_collapsed, smooth_scroll = input$smooth_scroll)
             html_config$toc_float <- toc_float_config
           }
           if (input$html_number_sections) html_config$`number-sections` <- TRUE
           if (input$html_code_copy) html_config$`code-copy` <- "hover"
           if (input$html_code_link) html_config$`code-link` <- TRUE
           config$html <- html_config
         },
         "pdf" = {
           pdf_config <- list()
           pdf_config$`pdf-engine` <- input$pdf_engine
           pdf_config$documentclass <- input$documentclass
           if (input$pdf_toc) pdf_config$toc <- TRUE
           pdf_config$papersize <- input$pdf_papersize
           pdf_config$fontsize <- input$pdf_fontsize
           if (nchar(trimws(input$pdf_mainfont)) > 0) pdf_config$mainfont <- input$pdf_mainfont
           if (nchar(trimws(input$pdf_margin)) > 0) pdf_config$margin <- input$pdf_margin
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


clean_yaml_list <- function(yaml_list) { if (!is.list(yaml_list)) return(yaml_list); yaml_list <- yaml_list[!sapply(yaml_list, is.null)]; yaml_list <- yaml_list[!sapply(yaml_list, function(x) is.character(x) && length(x) == 1 && nchar(trimws(x)) == 0)]; yaml_list <- lapply(yaml_list, function(x) { if (is.list(x)) clean_yaml_list(x) else x }); yaml_list <- yaml_list[!sapply(yaml_list, function(x) is.list(x) && length(x) == 0)]; return(yaml_list) }

populate_form_from_yaml <- function(session, input, yaml_data) { 
  if (is.null(yaml_data)) return()
  
  # Helper
  update_input <- function(id, value, is_text = FALSE) {
    if (!is.null(value)) {
      if (is_text) {
        updateTextInput(session, id, value = value)
      } else if (is.logical(value)) {
        updateCheckboxInput(session, id, value = value)
      } else if (is.list(value) || length(value) > 1) {
        updateTextInput(session, id, value = paste(value, collapse = ", "))
      } else {
        updateSelectInput(session, id, selected = value)
      }
    }
  }
  
  # Poblar metadatos
  updateTextInput(session, "title", value = yaml_data$title %||% "")
  updateTextInput(session, "subtitle", value = yaml_data$subtitle %||% "")
  updateTextInput(session, "author", value = yaml_data$author %||% "")
  updateTextAreaInput(session, "description", value = yaml_data$description %||% "")
  updateTextAreaInput(session, "abstract", value = yaml_data$abstract %||% "")
  updateTextInput(session, "bibliography", value = yaml_data$bibliography %||% "")
  update_input("keywords", yaml_data$keywords)
  
  # Poblar fecha
  if (!is.null(yaml_data$date)) {
    if (yaml_data$date == "`r Sys.Date()`") {
      updateRadioButtons(session, "date_type", selected = "system")
    } else {
      updateRadioButtons(session, "date_type", selected = "custom")
      tryCatch({ updateDateInput(session, "date", value = as.Date(yaml_data$date)) }, error = function(e) {})
    }
  }
  
  # Poblar Opciones de Ejecución (FASE 3)
  if (!is.null(yaml_data$execute)) {
    exec_conf <- yaml_data$execute
    updateCheckboxInput(session, "exec_echo", value = exec_conf$echo %||% TRUE)
    updateCheckboxInput(session, "exec_eval", value = exec_conf$eval %||% TRUE)
    updateCheckboxInput(session, "exec_warning", value = exec_conf$warning %||% TRUE)
    updateCheckboxInput(session, "exec_error", value = exec_conf$error %||% FALSE)
  } else {
    updateCheckboxInput(session, "exec_echo", value = TRUE)
    updateCheckboxInput(session, "exec_eval", value = TRUE)
    updateCheckboxInput(session, "exec_warning", value = TRUE)
    updateCheckboxInput(session, "exec_error", value = FALSE)
  }
  
  # Poblar formato
  if (!is.null(yaml_data$format)) {
    format_name <- names(yaml_data$format)[1]
    if (format_name == input$output_format) {
      config <- yaml_data$format[[1]]
      switch(format_name,
             "html" = { 
               update_input("theme", config$theme)
               update_input("highlight_style", config$`highlight-style`)
               update_input("toc", config$toc)
               update_input("code_fold", config$`code-fold`)
               update_input("toc_float", !is.null(config$toc_float))
               if (!is.null(config$toc_float)) {
                 update_input("toc_collapsed", config$toc_float$collapsed)
                 update_input("smooth_scroll", config$toc_float$smooth_scroll)
               }
               update_input("html_number_sections", config$`number-sections`)
               updateCheckboxInput(session, "html_code_copy", value = !is.null(config$`code-copy`) && config$`code-copy` == "hover")
               update_input("html_code_link", config$`code-link`)
             },
             "pdf" = {
               update_input("pdf_engine", config$`pdf-engine`)
               update_input("documentclass", config$documentclass)
               update_input("pdf_toc", config$toc)
               update_input("pdf_papersize", config$papersize)
               update_input("pdf_fontsize", config$fontsize)
               update_input("pdf_mainfont", config$mainfont, is_text = TRUE)
               update_input("pdf_margin", config$margin, is_text = TRUE)
             },
             "docx" = { update_input("docx_toc", config$toc) },
             "revealjs" = {
               update_input("revealjs_theme", config$theme)
               update_input("incremental", config$incremental)
             }
      )
    } else {
      updateSelectInput(session, "output_format", selected = format_name)
    }
  }
}

shinyApp(ui = ui, server = server)

