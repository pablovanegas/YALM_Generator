# ==============================================================================
# Módulo de Shiny: Carga de Archivos
#
# Este módulo encapsula la UI y la lógica del servidor para cargar
# un documento (.qmd, .md, .Rmd), leer su contenido y extraer el YAML.
# ==============================================================================

#' fileUploadUI
#'
#' Crea la UI para el módulo de carga de archivos.
#'
#' @param id El namespace del módulo.
#'
#' @return Un tagList con los elementos de la UI.
fileUploadUI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("File Operations"),
    fileInput(ns("file_upload"), "Upload Document:",
              accept = c(".md", ".Rmd", ".qmd"),
              multiple = FALSE)
  )
}

#' fileUploadServer
#'
#' Define la lógica del servidor para el módulo de carga de archivos.
#'
#' @param id El namespace del módulo.
#'
#' @return Una lista reactiva con los datos del archivo cargado:
#'         - name: Nombre del archivo.
#'         - content: Contenido completo como vector de líneas.
#'         - original_content: Contenido sin el bloque YAML.
#'         - loaded_yaml: La lista de R parseada desde el YAML.
fileUploadServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    file_data <- reactive({
      req(input$file_upload)
      
      file_info <- input$file_upload
      
      # Validar extensión
      if (!validate_file_extension(file_info$name)) {
        showNotification("Please upload a .qmd, .md, or .Rmd file", type = "error", duration = 5)
        return(NULL)
      }
      
      # Leer archivo
      read_result <- safe_read_file(file_info$datapath)
      
      if (read_result$success) {
        content <- read_result$content
        
        # Extraer YAML y contenido
        existing_yaml <- extract_yaml_from_content(content)
        original_content <- remove_yaml_from_content(content)
        
        showNotification(paste("File", file_info$name, "loaded successfully"), type = "message", duration = 3)
        
        # Retornar una lista con toda la información procesada
        return(list(
          name = file_info$name,
          content = content,
          original_content = original_content,
          loaded_yaml = existing_yaml
        ))
      } else {
        showNotification(paste("Error reading file:", read_result$error), type = "error", duration = 5)
        return(NULL)
      }
    })
    
    return(file_data)
  })
}
