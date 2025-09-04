# ==============================================================================
# Módulo de Shiny: Acciones del Documento
#
# Este módulo encapsula los botones de acción principales como
# la descarga del documento y la limpieza del formulario.
# ==============================================================================

#' docActionsUI
#'
#' Crea la UI para el módulo de acciones.
#'
#' @param id El namespace del módulo.
#'
#' @return Un tagList con los elementos de la UI.
docActionsUI <- function(id) {
  ns <- NS(id)
  tagList(
    hr(),
    h4("Document Actions"),
    div(
      style = "display: flex; justify-content: space-between; align-items: center;",
      downloadButton(ns("download_document"), "Download Document", 
                     class = "btn-success", 
                     icon = icon("download")),
      actionButton(ns("clear_form"), "Clear Form", 
                   class = "btn-warning", 
                   icon = icon("eraser"))
    )
  )
}

#' docActionsServer
#'
#' Define la lógica del servidor para el módulo de acciones.
#'
#' @param id El namespace del módulo.
#' @param yaml_string Una expresión reactiva que devuelve el YAML actual.
#' @param content_string Una expresión reactiva que devuelve el contenido del editor.
#' @param file_name_reactive Una expresión reactiva que devuelve el nombre del archivo cargado.
#' @param title_reactive Una expresión reactiva que devuelve el título del documento.
#'
#' @return Una señal reactiva (`clear_trigger`) que se activa cuando se hace
#'         clic en el botón de limpiar.
docActionsServer <- function(id, yaml_string, content_string, file_name_reactive, title_reactive) {
  moduleServer(id, function(input, output, session) {
    
    # Manejador de Descarga (encapsulado dentro del módulo)
    output$download_document <- downloadHandler(
      filename = function() {
        file_name <- file_name_reactive()
        if (!is.null(file_name) && nchar(file_name) > 0) {
          return(file_name)
        } else {
          return(generate_filename(title_reactive(), "html"))
        }
      },
      content = function(file) {
        tryCatch({
          content <- content_string()
          final_content <- combine_yaml_and_content(yaml_string(), strsplit(content, "\n")[[1]])
          writeLines(final_content, file)
        }, error = function(e) {
          writeLines(c("# Error generating document", paste("# Error:", e$message)), file)
        })
      }
    )
    
    # Valor reactivo para comunicar el clic de "limpiar" a la app principal
    clear_trigger <- reactiveVal(0)
    observeEvent(input$clear_form, {
      clear_trigger(clear_trigger() + 1)
    })
    
    return(clear_trigger)
  })
}
