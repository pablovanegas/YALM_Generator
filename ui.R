
library(shiny)
library(shinyAce)
library(shinythemes)
ui <- fluidPage(
  #shinythemes::themeSelector(),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
    tags$script(src = "script.js")
  ),
  titlePanel("HEADER GENERATOR"),
  
  sidebarLayout(
    
    sidebarPanel(
      img(src="portada.png", height=200, width=200),
      textInput('title', 'Title'),
      textInput('subtitle', 'Subtitle'),
      textInput('author', 'Author'),
      radioButtons('date_type', 'Date', 
                   choices = c('Use system date' = '`r Sys.Date()`', 'Enter a date' = 'custom')),
      conditionalPanel(
        condition = "input.date_type == 'custom'",
        dateInput('date', 'Date', value = Sys.Date())
      ),
      textInput('description', 'Description'),
      selectInput('highlight_style', 'Choose highlight style', 
                  choices = c('default', 'github', 'tango', 'pygments', 'kate', 'monochrome', 'espresso', 'zenburn', 'haddock')),
      selectInput('theme', 'Choose theme', 
                  choices = c('cerulean', 'journal', 'flatly', 'darkly', 'readable', 'spacelab', 'united', 'cosmo', 'lumen', 'paper', 'sandstone', 'simplex', 'yeti')),
      checkboxInput('toc', 'Table of Contents', value = FALSE),
      checkboxInput('code-fold', 'Code fold', value = FALSE),
      checkboxInput('toc-float', 'Table of Contents Float', value = FALSE),
      conditionalPanel(
        condition = "input['toc-float'] == true",
        checkboxInput('collapsed', 'Collapsed', value = TRUE),
        checkboxInput('smooth_scroll', 'Smooth Scroll', value = TRUE)
      ),
  
      helpText(),
      fileInput('file1', 'Choose qmd or Markdown File',
                accept=c('text/qmd', 'text/markdown',
                         'text/comma-separated-values,text/plain',
                         '.csv', '.qmd', '.md')), # End of file input
      downloadButton('Download', 'Download Document'),
      actionButton('generate', 'Generate YAML Header'),
      tags$a(href = "https://github.com/pablovanegas/runr", target = "_blank", class = "btn btn-default shiny-bound-input", "Ver Código Fuente"),
      tags$a(href = "https://huggingface.co/spaces/pajuan/bbbb", target = "_blank", class = "btn btn-default shiny-bound-input", "Markdown generator") 
    ), # End of sidebar panel
    
    mainPanel(
      h2("This is an app to help you create YALM headers for your documents"),
      # User Guide #generate the .col-sm-8 class 
      tags$div( 
        h2("User Guide", class = 'guide-title'), 
        tags$ol(
          tags$li("If you're going to update your document make sure to delete this before you begin."),
          tags$li("Custom your header: Enter your own title, author, and description in the respective text input fields on the sidebar panel."),
          tags$li("Choose the Date: You have the option to use the system date or enter a custom date. Select your preference using the 'Date' radio buttons."),
          tags$li("Select Highlight Style and Theme: Choose your preferred highlight style and theme from the dropdown menus."),
          tags$li("Table of Contents and Code Fold: If you want a table of contents or code fold in your document, check the respective boxes."),
          tags$li("Upload Your File: Choose the QMD or Markdown file you want to work with using the 'Choose qmd or Markdown File' button."),
          tags$li("Download Your Document: Once you're done, click the 'Download Document' button to get your document.")
        ),
        p("Remember, make sure to decide on the parameters before inserting the archive or you can generate the header without the file and click on generate YALM"),
      ), # End of User Guide
      
      
      img(src = "guia.png", height = 300, width = 1000, align = "center"),
      br(),
      p(" "),
      p("Your YALM : "),
      tableOutput("contents"),
      
      fluidRow(
        column(12, wellPanel(
          aceEditor('code', mode = 'r', theme = 'chaos', fontSize = 12, height = '400px')
        ))
      ) # End of fluid row
      
    ) #End Main Panel
    
  ) # End Slider Layout
  
)  # End of fluid page
