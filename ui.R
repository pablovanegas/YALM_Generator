library(shiny)
library(shinyAce)
ui <- fluidPage(
  titlePanel("QMD RMARKDOWN HEADER GENERATOR"),
  
  sidebarLayout(
    
    sidebarPanel(
      img(src="portada.png", height=200, width=200),
      textInput('title', 'Title'),
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
      checkboxInput('toc', 'Table of Contents', value = TRUE),
      checkboxInput('code-fold', 'Code fold', value = TRUE),
      helpText(),
      fileInput('file1', 'Choose qmd or Markdown File',
                accept=c('text/qmd', 'text/markdown',
                         'text/comma-separated-values,text/plain',
                         '.csv', '.qmd', '.md')), # End of file input
      radioButtons('format', 'Document format', 
                   choices = c('qmd', 'md')),
      downloadButton('Download', 'Download Document'),
    ), # End of sidebar panel
    
    mainPanel(
      h1("Make your qmd, Rmarkdown better"),
      h2("This is a simple  app to help you create qmd, Rmarkdown documents"),
      p("Insert your qmd, Rmarkdown content in the editor and click the run button to see the output"),
      # User Guide
      tags$div(
        h2("User Guide"),
        p("Welcome to the QMD RMarkdown Header Generator! This application helps you create QMD and RMarkdown documents with ease. Follow these simple steps to get started:"),
        tags$ol(
          tags$li("Delete the Default YAML Header: The application starts with a default YAML header. Make sure to delete this before you begin."),
          tags$li("Insert Your Own Header: Enter your own title, author, and description in the respective text input fields on the sidebar panel."),
          tags$li("Choose the Date: You have the option to use the system date or enter a custom date. Select your preference using the 'Date' radio buttons."),
          tags$li("Select Highlight Style and Theme: Choose your preferred highlight style and theme from the dropdown menus."),
          tags$li("Table of Contents and Code Fold: If you want a table of contents or code fold in your document, check the respective boxes."),
          tags$li("Upload Your File: Choose the QMD or Markdown file you want to work with using the 'Choose qmd or Markdown File' button."),
          tags$li("Choose the Document Format: Select the format you want your document to be in (QMD, MD, PDF, HTML, DOCX) using the 'Document format' radio buttons."),
          tags$li("Download Your Document: Once you're done, click the 'Download Document' button to get your document.")
        ),
        p("Remember, make sure to decide on the parameters before inserting the archive. Enjoy creating your QMD, RMarkdown documents!"),
        p("Make sure to ", tags$b("DELETE"), " the default YAML header"),
        
      ), # End of User Guide
      
      
      img(src = "guia.png", height = 300, width = 1000, align = "center"),
      br(),
      p("☕️ Support me:", a(href="https://www.buymeacoffee.com/juvanegas", "Buymeacoffe", target="_blank")),
      
      p("GitHub:", "🚀", a(href = "https://github.com/pablovanegas", "GitHubProfile",target = "_blank")),
      p(" "),
      p("Your Input: "),
      tableOutput("contents"),
      
      fluidRow(
        column(12, wellPanel(
          aceEditor('code', mode = 'r', theme = 'twilight', fontSize = 12, height = '400px')
        ))
      ) # End of fluid row
      
    ) #End Main Panel
    
  ) # End Slider Layout
  
)  # End of fluid page
