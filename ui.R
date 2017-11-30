shinyUI(fluidPage(
  titlePanel("Data Visualizer"),
  
  sidebarLayout(
    sidebarPanel("Settings:",
                 
                 selectInput('element_id', label = 'Select an algorithm', choices = c("PCA","LDA", "tSNE")), # algorithm list
                  textInput('title_text_box_id', label = 'Enter a title for the plot')), # plot title
    mainPanel("Output:",
              plotOutput('dynamicPlot'))
  )
))