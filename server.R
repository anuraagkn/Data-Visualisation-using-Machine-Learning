library(ggplot2)

shinyServer(function(input, output){
  
  .theme<- theme(
    axis.line = element_line(colour = 'gray', size = .75), 
    panel.background = element_blank(),  
    plot.background = element_blank()
  )	
  
  # file info
  output$dynamicPlot <- renderPlot({
    ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point() + ggtitle(input$title_text_box_id)
  })
})