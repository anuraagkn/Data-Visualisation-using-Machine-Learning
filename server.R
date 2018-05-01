# let user select default size
# add threshold function
# for iteration update message: x <- epoch; epoch_callback(x+epoch)
# add epoch option
# ----------- packages -------------
require(DT) # datatable
require(ggplot2) # plots
require(tsne)
require(ggfortify)
require(scales)
require(MASS) # lda
require(screeplot)

#require(shinyjs)
# max data size
options(shiny.maxRequestSize=200*1024^2)

shinyServer(function(input, output, session){
  
  # ---------- display input data ---------
  output$data_in <- renderDataTable({
    
    validate( # printing message to user
      need(input$file != '', 'Please load a data set to analyse.')
    )
    
    assign('c', input$center, envir = .GlobalEnv) # centering variable
    assign('s', input$scale, envir = .GlobalEnv) # scaling variable
    
    if(is.null(input$file)){return()} else{
      assign('df', read.csv(input$file$datapath, header= T, stringsAsFactors = T), envir = .GlobalEnv)
      assign('df_na', na.omit(df), envir = .GlobalEnv) # removing rows containing NA values
      assign('numeric', sapply(df_na, is.numeric), envir = .GlobalEnv) # checking for numeric columns
      assign('factor', sapply(df_na, is.factor), envir = .GlobalEnv) # checking for non-numeric columns
      assign('df_num', df_na[, numeric], envir = .GlobalEnv) # creating numeric data frame for scaling
      assign('df_scale', scale(df_num, center = c, scale = s), envir = .GlobalEnv)
      #assign('df_fac', df_na[, factor], envir = .GlobalEnv)
      #assign('df_facnames', names(Filter(is.factor, df_na)), envir = .GlobalEnv) # alternative to 'factor'
      assign('df_fac', df_na[factor], envir = .GlobalEnv) # creating non-numeric data set
      assign('df_factor', data.frame(df_fac), envir = .GlobalEnv) # may not be needed - CHECK!!
      assign('df_comp', data.frame(cbind(df_fac, df_scale)), envir = .GlobalEnv)
      assign('df_glyph', data.frame(cbind(df_fac, df_num)), envir = .GlobalEnv) # to embed varaibles into glyphs. Could just use df_na. No time to test.
      #assign('df_na[, numeric]', scale(df_na[, numeric], center = c, scale = s), envir = .GlobalEnv)
      
      df_na[, numeric] <- scale(df_na[, numeric], center = c, scale = s) # applying c & s to the numeric data
      
      # formatting the data table
      datatable(
        df_na, extensions = 'Buttons', class = 'row-border hover compact order-column cell-border'
      ) 
    }
  })
  
  output$data_in_csv <- downloadHandler(
    filename = function() { paste(input$file, '.csv', sep='') },
    content = function(file) {
      write.csv(df_comp, file, row.names = FALSE)
    }
  )
  
  # updating lists of columns
  observeEvent(input$file,{
    #req(input$file$datapath)
    dfnames <- names(df_glyph)
    col_options <- list()
    col_options[dfnames] <- dfnames
    
    updateSelectInput(session, 'x_var_in',
                      label = 'X-axis variable: ',
                      choices = c(col_options),
                      selected = '')
    
    updateSelectInput(session, 'y_var_in',
                      label = 'Y-axis variable: ',
                      choices = c(col_options),
                      selected = '')
    
    updateSelectInput(session, 'col_in',
                      label = 'Colour: ',
                      choices = c('Blue (default)', col_options),
                      selected = 'Blue (default)')
    
    updateSelectInput(session, 'shape_in',
                      label = 'Shape: ',
                      choices = c('Default', col_options),
                      selected = 'Default')
    
    updateSelectInput(session, 'size_in',
                      label = 'Size: ',
                      choices = c('Default', col_options),
                      selected = 'Default')  
    
    updateSelectInput(session, 'group_options',
                      label = 'Grouping: ',
                      choices = c(col_options),
                      selected = '')
    
    updateSelectInput(session, 'col_out',
                      label = 'Colour: ',
                      choices = c('Black (Default)', col_options),
                      selected = 'Black (Default)')
    
    updateSelectInput(session, 'shape_out',
                      label = 'Shape: ',
                      choices = c('Default', col_options),
                      selected = 'Default')
    
    updateSelectInput(session, 'size_out',
                      label = 'Size: ',
                      choices = c('Default', col_options),
                      selected = 'Default')
    
    updateSelectInput(session, 'text_out',
                      label = 'Text: ',
                      choices = c(col_options),
                      selected = col_options[1])
  })
  
  plot_in <- reactive({
    if(input$col_in == 'Blue (default)'){col_aes <- 1}else{col_aes <- df_comp[,input$col_in]}
    if(input$size_in == 'Default'){size_aes <- 1}else{size_aes <- df_comp[, input$size_in]}
    if(input$shape_in == 'Default'){shape_aes <- 'Default'}else{shape_aes <- shape_aes <- df_comp[, input$shape_in]}
    
    x <- input$x_var_in
    y <- input$y_var_in
    
    #grid
    if(input$grid_major_in == TRUE){grid_major_in <- element_line(colour = 'grey')}else{grid_major_in <- element_blank()}
    if(input$grid_minor_in == TRUE){grid_minor_in <- element_line(colour = 'grey')}else{grid_minor_in <- element_blank()}
    
    #border
    if(input$border_in == TRUE){border_in <- element_rect(colour = 'black', fill = NA)}else{border_in <- element_blank()}
    
    # axes
    if(input$axes_in == TRUE){
      lines_in <- element_line(colour = 'black')
      values_in <- element_text(colour = 'black')
      ticks_in <- element_line(colour = 'black')
    }else{
      lines_in <- element_blank()
      values_in <- element_blank()
      ticks_in <- element_blank()
    }
    
    #axis labels
    if(input$labels_in == TRUE){labels_in <- element_text(colour = 'black')}else{labels_in <- element_blank()}
    
    theme <- theme(panel.border = border_in, panel.grid.major = grid_major_in, 
                   panel.grid.minor = grid_minor_in, panel.background = element_blank(),
                   axis.title = labels_in, axis.text = values_in, axis.ticks = ticks_in, axis.line = lines_in)
    
    
    plot.in <- ggplot(df_comp) + geom_point(aes(df_comp[,x], df_comp[,y], col = col_aes, size = size_aes, shape = shape_aes)) + 
      labs(x = paste(names(x)),
           y = paste(names(y)),
           title = input$plot_title_in, caption = input$plot_caption_in,
           colour = input$col_in, size = input$size_in, shape = input$shape_in) +
      theme
    
  })
  
  # ---------------- performing the analysis ---------
  analysis <- reactive({
    
    assign('analysis_comp', FALSE, envir = .GlobalEnv) # flag to check if analysis is complete
    
    if(input$analyse){
      
      if(isolate(input$alg) == 'PCA'){ # isolate alg to stop it from running w/o clicking 'start'
        
        start_time <- proc.time() # current session running time
        assign('pca', prcomp(df_scale), envir = .GlobalEnv) # applying pca to numeric data
        proc_time <- proc.time() - start_time # time taken to run analysis
        assign('sec', proc_time[1], envir = .GlobalEnv) # make the time taken available globally
        assign('min', proc_time[1]/60, envir = .GlobalEnv) # time taken in minutes
        assign('var_pca', pca$sdev^2, envir = .GlobalEnv)
        assign('prop_pca', var_pca/sum(var_pca), envir = .GlobalEnv) # calculating explained variance
        assign('output_dataset', data.frame(pca = pca$x), envir = .GlobalEnv) # reduced dataset
        assign('exp_var_dataset', rbind(pca$rotation[ 0, ], 100*prop_pca), envir = .GlobalEnv)
        assign('var_dataset', rbind(pca$rotation[ 0, ], var_pca), envir = .GlobalEnv)
      }
      
      else if (isolate(input$alg) == 'LDA'){
        
        group_name = isolate(input$group_options) # grouping
        
        #group = df_comp[, group_name] # used for LDA
        #ex_num <- grep(user_ex, colnames(df_comp))
        #lda <- lda(as.formula(paste(user_ex, ".", sep = "~")), df) # LDA on the dataset
        #assign('lda',lda(as.formula(paste(user_ex, ".", sep = "~")), lda_in), envir = .GlobalEnv )
        #assign('lda',lda(df_na[,-ex_num], grouping = df_na[, user_ex]), envir = .GlobalEnv )
        
        start_time <- proc.time()
        assign('lda', lda(as.formula(paste(group_name, '.', sep = '~')), df_comp), envir = .GlobalEnv)
        proc_time <- proc.time() - start_time
        assign('sec', proc_time[1], envir = .GlobalEnv) # make the time taken available globally
        assign('min', proc_time[1]/60, envir = .GlobalEnv) # time taken in minutes
        assign('var_lda', lda$svd^2, envir = .GlobalEnv)
        assign('prop_lda', var_lda/sum(var_lda), envir = .GlobalEnv)
        
        plda <- predict(object = lda, newdata = df_na)
        
        assign('output_dataset', data.frame(lda = plda$x), envir = .GlobalEnv) # reduced dataset
        assign('exp_var_dataset', rbind(lda$scaling[ 0, ], 100*prop_lda), envir = .GlobalEnv)
        assign('var_dataset', rbind(lda$scaling[ 0, ], var_lda), envir = .GlobalEnv)
        
      }
      
      else if (isolate(input$alg) == 't-SNE'){
        
        #user_k <- as.numeric(input$k)
        assign('perp', as.numeric(isolate(input$perp)), envir = .GlobalEnv)
        assign('max_i', as.numeric(isolate(input$max_i)), envir = .GlobalEnv)
        user_min_cost <- as.numeric(isolate(input$min_cost))
        
        start_time <- proc.time()
        #ecb <- function(x,y){ plot(x,t='n'); text(x) } # not currently useful
        tsne_data <-  tsne(df_scale, perplexity = perp, max_iter = max_i, min_cost = user_min_cost)
        proc_time <- proc.time() - start_time
        assign('sec', proc_time[1], envir = .GlobalEnv) # make the time taken available globally
        assign('min', proc_time[1]/60, envir = .GlobalEnv) # time taken in minutes
        
        #tsne_df = data.frame(tsne_data)
        assign('output_dataset', data.frame(tsne_data), envir = .GlobalEnv)
        
      }
    }
    assign('ObservationID', 1:nrow(output_dataset), envir = .GlobalEnv)
    assign('names_data', cbind(ObservationID, output_dataset), envir = .GlobalEnv)
    
    # update the output list of options for variables to plot
    dfnames <- names(names_data)
    col_options <- list()
    col_options[dfnames] <- dfnames
    
    updateSelectInput(session, 'x_var_out',
                      label = 'X-axis variable: ',
                      choices = c(col_options),
                      selected = '')
    
    updateSelectInput(session, 'y_var_out',
                      label = 'Y-axis variable: ',
                      choices = c(col_options),
                      selected = '')
    
  })
  
  output$plot_in <- reactivePlot(function(){
    #name <- paste0(isolate(input$png_name), ".png")
    #  
    #  if(input$save_plot) { # saving plot
    #    ggsave(name, plot(), type="cairo-png") # cairo-png provides higher quality than png
    #  }
    if(input$print_plot_in){isolate(print(plot_in()))}
    # only print plot if 'print plot' is clicked
  })
  
  output$initiate_analysis <- renderText({
    if(input$analyse){
      print(analysis())
      "" # this is to start the analysis without having to go to the output data tabs or clicking the plot button
    }
  })
  
  output$proc_time <- renderText({
    if(input$analyse){
      paste('Time to run: ', sec, 's (', min, ' mins.)', sep = '')
      #validate('Time to run: ', seconds, 's (', minutes, ' mins.)', sep = '')
    }
  })
  
  output$data_out_csv <- downloadHandler(
    filename = function() { paste(input$alg, '_Data.csv', sep='') },
    content = function(file) {
      write.csv(output_dataset, file, row.names = FALSE)
    }
  )
  
  output$vectors_csv <- downloadHandler(
    filename = function() { paste(input$alg, '_Vectors.csv', sep='') },
    content = function(file) {
      write.csv(component_dataset, file, row.names = FALSE)
    }
  )
  
  output$exp_var_csv <- downloadHandler(
    filename = function() { paste(input$alg, '_Variance.csv', sep='') },
    content = function(file) {
      write.csv(exp_var_dataset, file, row.names = FALSE)
    }
  )
  
  # display transformed data
  output$data_out <- renderDataTable({
    
    if(input$analyse){
      # table
      
      datatable(
        output_dataset, extensions = 'Buttons', class = 'row-border hover compact order-column cell-border', 
        options = list(
        )
      )
      
    }
  })
  
  output$vectors <- renderDataTable({
    
    if(input$analyse){
      
      #print(analysis()) # get results of analysis
      
      # pca
      
      if(isolate(input$alg) == 'PCA'){component_dataset <- pca$rotation}
      
      # lda
      
      else if(isolate(input$alg) == 'LDA'){component_dataset <- lda$scaling}
      
      # tsne
      
      else if(isolate(input$alg) == 't-SNE'){validate("Not available for t-SNE")  }
      
      # table
      validate( # printing message to user
        need(input$analyse != '', 'Please load a data set to analyse.')
      )
      datatable(
        component_dataset, extensions = 'Buttons', class = 'row-border hover compact order-column cell-border'
      )
      
    }
  })
  
  output$exp_var_plot <- renderPlot({
    # table
    validate( # printing message to user
      need(input$analyse != '', 'Please load a data set to analyse.')
    )
    if(isolate(input$alg) == 't-SNE' || isolate(input$alg) == 'LDA'){validate("Not available for LDA or t-SNE")}
    
    #pca_info <- data.frame(pca$sdev^2)
    #n <- NROW(pca$info)
    #x <- c(1:n)
    if(input$analyse){
      if(isolate(input$alg == 'PCA')){
        screeplot(pca, npcs = 24, type = "lines")
        abline(h=1,lty=3, col="red")
      }
    }
  })
  
  output$exp_var <- renderDataTable({
    if(input$analyse){
      
      #print(analysis()) # get results of analysis
      
      # pca
      
      #if(isolate(input$alg) == 'PCA'){exp_var_dataset <- rbind(pca$rotation[ 0, ], 100*prop_pca)}
      
      # lda
      
      #else if(isolate(input$alg) == 'LDA'){exp_var_dataset <- rbind(lda$scaling[ 0, ], 100*prop_lda)}
      
      # table
      #validate( # printing message to user
      #  need(input$analyse != '', 'Please load a data set to analyse.')
      #)
      
      # tsne
      
      #if(isolate(input$alg) == 't-SNE'){validate("Not available for t-SNE")}
      #else{
        datatable( # t() transposes the data so that it is displayed as a column
          t(exp_var_dataset), extensions = 'Buttons', class = 'row-border hover compact order-column cell-border'
        )
      #}
      
      
      
    }
  })
  
  output$var <- renderDataTable({
    if(input$analyse){
      datatable( # t() transposes the data so that it is displayed as a column
        t(var_dataset), extensions = 'Buttons', class = 'row-border hover compact order-column cell-border'
      )
    }
  })
  
  plot_out <- reactive({
    if(input$analyse){
      
      # ---------- display options --------------
      
      #grid
      if(input$grid_major_out == TRUE){grid_major <- element_line(colour = 'grey')}else{grid_major <- element_blank()}
      if(input$grid_minor_out == TRUE){grid_minor <- element_line(colour = 'grey')}else{grid_minor <- element_blank()}
      
      #border
      if(input$border_out == TRUE){border <- element_rect(colour = 'black', fill = NA)}else{border <- element_blank()}
      
      # axes
      if(input$axes_out == TRUE){
        lines <- element_line(colour = 'black')
        values <- element_text(colour = 'black')
        ticks <- element_line(colour = 'black')
      }else{
          lines <- element_blank()
          values <- element_blank()
          ticks <- element_blank()
          }
      
      #axis values
      #if(input$values == TRUE){
      #  values <- element_text(colour = 'black')
      #  ticks <- element_line(colour = 'black')
      #}else{
      #  values <- element_blank()
      #  ticks <- element_blank()
      #}
      
      #axis labels
      if(input$labels_out == TRUE){labels <- element_text(colour = 'black')}else{labels <- element_blank()}
      
      if(input$legend_out == TRUE){legend_fill <- 'right'}
      else{legend_fill <- 'none'}
      
      #legend <- theme(legend.position="none")
      
      theme <- theme(panel.border = border, panel.grid.major = grid_major, 
                     panel.grid.minor = grid_minor, panel.background = element_blank(),
                     axis.title = labels, axis.text = values, axis.ticks = ticks, axis.line = lines, legend.position = legend_fill)
      # -------------- aesthetics ---------------
      
      #aesthetics
      if(input$col_out == 'Black (Default)'){col_aes <- 'Black (Default)'}else{col_aes <- df_glyph[,input$col_out]}
      if(input$size_out == 'Default'){size_aes <- 'Default'}else{size_aes <- df_glyph[, input$size_out]}
      if(input$shape_out == 'Default'){shape_aes <- 'Default'}else{shape_aes <- df_glyph[, input$shape_out]}
      #if(input$text_out == '.'){text_aes <- '.'}else{text_aes <- df_comp[, input$text_out]}
      text_aes <- df_glyph[, input$text_out]
      
      #if(input$glyph_out == 'shape'){
      #  shape_glyph <- geom_point(aes(pca.PC1, pca.PC2, col = col_aes, size = size_aes, shape = shape_aes))
      #  text_glyph <- NULL
      #}else if(input$glyph_out == 'Text'){
      #  shape_glyph <- NULL
      #  text_glyph <- geom_text(aes(pca.PC1, pca.PC2, col = col_aes, size = size_aes, label = text_aes), vjust = 0, nudge_y = 0.5, check_overlap = TRUE)
      #}else if(input$glyph_out == 'Both'){
      #  shape_glyph <- geom_point(aes(pca.PC1, pca.PC2, col = col_aes, size = size_aes, shape = shape_aes))
      #  text_glyph <- geom_text(aes(pca.PC1, pca.PC2, col = col_aes, label = text_aes), vjust = 0, nudge_y = 0.5, check_overlap = TRUE)
      #}
      
      # variables and axis names
      
      x_col_no <- grep(input$x_var_out, colnames(output_dataset))
      y_col_no <- grep(input$y_var_out, colnames(output_dataset))
     
      if(input$x_var_out == 'ObservationID'){
        x_var_out <- ObservationID
        x_lab_out <- paste('Observation number')
      }else{
          x_var_out <- output_dataset[,input$x_var_out]
          if(input$alg == 'PCA'){x_lab_out <- paste('PC', x_col_no, ' (', percent(prop_pca[x_col_no]),')', sep = '')}
          else if(input$alg == 'LDA'){x_lab_out <- paste('LD', x_col_no, ' (', percent(prop_lda[x_col_no]),')', sep = '')}
          else if(input$alg == 't-SNE'){x_lab_out <- paste('')}
          }
      if(input$y_var_out == 'ObservationID'){
        y_var_out <- ObservationID
        y_lab_out <- paste('Observation number')
      }else{
          y_var_out <- output_dataset[,input$y_var_out]
          if(input$alg == 'PCA'){y_lab_out <- paste('PC', y_col_no, ' (', percent(prop_pca[y_col_no]),')', sep = '')}
          else if(input$alg == 'LDA'){y_lab_out <- paste('LD', y_col_no, ' (', percent(prop_lda[y_col_no]),')', sep = '')}
          else if(input$alg == 't-SNE'){y_lab_out <- paste('')}
          }
      #x_var_out <- output_dataset[,input$x_var_out]
      #y_var_out <- output_dataset[,input$y_var_out]
      
      # labels
      #x_col_no <- grep(input$x_var_out, colnames(output_dataset))
      #y_col_no <- grep(input$y_var_out, colnames(output_dataset))
      #x_lab_out <- paste('PC', x_col_no, ' (', percent(prop_pca[x_col_no]),')', sep = '')
      #y_lab_out <- paste('PC', y_col_no, ' (', percent(prop_pca[y_col_no]),')', sep = '')
      
      # glyphs
      plain_shape <- geom_point(aes(x_var_out, y_var_out, shape = shape_aes))
      plain_shape2 <- geom_point(aes(x_var_out, y_var_out, shape = shape_aes, size = size_aes))
      plain_shape3 <- geom_point(aes(x_var_out, y_var_out, shape = shape_aes))
      #plain_shape3 <- geom_point(aes(x_var_out, y_var_out, col = col_aes, shape = shape_aes))
      plain_text <- geom_text(aes(x_var_out, y_var_out, label = text_aes), vjust = -1, check_overlap = TRUE)
      shape_glyph <- geom_point(aes(x_var_out, y_var_out, col = col_aes, size = size_aes, shape = shape_aes))
      text_glyph <- geom_text(aes(x_var_out, y_var_out, col = col_aes, label = text_aes), vjust = -1, check_overlap = TRUE)
      
      if(col_aes == 'Black (Default)'){
        if(size_aes == 'Default'){
          if(input$glyph_out == 'Shape'){
            glyph1 <- plain_shape3
            glyph2 <- NULL
          }else if(input$glyph_out == 'Text'){
            glyph1 <- NULL
            glyph2 <- plain_text
          }else if(input$glyph_out == 'Both'){
            glyph1 <- plain_shape
            glyph2 <- plain_text
          }
        }else{
          if(input$glyph_out == 'Shape'){
            glyph1 <- plain_shape2
            glyph2 <- NULL
          }else if(input$glyph_out == 'Text'){
            glyph1 <- NULL
            glyph2 <- plain_text
          }else if(input$glyph_out == 'Both'){
            glyph1 <- plain_shape2
            glyph2 <- plain_text
          }
        }
      }else{
        if(input$glyph_out == 'Shape'){
          glyph1 <- shape_glyph
          glyph2 <- NULL
        }else if(input$glyph_out == 'Text'){
          glyph1 <- NULL
          glyph2 <- text_glyph
        }else if(input$glyph_out == 'Both'){
          glyph1 <- shape_glyph
          glyph2 <- text_glyph
        }
      }
      
      plot.out <- ggplot(output_dataset) + glyph1 + glyph2 +  
        labs(x = x_lab_out,#paste("PC1",output_dataset[,x_var_out], "(", percent(prop_pca[1]), ")", sep=""),
             y = y_lab_out,
             title = input$plot_title, subtitle = input$plot_subtitle, caption = input$plot_caption,
             colour = input$col_out, size = input$size_out, shape = input$shape_out) +
        theme
      
      #if(isolate(input$alg) == 'PCA'){ # isolate alg to stop it from running w/o clicking 'start'
        
        #plot.pca <- ggplot(pca_dataset) + geom_point(aes(pca.PC1, pca.PC2, col = col_aes, size = size_aes, shape = shape_aes)) + 
        #labs(x = paste("PC1 (", percent(prop_pca[1]), ")", sep=""),
        #     y = paste("PC2 (", percent(prop_pca[2]), ")", sep=""),
        #     title = input$plot_title, subtitle = input$plot_subtitle, caption = input$plot_caption,
        #     colour = input$col_out, size = input$size_out, shape = input$shape_out) +
        #theme + geom_text(aes(pca.PC1, pca.PC2, col = col_aes, size = size_aes, label = col_aes))
        
      #  plot.pca <- ggplot(output_dataset) + glyph1 + glyph2 +  
      #    labs(x = x_lab_out,#paste("PC1",output_dataset[,x_var_out], "(", percent(prop_pca[1]), ")", sep=""),
      #         y = y_lab_out,
      #         title = input$plot_title, subtitle = input$plot_subtitle, caption = input$plot_caption,
      #         colour = input$col_out, size = input$size_out, shape = input$shape_out) +
      #    theme
      #}
      
      #else if (isolate(input$alg) == 'LDA'){
      #  
      #  plot.lda <- ggplot(lda_dataset) + geom_point(aes(lda_dataset[,1], y_lda, col = col_aes, size = size_aes, shape = shape_aes)) + 
      #    labs(x = paste("LD1 (", percent(prop_lda[1]), ")", sep=""),
      #         y = y_lda_label,
      #         title = input$plot_title, subtitle = input$plot_subtitle, caption = input$plot_caption,
      #         colour = input$col_out, size = input$size_out, shape = input$shape_out) + ggtitle(input$plot_title) + 
      #    theme
      #}
      
      #else if (isolate(input$alg) == 't-SNE'){
      #  
      #  plot.tsne <- ggplot(tsne_dataset) + geom_point(aes(tsne_dataset[,1], tsne_dataset[,2], col = col_aes, size = size_aes, shape = shape_aes)) +
      #    labs(x = paste(''),
      #         y = paste(''),
      #         title = input$plot_title, subtitle = input$plot_subtitle, caption = input$plot_caption,
      #         colour = input$col_out, size = input$size_out, shape = input$shape_out) + ggtitle(input$plot_title) + 
      #    theme
      #}
      
    }
  })
  
  output$initiate_analysis <- renderText({
    if(input$analyse){
      print(analysis())
      '' # this is to start the analysis without having to go to the output data tabs or clicking the plot button
    }
  })
  
  # -------------- outputs -------------
  
  #output$plot <- reactivePlot(function(){
  #  name <- paste0(isolate(input$png_name), ".png")
  #  
  #  if(input$save_plot) { # saving plot
  #    ggsave(name, plot(), type="cairo-png") # cairo-png provides higher quality than png
  #  }
  #  
  #  if(input$print_plot){
  #    isolate(print(plot()))
  #    
  #  } # only print plot if 'print plot' is clicked
  #})
  
  output$plot_out <- reactivePlot(function(){
    #name <- paste0(isolate(input$png_name), ".png")
    
    #if(isolate(input$save_plot)) { # saving plot
    #  ggsave(name, plot(), type="cairo-png") # cairo-png provides higher quality than png
    #}
    
    if(input$print_plot_out){
      isolate(print(plot_out()))
      
    } # only print plot if 'print plot' is clicked
    
    #if(input$save_plot){
    #  png(isolate(name))
    #  isolate(isolate(print(plot())))
    #  dev.off()
    #  input$save_plot = FALSE
    #}
    
    #if(input$save_plot){
    #  png("myplot.png")
    #  plot1
    #  dev.off()
    #}
  })
})