shinyUI(fluidPage(
  #shinythemes::themeSelector(),
  
  navbarPage('rGrapher', # title
             navbarMenu('Setup',
                        # input panel
                        tabPanel('Load & Setup',
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
                                     fileInput('file', 'Choose File', multiple=TRUE), #data upload
                                     textOutput('check_file'),
                                     checkboxInput('center', 'Center', FALSE), # center the data
                                     checkboxInput('scale', 'Scale', FALSE ), # scale the data
                                     selectInput('alg', label = 'Select an algorithm: ', choices = c('PCA', 'LDA', 't-SNE')), # algorithm list
                                     actionButton('analyse', 'Start Analysis'), # start analysis
                                     textOutput('initiate_analysis'),
                                     tags$hr(),
                                     tags$b('Linear Discriminant Analysis'),
                                     #tags$br(),
                                     selectInput('group_options', label = 'Grouping: ', choices = NULL), # list of columns containing group names (for LDA)
                                     tags$hr(),
                                     tags$b('t-SNE'),
                                     #tags$br(),
                                     #textInput('k', label = 'No. of output dimensions: ', value = '2'),
                                     textInput('perp', label = 'Perplexity: ', value = '30'),
                                     textInput('max_i', label = 'Iterations: ', value = '1000'),
                                     textInput('min_cost', label = 'Min. cost value (error) to halt iterations: ', value = '0')
                                   ),
                                   mainPanel(
                                     downloadButton('data_in_csv', 'Download as CSV'),
                                     dataTableOutput('data_in')
                                   )
                                 )
                        ),
                        
                        tabPanel('Explore',
                                 sidebarLayout(
                                   sidebarPanel(width = 3,
                                     selectInput('x_var_in', label = 'X-axis variable: ', choices = NULL),
                                     selectInput('y_var_in', label = 'Y-axis variable: ', choices = NULL),
                                     actionButton('print_plot_in', label = 'Plot Data'),
                                     tags$hr(),
                                     selectInput('col_in', label = 'Colour: ', choices = 'Blue (default)'),
                                     selectInput('shape_in', label = 'Shape: ', choices = 'Default'),
                                     selectInput('size_in', label = 'Size: ', choices = 'Default')
                                     
                                   ),
                                   mainPanel(
                                    # column(3,
                                    #        
                                    #        checkboxInput('axes_in', 'Axes', TRUE),
                                    #        checkboxInput('labels_in', 'Axis labels', TRUE),
                                    #        textInput('plot_title_in', label = 'Title: ')
                                    # ),
                                    # column(3,
                                    #        checkboxInput('grid_major_in', 'Major grid', FALSE),
                                    #        checkboxInput('grid_minor_in', 'Minor grid', FALSE),
                                    #        checkboxInput('border_in', 'Border', FALSE),
                                    #        textInput('plot_caption_in', label = 'Caption: ')
                                     #),
                                     column(2, checkboxInput('axes_in', 'Axes', TRUE)),
                                     column(2, checkboxInput('labels_in', 'Axis labels', TRUE)),
                                     column(2, checkboxInput('grid_major_in', 'Major grid', FALSE)),
                                     column(2, checkboxInput('grid_minor_in', 'Minor grid', FALSE)),
                                     column(2, checkboxInput('border_in', 'Border', FALSE)),
                                     plotOutput('plot_in', height = 450),
                                     tags$br(),
                                     tags$br(),
                                     column(6, textInput('plot_title_in', label = 'Title: ')),
                                     column(6, textInput('plot_caption_in', label = 'Caption: '))
                                   )
                                 )
                        )
             ),
             
             # data display panel
             navbarMenu('Output',
                        tabPanel('Transformed Data',
                                 downloadButton('data_out_csv', 'Download as CSV'),
                                 textOutput('proc_time'),
                                 dataTableOutput('data_out')),
                        tabPanel('Vectors', 
                                 downloadButton('vectors_csv', 'Download as CSV'),
                                 dataTableOutput('vectors')),
                        tabPanel('Variance',
                                 sidebarLayout(
                                    sidebarPanel(width = 6,
                                      plotOutput('exp_var_plot')
                                    ),
                                    mainPanel( width = 6,
                                               tabsetPanel(
                                                 tabPanel('Explained Variance (%)',
                                                          downloadButton('exp_var_csv', 'Download as CSV'),
                                                          dataTableOutput('exp_var')
                                                 ),
                                                 tabPanel('Variance',
                                                          downloadButton('var_csv', 'Download as CSV'),
                                                          dataTableOutput('var')
                                                 )
                                               )
                                    )
                                 )
                                 #plotOutput('exp_info_plot'),
                                 #dataTableOutput('exp_info')
                                 )
             ),
             
             tabPanel('Visualise', 
                      #plotOutput('plot'),
                      sidebarLayout(
                        sidebarPanel(width = 3,
                          selectInput('x_var_out', label = 'X-axis variable: ', choices = NULL),
                          selectInput('y_var_out', label = 'Y-axis variable: ', choices = NULL),
                          actionButton('print_plot_out', label = 'Update Plot'),
                          tags$hr(),
                          selectInput('glyph_out', label = 'Glyph: ', choices = c('Shape', 'Text', 'Both')),
                          selectInput('col_out', label = 'Colour: ', choices = 'Black'),
                          selectInput('size_out', label = 'Size: ', choices = 'Default'),
                          selectInput('shape_out', label = 'Shape: ', choices = 'Default'),
                          selectInput('text_out', label = 'Text: ', choices = NULL)
                          #tags$hr(),
                          #textInput('png_name', label = 'Save as: Enter a filename'),
                          #actionButton('save_plot', label = 'Plot & Save as PNG')
                        ),
                        mainPanel(
                          column(2, checkboxInput('axes_out', 'Axes', TRUE)),
                          #column(2,checkboxInput('values', 'Axis values', TRUE)),
                          column(2, checkboxInput('labels_out', 'Axis labels', TRUE)),
                          column(2, checkboxInput('grid_major_out', 'Major grid', FALSE)),
                          column(2, checkboxInput('grid_minor_out', 'Minor grid', FALSE)),
                          column(2, checkboxInput('legend_out', 'Legend', TRUE)),
                          column(2, checkboxInput('border_out', 'Border', FALSE)),
                          plotOutput('plot_out', height = 550),
                          tags$br(),
                          tags$br(),
                          column(6,
                                 textInput('plot_title', label = 'Title: ')
                                 
                                 #textInput('plot_subtitle', label = 'Subtitle: '),
                                 #textInput('plot_caption', label = 'Caption: ')
                                 #checkboxInput('caption', label = 't-SNE parameters in caption'),
                          ),
                          column(6,
                                 textInput('plot_caption', label = 'Caption: ')
                                 #textInput('png_name', label = 'Save as: Enter a filename'),
                                 #tags$hr(),
                                 #actionButton('save_plot', label = 'Plot & Save as PNG')
                                 )
                          
                        )
                      )
                      #fluidRow(
                      #   column(3,
                      #      actionButton('print_plot', label = 'Plot Data'),
                      #      tags$hr(),
                      #      textInput('png_name', label = 'Save as: Enter a filename'),
                      #      actionButton('save_plot', label = 'Save as PNG'),
                      #      tags$hr()
                      #    ),
                      #   column(3,
                      #      h4('Plot options'),
                      #      tags$hr(),
                      #      textInput('plot_title', label = 'Title: '),
                      #      tags$hr()
                      #    ),
                      #   column(3,
                      #      checkboxInput('grid', 'Show grid', FALSE),
                      #      tags$hr()
                      #   )
                      
                      #  )
             )
             
  )
))