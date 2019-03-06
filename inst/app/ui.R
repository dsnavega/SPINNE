library(shiny)
library(grnnet)
library(corrplot)
library(ggplot2)

vertebrae.db <- readRDS(file = "data/dvb.rds")
vertebrae.female <- vertebrae.db[vertebrae.db$SEX == 'Female', ]
vertebrae.male <- vertebrae.db[vertebrae.db$SEX == 'Male', ]
codenames <- colnames(vertebrae.db[ , -c(1:3)])
outnames <- codenames

shinyUI(
  fluidPage(
    navbarPage(
      'SPINNE', collapsible = T, theme = 'bootstrap.css',
      tabPanel(
        'Estimate Missing Values',
        fluidPage(
          fluidRow(
            column(2,
                   selectInput(inputId = 'SEX', label = strong('Sex of subject'),
                               choices = c('Unknown', 'Female', 'Male'),
                               selected = 'Unkown'),
                   strong('Input Variables'),
                   selectInput(
                     'variable', '(predictive vertebrae types - usually vertebrae adjacent to the ones missing)',
                     choices = c(codenames), multiple = TRUE,
                     selected = sample(codenames, size = 3)),
                   strong('Output Variables'),
                   selectInput(
                     'variable2', '(missing vertebrae types - those you want to estimate)',
                     choices = outnames,
                     multiple = TRUE
                   ),
                   strong('Confidence level'),
                   sliderInput('elf', 'Alpha', min = 0.05, max = 0.5,
                               step = 0.01, value = 0.05)
            ),
            column(2,
                   strong('Measurements'),
                   p('(milimeters)'),
                   uiOutput('vars'), # Creates automatic list of variable-input.
                   hr(),
                   actionButton("Go", "Calculate Missing Values!"),
                   hr(),
                   p('Default values are arithmetic means from our database.'),
                   p('Min and max are limited by a 6sigma factor.')
            ),
            column(8,
                   strong('Results'),
                   textOutput('total'),
                   hr(),
                   tableOutput('predictions')
            )
          ),
          fluidRow(
            p(strong('MAE'),'- mean absolute error;',
              strong('RMSE'),'- root mean squared error;',
              strong('NRMSE'),'- normalized root mean squared error;',
              strong('R.Squared'),'- R-Squared;',
              strong('Bias'), '- difference between expected value and the true value of the parameter being estimated;',
              strong('Coverage'), '- the proportion of the time that the interval contains the true value of interest;',
              strong('PIW'),'- predictive interval mean width.'
            )
          )
        )
      ),
      tabPanel(
        'Data Exploration',
        fluidPage(
          tabsetPanel(
            id = 'tabsetExplore',
            tabPanel(
              'Density Plots',
              fluidRow(
                sidebarPanel(
                  selectInput('dpvar', 'Variable', choices = codenames),
                  hr(),
                  checkboxInput('filter', 'Filter by Sex', FALSE),
                  hr(),
                  selectInput('kernel', 'Kernel: ',
                              choices = c('gaussian', 'epanechnikov', 'rectangular', 'triangular', 'cosine')),
                  sliderInput('bandwidth', 'Bandwidth:', 
                              min = 0.05, max = 2, value = 1),
                  sliderInput('alpha', 'Filling opacity:',
                              min = 0.01, max = 1, value = 0.5)
                ),
                mainPanel(
                  fluidRow(
                    plotOutput('densPlot')
                  ),
                  fluidRow(
                    hr(),
                    textOutput('normTest')
                  )
                )
              )
            ),
            tabPanel(
              'Box Plots',
              fluidRow(
                sidebarPanel(
                  selectInput('bpvar', 'Variable', choices = codenames),
                  hr(),
                  checkboxInput('truedata', 'Add data points', FALSE),
                  hr(),
                  strong('Lower Whisker'),
                  p('Smallest observation greater than or equal to lower hinge - 1.5 * IQR'),
                  strong('Lower Hinge'),
                  p('25% quantile'),
                  strong('Middle'),
                  p('Median, 50% quantile'),
                  strong('Diamond'),
                  p('Arithmetic mean'),
                  strong('Upper Hinge'),
                  p('75% quantile'),
                  strong('Upper Whisker'),
                  p('Largest observation less than or equal to upper hinge + 1.5 * IQR')
                ),
                mainPanel(
                  plotOutput('boxPlot')
                )
              )
            ),
            tabPanel(
              'Scatter Plots',
              fluidRow(
                sidebarPanel(
                  selectInput('xvar', 'X-axis variable', codenames, selected = 'C2'),
                  selectInput('yvar', 'Y-axis variable', codenames, selected = 'S1'),
                  hr(),
                  checkboxInput('filter2', 'Filter by Sex', FALSE),
                  hr(),
                  sliderInput('spanner', label = 'Smoothing:', value = 0.6, min = 0.2, max = 1)
                ),
                mainPanel(
                  plotOutput('scatterPlot')
                )
              ) # fluidRow [Plot]
            ), # tabPanel 'Scatter Plots'
            tabPanel(
              'Correlation Matrix',
              fluidRow(
                sidebarPanel(
                  selectInput('cor.filter', 'Filter dataset by sex:',
                              c('all', 'males', 'females'), 'all'),
                  selectInput('cor.method', 'Correlation Method',
                              c('pearson', 'kendall', 'spearman'), 'pearson'),
                  selectInput('upper.visuals', 'Visuals Upper',
                              eval(formals(corrplot)$method), 'number'),
                  selectInput('lower.visuals', 'Visuals Lower',
                              eval(formals(corrplot)$method), 'square'),
                  selectInput('orderVisuals', 'Reorder Algorithm',
                              eval(formals(corrplot)$order)),
                  conditionalPanel(
                    condition = "input.orderVisuals == 'hclust'",
                    selectInput("plotHclustMethod", "Hierarchical clustering method",
                                eval(formals(corrplot)$hclust.method)),
                    numericInput("plotHclustAddrect", "Number of Rectangles", 3, 0, NA)
                  )
                  
                ),
                mainPanel(
                  plotOutput('corrPlot', height = 700)
                )
              ) # fluidRow [Correlations]
            ) # tabPanel [Correlations]
          ) # tabsetPanel [Explore]
        ) # fluidPage [Explore]
      ), # tabPanel [Explore]
      tabPanel(
        'Help',
        fluidPage(
          h3('Descriptions'),
          hr(),
          strong('C2, 2nd cervical vertebra height'),
          br(),
          p(style = "text-align: justify", 
            "The most superior point of the odontoid process (dens) to the most inferior point of the anterioinferior rim of the vertebral body."
          ),
          p('(Spreading caliper)'),
          br(),
          strong('C3 - C7, 3rd-7th cervical vertebra height'),
          br(),
          p(style = "text-align: justify", 
            "The maximum height of the vertebral body, measured in its anterior third, medial to the superiorly curving edges of the centrum."
          ),
          p('(Spreading caliper)'),
          br(),
          strong('T1 - T12, thoracic vertebrae height'),
          br(),
          p(style = "text-align: justify", 
            "The maximum height of the vertebral body, anterior to the rib articular facets and pedicles."
          ),
          p('(Spreading caliper)'),
          br(),
          strong('L1 - L5, lumbar vertebrae height'),
          br(),
          p(style = "text-align: justify", 
            "The maximum height of the vertebral body, anterior to the pedicles, not including any swelling of the centrum due to the pedicles."
          ),
          p('(Spreading caliper)'),
          br(),
          strong('S1, 1st sacral vertebra height'),
          br(),
          p(style = "text-align: justify", 
            "The maximum height between the anterior-superior rim of the body (i.e., the sacral promontory) and its point of fusion/articulation with the second sacral vertebra.
							This most commonly occurs in the midline. Measure with the calipers parallel to the anterior surface of S1."
          ),
          p('(Spreading caliper)')
        )
      ),
      tabPanel(
        'About',
        fluidPage(
          fluidRow(
            h1('Spine Proportion through Implementation of Neural Networks', align = 'center')
          ),
          br(),
          fluidRow(
            h3('Authors'),
            column(12,
                   p("Daniela Vilas Boas, Sofia N. Wasterlain, João d'Oliveira Coelho, David Navega David Gonçalves")
            )
          ),
          fluidRow(
            h3('References'),
            column(12,
                   p(align = 'justify', 'COMING SOON: Vilas Boas et al. 2019. Forensic Science International.'),
                   column(12,
                          p(align = 'justify', a(href = 'dvb_dissertation.pdf', target = '_blank', 'Daniela Vilas Boas, 2016.
                                          Missing values: estimativa da altura de vértebras ausentes
                                          ou mal preservadas para a aplicação do método anatómico na estimativa da estatura.'),
                            'Dissertação de Mestrado em Evolução e Biologia Humanas, orientada pela 
                     Professora Doutora Sofia Wasterlain e pelo Doutor David Gonçalves e
                     apresentada ao Departamento de Ciências da Vida da Faculdade de Ciências
                     e Tecnologia da Universidade de Coimbra')
                          
                   )
            )
          ),
          fluidRow(
            h3('Disclaimer'),
            column(12,
                   p("This", strong("decision support system"), "is freely provided as an aid for
                   profiling skeletal material. The authors hold no responsibility
                   for its ultimate use or misuse. While creators we try to ensure
                   that the software is theoretically grounded and statically accurate,
                     we provide no warranty and make no specific claims as to its performance
                     or its appropriateness for use in any particular situation. Please read
                     any research paper regarding this tool before applying it.
                     Always keep in mind: you are the expert, this is just decision support system."
                   )
            )
          ),
          fluidRow(
            h3('App Development'),
            column(6,
                   a(href = 'http://jcoelho.com', target = '_blank', p("João d'Oliveira Coelho")),
                   a(href = 'https://github.com/dsnavega', target = '_blank', p('David Navega'))
            ),
            column(6,
                   a(href = 'http://osteomics.com', target = '_blank',
                     img(src = 'http://osteomics.com/img/logo.png',
                         align = 'right')
                   )
            )
          )
        )
      ), # tabPanel('About')
      tabPanel(
        title = HTML("<li><a href='http://osteomics.com' target='_blank'>Back to osteomics.com</a></li>")
      )
    )
  )
)