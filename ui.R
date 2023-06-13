
library(shiny)
library(shinyjs)
library(reactable)
library(shinythemes)
library(networkD3)

ui <- fluidPage(
  
  theme = shinytheme("spacelab"),
  
  useShinyjs(),
  fluidRow(
    h1('ARTIS Data Analysis Dashboard'),
    "Trade filtered to live weight flows > 10 t. Estimates from March 28, 2023 model run."
  ),
  fluidRow(
    column(3,
           uiOutput('species_select')
           ),
    column(3,
           uiOutput('source_country_select')
    ),
    column(3,
           uiOutput('exporter_select')
    ),
    column(3,
           uiOutput('importer_select')
    )
  ),
  fluidRow(
    column(3,
           uiOutput('year_select')
    ),
    column(2,
           uiOutput('method_select')
    ),
    column(2,
           uiOutput('dom_select')
    ),
    column(2,
           uiOutput('habitat_select')
    ),
    column(2,
           radioButtons(
             inputId="quantity_selection",
             label="Select Quantity type:",
             choices=c("Product Weight", "Live Weight"),
             select="Product Weight"
           )
    ),
    column(3,
           actionButton(inputId='run_query_btn',
                        'Run Query',
                        style='color: #ffffff; background-color: #2986cc'
                        ),
           actionButton(inputId='reset_selects_btn',
                        'Clear Selections',
                        style='color: #ffffff; background-color: #d2190b'
                        )
    )
  ),
  fluidRow(
    tabsetPanel(
      tabPanel(
        'Summary Tables',
        column(4,
               h4('Total Trade Summary'),
               reactableOutput('trade_summary'),
               h4('Top 5 Species'),
               reactableOutput('species_summary')
        ),
        column(4,
               h4('Top Exporters'),
               reactableOutput('top_exporters')
        ),
        column(4,
               h4('Top Importers'),
               reactableOutput('top_importers')
        )
      ),
      tabPanel(
        'Species Trends',
        column(3,
               sliderInput(inputId='prop_species_cutoff_input',
                           label='Select a cutoff proportion rate:',
                           min=0,
                           max=0.20,
                           value=0.05,
                           step=0.01,
                           round=FALSE)
        ),
        column(9,
               plotOutput('stacked_species')
        )
      ),
      tabPanel(
        'Trade Trends',
        column(3,
          selectInput(inputId='stacked_trade_select',
                      label='Select Stacked Graph:',
                      choices=c('Importers', 'Exporters'),
                      selected=c('Importers')
                      ),
          br(),
          sliderInput(inputId='prop_trade_cutoff_input',
                      label='Select a cutoff proportion rate:',
                      min=0,
                      max=0.20,
                      value=0.05,
                      step=0.01,
                      round=FALSE)
        ),
        column(9,
               plotOutput('stacked_trade')
               )
        
      ),
      tabPanel(
        "Trade Flows",
        column(3,
               sliderInput(inputId="prop_sankey_cutoff_input",
               label="Select a cutoff proportion rate:",
               min=0,
               max=0.20,
               value=0.05,
               step=0.01,
               round=FALSE)),
        column(9,
               plotOutput("trade_sankey")
               )
      )
    )
    
  )
)