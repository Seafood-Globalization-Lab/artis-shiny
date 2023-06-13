
library(tidyverse)
library(shiny)
library(shinyjs)
library(reactable)
library(shinyWidgets)
library(networkD3)
library(exploreARTIS)
library(countrycode)

load('data.RData')

df <- df %>%
  rename(total_q = product_weight_t,
         environment = habitat)

species <- species %>%
  mutate(species = as.character(species))

dom_sources <- dom_sources %>%
  mutate(dom_source = as.character(dom_source))

isscaap_groups <- isscaap_groups %>%
  mutate(isscaap_groups = as.character(isscaap_groups))

create_select_input <- function(in_df, in_id, in_label) {
  
  multiInput(
    inputId=in_id,
    label=in_label,
    choices=in_df
  )
}

calc_trade_summary <- function(df) {
  
  df <- df %>%
    group_by(method) %>%
    summarize(Total=round(sum(total_q))) %>%
    mutate(Percentage = round(Total * 100 / sum(Total), 2))
  
  total_traded <- sum(df$Total)
  
  df <- df %>%
    bind_rows(data.frame('method'='Total', 'Total'=total_traded, 'Percentage'=100.00))
  
  return(df)
  
}

calc_partner_summary <- function(df, trade_type) {
  
  if (trade_type == 'exporter') {
    df <- df %>%
      group_by(exporter_name) %>%
      summarize(Total=sum(total_q)) %>%
      arrange(Total) %>%
      mutate(Total = round(Total, 0))
  }
  
  if (trade_type == 'importer') {
    df <- df %>%
      group_by(importer_name) %>%
      summarize(Total=sum(total_q)) %>%
      arrange(Total) %>%
      mutate(Total = round(Total, 0))
  }
  
  return(df)
}

calc_species_summary <- function(df) {
  
  df <- df %>%
    group_by(sciname) %>%
    summarize(Total=sum(total_q))  %>%
    mutate(Total = round(Total, 0))
  
  return(df)
}

server <- function(input, output, session) {
  
  vals <- reactiveValues(
    active_df=df
    )
  
  output$species_select <- renderUI({
    create_select_input(sort(species$species), 'in_species', 'Select Species:')
  })
  
  output$source_country_select <- renderUI({
    create_select_input(sort(source_countries$source_country_name), "in_source_country", "Select Producer:")
  })
  
  output$dom_select <- renderUI({
    checkboxGroupInput('in_dom', 'Select Production Origin:',
                       choices = str_to_sentence(sort(unique(dom_sources$dom_source))))
  })
  
  output$habitat_select <- renderUI({
    checkboxGroupInput('in_habitat', 'Select Habitat:',
                       choices = str_to_sentence(sort(unique(df$environment))))
  })
  
  output$method_select <- renderUI({
    checkboxGroupInput('in_method', 'Select Method:',
                       choices = str_to_sentence(sort(unique(df$method))))
    })
  
  output$year_select <- renderUI({
      create_select_input(years$years, 'in_years', 'Select Year:')
    sliderInput('in_years', label = 'Select Year:', min = min(years$years), 
                max = max(years$years), value = c(min(years$years), max(years$years)), sep ='')
    })
  
  output$exporter_select <- renderUI({
    create_select_input(sort(exporters$exporter_name), 'in_exporters', 'Select Exporters:')
  })
  
  output$importer_select <- renderUI({
    create_select_input(sort(importers$importer_name), 'in_importers', 'Select Importers:')
  })
  
  output$trade_summary <- renderReactable({
    reactable(calc_trade_summary(vals$active_df))
  })
  
  output$species_summary <- renderReactable({
    species_table <- calc_species_summary(vals$active_df)
    reactable(species_table, defaultPageSize=5, searchable=TRUE, defaultSorted=list('Total'='desc'))
  })
  
  output$top_exporters <- renderReactable({
    partners <- calc_partner_summary(vals$active_df, 'exporter')
    reactable(partners, defaultPageSize=10, searchable=TRUE, defaultSorted=list('Total'='desc'))
  })
  
  output$top_importers <- renderReactable({
    partners <- calc_partner_summary(vals$active_df, 'importer')
    reactable(partners, defaultPageSize=10, searchable=TRUE, defaultSorted=list('Total'='desc'))
  })
  
  output$stacked_species <- renderPlot({
    plot_ts(vals$active_df, prop_flow_cutoff=input$prop_species_cutoff_input, artis_var="sciname", plot.type="stacked")
  })
  
  output$stacked_trade <- renderPlot({
    if (input$stacked_trade_select == 'Importers') {
      plot_ts(vals$active_df, prop_flow_cutoff=input$prop_trade_cutoff_input, artis_var="importer_name", plot.type = "stacked")
    }
    else {
      plot_ts(vals$active_df, prop_flow_cutoff=input$prop_trade_cutoff_input, artis_var="exporter_name", plot.type = "stacked")
    }
  })
  
  output$trade_sankey <- renderPlot(
    plot_sankey(vals$active_df %>%
                  mutate(exporter_iso3c = countrycode(exporter_name, "country.name", "iso3c"),
                         importer_iso3c = countrycode(importer_name, "country.name", "iso3c"),
                         source_country_iso3c = countrycode(source_country_name, "country.name", "iso3c")),
                prop_flow_cutoff=input$prop_sankey_cutoff_input)
  )
  
  observeEvent(input$run_query_btn, {
    
    # starts with a clean copy of the original data frame
    vals$active_df <- df
    
    # check with quantity type was selected
    # No need for product type because this is the default and will get reset automatically since we start with the original data frame
    if (input$quantity_selection == "Live Weight") {
      vals$active_df <- vals$active_df %>%
        rename(product_weight_t=total_q) %>%
        rename(total_q=live_weight_t)
    }
    
    # check habitat selection
    if (length(input$in_habitat) > 0) {
      vals$active_df <- vals$active_df %>%
        filter(environment %in% tolower(input$in_habitat))
    }
    
    # check if species are selected
    if (length(input$in_species) > 0) {
      vals$active_df <- vals$active_df %>%
        filter(sciname %in% input$in_species)
    }
    
    # check if exporters are selected
    if (length(input$in_exporters) > 0) {
      vals$active_df <- vals$active_df %>%
        filter(exporter_name %in% input$in_exporters)
    }
    
    # check if importers are selected
    if (length(input$in_importers) > 0) {
      vals$active_df <- vals$active_df %>%
        filter(importer_name %in% input$in_importers)
    }
    
    # check if source countries are selected
    if (length(input$in_source_country) > 0) {
      vals$active_df <- vals$active_df %>%
        filter(source_country_name %in% input$in_source_country)
    }
    
    # check if a specific year is selected
    if (length(input$in_years) > 0) {
      start_year <- min(input$in_years)
      end_year <- max(input$in_years)
      
      vals$active_df <- vals$active_df %>%
        filter(year >= start_year & year <= end_year)
    }
    
    # check if a dom_source is selected
    if (length(input$in_dom) > 0) {
      vals$active_df <- vals$active_df %>%
        filter(dom_source %in% tolower(input$in_dom))
    }
    
    # check if a specific method is selected
    if (length(input$in_method) > 0) {
      vals$active_df <- vals$active_df %>%
        filter(method %in% tolower(input$in_method))
    }
  })
  
  observeEvent(input$reset_selects_btn, {
    
    # reset input selectors
    reset()

    # reset active dataframe
    vals$active_df <- df
  })
}