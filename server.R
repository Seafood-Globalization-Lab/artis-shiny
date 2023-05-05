
library(tidyverse)
library(shiny)
library(shinyjs)
library(reactable)
library(shinyWidgets)
library(networkD3)

load('data.RData')

df <- df %>%
  rename(total_q = product_weight_t,
         SciName = sciname,
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
    group_by(SciName) %>%
    summarize(Total=sum(total_q))  %>%
    mutate(Total = round(Total, 0))
  
  return(df)
}

create_stacked_graph <- function(in_df, prop_flow_cutoff=0.05, column_type) {
  
  stacked_df <- in_df
  
  if (column_type == 'exporter') {
    stacked_df <- stacked_df %>%
      rename(stack_group = exporter_name)
  }
  
  if (column_type == 'importer') {
    stacked_df <- stacked_df %>%
      rename(stack_group = importer_name)
  }
  
  if (column_type == 'species') {
    stacked_df <- stacked_df %>%
      rename(stack_group = SciName)
  }

  stacked_df <- stacked_df %>%
    group_by(year, stack_group) %>%
    summarize(quantity = sum(total_q)) %>%
    group_by(year) %>%
    mutate(global_annual = sum(quantity)) %>%
    mutate(prop_flow = quantity / global_annual) %>%
    mutate(stack_group = if_else(is.na(prop_flow)==FALSE & prop_flow < prop_flow_cutoff, true = "Other", false = stack_group)) %>%
    group_by(year, stack_group) %>%
    summarise(quantity = sum(quantity)) %>%
    ungroup()
  

  # Output as stacked line graph
  # Create full list of specific stacked groups and years 
  stack_year_grid <- expand_grid(stack_group = unique(stacked_df$stack_group), 
                                 year = unique(stacked_df$year))
  
  # Bind stacked line graph data to grid so that zeroes can be filled in for "missing" years
  # Need to do this for time series to plot correctly
  stacked_df <- stacked_df %>%
    full_join(stack_year_grid) %>%
    arrange(year, stack_group) %>%
    mutate(quantity = if_else(is.na(quantity), true = 0, false = quantity)) %>%
    mutate(stack_group = fct_reorder(stack_group, quantity))
  
  # rename column back accordingly
  if (column_type == 'exporter') {
    stacked_df <- stacked_df %>%
      rename(Exporter = stack_group)
    
    col_title <- 'Exporter'
    col_name <- 'Exporter'
  }
  
  if (column_type == 'importer') {
    stacked_df <- stacked_df %>%
      rename(Importer = stack_group)
    
    col_title <- 'Importer'
    col_name <- 'Importer'
  }
  
  if (column_type == 'species') {
    stacked_df <- stacked_df %>%
      rename(SciName = stack_group)
    
    col_title <- 'Species'
    col_name <- 'SciName'
  }
  

  stacked_plot <- ggplot() +
    geom_area(data=stacked_df, aes_string(x = 'year', y = 'quantity', fill = col_name)) +
    scale_fill_viridis_d() +
    labs(y = "Quantity (tonnes)", x = "Year", title =  col_title, fill = col_title) +
    theme_bw()
  
  return(stacked_plot)
    
}

create_bilateral_relationships <- function(in_df, prop_flow_cutoff=0.05) {
  
  # Links dataframe
  links <- in_df %>%
    group_by(exporter_name, importer_name, source_country_name) %>%
    summarize(total_q = sum(total_q, na.rm=TRUE)) %>%
    filter(!is.na(source_country_name) & !is.na(exporter_name) & !is.na(importer_name))
  
  # Getting list of producers limited by proportional flow cutoff
  producers <- links %>%
    group_by(source_country_name) %>%
    summarize(total_q = sum(total_q, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(total = sum(total_q, na.rm=TRUE)) %>%
    mutate(prop = total_q / total) %>%
    mutate(source_country_name = case_when(
      prop < prop_flow_cutoff ~ "Other",
      TRUE ~ source_country_name
    ))
  
  # Getting list of exporters limited by proportional flow cutoff
  exporters <- links %>%
    group_by(exporter_name) %>%
    summarize(total_q = sum(total_q, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(total = sum(total_q, na.rm=TRUE)) %>%
    mutate(prop = total_q / total) %>%
    mutate(exporter_name = case_when(
      prop < prop_flow_cutoff ~ "Other",
      TRUE ~ exporter_name
    ))
  
  # Getting list of importers limited by proportional flow cutoff
  importers <- links %>%
    group_by(importer_name) %>%
    summarize(total_q = sum(total_q, na.rm=TRUE)) %>%
    ungroup() %>%
    mutate(total = sum(total_q, na.rm=TRUE)) %>%
    mutate(prop = total_q / total) %>%
    mutate(importer_name = case_when(
      prop < prop_flow_cutoff ~ "Other",
      TRUE ~ importer_name
    ))
  
  # country names of producers, exporters, importers
  country_names <- unique(c(producers$source_country_name, exporters$exporter_name, importers$importer_name))
  
  # producers exporter trade flows
  producer_exporter <- links %>%
    mutate(source_country_name = case_when(
      !(source_country_name %in% country_names) ~ "Other",
      TRUE ~ source_country_name
    ),
    exporter_name = case_when(
      !(exporter_name %in% country_names) ~ "Other",
      TRUE ~ exporter_name
    )) %>%
    group_by(source_country_name, exporter_name) %>%
    summarize(total_q = sum(total_q, na.rm=TRUE)) %>%
    mutate(source_country_name = paste(source_country_name, "_source", sep=""),
           exporter_name = paste(exporter_name, "_exp", sep="")) %>%
    rename(source = source_country_name,
           target = exporter_name)
  
  # exporters importers trade flows
  exporter_importer <- links %>%
    mutate(exporter_name = case_when(
      !(exporter_name %in% country_names) ~ "Other",
      TRUE ~ exporter_name
    ),
    importer_name = case_when(
      !(importer_name %in% country_names) ~ "Other",
      TRUE ~ importer_name
    )) %>%
    group_by(exporter_name, importer_name) %>%
    summarize(total_q = sum(total_q, na.rm=TRUE)) %>%
    mutate(exporter_name = paste(exporter_name, "_exp", sep=""),
           importer_name = paste(importer_name, "_imp", sep="")) %>%
    rename(source = exporter_name,
           target = importer_name)
  
  # Joining producer exporter and exporter importer trade flows together
  trade_flows <- producer_exporter %>%
    bind_rows(exporter_importer)
  
  # Creating nodes dataframe
  nodes <- data.frame(
    name = unique(c(trade_flows$source, trade_flows$target))
    ) %>% 
    mutate(label = gsub("_.{3,6}", "", name))
  
  # Mapping source and target names to respective row in the nodes dataframe, 0-indexed
  trade_flows <- trade_flows %>%
    mutate(source_id = match(source, nodes$name) - 1,
           target_id = match(target, nodes$name) - 1)
  
  sankeyNetwork(Links = trade_flows, Nodes = nodes, Source = "source_id", Target = "target_id",
                Value = "total_q", NodeID = "label")
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
    create_stacked_graph(vals$active_df, prop_flow_cutoff=input$prop_species_cutoff_input, column_type='species')
  })
  
  output$stacked_trade <- renderPlot({
    if (input$stacked_trade_select == 'Importers') {
      create_stacked_graph(vals$active_df, prop_flow_cutoff=input$prop_trade_cutoff_input, column_type='importer')
    }
    else {
      create_stacked_graph(vals$active_df, prop_flow_cutoff=input$prop_trade_cutoff_input, column_type='exporter')
    }
  })
  
  output$trade_sankey <- renderSankeyNetwork(
    create_bilateral_relationships(vals$active_df, prop_flow_cutoff=input$prop_sankey_cutoff_input)
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
        filter(SciName %in% input$in_species)
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
    
    print(unique(vals$active_df$dom_source))

  })
  
  observeEvent(input$reset_selects_btn, {
    
    # reset input selectors
    reset()

    # reset active dataframe
    vals$active_df <- df
  })
}