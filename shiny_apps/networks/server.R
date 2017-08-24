# TODO
# - childes associations
# - cross-linguisic using unilemmas

library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(langcog)
library(wordbankr)
library(stringr)
library(networkD3)
library(feather)

theme_set(theme_mikabr(base_size = 14))
font <- theme_mikabr()$text$family

######## DATA PROCESSING

mcrae <- read_feather("assocs/mcrae.feather")
phons <- read_feather("assocs/phons.feather")
uni_lemmas <- read_feather("assocs/uni_lemmas.feather")

w2v_assocs<- read_csv(file= 'assocs/w2v_assocs.csv')

ws_aoas <- read_csv("aoas/eng_ws_production_aoas.csv") 
wg_comp_aoas <- read_csv("aoas/eng_wg_production_aoas.csv") 
wg_prod_aoas <- read_csv("aoas/eng_wg_comprehension_aoas.csv") 



###### SHINY SERVER ###### 

shinyServer(function(input, output) {
  output$loaded <- reactive(0)
  outputOptions(output, "loaded", suspendWhenHidden = FALSE)
  
  ########## GET MEASURE
  output$measure <- renderUI({
    req(input$instrument) 
    
    if (input$instrument == "WS") {
      choices <- c("Production" = "production")
    } else if (input$instrument == "WG") {
      choices <- c("Comprehension" = "comprehension", 
        "Production" = "production")
    }
    
    selectInput("measure", "AoA measure",
                   choices = choices,
                   selected = "production")
  })
  
  
  ########## GET MEASURE
  output$language <- renderUI({
    req(input$source) 
    
    if (input$source == "W2V") {
      choices <- c("English (American)")
    } else if(input$source == "MFN") {
      choices <- unique(uni_lemmas$language)
    } else if(input$source == "Phon") {
      choices <- unique(phons$language)
    }
    
    selectInput("language", "Language",
                choices = choices,
                selected = "English (American)")
  })
  
  
  output$assoc_control <- renderUI({
    req(input$source)
    if(input$source == "MFN") {
    selectInput("assocs", "Association type",
              choices = c("Conceptual" = "conceptual", 
                          "Perceptual" = "perceptual", 
                          "All" = "all"),
              selected = "all")
    }
  })
  
  ####### CHOOSE A SCALE FOR CUTOFF
  output$cutoff <- renderUI({
    req(input$source)
    
    if (input$source == "W2V"){
      title <- "Minimum Cosine Similarity"
      high_point <- .7
      start_point <- .4
      low_point <- .1
      step_size <- .1
    # } else if (input$source == "PB"){
    #   title <- "Cosine Similarity"
    #   high_point <- 1
    #   start_point <- .6
    #   low_point <- -1
    #   step_size <- .1
    } else if (input$source == "MFN"){
      title <- "Number of Shared Features"
      high_point <- 4
      start_point <- 2
      low_point <- 1
      step_size <- 1
    } else if (input$source == "Phon"){
      title <- "Phonetic Distance"
      high_point <- 5
      start_point <- 2
      low_point <- 1
      step_size <- 1
    }
  
    sliderInput("cutoff", label=title,
                  min = low_point, max = high_point, 
                  value = start_point, step = step_size)
  })

  ########## READ IN AOAS
  aoa_data <- reactive({
    
    req(input$source)
    
    if (input$instrument  == "WS") {
      raw_aoas <- ws_aoas
    } else if (input$instrument == "WG" & input$measure == "comprehension" ) {
      raw_aoas <- wg_comp_aoas
    }else if (input$instrument == "WG" & input$measure == "production" ) {
      raw_aoas <- wg_prod_aoas
    }
         
    raw_aoas %>%
      rename(label = uni_lemma) %>% 
      mutate(aoa = round(aoa))  
  })  
  
  ########## READ IN ASSOCIATIONS
  assoc_mat <- reactive({
    req(input$source)
    req(input$language)
    req(input$assocs)
    
    if(input$source == "W2V") {
      assocs <- w2v_assocs
    }
    else if(input$source == "MFN") {
      if (input$assocs == "all") {
          assocs <- mcrae %>% 
            select(U1, U2, McRae_all) %>% 
            spread(U2, McRae_all) %>%
            rename(in_node = U1)
      } else if (input$assocs == "perceptual") {
          assocs <- mcrae %>% 
            select(U1, U2, McRae_p) %>% 
            spread(U2, McRae_all) %>%
            rename(in_node = U1)
      } else {
          assocs <- mcrae %>% 
            select(U1, U2, McRae_c) %>% 
            spread(U2, McRae_all)  %>%
            rename(in_node = U1)
      }
    } else if (input$source == "Phon") {
      assocs <- phons %>%
        filter(language == input$language) %>%
        select(W1, W2, PhonoDist) %>%
        spread(W2,PhonoDist) %>%
        rename(in_node = W1)
    }
    
    assoc_mat <- as.matrix(select(assocs, -in_node))
    assoc_mat[lower.tri(assoc_mat)] <- NA
    
    data.frame(assoc_mat, check.names = FALSE,
               in_node = assocs$in_node, 
               stringsAsFactors = FALSE)
  })
  
  ########## FILTER NODE DATA
  assoc_nodes <- reactive({
    # aoa_labels <- filter(aoa_data, aoa <= input$age)$label
    
    assoc_nodes <- data.frame(label = assoc_mat()$in_node, 
                              stringsAsFactors = FALSE) %>%
      left_join(aoa_data()) %>% #left_join in wide ? format version of english item csv with category
      filter(!is.na(aoa), aoa <= input$age) %>%
      mutate(id = 0:(n()-1), 
             identity = 1) %>%
      select_("label", "id", input$group) %>%
      rename_("group" = input$group)
  })
  
  ########## PARSE EDGE DATA
  assoc_edge_data <- reactive({
    nodes <- assoc_nodes()
    
    # get the matrix in an id-based form
    assoc_mat() %>%
      gather(out_node, width, -in_node) %>%
      filter(!is.na(width), in_node %in% nodes$label, 
             out_node %in% nodes$label) %>%
      rename(label = in_node) %>%
      left_join(nodes) %>%
      select(-label) %>%
      rename(in_node = id, 
             label = out_node) %>%
      select(label, width, in_node) %>%
      left_join(nodes) %>%
      select(-label) %>%
      rename(out_node = id) %>%
      select(in_node, out_node, width) %>%
      filter(in_node != out_node)
  })
      
  ########## FILTER EDGES 
  assoc_edges <- reactive({
    req(input$weighted)
    req(input$cutoff)
    req(input$source)
    
    scaling = ifelse(input$source == "W2V", 4, 1)
    
    edges <- assoc_edge_data() %>%
      mutate(width = scaling*width) 
    
    if(input$source == "Phon") edges <- filter(edges, width <= (scaling*input$cutoff))
    else edges <- filter(edges, width >= (scaling*input$cutoff)) 
    
    if (input$weighted == "TRUE") {
      edges
    } else {
      edges %>% select(-width) 
    }
    
  })
  
  ########## RENDER GRAPH
  # output$network <- renderForceNetwork({
  #   forceNetwork(Links = assoc_edges(), Nodes = assoc_nodes(), Source = "in_node",
  #                Target = "out_node", Value = "width", NodeID = "label",
  #                linkWidth = JS("function(d) { return d.value; }"),
  #                Group = "group", opacity = .8, zoom = TRUE, opacityNoHover = .8,
  #                legend = input$group != "identity",
  #                linkColour = "#cccccc", fontSize = 12)#,
  #                # colourScale = ifelse(length(unique(assoc_nodes()$group)) > 10, 
  #                #                      JS("d3.scale.category20()"),
  #                #                      JS("d3.scale.category10()")))
  # })
  
  output$network <- renderVisNetwork({
    visNetwork(assoc_nodes(),
               rename(assoc_edges(), from = in_node, to = out_node),
               width = "100%", height="100%") %>%
      visPhysics(stabilization = TRUE) %>%
     visEdges(smooth = FALSE, selfReferenceSize= FALSE)

  })
  
  output$loaded <- reactive(1)
})

