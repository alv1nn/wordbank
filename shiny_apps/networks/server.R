# TODO
# - childes associations
# - cross-linguisic using unilemmas

library(shiny)
library(langcog)
library(wordbankr)
library(stringr)
library(networkD3)
library(igraph)
library(visNetwork)
library(RColorBrewer)
library(tidyverse)
library(feather)

theme_set(theme_mikabr(base_size = 14))
font <- theme_mikabr()$text$family

######## DATA PROCESSING

mcrae <- read_feather("assocs/mcrae.feather")
phons <- read_feather("assocs/phons.feather")
uni_lemmas <- read_feather("assocs/uni_lemmas.feather")

#w2v_assocs<- read_csv(file= 'assocs/w2v_assocs.csv')
w2v <- read_feather("assocs/w2v.feather")

aoas <- read_feather("aoas/all_aoas.feather") %>%
  mutate(measure = if_else(measure == "understands", "comprehension", "production"))


instruments <- distinct(aoas, language, form)
#ws_aoas <- read_csv("aoas/eng_ws_production_aoas.csv") 
#wg_comp_aoas <- read_csv("aoas/eng_wg_production_aoas.csv") 
#wg_prod_aoas <- read_csv("aoas/eng_wg_comprehension_aoas.csv") 

######### Constants

## lexical class colors
lex_class_col <- tibble(
  group = c("nouns", "other", "verbs", "adjectives", "function_words"),
  color = c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00')
)

## lexical categories colors 
categories <- aoas %>% 
  filter(!is.na(category)) %>%
           pull(category) %>% unique()
         
lex_cat_col <- tibble(
  group = categories,
  color = c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c',
            '#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928',
            '#8dd3c7','#ffffb3','#bebada','#fb8072','#80b1d3','#fdb462',
            '#b3de69','#fccde5','#d9d9d9','#bc80bd','#ccebc5',
            '#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c',
            '#fdbf6f','#ff7f00','#cab2d6','#6a3d9a','#ffff99','#b15928')
)


###### SHINY SERVER ###### 

shinyServer(function(input, output) {
  output$loaded <- reactive(0)
  outputOptions(output, "loaded", suspendWhenHidden = FALSE)
  
  ########## GET MEASURE
  output$measure <- renderUI({
    
    req(input$language)
    req(input$instrument) 
    
    
    meas_choices <- forms %>%
      filter(language == input$language, form == input$instrument) %>%
      pull(measure)
    
    if(length(meas_choices) == 2) {
      choices <- c("Production" = "production", 
                   "Comprehension" = "comprehension")
      selected <- "comprehension"
    } else if(meas_choices == "production") {
      choices <-  c("Production" = "production")
      selected <- "production"
    } else if(meas_choices == "comprehension") {
      choices <-  c("Comprehension" = "comprehension")
      selected <- "comprehension"
    }
    
    selectInput("measure", "AoA measure",
                   choices = choices,
                   selected = selected)
  })
  
  
  ########## GET LANGUAGE
  output$language <- renderUI({
    req(input$source) 
    
    if (input$source == "W2V") {
      choices <- unique(w2v$language)
    } else if(input$source == "MFN") {
      choices <- unique(uni_lemmas$language)
    } else if(input$source == "Phon") {
      choices <- unique(phons$language)
    }
    
    selectInput("language", "Language",
                choices = choices,
                selected = "English (American)")
  })
  
  ########## GET MEASURE
  output$instrument <- renderUI({
    req(input$language)
    
    lang_choices <- instruments %>%
      filter(language == input$language) %>%
      pull(form)
    
    
    if(length(lang_choices) == 2) {
      choices <- c("Words & Sentences" = "WS", 
                   "Words & Gestures" = "WG")
      selected <- "WG"
    } else if(lang_choices == "WS") {
      choices <-  c("Words & Sentences" = "WS")
      selected <- "WS"
    } else if(lang_choices == "WG") {
      choices <-  c("Words & Gestures" = "WG")
      selected <- "WG"
    }
    
    selectInput("instrument", "Instrument",
                choices = choices,
                selected = selected)
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
    req(input$language)
    req(input$instrument)
    
    aoas <- aoas %>%
      filter(language == input$language,
             measure == input$measure,
             form == input$instrument) %>%
      mutate(aoa = round(aoa))

    print(aoas)
    
    if(input$source == "MFN")
      rename(aoas, label = uni_lemma)
    else
      rename(aoas, label = definition)

  })  
  
  ########## READ IN ASSOCIATIONS
  assoc_mat <- reactive({
    req(input$source)
    req(input$language)
    
    if(input$source == "MFN") {
      req(input$assocs)  
    }
    
    if(input$source == "W2V") {
      assocs <- w2v %>% 
        filter(language == input$language) %>%
        select(W1, W2, CosSim) %>% 
        spread(W2, CosSim) %>%
        rename(in_node = W1)
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
      rename_("group" = input$group) %>% 
      mutate(value = 2) 
      
    #### ADD COLORS BASED ON GROUPING VARIABLE
    
    if(input$group == "lexical_class") {
      assoc_nodes <- assoc_nodes %>% 
        left_join(., lex_class_col, by = "group")
    
    } else if (input$group == "category") {
      assoc_nodes <- assoc_nodes %>% 
        left_join(., lex_cat_col, by = "group")
    } else {
      assoc_nodes <- assoc_nodes %>% mutate(color = "dodgerblue")
    }
    
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
    #req(input$weighted)
    req(input$cutoff)
    req(input$source)
    
    scaling = ifelse(input$source == "W2V", 4, 1)
    
    edges <- assoc_edge_data() %>%
      mutate(width = scaling*width) 
    
    if(input$source == "Phon") edges <- filter(edges, width <= (scaling*input$cutoff))
    else edges <- filter(edges, width >= (scaling*input$cutoff)) 
    
    # if (input$weighted == "TRUE") {
    #   edges
    # } else {
    #   edges %>% select(-width) 
    # }
    
  })
  
  #### create custom mappings for the legend
  
  lnodes <- reactive({
    req(input$group)
    if(input$group == "lexical_class") {
      nodes_legend <- lex_class_col %>% 
        filter(group %in% unique(assoc_nodes()$group)) %>% 
        rename(label = group) %>% 
        mutate(shape = "ellipse")
    } else if (input$group == "category") {
      nodes_legend <- lex_cat_col %>% 
        filter(group %in% unique(assoc_nodes()$group)) %>% 
        rename(label = group) %>% 
        mutate(shape = "ellipse")
      
    } else {
      nodes_legend <- lex_cat_col %>%
        filter(group %in% unique(assoc_nodes()$group)) %>% 
        rename(label = group) %>%
        mutate(color = "dodgerblue",
               shape = "ellipse",
               label = "word")
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
               width = "100%", height = "100%") %>%
      visIgraphLayout(layout = "layout_nicely", randomSeed = 123,
                      physics = F) %>%
      visEdges(color = "darkgrey") %>%
      visNodes(font = list(size = 30), size = 20) %>%
      visOptions(highlightNearest = list(enabled = T, degree = 2, hover = F),
                  selectedBy = "group") %>% 
      visLegend(width = 0.2, position = "right", 
                addNodes = lnodes(), useGroups = F, ncol = 1,
                stepY = 50)
  })
  
  output$loaded <- reactive(1)
})

