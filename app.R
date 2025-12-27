# =========================================================
# Enron Email Explorer — Shiny Application (Fixed)
# Author: Younes SOGANDI (Modified for actual column names)
#
# Features:
# - Top senders and receivers
# - Roles and communication network
# - Temporal activity
# - Basic text mining
# =========================================================

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(lubridate)
library(DT)
library(tidytext)
library(stringr)
library(igraph)
library(ggraph)
library(tidyr)

# ---- Load the Enron data ----
load("Enron.Rdata")

# Ensure proper columns
message <- message %>% filter(!is.na(date), !is.na(sender))
recipientinfo <- recipientinfo %>% filter(!is.na(rvalue))
message$date <- as.POSIXct(message$date)

# ---- User Interface ----
ui <- dashboardPage(
  dashboardHeader(title = "Enron Emails Explorer"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overview", tabName = "overview", icon = icon("home")),
      menuItem("Roles & Network", tabName = "roles", icon = icon("users")),
      menuItem("Temporal Activity", tabName = "time", icon = icon("chart-line")),
      menuItem("Message Content", tabName = "content", icon = icon("file-alt"))
    ),
    
    dateRangeInput("daterange", "Select time period:",
                   start = min(as.Date(message$date)),
                   end   = max(as.Date(message$date))),
    
    sliderInput("topn", "Number of top users to display:",
                min = 5, max = 40, value = 15)
  ),
  
  dashboardBody(
    tabItems(
      # ---- OVERVIEW TAB ----
      tabItem(tabName = "overview",
              fluidRow(
                box(title = "Top Email Senders", width = 6,
                    plotOutput("top_senders")),
                box(title = "Top Email Receivers", width = 6,
                    plotOutput("top_receivers"))
              ),
              fluidRow(
                box(title = "Messages Table", width = 12,
                    DTOutput("msg_table"))
              )
      ),
      
      # ---- ROLES & NETWORK TAB ----
      tabItem(tabName = "roles",
              fluidRow(
                box(title = "Employee Status Distribution", width = 6,
                    plotOutput("status_plot")),
                box(title = "Top Network Centrality", width = 6,
                    DTOutput("centrality_table"))
              ),
              fluidRow(
                box(title = "Communication Network (Top 30 Users)", width = 12,
                    plotOutput("network_plot", height = 450))
              )
      ),
      
      # ---- TEMPORAL TAB ----
      tabItem(tabName = "time",
              fluidRow(
                box(title = "Emails Over Time", width = 12,
                    plotOutput("time_plot", height = 350))
              ),
              fluidRow(
                box(title = "Key Enron Events", width = 12,
                    verbatimTextOutput("events_text"))
              )
      ),
      
      # ---- CONTENT TAB ----
      tabItem(tabName = "content",
              fluidRow(
                box(title = "Most Frequent Words", width = 6,
                    plotOutput("word_plot")),
                box(title = "TF-IDF by Sender", width = 6,
                    DTOutput("tfidf_table"))
              )
      )
    )
  )
)

# ---- Server Logic ----
server <- function(input, output) {
  
  # ---- Reactive filtered messages ----
  filtered_msgs <- reactive({
    req(input$daterange)
    message %>%
      filter(as.Date(date) >= input$daterange[1],
             as.Date(date) <= input$daterange[2])
  })
  
  filtered_msg_ids <- reactive({
    filtered_msgs()$mid  # FIXED: changed from $id to $mid
  })
  
  # ---- TOP SENDERS ----
  output$top_senders <- renderPlot({
    top_senders <- filtered_msgs() %>%
      count(sender, sort = TRUE) %>%
      slice_head(n = input$topn)
    
    ggplot(top_senders, aes(reorder(sender, n), n)) +
      geom_col(fill = "steelblue") +
      coord_flip() +
      labs(x = "", y = "Emails sent") +
      theme_minimal()
  })
  
  # ---- TOP RECEIVERS ----
  output$top_receivers <- renderPlot({
    recipients <- recipientinfo %>%
      filter(mid %in% filtered_msg_ids()) %>%
      count(rvalue, sort = TRUE) %>%
      slice_head(n = input$topn)
    
    if(nrow(recipients) == 0){
      plot.new()
      text(0.5, 0.5, "No recipient data available", cex = 2)
      return()
    }
    
    ggplot(recipients, aes(reorder(rvalue, n), n)) +
      geom_col(fill = "coral") +
      coord_flip() +
      labs(x = "", y = "Emails received") +
      theme_minimal()
  })
  
  # ---- MESSAGE TABLE ----
  output$msg_table <- renderDT({
    filtered_msgs() %>%
      select(mid, date, sender, subject) %>%
      arrange(desc(date))
  }, options = list(pageLength = 10, scrollX = TRUE))
  
  # ---- STATUS DISTRIBUTION ----
  output$status_plot <- renderPlot({
    employeelist %>%
      count(status, sort = TRUE) %>%
      ggplot(aes(reorder(status, n), n)) +
      geom_col(fill = "lightgreen") +
      coord_flip() +
      labs(x = "", y = "Number of employees") +
      theme_minimal()
  })
  
  # ---- NETWORK CENTRALITY ----
  output$centrality_table <- renderDT({
    edges <- recipientinfo %>%
      filter(mid %in% filtered_msg_ids()) %>%
      left_join(message %>% select(mid, sender), by = "mid") %>%  # FIXED: changed from id to mid
      transmute(from = sender, to = rvalue) %>%
      filter(!is.na(from), !is.na(to))
    
    if(nrow(edges) == 0) return(data.frame(Name = character(), InDegree = numeric(), OutDegree = numeric()))
    
    g <- graph_from_data_frame(edges, directed = TRUE)
    centrality <- data.frame(
      Name = V(g)$name,
      InDegree = degree(g, mode = "in"),
      OutDegree = degree(g, mode = "out")
    )
    centrality %>% arrange(desc(OutDegree)) %>% slice_head(n = input$topn)
  }, options = list(pageLength = 10))
  
  # ---- NETWORK PLOT (with ggraph, colored by status) ----
  output$network_plot <- renderPlot({
    edges <- recipientinfo %>%
      filter(mid %in% filtered_msg_ids()) %>%
      left_join(message %>% select(mid, sender), by = "mid") %>%  # FIXED: changed from id to mid
      transmute(from = sender, to = rvalue) %>%
      filter(!is.na(from), !is.na(to))
    
    if(nrow(edges) == 0){
      plot.new(); text(0.5, 0.5, "No data available for selected period", cex = 2)
      return()
    }
    
    g <- graph_from_data_frame(edges, directed = TRUE)
    top_nodes <- names(sort(degree(g), decreasing = TRUE))[1:min(30, length(V(g)))]
    g_small <- induced_subgraph(g, vids = top_nodes)
    
    node_data <- data.frame(name = V(g_small)$name) %>%
      left_join(employeelist %>% select(email, status), by = c("name" = "email"))
    
    V(g_small)$status <- node_data$status
    
    ggraph(g_small, layout = 'fr') +
      geom_edge_link(alpha = 0.3, arrow = arrow(length = unit(2, 'mm')), end_cap = circle(2, 'mm')) +
      geom_node_point(aes(color = status), size = 5) +
      geom_node_text(aes(label = name), repel = TRUE, size = 3) +
      theme_void() +
      labs(color = "Status")
  })
  
  # ---- TEMPORAL EVOLUTION ----
  output$time_plot <- renderPlot({
    filtered_msgs() %>%
      mutate(day = as.Date(date)) %>%
      count(day) %>%
      ggplot(aes(day, n)) +
      geom_line(color = "steelblue", size = 0.8) +
      geom_point(color = "steelblue", size = 1.5) +
      geom_vline(xintercept = as.Date("2001-10-31"), linetype = "dashed", color = "red") +
      geom_vline(xintercept = as.Date("2001-12-02"), linetype = "dashed", color = "darkred") +
      labs(x = "Date", y = "Daily emails") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$events_text <- renderText({
    paste(
      "Oct 31, 2001  - SEC launches investigation into Enron",
      "Dec 02, 2001  - Enron files for bankruptcy (largest in US history at the time)",
      sep = "\n"
    )
  })
  
  # ---- CONTENT ANALYSIS ----
  output$word_plot <- renderPlot({
    text_data <- filtered_msgs() %>%
      mutate(text = paste(subject)) %>%
      unnest_tokens(word, text) %>%
      anti_join(get_stopwords(), by = "word") %>%
      mutate(word = str_remove_all(word, "[^a-zA-Z]")) %>%
      filter(word != "")
    
    if(nrow(text_data) == 0){
      plot.new(); text(0.5, 0.5, "No text data available", cex = 2); return()
    }
    
    text_data %>%
      count(word, sort = TRUE) %>%
      slice_head(n = 25) %>%
      ggplot(aes(reorder(word, n), n)) +
      geom_col(fill = "darkgreen") +
      coord_flip() +
      labs(x = "", y = "Frequency") +
      theme_minimal()
  })
  
  output$tfidf_table <- renderDT({
    text_data <- filtered_msgs() %>%
      mutate(text = paste(subject)) %>%
      unnest_tokens(word, text) %>%
      anti_join(get_stopwords(), by = "word") %>%
      mutate(word = str_remove_all(word, "[^a-zA-Z]")) %>%
      filter(word != "")
    
    if(nrow(text_data) == 0) return(data.frame(sender=character(), word=character(), tf_idf=numeric()))
    
    text_data %>%
      count(sender, word) %>%
      bind_tf_idf(word, sender, n) %>%
      arrange(desc(tf_idf)) %>%
      slice_head(n = 200) %>%
      select(sender, word, tf_idf)
  }, options = list(pageLength = 10, scrollX = TRUE))
}

# ---- Run App ----
shinyApp(ui, server)