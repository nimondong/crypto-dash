library(shiny)
library(tidyverse)
library(plotly)
library(shinydashboard)
library(tidyquant)
library(coinmarketcapr)
library(treemap)
library(janitor)
library(scales)
library(statebins)
library(viridis)
library(janitor)
library(skimr)
library(ggstance)
library(lubridate)
library(RColorBrewer)
library(splines)
library(ggrepel)
library(viridis)
library(sf)
library(lattice)
library(wordcloud)
library(igraph)
library(ggraph)
library(textrank)
library(udpipe)

price_dat <- read_csv("data/price_dat.csv")
google_geo_dat <- read_csv("data/google_geo_dat.csv")
google_time_dat <- read_csv("data/google_time_dat.csv")
ant_18 <- read_csv("data/ant_18.csv")

theme_global <- theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, family = "Arial", face = "bold", size = 13),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.text.x = element_text(angle = 70, hjust = 1),
        axis.text = element_text(family = "Arial", size = 7),
        axis.title = element_text(family = "Arial", size = 10, face = "bold"),
        legend.text = element_text(family = "Arial", size = 7),
        legend.title = element_text(family = "Arial", size = 10))

ui <- dashboardPage(
  
  dashboardHeader(
    title = "CRYPTO-DASH"
  ),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(h4(strong("Hourly Closing Price")), tabName = "closing_price"),
      menuItem(h4(strong("Market Capitalization")), tabName = "market_cap"),
      menuItem(h4(strong("Search Trends")), tabName = "search_trends"),
      menuItem(h4(strong("Buzz Words")), tabName = "buzz_words"),
      menuItem(h4(strong("About")), tabName = "about"))
  ),
  
  dashboardBody(
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    
    tabItems(
      
      tabItem(tabName = "closing_price",
              fluidRow(
                box(
                  width = 3,
                  selectInput("symb", 
                              label = "Cryptocurrency Symbol",
                              choices = c("BTC-USD", 
                                          "ETH-USD",
                                          "LTC-USD"),
                              selected = "BTC-USD"),
                  dateRangeInput('dateRange',
                                 label = paste("Date Range"),
                                 start = Sys.Date() -7, end = Sys.Date(),
                                 min = Sys.Date() - 365, max = Sys.Date(),
                                 separator = " to ", format = "mm/dd/yyyy")
                ),
                box(width = 9 , plotlyOutput("graph_closing_price", width = "100%"))
              ),
              fluidRow(
                box(width = 3,
                    radioButtons("pickacoin", "Select Coin",
                                 c("BTC-USD" = "BTC", 
                                   "ETH-USD" = "ETH", 
                                   "LTC-USD" = "LTC",
                                   "USDT-USD" = "USDT",
                                   "XRP-USD" = "XRP"),
                                 selected = "BTC"),
                    dateRangeInput('dateRangeMin',
                                   label = paste("Date Range"),
                                   start = "2019-01-01", end = "2019-05-31",
                                   min = "2013-04-28", max = "2019-12-04",
                                   separator = " to ", format = "mm/dd/yyyy")
                ),
                box(width = 9, plotOutput("candle_stick", width = "100%"))
              )
      ),
      
      tabItem(tabName = "market_cap", 
              fluidRow(
                box(width = 12, plotOutput("graph_tree_map", width = "100%"))
              ),
              fluidRow(
                box(width = 3,
                    checkboxGroupInput("coinselect", 
                                       label = "Select Currencies", 
                                       choices = list("BTC-USD" = "BTC", 
                                                      "ETH-USD" = "ETH", 
                                                      "LTC-USD" = "LTC",
                                                      "USDT-USD" = "USDT",
                                                      "XRP-USD" = "XRP"),
                                       selected = "BTC")),
                box(width = 9, plotlyOutput("graph_market_cap", width = "100%"))
              )
      ),
      
      tabItem(tabName = "search_trends", 
              fluidRow(
                box(width = 3,
                    radioButtons("coin_search", "Select Coin",
                                 c("BTC-USD" = "BTC", 
                                   "ETH-USD" = "ETH", 
                                   "LTC-USD" = "LTC",
                                   "USDT-USD" = "USDT",
                                   "XRP-USD" = "XRP"),
                                 selected = "BTC")),
                box(width = 9, plotOutput("search_output", width = "100%"))
              ),
              fluidRow(
                box(width = 3,
                    radioButtons("coin_search_trends", "Select Coin",
                                 c("BTC-USD" = "BTC", 
                                   "ETH-USD" = "ETH", 
                                   "LTC-USD" = "LTC",
                                   "USDT-USD" = "USDT",
                                   "XRP-USD" = "XRP"),
                                 selected = "BTC"),
                    dateRangeInput('coin_seach_trends_time',
                                   label = paste("Date Range"),
                                   start = "2016-01-01", end = "2019-05-31",
                                   min = "2013-01-01", max = "2020-06-01",
                                   separator = " to ", format = "mm/dd/yyyy")),
                box(width = 9, plotOutput("search_trends_output", width = "100%"))
              )
      ),
      
      tabItem(tabName = "buzz_words", 
              fluidRow(
                box(width = 12, plotOutput("buzz_words_nouns", width = "100%")),
                box(width = 12, plotOutput("buzz_words_adj", width = "100%")),
                box(width = 12, plotOutput("buzz_words_verb", width = "100%")),
                box(width = 12, plotOutput("buzz_words_rake", width = "100%")),
                box(width = 12, plotOutput("buzz_words_cooccurrences", width = "100%"))
              )),
      
      tabItem(tabName = "about", 
              h2("About", align="center"),
              p("Created by Nimon Dong, Min Kim, and Dane Holmes", style = "align: center"),
              p("Source 1: https://www.kaggle.com/sudalairajkumar/cryptocurrencypricehistory"),
              p("Source 2: https://www.kaggle.com/kashnitsky/news-about-major-cryptocurrencies-20132018-40k"),
              p("Source 3: https://trends.google.com/trends/explore?date=2013-01-01%202018-12-31&geo=US&q=bitcoin"))
    )))

server <- function(input, output) {
  
  tiingo_api_key('a517f985b208a4b4a535f99f26e303625d699caa')
  setup(api_key = "b66a77f4-a35c-4ef2-81a8-c9fabf528ccc")
  
  output$graph_closing_price <- renderPlotly({
    
    select_coin <- switch(input$symb, 
                          "BTC-USD" = "btcusd",
                          "ETH-USD" = "ethusd",
                          "LTC-USD" = "ltcusd")
    
    Coin <- tq_get(select_coin,
                   get    = "tiingo.crypto",
                   from   = input$dateRange[1],
                   to     = input$dateRange[2],
                   resample_frequency = "1day")
    
    ggplotly(Coin %>%
               ggplot(aes(x = date, y = close)) +
               geom_line() +
               labs(y = "Closing Price", x = NULL))
    
    
  })
  
  output$candle_stick <- renderPlot({
    
    price_dat %>%
      filter(symbol == input$pickacoin,
             date < input$dateRangeMin[2] & date > input$dateRangeMin[1]) %>% 
      ggplot(aes(x = date, y = close)) +
      geom_smooth(size = 0.5, se = FALSE, span = 0.3, color = "gold") +
      tidyquant::geom_candlestick(aes(open = open, high = high, low = low, close = close), 
                                  colour_up = "darkgreen", colour_down = "darkred", 
                                  fill_up = "darkgreen", fill_down = "darkred") +
      ggtitle("Historical Trading Prices") +
      labs(x = "Date", y = "Price (USD)")
    
    
  })
  
  output$graph_tree_map <- renderPlot({
    
    all_coins <- get_crypto_listings() %>% 
      as_tibble() %>% 
      clean_names()
    
    all_coins %>%
      select(name, symbol, usd_market_cap) %>%
      mutate(usd_market_cap_formated = paste0(name, '\n', "(",symbol, "-USD", ")", '\n', 
                                              dollar(usd_market_cap))) %>%
      treemap(index = "usd_market_cap_formated", 
              vSize = "usd_market_cap", 
              title = "Cryptocurrency Market Cap", 
              palette = "Purples")
  })
  
  output$graph_market_cap <- renderPlotly({
    
    price_dat %>%
      filter(symbol == c(input$coinselect)) %>%
      ggplot(aes(x = date, y = market_cap, color = symbol)) +
      geom_line() + 
      ggtitle("Historical Market Cap") +
      labs(x = "Date", y = "Market Cap")  +
      theme_global
    
  })
  
  output$search_output <- renderPlot({
    
    google_geo_dat %>% 
      pivot_longer(cols = 2:12, names_to = "currency", values_to = "search_popularity") %>%
      filter(currency == input$coin_search) %>%
      ggplot(aes(fill = search_popularity, geometry = geometry)) +
      geom_statebins(aes(state = Region)) + 
      scale_fill_viridis(discrete = FALSE) + 
      theme_void()
    
  })
  
  output$search_trends_output <- renderPlot({
    
    google_time_dat$Region <- as.Date(paste(google_time_dat$Region, "-01", sep = ""))
    
    google_time_dat %>%
      rename("date" = "Region") %>%
      pivot_longer(cols = 2:12, names_to = "symbol", values_to = "search_popularity") %>% 
      filter(symbol == input$coin_search_trends) %>%
      filter(date < date(input$coin_seach_trends_time[2]) & date > date(input$coin_seach_trends_time[1])) %>% 
      ggplot(aes(x = date, y = search_popularity, color = symbol)) +
      geom_line() + 
      ggtitle("Historical Google Search Popularity") +
      labs(x = "Date", y = "Search Popularity") +
      theme_global
    
  })
  
  output$buzz_words_nouns <- renderPlot({
    
    stats <- ant_18 %>% subset(upos %in% c("NOUN"))
    stats <- txt_freq(stats$token)
    stats$key <- factor(stats$key, levels = rev(stats$key))
    
    ggplot(head(stats, 20), aes(x = key, y = freq)) + 
      geom_col(fill = "#6D9EEB") + 
      coord_flip() +
      ggtitle("Frequently Occurring Nouns") + 
      labs(x = "Word", y = "Frequency") +
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Arial", face = "bold"),
            axis.text = element_text(family = "Arial", color = "black", size = 5),
            axis.title = element_text(family = "Arial", size = 8, face = "bold")) +
      scale_y_continuous(expand = expand_scale(mult = c(0, .2)))
    
  })
  
  output$buzz_words_adj <- renderPlot({
    
    stats <- subset(ant_18, upos %in% c("ADJ")) 
    stats <- txt_freq(stats$token)
    stats$key <- factor(stats$key, levels = rev(stats$key))
    
    ggplot(head(stats, 20), aes(x = key, y = freq)) + 
      geom_col(fill = "#FFD965") + 
      coord_flip() +
      ggtitle("Frequently Occurring Adjectives") + 
      labs(x = "Word", y = "Frequency") +
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Arial", face = "bold"),
            axis.text = element_text(family = "Arial", color = "black", size = 5),
            axis.title = element_text(family = "Arial", size = 8, face = "bold")) +
      scale_y_continuous(expand = expand_scale(mult = c(0, .2)))
  })
  
  output$buzz_words_verb <- renderPlot({
    
    stats <- subset(ant_18, upos %in% c("VERB")) 
    stats <- txt_freq(stats$token)
    stats$key <- factor(stats$key, levels = rev(stats$key))
    
    ggplot(head(stats, 20), aes(x = key, y = freq)) + 
      geom_col(fill = "#E06566") + 
      coord_flip() +
      ggtitle("Frequently Occurring Verbs") + 
      labs(x = "Word", y = "Frequency") +
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Arial", face = "bold"),
            axis.text = element_text(family = "Arial", color = "black", size = 5),
            axis.title = element_text(family = "Arial", size = 8, face = "bold")) +
      scale_y_continuous(expand = expand_scale(mult = c(0, .2)))
  })
  
  output$buzz_words_rake <- renderPlot({
    
    stats <- keywords_rake(x = ant_18, term = "lemma", group = "doc_id", 
                           relevant = ant_18$upos %in% c("NOUN", "ADJ"))
    stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
    
    ggplot(head(subset(stats, freq > 3), 20), aes(x = key, y = rake)) + 
      geom_col(fill = "#93C47C") + 
      coord_flip() +
      ggtitle("Keywords identified by RAKE") + 
      labs(x = "Word", y = "Frequency") +
      theme_classic() + 
      theme(plot.title = element_text(hjust = 0.5, size = 12, family = "Arial", face = "bold"),
            axis.text = element_text(family = "Arial", color = "black", size = 5),
            axis.title = element_text(family = "Arial", size = 8, face = "bold")) +
      scale_y_continuous(expand = expand_scale(mult = c(0, .2)))
  })
  
  output$buzz_words_cooccurrences <- renderPlot({
    
    stats <- cooccurrence(x = subset(ant_18, upos %in% c("NOUN", "ADJ")), 
                          term = "lemma", group = c("doc_id", "paragraph_id", "sentence_id"))
    
    wordnet <- head(stats, 40)
    wordnet <- graph_from_data_frame(wordnet)
    ggraph(wordnet, layout = "fr") +
      geom_edge_link(aes(width = cooc, edge_alpha = cooc), edge_colour = "blue") +
      geom_node_text(aes(label = name), col = "purple", size = 6) +
      theme_graph(base_family = "Arial") +
      theme(legend.position = "none") +
      ggtitle("Cooccurrences within 3 words distance")
    
  })
}

shinyApp(ui, server)