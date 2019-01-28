packages <- c('shiny', 'shinydashboard', 'ggplot2', 'arm', 'plyr', 'stringr')

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

library("shinydashboard")
library('leaflet')
library('plyr')
library('ggplot2')
library('dplyr')

server <- function(input, output, session) { 
  assign("play", T, envir = .GlobalEnv)
  autoInvalidate <- reactiveTimer(3500)
  
  observe({
    autoInvalidate()
    if (play) {
      isolate(updateSliderInput(session, 'century', 'Please Select a Century', min = 8, max = 20, value = input$century + 1))
    }
  })
  
  # Read in data
  df <- read.csv('Final-Database.csv')
  # Remove first column - index column
  df <- df[,2:ncol(df)]
  
  coords <- read.csv('span_coords.csv')
  cum_df <- read.csv('CumSumLangYear.csv')
  cum_df[is.na(cum_df)] <- 0
  map_df <- merge(cum_df, coords, by='lang')

  ##Assigning each column name a variable for putting on dashboard##
  all_times <- unique((df$time))
  all_languages <- unique((df$lang))
  all_pos <- unique((df$pos))
  
  ##Creating filters for final_database.csv##
  output$time_filter <- renderUI({
    selectInput('time_filter', label = "Filter By Time",
                   choices = all_times, 
                  selected=all_times,
                   multiple=TRUE)
  })
  
  output$lang_filter <- renderUI({
    selectInput('lang_filter', label = "Filter By Language",
                   choices = all_languages, selected=all_languages,
                   multiple=TRUE)
  })

  
  
  
  output$pos_filter <- renderUI({
    selectInput('pos_filter', label = "Filter By Part of Speech",
                   choices = all_pos, selected=all_pos,
                   multiple=TRUE)
  })
  
  
  # DataTable Render for Database - Tab 1
  output$raw_data <- renderDataTable({
    tmp_df <- df

    # Filter By Time
    if(!is.null(input$time_filter)) {
      # Subset data to only include records whose time is found in the user filtered times
      tmp_df <- subset(tmp_df, time %in% input$time_filter)
    }
    
    # Filter By Language
    if(!is.null(input$lang_filter)) {
      # Subset data to only include records whose language is found in the user filtered languages
      tmp_df <- subset(tmp_df, lang %in% input$lang_filter)
    }

    # Filter By PoS
    if(!is.null(input$pos_filter)) {
      # Subset data to only include records whose PoS is found in the user filtered PoS
      tmp_df <- subset(tmp_df, pos %in% input$pos_filter)
    }
    tmp_df
  })
  
  
  
  
  output$test <- renderText({
    length(input$time_filter) > 0
  })
  
  
  
  observe({
    # When the Play Button is Pressed, change the state of the global play variable
    input$play
    assign("play", !play, envir = .GlobalEnv)
    if (play) {
      updateActionButton(session, 'play', 'Pause')
    } 
    else {
      updateActionButton(session, 'play', 'Play')
    }
  })
  
  dispal <- colorFactor("Spectral", domain = map_df$lang, na.color = "black")
  
  
  
  output$map <- renderLeaflet({
    current_century <- input$century - 6
    select_cols <- c(1, 15, 16, current_century)
    map_selected_df <- map_df[, select_cols]
    names(map_selected_df)[4] <-  'freq'
    
    century_filter <- paste(input$century,"th cent.", sep='')
    words_df <- subset(df, time == century_filter)
    words_df <- words_df[sample(nrow(words_df)),]
    first_10_per_lang <- ddply(words_df, "lang", head, 10)
    
    ten_df <- first_10_per_lang %>% 
      group_by(lang) %>% 
      mutate(ten_words = paste0(word, collapse = "&emsp;</br>")) %>%
      ddply("lang", head, 1)
    
    ten_df <- ten_df[,c('lang', 'ten_words')]  
    
    full_map_df <- join(x=map_selected_df, y=ten_df, by='lang', type='left')
    full_map_df[is.na(full_map_df$ten_words), c('ten_words')] <- 'None'
    
    m <- leaflet() %>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>% 
      addCircleMarkers(data = subset(full_map_df, freq > 0), lng = ~long, lat = ~lat,
                       fillOpacity = 1, stroke = 8, color=~dispal(lang),
                       radius = ~3*log(freq),
                       popup = ~paste("<style>div.leaflet-popup-content-wrapper {opacity: .8;}</style><h4>", lang, "</h4>",
                                      "<b>Total Number of Words: ", freq, 
                                      "</br>Random Words Originated This Century:</br>", ten_words,
                                      sep = ""))
    }
    
    
  )
  
  output$bar <- renderPlot({
    cur_century <- input$century - 6
    bar_df <- cum_df[,c(1, cur_century)]
    names(bar_df) <- c('Language', 'Cumulative_Count')
    ggplot(bar_df) + 
      geom_bar(aes(x=Language, y=Cumulative_Count), stat="identity") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })
}
