library("shinydashboard")
library("leaflet")

dashboardPage(skin = "purple",
  dashboardHeader(title = "Spanish Etymology"),
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Database", tabName = 'data', icon = icon('database')),
      menuItem("Map", tabName = "map", icon = icon("globe"))
      
    )
  ),
  ## Body content
  dashboardBody(
    tabItems(
      # Database
      tabItem(tabName = "data",
              fluidRow(
                box(dataTableOutput('raw_data'), 
                    width = 8),
                box(uiOutput('time_filter'),
                    uiOutput('lang_filter'),
                    uiOutput('pos_filter'),
                    width = 4))),
      
      
      # Map
      tabItem(tabName = "map",
              fluidRow(leafletOutput('map')),
              fluidRow(column(3),column(1, actionButton("play", "Play")),
                       column(8,sliderInput('century', 'Please Select A Century',min = 8, max = 20, 
                        value = 8, step = 1,
                                       sep = ''))),
              fluidRow(plotOutput("bar",height = 500)))

  )
  
      
      ))
  

