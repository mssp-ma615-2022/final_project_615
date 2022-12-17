pacman::p_load(shiny, leaflet, magrittr, dplyr, tidyverse, googleway, mapsapi, ggplot2, kableExtra, knitr)

# stoptime <- read.csv("stoptime_final.csv")
stoptime <- read.delim("stop_times_final.txt", sep = ",", header = TRUE,fill = TRUE)
stop <- read.delim("stops.txt", sep = ",", header = TRUE,fill = TRUE)
route01 <- read.csv("route01_final.csv")
ferry_stop <- read.csv("ferry_stop_final.csv")
ferry_daily <- read.csv("ferry_daily_final.csv")
rail_stop <- read.csv("rail_stop_final.csv")
rail <- read.csv("rail_final.csv")
tt_q1_2021_lr <- read.csv("tt_q1_2021_lr_final.csv")



ui <- fluidPage(
  navbarPage("Boston Public Transit Map", collapsible = TRUE, inverse = TRUE,
             tabPanel("Boston MBTA Rapid Transit", 
                      fluidRow(
                        column(
                          width = 3,
                          selectInput(inputId = "Route", label = "Select the line:", multiple = FALSE, choices = unique(tt_q1_2021_lr$route_id),
                                      selected = "Green-B")),
                        column(
                          width = 3,
                          uiOutput("select_var1")),
                        column(
                          width = 3,
                          uiOutput("select_var2"))
                      ),
                      leafletOutput(outputId = "map"),
                      
                      hr(),
                      
                      fluidRow(
                        column(
                          width = 6,
                          h4("Graph of Time Distribution"),
                          plotOutput("plot")),
                        column(
                          width = 6,
                          h4("Summary Table of Time Distribution"),
                          verbatimTextOutput("mtba_sum"),
                          verbatimTextOutput("mtba_out")
                        )
                      )
                      
             ),
             
             tabPanel("Boston Commuter Rail", 
                      fluidRow(
                        column(
                          width = 3,
                          selectInput(inputId = "Route_rail", label = "Select the line:", multiple = FALSE, 
                                      choices = unique(rail$route_id),
                                      selected = "CR-Middleborough")),
                        column(
                          width = 3,
                          uiOutput("select_var1_rail")),
                        column(
                          width = 3,
                          uiOutput("select_var2_rail"))
                      ),
                      leafletOutput(outputId = "map_rail"),
                      
                      hr(),
                      
                      fluidRow(
                        column(
                          width = 6,
                          h4("Graph of Time Distribution"),
                          plotOutput("plot_rail")),
                        column(
                          width = 6,
                          h4("Summary Table of Time Distribution"),
                          verbatimTextOutput("mtba_sum_rail"),
                          verbatimTextOutput("mtba_out_rail")
                        )
                      )
                      
             ),
             
             tabPanel("Boston Bus Transit", 
                      fluidRow(
                        column(
                          width = 3,
                          selectInput(inputId = "Route_bus", label = "Select the Bus Route:", multiple = FALSE, 
                                      choices = unique(route01$route_id))),
                        column(
                          width = 3,
                          uiOutput("select_var1_bus")),
                        column(
                          width = 3,
                          uiOutput("select_var2_bus"))
                      ),
                      leafletOutput(outputId = "map_bus"),
                      
                      hr(),
                      
                      h4("Graph of Time Distribution"),
                      plotOutput("plot_bus")
             ),
             tabPanel("Boston Ferry", 
                      fluidRow(
                        column(
                          width = 3,
                          selectInput(inputId = "Route_ferry", label = "Select the ferry Route:", multiple = FALSE, 
                                      choices = unique(ferry_daily$route_id),
                                      selected = "F1")),
                        column(
                          width = 3,
                          uiOutput("select_var1_ferry")),
                        column(
                          width = 3,
                          uiOutput("select_var2_ferry"))
                      ),
                      leafletOutput(outputId = "map_ferry"),
                      
                      hr(),
                      
                      h4("Graph of Time Distribution"),
                      plotOutput("plot_ferry")
             )
  )
)



server <- function(input, output) {
  
  ### Table part
  ## Rapid Transit
  
  tab <- reactive({ 
    start <- stop %>% filter(stop_name==input$Stop1)
    filter1 <- tt_q1_2021_lr %>% filter(from_stop_id %in% start$stop_id)
    end <- stop %>% filter(stop_name==input$Stop2)
    temp <- filter1 %>% filter(to_stop_id %in% end$stop_id) %>% mutate(travel_time_min=travel_time_sec/60)
    temp$travel_time_min <- round(temp$travel_time_min,2)
    temp
  })
  
  
  stop_names <- reactive({
    all_name <- tt_q1_2021_lr %>% filter(route_id==input$Route) 
    from <- unique(all_name$from_stop_id)
    to <- unique(all_name$to_stop_id)
    gb_stopid <- union(from,to)
    stop %>% filter(stop_id %in% gb_stopid) %>% arrange(stop_id)
  })
  
  output$select_var1 <- renderUI({
    
    selectizeInput(inputId = "Stop1", label = "Select origin:", multiple = FALSE, choices = sort(stop_names()$stop_name))
  })
  
  output$select_var2 <- renderUI({
    
    selectizeInput(inputId = "Stop2", label = "Select destination:", multiple = FALSE, choices = sort(stop_names()$stop_name))
  })
  
  
  output$plot<-renderPlot({
    tab() %>% ggplot(aes(x=travel_time_min)) +
      geom_histogram(colour="black", fill="white") + xlim(0,2*median(tab()$travel_time_min)) +
      geom_vline(aes(xintercept = median(tab()$travel_time_min)), colour="red", size=1.5) +
      geom_text(aes(x=median(tab()$travel_time_min), y=0, label="Median"), colour="red", size=5, angle=0, vjust=-0.4, hjust=0) +
      labs(x="Time in minutes")
  })
  
  output$mtba_sum <- renderPrint ({
    summary(tab()$travel_time_min, digits=2)  
  })
  output$mtba_out <- renderPrint ({
    pvec <- c(0.05, 0.1, 0.5, 0.9,0.95) 
    quantile(tab()$travel_time_min, pvec, digits=2)
  }) 
  
  ## Bus
  
  tab_bus <- reactive({ 
    start <- stop %>% filter(stop_name==input$Stop1_bus)
    filter1 <- route01 %>% filter(from_stop_id %in% start$stop_id)
    end <- stop %>% filter(stop_name==input$Stop2_bus)
    filter1 %<>% filter(to_stop_id %in% end$stop_id)
    
  })
  
  
  stop_names_bus_from <- reactive({
    all_name_from <- route01 %>% filter(route_id==input$Route_bus) 
    from <- unique(all_name_from$from_stop_id)
    stop %>% filter(stop_id %in% from) %>% arrange(stop_id)
  })
  
  stop_names_bus_to <- reactive({
    from_busstop <- stop_names_bus_from() %>% filter(stop_name==input$Stop1_bus)
    all_name_to <- route01 %>% filter(route_id==input$Route_bus & from_stop_id==from_busstop$stop_id) 
    to <- unique(all_name_to$to_stop_id)
    stop %>% filter(stop_id %in% to) %>% arrange(stop_id)
  })
  
  output$select_var1_bus <- renderUI({
    
    selectizeInput(inputId = "Stop1_bus", label = "Select origin:", multiple = FALSE, 
                   choices = sort(stop_names_bus_from()$stop_name))
  })
  
  output$select_var2_bus <- renderUI({
    
    selectizeInput(inputId = "Stop2_bus", label = "Select destination:", multiple = FALSE, 
                   choices = sort(stop_names_bus_to()$stop_name))
  })
  
  
  output$plot_bus <- renderPlot({
    tab_bus() %>% ggplot(aes(x=time)) +
      geom_histogram(colour="black", fill="white") + xlim(0,2*median(tab_bus()$time)) +
      labs(x="Time in minutes")
  })
  
  
  ## Ferry
  
  
  stop_names_ferry_from <- reactive({
    ferry_daily %>% dplyr::filter(route_id==input$Route_ferry) 
    
  })
  
  stop_names_ferry_to <- reactive({
    stop_names_ferry_from() %>% dplyr::filter(arrival_terminal==input$Stop1_ferry)
  })
  
  output$select_var1_ferry <- renderUI({
    
    selectizeInput(inputId = "Stop1_ferry", label = "Select origin:", multiple = FALSE, 
                   choices = unique(stop_names_ferry_from()$departure_terminal))
  })
  
  output$select_var2_ferry <- renderUI({
    
    selectizeInput(inputId = "Stop2_ferry", label = "Select destination:", multiple = FALSE, 
                   choices = unique(stop_names_ferry_to()$departure_terminal))
  })
  
  
  output$plot_ferry <- renderPlot({
    stop_names_ferry_to() %>% ggplot(aes(x=time)) +
      geom_histogram(colour="black", fill="white") + xlim(0,2*median(stop_names_ferry_to()$time)) +
      labs(x="Time in minutes")
  })
  
  ## Commuter Rail
  
  tab_rail <- reactive({ 
    start <- rail_stop %>% filter(stop_name==input$Stop1_rail)
    filter1 <- rail %>% filter(from_stop_id %in% start$stop_id)
    end <- rail_stop %>% filter(stop_name==input$Stop2_rail)
    filter1 %>% filter(to_stop_id %in% end$stop_id) 
  })
  
  
  stop_names_rail <- reactive({
    all_name <- rail %>% filter(route_id==input$Route_rail) 
    from <- unique(all_name$from_stop_id)
    to <- unique(all_name$to_stop_id)
    rail_stopid <- union(from,to)
    rail_stop %>% filter(stop_id %in% rail_stopid) %>% arrange(stop_id)
  })
  
  output$select_var1_rail <- renderUI({
    
    selectizeInput(inputId = "Stop1_rail", label = "Select origin:", multiple = FALSE, 
                   choices = sort(stop_names_rail()$stop_name))
  })
  
  output$select_var2_rail <- renderUI({
    
    selectizeInput(inputId = "Stop2_rail", label = "Select destination:", multiple = FALSE, choices = sort(stop_names_rail()$stop_name))
  })
  
  
  output$plot_rail<-renderPlot({
    tab_rail() %>% ggplot(aes(x=time)) +
      geom_histogram(colour="black", fill="white") + xlim(0,2*median(tab_rail()$time)) +
      geom_vline(aes(xintercept = median(tab_rail()$time)), colour="red", size=1.5) +
      geom_text(aes(x=median(tab_rail()$time), y=0, label="Median"), 
                colour="red", size=5, angle=0, vjust=-0.4, hjust=0) +
      labs(x="Time in minutes")
  })
  
  output$mtba_sum_rail <- renderPrint ({
    summary(tab_rail()$time, digits=2)  
  })
  output$mtba_out_rail <- renderPrint ({
    pvec <- c(0.05, 0.1, 0.5, 0.9,0.95) 
    quantile(tab_rail()$time, pvec, digits=2)
  }) 
  
  
  ### Map Part
  
  ## Commuter Rail
  
  all_stops_rail <- reactive({
    start <- rail_stop %>% filter(stop_name==input$Stop1_rail)
    filter1 <- rail %>% filter(from_stop_id %in% start$stop_id)
    end <- rail_stop %>% filter(stop_name==input$Stop2_rail)
    filter2 <- filter1 %>% filter(to_stop_id %in% end$stop_id)
    all_stop <- rail %>% filter(direction_id %in% filter2$direction_id & route_id==input$Route_rail)
    raila <- unique(all_stop$from_stop_id)
    railb <- unique(all_stop$to_stop_id)
    sall <- union(raila,railb)
    all_stop <- rail_stop %>% filter(stop_id %in% sall) %>% arrange(stop_id)
    
    if(input$Route_rail=="CR-Middleborough" | input$Route_rail=="CR-Providence" |
       input$Route_rail=="CR-Fairmount" | input$Route_rail=="CR-Kingston" | input$Route_rail=="CR-Lowell" |
       input$Route_rail=="CR-Haverhill" | input$Route_rail=="CR-Newburyport" | input$Route_rail=="CR-Foxboro"){
      all_stop %>% arrange(Y)
    } 
    else if(input$Route_rail=="CR-Franklin" | input$Route_rail=="CR-Greenbush"){
      all_stop %>% arrange(X)
    }
    else if(input$Route_rail=="CR-Needham"){
      all_stop %<>% arrange(X)
      all_stop %>% mutate(order=c(2,1,3:12)) %>% arrange(order)
    }
    
    
    else{
      all_stop %>% arrange(stop_id)
    }
    
  })
  
  
  chose_stops_rail <- reactive({
    start <- rail_stop %>% filter(stop_name==input$Stop1_rail)
    filter1 <- rail %>% filter(from_stop_id %in% start$stop_id)
    end <- rail_stop %>% filter(stop_name==input$Stop2_rail)
    filter2 <- filter1 %>% filter(to_stop_id %in% end$stop_id)
    start_id <- unique(filter2$from_stop_id)[[1]]
    end_id <- unique(filter2$to_stop_id)[[1]]
    start_index <- which(all_stops_rail()$stop_id==start_id)
    end_index <- which(all_stops_rail()$stop_id==end_id)
    all_stops_rail()[start_index:end_index,] 
  })
  
  rail_stops <- reactive({
    
    all_stops_rail() %>% mutate(all_stops_rail(), cntnt=paste0('<br><strong>Stop Name:</strong> ',stop_name
    ))
  })
  
  chose_stop_rail <- reactive({
    
    chose_stops_rail() %>% mutate(chose_stops_rail(), cntnt=paste0('<br><strong>Stop Name:</strong> ',stop_name
    ))
  })
  
  output$map_rail <- renderLeaflet({
    
    leaflet() %>%
      # addCircles(data = rail_stops(),lng = ~X, lat = ~Y) %>% 
      addTiles() %>%
      addPolylines(
        data = rail_stops(), lat =~Y, lng =~X, color = "green",
        labelOptions = labelOptions(noHide = TRUE, direction = 'top')) %>%
      
      addCircleMarkers(data = rail_stops(), lat =~Y, lng =~X, 
                       radius = 3, 
                       popup = ~as.character(rail_stops()$cntnt),
                       color = "blue",
                       stroke = FALSE, 
                       fillOpacity = 0.8) %>%
      
      addPolylines(
        data = chose_stop_rail(), lat =~Y, lng =~X, color = "red",
        labelOptions = labelOptions(noHide = TRUE, direction = 'top')) %>%
      
      addCircles(data=chose_stops_rail(), lat =~Y, lng =~X, color = "red", radius = 5,
                 popup = ~as.character(chose_stop_rail()$cntnt)) %>%
      
      addProviderTiles(
        "CartoDB.Positron",
        group = "CartoDB.Positron"
      )
  })
  
  ## Ferry
  
  all_stops_ferry <- reactive({
    ferry_stop %>% dplyr::filter(stop_name==input$Stop1_ferry | stop_name==input$Stop2_ferry)
    
  })
  
  
  ferry_stops <- reactive({
    
    all_stops_ferry() %>% mutate(all_stops_ferry(), cntnt=paste0('<br><strong>Stop Name:</strong> ',stop_name
    ))
  })
  
  output$map_ferry <- renderLeaflet({
    
    leaflet() %>%
      # addCircles(data = ferry_stops(),lng = ~X, lat = ~Y) %>% 
      addTiles() %>%
      addPolylines(
        data = ferry_stops(), lat =~Y, lng =~X, color = "green",
        labelOptions = labelOptions(noHide = TRUE, direction = 'top')) %>%
      
      addCircleMarkers(data = ferry_stops(), lat =~Y, lng =~X, 
                       radius = 3, 
                       popup = ~as.character(ferry_stops()$cntnt),
                       color = "blue",
                       stroke = FALSE, 
                       fillOpacity = 0.8) %>%
      
      addProviderTiles(
        "CartoDB.Positron",
        group = "CartoDB.Positron"
      )
  })
  
  ## Bus
  
  all_stops_bus <- reactive({
    start <- stop %>% filter(stop_name==input$Stop1_bus)
    filter1 <- route01 %>% filter(from_stop_id %in% start$stop_id)
    end <- stop %>% filter(stop_name==input$Stop2_bus)
    filter2 <- filter1 %>% filter(to_stop_id %in% end$stop_id)
    all_stop <- route01 %>% filter(direction_id %in% filter2$direction_id & route_id==input$Route_bus)
    busa <- unique(all_stop$from_stop_id)
    busb <- unique(all_stop$to_stop_id)
    sall <- union(busa,busb)
    all_stop <- stop %>% filter(stop_id %in% sall) %>% arrange(stop_id)
    all_stop %<>% arrange(stop_id)
    
    if(input$Route_bus=="1" & all_stop$stop_id[1]!="110"){
      test1 <-  all_stop %>% mutate(row_num=c(3, 7, 2, 1, 6, 8, 9, 5, 4))
      test1 %>% arrange(row_num)
    } 
    else if(input$Route_bus=="1" & all_stop$stop_id[1]=="110"){
      test1 <-  all_stop %>% mutate(row_num=c(1, 6, 7, 8, 9, 2, 3, 4, 5))
      test1 %>% arrange(row_num)
    } 
    
  })
  
  
  bus_stops <- reactive({
    
    all_stops_bus() %>% mutate(all_stops_bus(), cntnt=paste0('<br><strong>Stop Name:</strong> ',stop_name
    ))
  })
  
  
  output$map_bus <- renderLeaflet({
    
    leaflet() %>%
      # addCircles(data = bus_stops(),lng = ~stop_lon, lat = ~stop_lat) %>% 
      addTiles() %>%
      addPolylines(
        data = bus_stops(), lat =~stop_lat, lng =~stop_lon, color = "green",
        labelOptions = labelOptions(noHide = TRUE, direction = 'top')) %>%
      
      addCircleMarkers(data = bus_stops(), lat =~stop_lat, lng =~stop_lon, 
                       radius = 3, 
                       popup = ~as.character(bus_stops()$cntnt),
                       color = "blue",
                       stroke = FALSE, 
                       fillOpacity = 0.8) %>%
      
      # addPolylines(
      #   data = chose_stop_bus(), lat =~stop_lat, lng =~stop_lon, color = "red",
      #   labelOptions = labelOptions(noHide = TRUE, direction = 'top')) %>%
      # 
      # addCircles(data=chose_stop_bus(), lat =~stop_lat, lng =~stop_lon, color = "red", radius = 5,
      #             popup = ~as.character(chose_stop_n()$cntnt)) %>%
      
      addProviderTiles(
        "CartoDB.Positron",
        group = "CartoDB.Positron"
      )
  })
  
  
  ## Rapid Transit
  
  all_stops <- reactive({
    start <- stop %>% filter(stop_name==input$Stop1)
    filter1 <- tt_q1_2021_lr %>% filter(from_stop_id %in% start$stop_id)
    end <- stop %>% filter(stop_name==input$Stop2)
    filter2 <- filter1 %>% filter(to_stop_id %in% end$stop_id)
    all_stop <- tt_q1_2021_lr %>% filter(direction_id %in% filter2$direction_id & route_id==input$Route)
    sa <- unique(all_stop$from_stop_id)
    sb <- unique(all_stop$to_stop_id)
    sall <- union(sa,sb)
    all_stop <- stop %>% filter(stop_id %in% sall) %>% arrange(stop_id)
    
    ## optimize for Green-B line
    if(input$Route=="Green-B"){
      all_stop$stop_id <- as.numeric(all_stop$stop_id)
      # all_stop %<>% mutate(stop_code=ifelse(is.na(stop_code), stop_id,stop_code))
      # all_stop %>% mutate(stop_code=ifelse(stop_code>=71150,stop_code-1000,stop_code)) %>%
      #   arrange(stop_code)
      # if(any(all_stop$stop_id=="71150")){
      #   all_stop[which(all_stop$stop_id=="71150"),]$stop_id <- "70150"}
      # if(any(all_stop$stop_id=="71151")){
      #   all_stop[which(all_stop$stop_id=="71151"),]$stop_id <- "70151"}
      # if(any(all_stop$stop_id=="71199")){
      #   all_stop[which(all_stop$stop_id=="71199"),]$stop_id <- "70199"}
      all_stop %>% arrange(stop_lon)
    } 
    ## optimize for Green-C line
    else if(input$Route=="Green-C"){
      # if(any(all_stop$stop_id<="70206")){
      all_stop$stop_id <- as.numeric(all_stop$stop_id)
      all_stop %>% mutate(stop_id=ifelse(stop_id<=70206,-stop_id,stop_id)) %>%
        arrange(stop_id)
      # } else{all_stop %>% arrange(stop_id)}
    }
    
    ## optimize for Green-D line
    else if(input$Route=="Green-D"){
      # if(any(all_stop$stop_id<="70187" & all_stop$stop_id>="70160")){
      all_stop$stop_id <- as.numeric(all_stop$stop_id)
      all_stop %>% mutate(stop_id=ifelse(all_stop$stop_id<=70187 & all_stop$stop_id>=70160,stop_id-100,stop_id)) %>%
        arrange(stop_id)
      # } else{all_stop %>% arrange(stop_id)}
    }
    
    ## optimize for Green-E line
    else if(input$Route=="Green-E"){
      # if(any(all_stop$stop_id<="70206" & all_stop$stop_id>="70154")){
      all_stop$stop_id <- as.numeric(all_stop$stop_id)
      all_stop %>% mutate(stop_id=ifelse(all_stop$stop_id<=70206 & all_stop$stop_id>=70154,-stop_id,stop_id)) %>%
        arrange(stop_id)
      # } else{all_stop %>% arrange(stop_id)}
    }
    
    ## optimize for Orange line
    else if(input$Route=="Orange"){
      all_stop$stop_id <- as.numeric(all_stop$stop_id)
      all_stop %>% mutate(stop_id=ifelse(all_stop$stop_id<=70036 & all_stop$stop_id>=70032,stop_id+300,stop_id)) %>%
        arrange(stop_id)
    }
    
    ## optimize for Blue line
    else if(input$Route=="Blue"){
      all_stop[which(all_stop$stop_id=="70838"),]$stop_id <- "70037" 
      all_stop %>% arrange(stop_id)
    }
    
    ## optimize for Red line
    else if(input$Route=="Red"){
      all_stop$stop_id <- as.numeric(all_stop$stop_id)
      temp <- all_stop %>% filter(stop_id<=70084) %>% mutate(stop_id=-stop_id)
      all_stop %>% mutate(stop_id=ifelse(all_stop$stop_id>=70095,-stop_id,stop_id)) %>%
        bind_rows(temp) %>%
        arrange(stop_id)
    }
    
    else{all_stop %>% arrange(stop_id)}
    
    
  })
  
  
  chose_stops <- reactive({
    start <- stop %>% filter(stop_name==input$Stop1)
    filter1 <- tt_q1_2021_lr %>% filter(from_stop_id %in% start$stop_id)
    end <- stop %>% filter(stop_name==input$Stop2)
    filter2 <- filter1 %>% filter(to_stop_id %in% end$stop_id)
    start_id <- unique(filter2$from_stop_id)
    end_id <- unique(filter2$to_stop_id)
    start_index <- which(all_stops()$stop_code %in% start_id)
    end_index <- which(all_stops()$stop_cod %in% end_id)
    all_stops()[min(start_index):max(end_index),] 
  })
  
  greenb_stop_n <- reactive({
    
    all_stops() %>% mutate(all_stops(), cntnt=paste0('<br><strong>Stop Name:</strong> ',stop_name
    ))
  })
  
  chose_stop_n <- reactive({
    
    chose_stops() %>% mutate(chose_stops(), cntnt=paste0('<br><strong>Stop Name:</strong> ',stop_name
    ))
  })
  
  output$map <- renderLeaflet({
    
    leaflet() %>%
      # addCircles(data = greenb_stop_n,lng = ~stop_lon, lat = ~stop_lat) %>% 
      addTiles() %>%
      addPolylines(
        data = greenb_stop_n(), lat =~stop_lat, lng =~stop_lon, color = "green",
        labelOptions = labelOptions(noHide = TRUE, direction = 'top')) %>%
      
      addCircleMarkers(data = greenb_stop_n(), lat =~stop_lat, lng =~stop_lon, 
                       radius = 3, 
                       popup = ~as.character(greenb_stop_n()$cntnt),
                       color = "blue",
                       stroke = FALSE, 
                       fillOpacity = 0.8) %>%
      
      addPolylines(
        data = chose_stop_n(), lat =~stop_lat, lng =~stop_lon, color = "red",
        labelOptions = labelOptions(noHide = TRUE, direction = 'top')) %>%
      
      addCircles(data=chose_stops(), lat =~stop_lat, lng =~stop_lon, color = "red", radius = 5,
                 popup = ~as.character(chose_stop_n()$cntnt)) %>%
      
      addProviderTiles(
        "CartoDB.Positron",
        group = "CartoDB.Positron"
      )
  })
}

shinyApp(ui,server)




