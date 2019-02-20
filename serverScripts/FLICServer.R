############################################################################
#FLIC

observeEvent(input$showFilters, {
  shinyjs::hide(id = "hiddenT")
  shinyjs::toggle(id = "visibleT")
  shinyjs::toggle(id = "filtersT")
})

observeEvent(input$hideFilters, {
  shinyjs::hide(id = "visibleT")
  shinyjs::show(id = "hiddenT")
  shinyjs::hide(id = "filtersT")
})
CustomList <- read_csv('data/wl/CustomList.csv')
vals <- reactiveValues(df = CustomList)

output$wlControl <- renderUI({
  WLList <- levels(as.factor(vals$df$ListName))
  pickerInput("wl","Select List", WLList,
              selected = 'Scorpio - Handy', multiple = TRUE,
              options = list(`actions-box` = TRUE, `live-search` = TRUE))
})



observeEvent(input$wlView,{
  updatePickerInput(session,'vsltypeFLIC',selected = c("Handy", "LR1",
                                                       "LR2", "MR"))


  updateSliderInput(session, 'loafilter',min = min(Draft()$LOA, na.rm = T), max = max(Draft()$LOA, na.rm = T),
              value = c(min(Draft()$LOA, na.rm = T),max = max(Draft()$LOA, na.rm = T)))
  updateSliderInput(session, 'GTfilter',min = min(Draft()$GT, na.rm=T),
              max = max(Draft()$GT, na.rm = T),
              value = c(min(Draft()$GT, na.rm = T),max(Draft()$GT, na.rm = T)))
  updatePickerInput(session, 'Ownerfilter',selected = "SCORPIO")
  updatePickerInput(session, 'IceClassfilter',selected = levels(as.factor(Draft()$`Ice Class`)))
  updateSliderInput(session, 'dwtfilter',min = min(Draft()$Dwt, na.rm = T),
              max = max(Draft()$Dwt, na.rm = T),
              value = c(min(Draft()$Dwt, na.rm = T),max(Draft()$Dwt, na.rm = T)))
  updatePickerInput(session,'legfilter', "Filter by Leg",selected = levels(as.factor(Draft()$Leg)))
  updatePickerInput(session,'IMOfilter', "Filter by IMO Type",selected = levels(as.factor(Draft()$IMOType)))
  updateSliderInput(session, 'builtfilter', "Filter by Age of Vessel", min = 0, max = 30, value = 30)
  updatePickerInput(session, 'CargoTypeFilter', 'Filter by Cargo Type', selected = levels(as.factor(Draft()$CargoType)))
  updatePickerInput(session, 'NavStatusFiletr', 'Filter by Navigation Status', selected = levels(as.factor(Draft()$navigationStatus)))
})

WVDData <- reactive({
  
  dest <- input$dest
  
  if (input$wlView == FALSE) {
    if (dest != '') {
      x <- Draft() %>% filter(VslType %in% input$vsltypeFLIC) %>%
        #https://stackoverflow.com/questions/50478123/textinput-filter-in-r-dataset-shiny
        filter(Reduce(`&`, lapply(strsplit(dest,' ')[[1]], grepl, destination,ignore.case = T))) %>%
        filter(LOA > input$loafilter[1] & LOA <= input$loafilter[2]) %>%
        filter(GT > input$GTfilter[1] & GT <= input$GTfilter[2]) %>%
        filter(Operator %in% input$Ownerfilter) %>%
        filter(`Ice Class` %in% input$IceClassfilter) %>%
        filter(Dwt >= input$dwtfilter[1] & Dwt <= input$dwtfilter[2]) %>%
        filter(Leg %in% input$legfilter) %>%
        filter(IMOType %in% input$IMOfilter) %>%
        filter(Built >= as.numeric((lubridate::year(Sys.Date()) - input$builtfilter))) %>% 
        filter(CargoType %in% input$CargoTypeFilter) %>% 
        filter(navigationStatus %in% input$NavStatusFilter)
    }
    else{
      x <- Draft() %>% filter(VslType %in% input$vsltypeFLIC) %>%
        filter(LOA > input$loafilter[1] & LOA <= input$loafilter[2]) %>%
        filter(GT > input$GTfilter[1] & GT <= input$GTfilter[2]) %>%
        filter(Operator %in% input$Ownerfilter) %>%
        filter(`Ice Class` %in% input$IceClassfilter) %>%
        filter(Dwt >= input$dwtfilter[1] & Dwt <= input$dwtfilter[2]) %>%
        filter(Leg %in% input$legfilter) %>%
        filter(IMOType %in% input$IMOfilter) %>%
        filter(Built >= as.numeric((lubridate::year(Sys.Date()) - input$builtfilter))) %>% 
        filter(CargoType %in% input$CargoTypeFilter) %>% 
        filter(navigationStatus %in% input$NavStatusFilter)
    }
  }

  if (input$wlView) {
    req(vals$df)
    CList <- vals$df
    if (dest != '') {
      x <- Draft() %>% filter(Name %in% CList$Name[CList$ListName %in% input$wl]) %>%
        filter(VslType %in% input$vsltypeFLIC) %>%
        filter(Reduce(`&`, lapply(strsplit(dest,' ')[[1]], grepl, destination,ignore.case = T))) %>%
        filter(LOA > input$loafilter[1] & LOA <= input$loafilter[2]) %>%
        filter(GT > input$GTfilter[1] & GT <= input$GTfilter[2]) %>%
        # filter(Owner %in% input$Ownerfilter) %>%
        filter(`Ice Class` %in% input$IceClassfilter) %>%
        filter(Dwt >= input$dwtfilter[1] & Dwt <= input$dwtfilter[2]) %>%
        filter(Leg %in% input$legfilter) %>%
        filter(IMOType %in% input$IMOfilter) %>%
        filter(Built >= as.numeric((lubridate::year(Sys.Date()) - input$builtfilter))) %>% 
        filter(CargoType %in% input$CargoTypeFilter) %>% 
        filter(navigationStatus %in% input$NavStatusFilter)
    }

    else {
      x <- Draft() %>% filter(Name %in% CList$Name[CList$ListName %in% input$wl]) %>%
        filter(VslType %in% input$vsltypeFLIC) %>%
        filter(LOA > input$loafilter[1] & LOA <= input$loafilter[2]) %>%
        filter(GT > input$GTfilter[1] & GT <= input$GTfilter[2]) %>%
        filter(`Ice Class` %in% input$IceClassfilter) %>%
        filter(Dwt >= input$dwtfilter[1] & Dwt <= input$dwtfilter[2]) %>%
        filter(Leg %in% input$legfilter) %>%
        filter(IMOType %in% input$IMOfilter) %>%
        filter(Built >= as.numeric((lubridate::year(Sys.Date()) - input$builtfilter))) %>% 
        filter(CargoType %in% input$CargoTypeFilter) %>% 
        filter(navigationStatus %in% input$NavStatusFilter)
    }

  } 
x
})

sDraftWVD <- SharedData$new(WVDData)
output$cmap <- renderLeaflet({
  req(sDraftWVD)
  SWVD <- sDraftWVD$origData()
  if (nrow(SWVD) < 1) return(NULL)
  leafIcons <- icons(iconUrl = ifelse(SWVD$Leg == 'Ballast',
                                      "www/img/10.png", "www/img/3.png"),
                     iconWidth = 11, iconHeight = 24, iconAnchorX = 5, iconAnchorY = 0)
  
  legend <- tags$h6("Red icon signifies ballast state and blue implies laden")
  
  leaflet(sDraftWVD) %>% addTiles() %>%
    addProviderTiles(providers$Esri.WorldTopoMap) %>%
    addTiles("https://tile.openweathermap.org/map/wind_new/{z}/{x}/{y}.png?appid=edd77e54e1509f4fe40f0fd4e41fd06d",
            group = "wind") %>%

    registerPlugin(rotatedMarkerPlugin) %>%
    addMarkers(lng = ~lon, lat = ~lat, icon = leafIcons,label = ~Name,
               options = markerOptions( rotationAngle = ~cog ),popup = ~CFpopup,
               group = "all_ships") %>%
    addTiles("https://tile.openweathermap.org/map/pressure_new/{z}/{x}/{y}.png?appid=edd77e54e1509f4fe40f0fd4e41fd06d",
             group = "pressure") %>%
    addResetMapButton() %>%
    addSearchFeatures(targetGroups = "all_ships",
                      options = searchFeaturesOptions(zoom = 10, openPopup = TRUE,
                                                      hideMarkerOnCollapse = TRUE)) %>%
    addControl(legend, position = "bottomright") %>%
    addLayersControl(overlayGroups = c("all_ships","wind", "pressure"),
                     options = layersControlOptions(collapsed = TRUE)) %>% hideGroup(c("wind", "pressure"))


})

output$cmapTable <- renderDT({
  req(sDraftWVD)
  datatable(sDraftWVD,extensions = c('Buttons', 'ColReorder'), rownames = FALSE,
            options = list(deferRender = TRUE,scroller = FALSE,scrollX = TRUE,
                           colReorder = TRUE,
                           dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print',I('colvis')),
                           pageLength = 10,lengthMenu = list(c(10,20, 50,-1), list('10', '20', '50','All')), paging = T,
                           columnDefs = list(list(visible = FALSE, targets = c(2:4,6,9,12,15:19,21,24)))),
            colnames = c("Name", "VslType","Type", "GT", "LOA", "DWT", "MMSI", "Cubics", "DFT","Owner","Ice","Dest",
                         "Timestamp", "NavStatus", "ETA", "Lat", "Lon", "SOG", "COG","Leg",
                         "PreviousPort", "Popup","Leg", "Operator", "IMO", "Built", "IMOType", "CargoType"),escape = FALSE) 

},server = FALSE)

########################################################################
output$cmap2 <- renderLeaflet({
  req(sDraftWVD)
  SWVD <- sDraftWVD$origData()
  if (nrow(SWVD) < 1) return(NULL)
  leafIcons <- icons(iconUrl = ifelse(SWVD$Leg == 'Ballast',
                                      "www/img/10.png", "www/img/3.png"),
                     iconWidth = 11, iconHeight = 24, iconAnchorX = 5, iconAnchorY = 0)
  
  legend <- tags$h6("Red icon signifies ballast state and blue implies laden")
  leaflet(sDraftWVD) %>% addTiles() %>%
    addProviderTiles(providers$Esri.WorldTopoMap) %>%
    registerPlugin(rotatedMarkerPlugin) %>%
    addMarkers(lng = ~lon, lat = ~lat, icon = leafIcons,label = ~Name,
               options = markerOptions( rotationAngle = ~cog ),popup = ~CFpopup,
               group = "all_ships") %>%
    addTiles("https://tile.openweathermap.org/map/wind_new/{z}/{x}/{y}.png?appid=edd77e54e1509f4fe40f0fd4e41fd06d",
             group = "wind") %>%
    addTiles("https://tile.openweathermap.org/map/pressure_new/{z}/{x}/{y}.png?appid=edd77e54e1509f4fe40f0fd4e41fd06d",
             group = "pressure") %>%
    addResetMapButton() %>%
    addSearchFeatures(targetGroups = "all_ships",
                      options = searchFeaturesOptions(zoom = 10, openPopup = TRUE,
                                                      hideMarkerOnCollapse = TRUE)) %>%
    addControl(legend, position = "bottomright") %>%
    addLayersControl(overlayGroups = c("all_ships","wind", "pressure"),
                     options = layersControlOptions(collapsed = TRUE)) %>% hideGroup(c("wind", "pressure"))


})

output$cmapTable2 <- renderDT({
  req(sDraftWVD)
  datatable(sDraftWVD,extensions = c('Buttons', 'ColReorder'), rownames = FALSE,
            options = list(deferRender = TRUE,scroller = FALSE,scrollX = TRUE, 
                           colReorder = TRUE,
                           dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print',I('colvis')),
                           pageLength = 10,lengthMenu = list(c(10,20, 50,-1), list('10', '20', '50','All')), paging = T,
                           columnDefs = list(list(visible = FALSE, targets = c(2:4,6,9,12,15:19,21,24)))),
            colnames = c("Name", "VslType","Type", "GT", "LOA", "DWT", "MMSI", "Cubics", "DFT","Owner","Ice","Dest",
                         "Timestamp", "NavStatus", "ETA", "Lat", "Lon", "SOG", "COG","Leg", "PreviousPort", "Popup","Leg", 
                         "Operator", "IMO", "Built", "IMOType", "CargoType"),escape = FALSE) 

},server = FALSE)


#Save Custom Lists
observeEvent(input$saveList,{
  req(input$wlName)

  if(input$wlName %in% unique(CustomList$ListName)){
    shinyalert("Sorry!", "A list with the same name already exists. Please save with another name", type = "error")
  }
  else{
    ListName <- rep(input$wlName, length(WVDData()$Name))
    myList <- as.data.frame(cbind(WVDData()$Name,ListName))
    write_csv(myList,'data/wl/CustomList.csv',append = TRUE)
    CustomList <- read_csv('data/wl/CustomList.csv')
    vals$df <- CustomList
  updateSelectInput(session, 'wl', choices = levels(as.factor(vals$df$ListName)))
  updateSelectInput(session, 'delList', choices = levels(as.factor(vals$df$ListName)))

  shinyalert("Success!", "Your custom watch list has been added", type = "success")

  }
})

#Delete watchlist
output$delList <- renderUI({
  selectInput('delList', "Select a list name to delete", choices = levels(as.factor(vals$df$ListName)))
})

observeEvent(input$deleteList,{
  req(input$delList)
  shinyalert(text = "The list you selected has been deleted", type = "success")
  x <- vals$df %>% filter(ListName != input$delList)
  write_csv(x, 'data/wl/CustomList.csv')
  CustomList <- read_csv('data/wl/CustomList.csv')
  vals$df <- CustomList
  updateSelectInput(session, 'wl', choices = levels(as.factor(vals$df$ListName)))
  updateSelectInput(session, 'delList', choices = levels(as.factor(vals$df$ListName)))

})

#Edit Watch List
output$editList <- renderUI({
  selectInput("listName","Select list to edit",choices = levels(as.factor(vals$df$ListName)))
})

output$multi <- renderUI({
  multiInput(
    inputId = "idWL", label = "Edit WatchList :",
    choices = Draft()$Name,
    selected   = vals$df$Name[vals$df$ListName == input$listName], width = "350px",
    options = list(
      enable_search = TRUE,
      non_selected_header = "Choose between:",
      selected_header = "You have selected:"
    )
  )
})

observeEvent(input$saveEdited,{
  req(input$listName)
  req(input$idWL)
  CustomList$Name[CustomList$ListName == input$listName] <- input$idWL
  write_csv(CustomList, 'data/wl/CustomList.csv')
  CustomList <- read_csv('data/wl/CustomList.csv')
  vals$df <- CustomList
})
