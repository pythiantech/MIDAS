#####################################################################################################

#Trade Flows
# 
# startperiod <- reactive({
#   input$dateRange[1]
# })
# 
# stopperiod <- reactive({
#   input$dateRange[2]
# })
# 
# #Filter by time on voyages
# vTime <- reactive({
#   req(startperiod())
#   req(stopperiod())
#   VType <- V_Dim_Vessel() %>% dplyr::select(Dim_Vessel_Id,VesselType)
#   VT <- V_Dim_Voyage() %>% filter((CommenceGMT >= startperiod() & CompleteGMT <= stopperiod()) |
#                                      (CommenceGMT < startperiod() & CompleteGMT > startperiod() & CompleteGMT <= stopperiod()) |
#                                      (CommenceGMT > startperiod() & CommenceGMT < stopperiod() & CompleteGMT > stopperiod()) |
#                                      (CommenceGMT < startperiod() & CompleteGMT > stopperiod())) %>%
#     left_join(VType, by = c("Fkey_Dim_Vessel_Id" = "Dim_Vessel_Id")) %>%
#     filter(VoyageEstimateExists == 'Voy With Estimate') %>%
#     dplyr::select(Dim_Voyage_Id, Fkey_Dim_Vessel_Id, voyNum,CommenceGMT, CompleteGMT, PortOfFirstLoad_PortName, Fkey_Dim_Ports_FirstLoad,
#                   PortOfLastDischarge_Portname, Fkey_Dim_Ports_LastDischarge,VesselName,oprType,
#                   Fkey_Enum_VoyageStatus_Id, Fkey_Dim_Company_Id,VesselType) %>%
#     mutate(VoyDays = round(if_else(CommenceGMT >= startperiod() & CompleteGMT <= stopperiod(), difftime(CompleteGMT, CommenceGMT, units = "days"),
#                                  if_else(CommenceGMT < startperiod() & CompleteGMT > startperiod() & CompleteGMT <= stopperiod(),
#                                          difftime(CompleteGMT, startperiod() , units = "days"),
#                                          if_else(CommenceGMT > startperiod() & CommenceGMT < stopperiod() & CompleteGMT > stopperiod(),
#                                                  difftime(stopperiod(),CommenceGMT, units = "days"),
#                                                  difftime(stopperiod(),startperiod(), units = "days")))),2)) %>%
#     filter(voyNum != 1, VesselName != grepl("SBI", VesselName),
#            oprType != 'RELT',
#            oprType != 'OVTO',
#            oprType != 'TCTO',
#            Fkey_Enum_VoyageStatus_Id != 4,
#            Fkey_Enum_VoyageStatus_Id != 1,
#            Fkey_Dim_Company_Id %!in% CIDs) %>%
#     dplyr::select(Vessel = VesselName, VesselID = Fkey_Dim_Vessel_Id,VesselType,Dim_Voyage_Id,from = CommenceGMT,
#                   to = CompleteGMT,Load = PortOfFirstLoad_PortName,Discharge = PortOfLastDischarge_Portname,
#                   Days = VoyDays,voyNum)
#   VT$LoadingRegion <- PortRegion$Region[match(VT$Load,PortRegion$PortName)]
#   VT$DischargeRegion <- PortRegion$Region[match(VT$Discharge,PortRegion$PortName)]
#   ComVoy <- V_Fact_VesselItinerary_Vsched() %>% filter(Fkey_Dim_Voyage_Id %in% VT$Dim_Voyage_Id) %>%
#     filter(Fkey_Dim_PortFunction_Id == 1) %>% dplyr::select(Fkey_Dim_Voyage_Id,Fkey_Dim_PortFunction_Id,Fkey_Dim_Ports_Id) %>%
#     left_join(V_Dim_Ports(),by = c("Fkey_Dim_Ports_Id" = "Dim_Ports_Id")) %>%
#     dplyr::select(Fkey_Dim_Voyage_Id,PortName,CArea = Area,CSubArea = SubArea)
#   VT$CommencingPort <- ComVoy$PortName[match(VT$Dim_Voyage_Id,ComVoy$Fkey_Dim_Voyage_Id)]
#   VT$CommencingRegion <- PortRegion$Region[match(VT$CommencingPort ,PortRegion$PortName)]
#   VT$CArea <- ComVoy$CArea[match(VT$CommencingPort,ComVoy$PortName)]
#   VT$CSubArea <- ComVoy$CSubArea[match(VT$CommencingPort,ComVoy$PortName)]
#   VT$LoadArea <- V_Dim_Ports()$Area[match(VT$Load,V_Dim_Ports()$PortName)]
#   VT$LoadRegion <- V_Dim_Ports()$Region[match(VT$Load,V_Dim_Ports()$PortName)]
#   VT$LoadSubArea <- V_Dim_Ports()$SubArea[match(VT$Load,V_Dim_Ports()$PortName)]
#   VT$DischargeArea <- V_Dim_Ports()$Area[match(VT$Discharge,V_Dim_Ports()$PortName)]
#   VT$DischargeRegion <- V_Dim_Ports()$Region[match(VT$Discharge,V_Dim_Ports()$PortName)]
#   VT$DischargeSubArea <- V_Dim_Ports()$SubArea[match(VT$Discharge,V_Dim_Ports()$PortName)]
#   vItin <- V_Fact_VesselItinerary_Vsched() %>% filter(Fkey_Dim_Voyage_Id %in% unique(VT$Dim_Voyage_Id))
#   PNL <- V_Fact_VoyPNL() %>% filter(Fkey_Dim_Voyage_Id %in% unique(vItin$Fkey_Dim_Voyage_Id)) %>%
#     group_by(Fkey_Dim_Voyage_Id) %>% summarise(CurrentDate = max(snapshotRefDate)) %>%
#     left_join(V_Fact_VoyPNL(), by = c("Fkey_Dim_Voyage_Id" = "Fkey_Dim_Voyage_Id",
#                                   "CurrentDate" = "snapshotRefDate")) %>%
#     dplyr::select(Fkey_Dim_Voyage_Id, CurrentDate, tcEquv_Act, ttlVoyDays_Act) %>%
#     group_by(Fkey_Dim_Voyage_Id)
#   PNL$tcEquv_Act <- round(PNL$tcEquv_Act,2)
# 
#   PNL <- PNL[complete.cases(PNL),]
#   PNL <- PNL[!duplicated(PNL),]
# 
#   #Compare with  our calculation of estimated days
#   VT <- VT %>% left_join(PNL,by = c("Dim_Voyage_Id" = "Fkey_Dim_Voyage_Id")) %>%
#     dplyr::select(names(vTime),TCE = tcEquv_Act) %>% mutate(Revenue = round(Days*TCE,2))
# 
#   VT
# })
# 
# observe({
#   updateSelectInput(session, 'vsltype', choices = c("All",levels(as.factor(vTime()$VesselType))))
# })
# VesselType <- reactive({
#   req(input$vsltype)
#   ifelse(input$vsltype == "All",VesselType <- vTime(),VesselType <- vTime() %>% filter(VesselType == input$vsltype))
#   VesselType
# })
# 
# #Commencing Area Filters
# output$CArea <- renderUI({
#   selectizeInput("CArea","Commencing Area:",choices = c("All",unique(VesselType()$CArea)),
#                  selected = "All", multiple = TRUE)
# })
# 
# output$CRegion <- renderUI({
#   selectizeInput("CRegion","Commencing Region:",choices = c("All",VesselType()$CommencingRegion[VesselType()$CArea %in% input$CArea]),
#                  selected = "All", multiple = TRUE)
# })
# 
# output$CSubArea <- renderUI({
#   selectizeInput("CSubArea","Commencing Sub Area:",choices = c("All",VesselType()$CSubArea[VesselType()$CommencingRegion %in% input$CRegion]),
#                  selected = "All", multiple = TRUE)
# })
# 
# #Loading Area Filters
# output$LArea <- renderUI({
#   selectizeInput("LArea","Loading Area:",choices = c("All",unique(VesselType()$LoadArea)),
#                  selected = "All", multiple = TRUE)
# })
# 
# output$LRegion <- renderUI({
#   selectizeInput("LRegion","Loading Region:",choices = c("All",VesselType()$LoadRegion[VesselType()$LoadArea %in% input$LArea]),
#                  selected = "All", multiple = TRUE)
# })
# 
# output$LSubArea <- renderUI({
#   selectizeInput("LSubArea","Loading Sub Area:",choices = c("All",VesselType()$LoadSubArea[VesselType()$LoadRegion %in% input$LRegion]),
#                  selected = "All", multiple = TRUE)
# })
# 
# #Discharge Area Filters
# output$DArea <- renderUI({
#   selectizeInput("DArea","Discharge Area:",choices = c("All",unique(VesselType()$DischargeArea)),
#                  selected = "All", multiple = TRUE)
# })
# 
# output$DRegion <- renderUI({
#   selectizeInput("DRegion","Discharge Region:",choices = c("All",VesselType()$DischargeRegion[VesselType()$DischargeArea %in% input$DArea]),
#                  selected = "All", multiple = TRUE)
# })
# 
# output$DSubArea <- renderUI({
#   selectizeInput("DSubArea","Discharge Sub Area:",choices = c("All",VesselType()$DischargeSubArea[VesselType()$DischargeRegion %in% input$DRegion]),
#                  selected = "All", multiple = TRUE)
# })
# 
# FD <- reactiveValues()
# filterdata <- reactive({
#   req(VesselType())
#   req(input$CArea)
#   req(input$CRegion)
# 
#   if (input$CArea == 'All') {filt1 <- quote(CArea != "@?><")}
#   else{filt1 <- quote(CArea %in% input$CArea)}
# 
#   if (input$CRegion == 'All') {filt2 <- quote(CommencingRegion != "@?><")}
#   else{filt2 <- quote(CommencingRegion %in% input$CRegion)}
# 
#   if (input$CSubArea == 'All') {filt3 <- quote(CSubArea != "@?><")}
#   else{filt3 <- quote(CSubArea %in% input$CSubArea)}
# 
#   if (input$LArea == 'All') {filt4 <- quote(LoadArea != "@?><")}
#   else{filt4 <- quote(LoadArea %in% input$LArea)}
# 
#   if (input$LRegion == 'All') {filt5 <- quote(LoadRegion != "@?><")}
#   else{filt5 <- quote(LoadRegion %in% input$LRegion)}
# 
#   if (input$LSubArea == 'All') {filt6 <- quote(LoadSubArea != "@?><")}
#   else{filt6 <- quote(LoadSubArea %in% input$LSubArea)}
# 
#   if (input$DArea == 'All') {filt7 <- quote(DischargeArea != "@?><")}
#   else{filt7 <- quote(DischargeArea %in% input$DArea)}
# 
#   if (input$DRegion == 'All') {filt8 <- quote(DischargeRegion != "@?><")}
#   else{filt8 <- quote(DischargeRegion %in% input$DRegion)}
# 
#   if (input$DSubArea == 'All') {filt9 <- quote(DischargeSubArea != "@?><")}
#   else{filt9 <- quote(DischargeSubArea %in% input$DSubArea)}
#   VT <- VesselType() %>%
#     filter_(filt1) %>%
#     filter_(filt2) %>%
#     filter_(filt3) %>%
#     filter_(filt4) %>%
#     filter_(filt5) %>%
#     filter_(filt6) %>%
#     filter_(filt7) %>%
#     filter_(filt8) %>%
#     filter_(filt9)
#   
#   FD$fd <- VT
#   VT
# })


# output$voyages <- renderDT({
#   datatable(filterdata() %>% select(VesselType, Vessel, voyNum,from, to, CommencingPort, Load, Discharge,
#                                     Days, TCE, Revenue) ,rownames = FALSE,
#             extensions = 'Buttons', options = list(dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print',I('colvis')),scrollX = TRUE),
#             selection = 'single') %>% formatDate(c(4,5), "toLocaleString")
# },server = FALSE)

####################################################################
#Get information on clicked voyage
# ShipId <- reactiveVal(NULL)
# observeEvent(input$voyages_rows_selected,{
#   ShipId(filterdata()[unlist(input$voyages_rows_selected),2])
# })
# 
# VoyId <- reactiveVal(NULL)
# observeEvent(input$voyages_rows_selected,{
#   VoyId(filterdata()[unlist(input$voyages_rows_selected),4])
# })
# strtp <- reactiveVal(NULL)
# observeEvent(input$voyages_rows_selected,{
#   strtp(filterdata()[unlist(input$voyages_rows_selected),5])
# })
# stptp <- reactiveVal(NULL)
# observeEvent(input$voyages_rows_selected,{
#   stptp(filterdata()[unlist(input$voyages_rows_selected),6])
# })
# ShipName <- reactiveVal(NULL)
# observeEvent(input$voyages_rows_selected,{
#   ShipName(filterdata()[unlist(input$voyages_rows_selected),1])
# 
# })


##################################
#Get info on these unique Voyages from VslItinerary
# 
# vItin <- reactive({
#   vIt <- V_Fact_VesselItinerary_Vsched() %>% filter(Fkey_Dim_Voyage_Id %in% unique(vTime()$Dim_Voyage_Id))
#   vIt
# })
# 
# #############################################################################
# #PNL Data
# PNL <- reactive({
#   PNL1 <- V_Fact_VoyPNL() %>% filter(Fkey_Dim_Voyage_Id %in% unique(vItin()$Fkey_Dim_Voyage_Id)) %>%
#     group_by(Fkey_Dim_Voyage_Id) %>% summarise(CurrentDate = max(snapshotRefDate)) %>%
#     left_join(V_Fact_VoyPNL(), by = c("Fkey_Dim_Voyage_Id" = "Fkey_Dim_Voyage_Id",
#                                   "CurrentDate" = "snapshotRefDate")) %>%
#     dplyr::select(Fkey_Dim_Voyage_Id, CurrentDate, tcEquv_Act, ttlVoyDays_Act) %>%
#     group_by(Fkey_Dim_Voyage_Id)
# 
#   PNL1 <- PNL1[complete.cases(PNL1),]
#   PNL1 <- PNL1[!duplicated(PNL1),]
# 
#   #Compare with  our calculation of estimated days
#   PNL1 <- PNL1 %>% left_join(vTime(),by = c("Fkey_Dim_Voyage_Id" = "Dim_Voyage_Id")) %>%
#     dplyr::select(names(PNL1),Days) %>% mutate(Profit = Days*tcEquv_Act)
#   PNL1
# })
# 
# #Get Port info
# portMatch <- reactive({
#   VDP <- V_Dim_Ports()
#   VDP$Dim_Ports_Id <- as.numeric(VDP$Dim_Ports_Id)
#   PM <- vItin() %>% left_join(VDP, by = c("Fkey_Dim_Ports_Id" = "Dim_Ports_Id")) %>%
#     left_join(V_Dim_Vessel(), by = c("Fkey_Dim_Vessel_Id" = "Dim_Vessel_Id")) %>%
#     group_by(Fkey_Dim_Voyage_Id, Imos_VoyNo) %>%
#     dplyr::select(Imos_VoyNo,ord_no_int,Fkey_Dim_PortFunction_Id,PortName,Fkey_Dim_Voyage_Id,
#                   Vessel, Fkey_Dim_Vessel_Id,Date_Arrival,Date_Departure,Fkey_Enum_BallastLaden_Id,
#                   Longitude, Latitude, Area, SubArea,TimeZone) %>% arrange(Fkey_Dim_Voyage_Id,ord_no_int) %>%
#     left_join(portFunction, by = c("Fkey_Dim_PortFunction_Id" = "ID")) %>%
#     mutate(ArrivalDate = Date_Arrival - (TimeZone*60*60))
#   #Add some jitter
#   PM$Longitude <- jitter(PM$Longitude, factor = 0.001)
#   PM$Latitude <- jitter(PM$Latitude, factor = 0.001)
#   PM <- PM %>% left_join(PNL(), by = "Fkey_Dim_Voyage_Id") %>%
#     mutate(TD = round(difftime(Date_Departure,ArrivalDate,units = "days") ,2))
#   PM$Region <- PortRegion$Region[match(PM$PortName,PortRegion$PortName)]
#   PM$ArrivalDate[PM$Function == "Commencing"] <- NA
#   PM$Fkey_Enum_BallastLaden_Id[PM$Fkey_Enum_BallastLaden_Id == 1] <- "Laden"
#   PM$Fkey_Enum_BallastLaden_Id[PM$Fkey_Enum_BallastLaden_Id == 0] <- "Ballast"
#   PM
# 
# 
# })
# 
# #Get Voyage Details
# voyDetails <- reactive({
#   req(ShipId)
#   req(VoyId)
#   req(portMatch())
#   PM <- portMatch()
#   vslid <- ShipId()$VesselID[1]
#   voyid <- VoyId()$Dim_Voyage_Id[1]
# 
#   PM %>%
#     filter(Fkey_Dim_Vessel_Id == vslid & Fkey_Dim_Voyage_Id == voyid) %>%
#     dplyr::select(PortName,Region,Function,ArrivalDate,DepartureDate = Date_Departure,Longitude,Latitude,
#                   Leg = Fkey_Enum_BallastLaden_Id)
# })


######################################################
# fetch_data_from_stratum <- reactive({
#   req(ShipId())
#   id <- ShipId()$VesselID[1]
#   strtp <- substr(as.character(isolate(strtp()$from[1])),1,10)
#   stpp <- ifelse(ymd(substr(as.character(isolate(stptp()$to[1])),1,10)) > Sys.Date(),
#                  as.character(Sys.Date()),substr(as.character(isolate(stptp()$to[1])),1,10))
#   shinyjs::show("page_shield")
#   future({
#     list(
#       data = fetch_voyage_data(id,strtp,stpp)
#     )
#   })
# })
# #
# voyData <- reactive({
#   then(
#     fetch_data_from_stratum(),
#     onFulfilled = function(value){
#       shinyjs::hide("page_shield")
#       value$data
#     }
#   )
# })
# output$voyageData <- renderDT({
#   req(voyData())
# 
#   then(
#     isolate(voyData()),
#     onFulfilled = function(value){
#       shinyjs::hide("page_shield")
#       datatable(value)
#     }
#   )
# 
# })
# 
# 
# ############################################################################
# #Aggregated Tables/Plots
# 
# output$RegRev <- renderDT({
#   VT <- req(FD$fd)
#   datatable(VT %>% group_by(LoadingRegion, CommencingRegion, DischargeRegion) %>% 
#               summarize(AvgRev = round(mean(Revenue))) %>% arrange(desc(AvgRev)) %>% ungroup() %>% 
#               top_n(10), rownames = FALSE, options = list(scrollX = TRUE))
# })
# 
# output$AvgDays <- renderDT({
#   VT <- req(FD$fd)
#   datatable(VT %>% group_by(LoadingRegion, CommencingRegion, DischargeRegion) %>% 
#               summarize(AvgDays = round(mean(Days),2)) %>% arrange(desc(AvgDays)) %>% ungroup() %>% 
#               top_n(10),rownames = FALSE, options = list(scrollX=TRUE))
# })

 ##########################################################################
 #World Fleet


###########################
#Filters dependent on wvd()

output$Operator <- renderUI({
  pickerInput('Operator',"Select Commercial Operator", choices = levels(as.factor(wvd()$`Commercial Operator`)),
              options = list(`actions-box` = TRUE,`live-search`=TRUE))
})

output$VesselT <- renderUI({
  pickerInput('VesselT',"Select Vessel Type", choices = levels(as.factor(wvd()$`Vessel Type`)),multiple = TRUE,
              selected = levels(as.factor(wvd()$`Vessel Type`)), 
              options = list(`actions-box` = TRUE,`live-search`=TRUE))
})
output$CargoTypeWF <- renderUI({
  pickerInput('CargoTypeWF', 'Filter by Cargo Type', choices = levels(as.factor(wvd()$Type)),multiple = TRUE,
              selected = levels(as.factor(wvd()$Type)), 
              options = list(`actions-box` = TRUE, `live-search` = TRUE))
})
 WFrv <- reactiveValues()

 observeEvent(input$DBexecute,{
  
   req(input$WorldDateRange)
   req(input$Operator)
   req(input$VesselT)
   req(input$CargoTypeWF)
   Operators <- input$Operator
   VTypes <- input$VesselT
   CargoTypeWF <- input$CargoTypeWF
   MMSIs <- wvd()$MMSI[wvd()$`Commercial Operator` %in% Operators & wvd()$`Vessel Type` %in% VTypes & wvd()$Type %in% CargoTypeWF]
   ScorpioMMSI <- wvd() %>% filter(`Commercial Operator` == 'SCORPIO') %>% 
     filter(`Vessel Type` %in% VTypes) %>% filter(Type %in% CargoTypeWF) %>% pull(MMSI)
   StartDate <- ymd(input$WorldDateRange[1])
   EndDate <- ymd(input$WorldDateRange[2])

   shinyjs::show("page_shield")
   StartDate <- paste0(StartDate,"T00:00:00")
   EndDate <- paste0(EndDate, "T00:00:00")

   
   
     StartDate <- as.numeric(gsub('-','',as.character(input$WorldDateRange[1])))
     EndDate <- as.numeric(gsub('-','',as.character(input$WorldDateRange[2])))
     maxt <- pool %>% tbl("IM_WVDListType13Day") %>% filter(date >= StartDate & date <= EndDate) %>%
       
       filter(mmsi %in% MMSIs) %>% arrange(mmsi, desc(timestamp)) %>% collect()
     maxt <- maxt %>% group_by(mmsi, date) %>% 
       do(head(., n = 1)) %>% collect()
       # collect() #With Giuseppe 23rd Oct 
     ###############################################
     maxt$Date <- ymd(substr(maxt$timestamp,1,10))
     WFheatmap <- maxt %>%  arrange(mmsi, desc(timestamp)) 
     WFheatmap$lat <- as.numeric(WFheatmap$lat)
     WFheatmap$lon <- as.numeric(WFheatmap$lon)
     WFheatmap$sogKts <- as.numeric(WFheatmap$sogKts)
     WFheatmap$mmsi <- as.numeric(WFheatmap$mmsi)
     WFheatmap$heading <- as.numeric(WFheatmap$heading)
     WFrv$data <- WFheatmap %>% select(-id)
     Draft5 <- pool %>% tbl("IM_WVDListType5") %>% filter(date >= StartDate & date <= EndDate) %>%
       filter(mmsi %in% MMSIs) %>% filter(portFunction == 'L') %>% arrange(mmsi, desc(timestamp)) %>%
       collect()
     Draft5$mmsi <- as.numeric(Draft5$mmsi)
     Draft5$lat <- as.numeric(Draft5$lat)
     Draft5$lon <- as.numeric(Draft5$lon)
     Draft5$draught <- as.numeric(Draft5$draught)
     Draft5 <- Draft5 %>% select(-id)
     WFrv$DraftIM <- Draft5
     
     #Scorpio Load Ports
     SLP <- pool %>% tbl("IM_WVDListType5") %>% filter(date >= StartDate & date <= EndDate) %>%
       filter(mmsi %in% ScorpioMMSI) %>% filter(portFunction == 'L') %>% arrange(mmsi, desc(timestamp)) %>%
       collect()
     SLP$mmsi <- as.numeric(SLP$mmsi)
     SLP$lat <- as.numeric(SLP$lat)
     SLP$lon <- as.numeric(SLP$lon)
     SLP$draught <- as.numeric(SLP$draught)
     SLP <- SLP %>% select(-id)
     WFrv$SLP <- SLP
     
     
     ###########################################
     #Discharge Ports
     DraftDP <- pool %>% tbl("IM_WVDListType5") %>% filter(date >= StartDate & date <= EndDate) %>%
       filter(mmsi %in% MMSIs) %>% filter(portFunction == 'D') %>% arrange(mmsi, desc(timestamp)) %>%
       collect()
     DraftDP$mmsi <- as.numeric(DraftDP$mmsi)
     DraftDP$lat <- as.numeric(DraftDP$lat)
     DraftDP$lon <- as.numeric(DraftDP$lon)
     DraftDP$draught <- as.numeric(DraftDP$draught)
     DraftDP <- DraftDP %>% select(-id)
     WFrv$DP <- DraftDP
     
     #Scorpio Load Ports
     SDP <- pool %>% tbl("IM_WVDListType5") %>% filter(date >= StartDate & date <= EndDate) %>%
       filter(mmsi %in% ScorpioMMSI) %>% filter(portFunction == 'D') %>% arrange(mmsi, desc(timestamp)) %>%
       collect()
     SDP$mmsi <- as.numeric(SDP$mmsi)
     SDP$lat <- as.numeric(SDP$lat)
     SDP$lon <- as.numeric(SDP$lon)
     SDP$draught <- as.numeric(SDP$draught)
     SDP <- SDP %>% select(-id)
     WFrv$SDP <- SDP
     ###########################################
     shinyjs::hide("page_shield")
   
 })

 observe({
   req(WFrv$data)
   animData <- isolate(WFrv$data)
   updateSliderInput(session, 'animation', min = min(animData$Date),
                     max = max(animData$Date),
                     value = min(animData$Date))
 })
 filteredData <- reactive({
   req(WFrv$data)
   req(input$animation)
   leafData <- req(WFrv$data)
   indate <- input$animation
   req(indate)
   leafData %>% filter(Date == indate)
 })

 #Heatmap for fleet distribution
 output$fleetdistribution <- renderLeaflet({
  req(WFrv$data)
  req(nrow(filteredData()) > 0)


   WFH <- filteredData()
   WFRV <- WFrv$data
   # saveRDS(WFH, 'WFH.Rds')
   # saveRDS(WFRV, 'WFRV.Rds')
   sf_obs <- st_as_sf(WFH, coords = c("lon", "lat"),
                     crs = st_crs(AllRegions))
   ###################################################################
   sf_obs1 <- st_as_sf(WFRV, coords = c("lon", "lat"),
                      crs = st_crs(AllRegions))
   x <- st_covers(AllRegions, sf_obs1)
   indices <- lengths(x)
   rowIndex <- unlist(x)
   RegionIndex <- vector('character')
   for (i in 1:nrow(AllRegions)) {
     RI <- rep(AllRegions$Region[i], indices[i])
     RegionIndex <- c(RegionIndex,RI)
   }
   WFRV$Region <- "At Sea"
   for (i in 1:length(rowIndex)) {
     WFRV$Region[rowIndex[i]] <- RegionIndex[i]
   }
   WFRV <- WFRV %>% filter(lon <= 180 ) %>% filter(lon >= -180) 
   WFRV$Region[WFRV$Region != 'At Sea'] <- AllRegions$Region2[match(WFRV$Region[WFRV$Region != 'At Sea'], AllRegions$Region)]
   WFRV$Region[WFRV$Region == 'At Sea' & WFRV$lon <= -35 & WFRV$lon >= -180] <- 'AMERICAS'
   WFRV$Region[WFRV$Region == 'At Sea' & WFRV$lon >= -35 & WFRV$lon <= 30] <- 'WEST OF SUEZ'
   WFRV$Region[WFRV$Region == 'At Sea' & WFRV$lon >= 30 & WFRV$lon <= 180] <- 'EAST OF SUEZ'
   
   #Choose dates for aggregation
  DateSeq <- seq(min(WFRV$Date), max(WFRV$Date), '2 week')
  # WFRVagg <- WFRV %>% filter(Date %in% DateSeq) %>% group_by(Date,Region) %>% summarise(Count = n()) %>% 
  #   spread(Date, Count, fill = 0)
  WFRVagg <- WFRV %>% filter(Date %in% DateSeq) %>% group_by(mmsi, Date,Region) %>% arrange(mmsi, Date,Region) %>% 
    select(mmsi, date, Region) %>% unique() %>% group_by(Date, Region) %>% summarise(Count = n()) %>% 
    spread(Date, Count, fill = 0)
   WFrv$AR <- WFRVagg
   #######################################################################
   
   x <- st_covers(AllRegions, sf_obs) 
   
   indices <- lengths(x)
   
   ARfilter <- AllRegions[indices > 0,]
   ARfilter$Count <- indices[indices > 0]
   ARfilter$Total <- rep(sum(ARfilter$Count), nrow(ARfilter)) - ARfilter$Count
   binpal <- colorBin("Reds", ARfilter$Count, 6, pretty = TRUE)
   # saveRDS(ARfilter,'ARfilter.Rds')
   map <- leaflet(WFH) %>%
     addTiles() %>%
     addProviderTiles(providers$CartoDB.Positron) %>%
     addHeatmap(lng = ~lon, lat = ~lat,
                blur = 15, max = 0.05, radius = 15, group = "HeatMaps") %>%
     addPolygons(data = ARfilter, stroke = FALSE, smoothFactor = 0.2, fillOpacity = 0.5,
                 color = ~binpal(Count),group = "AreaPolygons", label = ARfilter$Region,
                 labelOptions = labelOptions(noHide = F)) %>%
     addMinicharts(ARfilter$Lon, ARfilter$Lat,chartdata = ARfilter$Count, showLabels = TRUE,
                   width = 30) %>%
     addLayersControl(overlayGroups = c("HeatMaps","AreaPolygons"),
                      options = layersControlOptions(collapsed = TRUE)) %>%
     hideGroup(c("AreaPolygons"))
 })
 
 output$ARData <- renderDT({
   req(WFrv$AR)
   datatable(WFrv$AR, options = list(scrollX = TRUE))
 })

###############################################################
#Loading Heatmaps

#Scorpio Load Port Heatmaps
output$SCLP <- renderLeaflet({
  req(input$WorldDateRange)
  StartDate <- ymd(input$WorldDateRange[1])
  EndDate <- ymd(input$WorldDateRange[2])
  WFH <- req(WFrv$DraftIM)
  SLP <- req(WFrv$SLP) 

  leaflet() %>%
    addTiles() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addHeatmap(data = SLP, lng = ~lon, lat = ~lat, gradient = 'Spectral',
               blur = 10, max = 0.05, radius = 15, group = "Scorpio Load Port HeatMaps") %>%
    addHeatmap(data = WFH, lng = ~lon, lat = ~lat,
               blur = 25, max = 0.05, radius = 15, group = "Fleet Load Port HeatMaps") %>%
    addLayersControl(overlayGroups = c("Scorpio Load Port HeatMaps","Fleet Load Port HeatMaps"),
                     options = layersControlOptions(collapsed = TRUE)) %>%
    hideGroup(c("Scorpio Load Port HeatMaps"))
})
 
################################################################
 #Discharge Port Heatmaps
 output$DisPort <- renderLeaflet({
   req(input$WorldDateRange)
   StartDate <- ymd(input$WorldDateRange[1])
   EndDate <- ymd(input$WorldDateRange[2])
   WFH <- req(WFrv$DP)
   SDP <- req(WFrv$SDP) 
   
   leaflet() %>%
     addTiles() %>%
     addProviderTiles(providers$CartoDB.Positron) %>%
     addHeatmap(data = SDP, lng = ~lon, lat = ~lat, gradient = 'Spectral',
                blur = 10, max = 0.05, radius = 15, group = "Scorpio Discharge Port HeatMaps") %>%
     addHeatmap(data = WFH, lng = ~lon, lat = ~lat,
                blur = 25, max = 0.05, radius = 15, group = "Fleet Discharge Port HeatMaps") %>%
     addLayersControl(overlayGroups = c("Scorpio Discharge Port HeatMaps","Fleet Discharge Port HeatMaps"),
                      options = layersControlOptions(collapsed = TRUE)) %>%
     hideGroup(c("Scorpio Discharge Port HeatMaps"))
 })
 
 #####################################################################
 #Vessel History Tab

 output$FleetType <- renderUI({
   pickerInput('FleetType', 'Select Fleet Type', choices = c('World', 'Scorpio'),
               selected = 'World')
 })
 output$WVDVslType <- renderUI({
   pickerInput('WVDVslType', 'Select Vessel Type', choices = c('Handy','MR','LR1','LR2'), multiple = TRUE,
               selected = 'Handy')
 })
 output$VesselSelect <- renderUI({
   req(input$FleetType)
   req(input$WVDVslType)
   if(input$FleetType == 'World')
   choices <- wvd() %>% filter(`Commercial Operator` != 'SCORPIO') %>% filter(`Vessel Type` %in% input$WVDVslType) %>% 
     pull(Name)
   else choices <- wvd() %>% filter(`Commercial Operator` == 'SCORPIO') %>% filter(`Vessel Type` %in% input$WVDVslType) %>% 
     pull(Name)
   pickerInput('VesselSelect', 'Select Vessel', choices = choices,
               options = list(`actions-box` = TRUE,`live-search` = TRUE))
 }) 
 
 output$ScorpioDateRange <- renderUI({
   dateRangeInput('ScorpioDateRange', 'Select a date range', start = Sys.Date()-30,
                  end=Sys.Date())
 })
 
 #Get positional information
 
 observeEvent(input$ScorExec, {
   req(input$FleetType)
   req(input$VesselSelect)
   req(input$ScorpioDateRange)
   # SVD <- req(stratum_vdimvessel())
   SMMSI <- as.character(wvd()$MMSI[wvd()$Name == input$VesselSelect])
   
   StartDate <- as.numeric(gsub('-','',as.character(input$ScorpioDateRange[1])))
   EndDate <- as.numeric(gsub('-','',as.character(input$ScorpioDateRange[2])))
   
   shinyjs::show("page_shield")
   SHistory <- pool %>% tbl("WVDListFull") %>% filter(date >= StartDate & date <= EndDate) %>%
     filter(mmsi == SMMSI) %>% arrange(mmsi, desc(timestamp)) %>% collect()
   
  SHistory <- SHistory %>% select(lat, lon, timestamp, draught, destination, imoNumber) %>% filter(!is.na(lat))
  SHistory$lat <- as.numeric(SHistory$lat)
  SHistory$lon <- as.numeric(SHistory$lon)
  SHistory$draught <- as.numeric(SHistory$draught)
  SHistory$timestamp <- ymd_hms(SHistory$timestamp)
  SHistory$imoNumber <- as.numeric(SHistory$imoNumber)
  SHistory <- SHistory[!duplicated(SHistory),]

  shinyjs::hide("page_shield")
  
  #For world fleet, get load and discharge ports
  if (input$FleetType == 'World') {
    WFLD <- pool %>% tbl('IM_WVDListType5') %>% filter(date >= StartDate & date <= EndDate) %>%
      filter(mmsi == SMMSI) %>% arrange(mmsi, desc(timestamp)) %>% collect()
    WFLD <- WFLD %>% select(-id)
    WFLD <- WFLD[!duplicated(WFLD),]
    
    output$voyages <- renderDT({
      datatable(WFLD, rownames = FALSE)
    })
    
  }
  
  #Get Information on Voyages undertaken by the selected Scorpio vessel
  #Get IMO number
  if (input$FleetType == 'Scorpio'){
    IMON <- wvd()$`IMO No.`[wvd()$Name == input$VesselSelect]
    
    #Now get VIDs associated with this IMO number
    
    VIDs <- V_Dim_Vessel() %>% filter(imo_no == IMON) %>% pull(Dim_Vessel_Id)
    startperiod <- input$ScorpioDateRange[1]
    stopperiod <- input$ScorpioDateRange[2]
      VT <- V_Dim_Voyage() %>% filter((CommenceGMT >= startperiod & CompleteGMT <= stopperiod) |
                                        (CommenceGMT < startperiod & CompleteGMT > startperiod & CompleteGMT <= stopperiod) |
                                        (CommenceGMT > startperiod & CommenceGMT < stopperiod & CompleteGMT > stopperiod) |
                                        (CommenceGMT < startperiod & CompleteGMT > stopperiod)) %>%
        filter(Fkey_Dim_Vessel_Id %in% VIDs) %>%
        filter(VoyageEstimateExists == 'Voy With Estimate') %>%
        dplyr::select(Dim_Voyage_Id, Fkey_Dim_Vessel_Id, voyNum,CommenceGMT, CompleteGMT, PortOfFirstLoad_PortName, Fkey_Dim_Ports_FirstLoad,
                      PortOfLastDischarge_Portname, Fkey_Dim_Ports_LastDischarge,VesselName,oprType,
                      Fkey_Enum_VoyageStatus_Id, Fkey_Dim_Company_Id) %>%
        mutate(VoyDays = round(if_else(CommenceGMT >= startperiod & CompleteGMT <= stopperiod, difftime(CompleteGMT, CommenceGMT, units = "days"),
                                       if_else(CommenceGMT < startperiod & CompleteGMT > startperiod & CompleteGMT <= stopperiod,
                                               difftime(CompleteGMT, startperiod , units = "days"),
                                               if_else(CommenceGMT > startperiod & CommenceGMT < stopperiod & CompleteGMT > stopperiod,
                                                       difftime(stopperiod,CommenceGMT, units = "days"),
                                                       difftime(stopperiod,startperiod, units = "days")))),2)) %>%
        filter(voyNum != 1, VesselName != grepl("SBI", VesselName),
               oprType != 'RELT',
               oprType != 'OVTO',
               oprType != 'TCTO',
               Fkey_Enum_VoyageStatus_Id != 4,
               Fkey_Enum_VoyageStatus_Id != 1,
               Fkey_Dim_Company_Id %!in% CIDs) %>%
        dplyr::select(Vessel = VesselName, VesselID = Fkey_Dim_Vessel_Id,Dim_Voyage_Id,from = CommenceGMT,
                      to = CompleteGMT,Load = PortOfFirstLoad_PortName,Discharge = PortOfLastDischarge_Portname,
                      Days = VoyDays,voyNum)
      VT$LoadingRegion <- PortRegion$Region[match(VT$Load,PortRegion$PortName)]
      VT$DischargeRegion <- PortRegion$Region[match(VT$Discharge,PortRegion$PortName)]
      
      ComVoy <- V_Fact_VesselItinerary_Vsched() %>% filter(Fkey_Dim_Voyage_Id %in% VT$Dim_Voyage_Id) %>%
        filter(Fkey_Dim_PortFunction_Id == 1) %>% dplyr::select(Fkey_Dim_Voyage_Id,Fkey_Dim_PortFunction_Id,Fkey_Dim_Ports_Id) %>%
        left_join(V_Dim_Ports(),by = c("Fkey_Dim_Ports_Id" = "Dim_Ports_Id")) %>%
        dplyr::select(Fkey_Dim_Voyage_Id,PortName,CArea = Area,CSubArea = SubArea)
      
      VT$CommencingPort <- ComVoy$PortName[match(VT$Dim_Voyage_Id,ComVoy$Fkey_Dim_Voyage_Id)]
      VT$CommencingRegion <- PortRegion$Region[match(VT$CommencingPort ,PortRegion$PortName)]
      VT$CArea <- ComVoy$CArea[match(VT$CommencingPort,ComVoy$PortName)]
      VT$CSubArea <- ComVoy$CSubArea[match(VT$CommencingPort,ComVoy$PortName)]
      VT$LoadArea <- V_Dim_Ports()$Area[match(VT$Load,V_Dim_Ports()$PortName)]
      VT$LoadRegion <- V_Dim_Ports()$Region[match(VT$Load,V_Dim_Ports()$PortName)]
      VT$LoadSubArea <- V_Dim_Ports()$SubArea[match(VT$Load,V_Dim_Ports()$PortName)]
      VT$DischargeArea <- V_Dim_Ports()$Area[match(VT$Discharge,V_Dim_Ports()$PortName)]
      VT$DischargeRegion <- V_Dim_Ports()$Region[match(VT$Discharge,V_Dim_Ports()$PortName)]
      VT$DischargeSubArea <- V_Dim_Ports()$SubArea[match(VT$Discharge,V_Dim_Ports()$PortName)]
      
      vItin <- V_Fact_VesselItinerary_Vsched() %>% filter(Fkey_Dim_Voyage_Id %in% unique(VT$Dim_Voyage_Id))
      PNL <- V_Fact_VoyPNL() %>% filter(Fkey_Dim_Voyage_Id %in% unique(vItin$Fkey_Dim_Voyage_Id)) %>%
        group_by(Fkey_Dim_Voyage_Id) %>% summarise(CurrentDate = max(snapshotRefDate)) %>%
        left_join(V_Fact_VoyPNL(), by = c("Fkey_Dim_Voyage_Id" = "Fkey_Dim_Voyage_Id",
                                          "CurrentDate" = "snapshotRefDate")) %>%
        dplyr::select(Fkey_Dim_Voyage_Id, CurrentDate, tcEquv_Act, ttlVoyDays_Act) %>%
        group_by(Fkey_Dim_Voyage_Id)
      PNL$tcEquv_Act <- round(PNL$tcEquv_Act,2)
      
      PNL <- PNL[complete.cases(PNL),]
      PNL <- PNL[!duplicated(PNL),]
      
      #Compare with  our calculation of estimated days
      VT <- VT %>% left_join(PNL,by = c("Dim_Voyage_Id" = "Fkey_Dim_Voyage_Id")) %>%
        dplyr::select(names(VT),TCE = tcEquv_Act) %>% mutate(Revenue = round(Days*TCE,2))
      
      VDP <- V_Dim_Ports()
      VDP$Dim_Ports_Id <- as.numeric(VDP$Dim_Ports_Id)
      PM <- vItin %>% left_join(VDP, by = c("Fkey_Dim_Ports_Id" = "Dim_Ports_Id")) %>%
        left_join(V_Dim_Vessel(), by = c("Fkey_Dim_Vessel_Id" = "Dim_Vessel_Id")) %>%
        group_by(Fkey_Dim_Voyage_Id, Imos_VoyNo) %>%
        dplyr::select(Imos_VoyNo,ord_no_int,Fkey_Dim_PortFunction_Id,PortName,Fkey_Dim_Voyage_Id,
                      Vessel, Fkey_Dim_Vessel_Id,Date_Arrival,Date_Departure,Fkey_Enum_BallastLaden_Id,
                      Longitude, Latitude, Area, SubArea,TimeZone) %>% arrange(Fkey_Dim_Voyage_Id,ord_no_int) %>%
        left_join(portFunction, by = c("Fkey_Dim_PortFunction_Id" = "ID")) %>%
        mutate(ArrivalDate = Date_Arrival - (TimeZone*60*60))
      #Add some jitter
      PM$Longitude <- jitter(PM$Longitude, factor = 0.001)
      PM$Latitude <- jitter(PM$Latitude, factor = 0.001)
      PM <- PM %>% left_join(PNL, by = "Fkey_Dim_Voyage_Id") %>%
        mutate(TD = round(difftime(Date_Departure,ArrivalDate,units = "days") ,2))
      PM$Region <- PortRegion$Region[match(PM$PortName,PortRegion$PortName)]
      PM$ArrivalDate[PM$Function == "Commencing"] <- NA
      PM$Fkey_Enum_BallastLaden_Id[PM$Fkey_Enum_BallastLaden_Id == 1] <- "Laden"
      PM$Fkey_Enum_BallastLaden_Id[PM$Fkey_Enum_BallastLaden_Id == 0] <- "Ballast"
      
      PM <- PM %>% dplyr::select(PortName,Region,Function,ArrivalDate,DepartureDate = Date_Departure,Longitude,Latitude,
                          Leg = Fkey_Enum_BallastLaden_Id)
      
      output$voyages <- renderDT({
        req(VT)
        VT <- VT %>% arrange(desc(from))
        datatable(VT %>% select( Vessel, voyNum,from, to, CommencingPort, Load, Discharge,
                                          Days, TCE, Revenue) ,rownames = FALSE,
                  extensions = 'Buttons', options = list(dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print',I('colvis')),scrollX = TRUE),
                  selection = 'single') %>% formatDate(c(3,4), "toLocaleString")
      },server = FALSE)
     
    
  }

  
  
  #########################################################
  output$shipmaps <- renderLeaflet({
    if (input$FleetType == 'Scorpio') {
      if (nrow(SHistory) < 1 || !exists('PM')) return(NULL)
    req(SHistory)
    req(PM)
    VD <- PM 
    maxt <- SHistory
    maxt <- maxt %>% filter(lon <= 180 )
    # saveRDS(VD,'VD.Rds')

    if(nrow(maxt) > 4000){
      value <- nrow(maxt) %/% 3998
      mlength <-rep(value,3998)
      cumlength <- cumsum(mlength)
      test <- c(1, cumlength, nrow(maxt))
      maxt <- maxt[test,]
    }
    maxt <- maxt %>% arrange(ymd_hms(timestamp))
    maxt <- maxt %>% filter(!is.na(lat))
    # VD <- VD %>% filter(DepartureDate >= DepartureDate[1])
    for(i in 1:nrow(VD)){
      if(i < 2){
        maxt$Leg[ymd_hms(maxt$timestamp) <= VD$DepartureDate[i]] <- VD$Leg[i]
      }
      else{
        maxt$Leg[ymd_hms(maxt$timestamp) <= VD$DepartureDate[i] & ymd_hms(maxt$timestamp) > VD$DepartureDate[i-1]] <- VD$Leg[i-1]
      }
    }
    
    maxt$Color <- ifelse(maxt$Leg=="Ballast", "red", "blue")
    maxt <- maxt %>% filter(!is.na(lat))
    maxt <- maxt %>% mutate(lat2=lead(lat, default=tail(lat, 1)),
                     lon2=lead(lon, default=tail(lon, 1))) %>%
      mutate(segment = st_sfc(crs=4326,
                              pmap(.,
                                   function(lat, lon, lat2, lon2, ...)
                                     st_linestring(matrix(c(lon, lat, lon2, lat2),
                                                          byrow=TRUE, ncol=2)))))

    PFIcons <- icons(
      iconUrl =
        ifelse(VD$Function=='Commencing',letterC,
               ifelse(VD$Function=='Discharging',letterD,
                      ifelse(VD$Function=='Fuelling',letterF,
                             ifelse(VD$Function=='Canal Transit',letterI,
                                    ifelse(VD$Function=='Loading',letterL,
                                           ifelse(VD$Function=='Other',letterO,
                                                  ifelse(VD$Function=='Passing',letterP,
                                                         ifelse(VD$Function=='Port Call Cancelled',letterQ,
                                                                ifelse(VD$Function=='Repair',letterR,
                                                                       ifelse(VD$Function=='Sampling',letterS,
                                                                              ifelse(VD$Function=='Terminating',letterT,
                                                                                     ifelse(VD$Function=='Waiting',letterW,
                                                                                            ifelse(VD$Function=='Delivery',letterY,
                                                                                                   letterZ))))))))))))),
      iconWidth = 24, iconHeight = 24, iconAnchorX = 10, iconAnchorY = 10)
    # saveRDS(maxt, 'maxt.Rds')
     # Need to convert the lat-long data to a format that plays well with leafet JS
    data_js <- lapply(seq_along(maxt$lon), function(x) c(maxt$lat[x], maxt$lon[x]))
    legend <- tags$h6("Red line signifies ballast leg and blue implies laden")
    req(maxt)
    maxt$Color[is.na(maxt$Color)] <- "blue"
    leaflet(maxt$segment) %>%
      registerPlugin(polylineDecoratorPlugin) %>%
      addTiles() %>% addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addPolylines(color=maxt$Color) %>%
      # addPolylines(lng = maxt$lon, lat = maxt$lat, weight = 4, color = "blue") %>%
      onRender("
                        function(el, x, data) {
                        L.polylineDecorator(data, {
                        patterns: [{
                        offset: 0, repeat: 60,
                        symbol: L.Symbol.arrowHead({
                        pixelSize: 15, polygon: false, headAngle: 65,
                        pathOptions: {stroke: true, weight: 2}
                        })
                        }]
                        }).addTo(this);
                        }", data = data_js) %>%
      addMarkers(lng=VD$Longitude, lat=VD$Latitude,
                 label = paste(VD$Function, "at", VD$PortName), icon=PFIcons) %>%
      addControl(legend, position = "bottomright")
    }
    
    else {
      if ( nrow(SHistory) < 1 || !exists('WFLD')) return(NULL)
      if ( nrow(WFLD) < 1) return(NULL)
      req(SHistory)
      req(WFLD)
      
      
      maxt <- SHistory
      WFLD$lat <- as.numeric(WFLD$lat)
      WFLD$lon <- as.numeric(WFLD$lon)
      maxt <- maxt %>% filter(lon <= 180 )
      
      # saveRDS(maxt,'maxt.Rds')
      if(nrow(maxt) > 4000){
        value <- nrow(maxt) %/% 3998
        mlength <-rep(value,3998)
        cumlength <- cumsum(mlength)
        test <- c(1, cumlength, nrow(maxt))
        maxt <- maxt[test,]
      }
      maxt <- maxt %>% arrange(ymd_hms(timestamp))
      maxt <- maxt %>% filter(!is.na(lat))
      # VD <- VD %>% filter(DepartureDate >= DepartureDate[1])
      # for(i in 1:nrow(WFLD)){
      #   if(i < 2){
      #     maxt$Leg[ymd_hms(maxt$timestamp) <= VD$DepartureDate[i]] <- VD$Leg[i]
      #   }
      #   else{
      #     maxt$Leg[ymd_hms(maxt$timestamp) <= VD$DepartureDate[i] & ymd_hms(maxt$timestamp) > VD$DepartureDate[i-1]] <- VD$Leg[i-1]
      #   }
      # }
      
      # maxt$Color <- ifelse(maxt$Leg=="Ballast", "red", "blue")
      # maxt <- maxt %>% filter(!is.na(lat))
      maxt <- maxt %>% mutate(lat2=lead(lat, default=tail(lat, 1)),
                              lon2=lead(lon, default=tail(lon, 1))) %>%
        mutate(segment = st_sfc(crs=4326,
                                pmap(.,
                                     function(lat, lon, lat2, lon2, ...)
                                       st_linestring(matrix(c(lon, lat, lon2, lat2),
                                                            byrow=TRUE, ncol=2)))))
      
      PFIcons <- icons(
        iconUrl =
          ifelse(WFLD$portFunction =='L',letterL, letterD),
        iconWidth = 24, iconHeight = 24, iconAnchorX = 10, iconAnchorY = 10)
      # saveRDS(maxt, 'maxt.Rds')
      # Need to convert the lat-long data to a format that plays well with leafet JS
      data_js <- lapply(seq_along(maxt$lon), function(x) c(maxt$lat[x], maxt$lon[x]))
      # legend <- tags$h6("Red line signifies ballast leg and blue implies laden")
      req(maxt)
      leaflet(maxt$segment) %>%
        registerPlugin(polylineDecoratorPlugin) %>%
        addTiles() %>% addProviderTiles(providers$Esri.WorldTopoMap) %>% addPolylines() %>% 
        # addPolylines(color=maxt$Color) %>%
        # addPolylines(lng = maxt$lon, lat = maxt$lat, weight = 4, color = "blue") %>%
        onRender("
                        function(el, x, data) {
                        L.polylineDecorator(data, {
                        patterns: [{
                        offset: 0, repeat: 60,
                        symbol: L.Symbol.arrowHead({
                        pixelSize: 15, polygon: false, headAngle: 65,
                        pathOptions: {stroke: true, weight: 2}
                        })
                        }]
                        }).addTo(this);
                        }", data = data_js) %>%
        addMarkers(lng=WFLD$lon, lat=WFLD$lat,
                   label = paste(WFLD$destination), icon=PFIcons) #%>%
        # addControl(legend, position = "bottomright")
    }
  })
  
  
 })
 
 
 #####################################################################
 