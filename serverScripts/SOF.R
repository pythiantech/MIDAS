
output$VslTypeSOF <- renderUI({
  pickerInput('VslTypeSOF', "Select Vessel Type", choices = c("HANDYMAX","AFRAMAX","LR1","LR2","MR","PANAMAX","KAMSARMAX","ULTRAMAX"),
              multiple = TRUE, options = list(`actions-box` = TRUE))
})
output$PortSOF <- renderUI({
  selectInput('PortSOF', "Select Port", choices=unique(V_Dim_Ports()$PortName))
})
output$CargoSOF <- renderUI({
  pickerInput('CargoSOF', "Select Cargo Grade", choices = unique(V_Dim_CargoAndGrade()$CargoGrade_txt),
              multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE))
})
output$PortFuncSOF <- renderUI({
  pickerInput('PortFuncSOF',"Select Port Function", choices = c("Loading" = 5,"Discharging" = 2,
                                                                "Fuelling" = 3, "Waiting" = 12))
})


SOF <- reactiveValues()

observeEvent(input$SOFFilters, {
  req(input$VslTypeSOF)
  req(input$PortSOF)
  req(input$CargoSOF)
  req(input$PortFuncSOF)

  CargoGradeID <- V_Dim_CargoAndGrade()$Dim_CargoAndGrade_Id[V_Dim_CargoAndGrade()$CargoGrade_txt %in% input$CargoSOF]
  VIDs <- unique(V_Dim_Vessel()$Dim_Vessel_Id[V_Dim_Vessel()$VesselType %in% input$VslTypeSOF])
  VoyIDs <- unique(V_Dim_Voyage()$Dim_Voyage_Id[V_Dim_Voyage()$oprType %!in% c('RELT', 'OVTO', 'TCTO')])
  PortID <- V_Dim_Ports()$Dim_Ports_Id[V_Dim_Ports()$PortName == input$PortSOF]
  VoI <- V_Dim_CargoHandlingEvent() %>% filter(Fkey_Dim_Ports_Id == PortID) %>%
    filter(Fkey_Dim_CargoAndGrade_ID %in% CargoGradeID) %>% filter(Fkey_Dim_Vessel_Id %in% VIDs) %>%
    filter(Fkey_Dim_Voyage_Id %in% VoyIDs)

  PortAct <- V_Dim_PortActivity() %>% filter(Fkey_Dim_Ports_Id == PortID) %>% filter(Fkey_Dim_Vessel_Id %in% VoI$Fkey_Dim_Vessel_Id) %>%
    filter(Fkey_Dim_PortFunction_Id == input$PortFuncSOF) %>%
    filter(Date_ActivityStart >= ymd_hms(as.character("2017-01-01 00:00:01"))) %>%
    select(Fkey_Dim_Vessel_Id, Fkey_Dim_Voyage_Id,Fkey_Dim_PortFunction_Id, vlineNo, PortActionType_Code, PortActionType_Label,ActivityName, Date_ActivityStart) %>%
    arrange(Fkey_Dim_Vessel_Id, Fkey_Dim_Voyage_Id,Fkey_Dim_PortFunction_Id, vlineNo)
  PortAct$ActivityName <- trimws(PortAct$ActivityName)
  PortAct$VslName <- V_Dim_Vessel()$VesselName[match(PortAct$Fkey_Dim_Vessel_Id, V_Dim_Vessel()$Dim_Vessel_Id)]
  PortAct$voyNum <- V_Dim_Voyage()$voyNum[match(PortAct$Fkey_Dim_Voyage_Id, V_Dim_Voyage()$Dim_Voyage_Id)]

  SOF$PortAct <- PortAct

  #######################################
  #Activity Time Function
  ActivityCodeTime <- function(act1, act2){
    string1 <- paste(act1)
    string2 <- paste(act2)
    df <- PortAct %>% group_by(Fkey_Dim_Vessel_Id, Fkey_Dim_Voyage_Id) %>%
      mutate(new = match(string1, PortActionType_Code)) %>% na.omit() %>%
      slice(new:n()) %>%
      distinct(PortActionType_Code, .keep_all = TRUE) %>%
      filter(PortActionType_Code %in% c(string1, string2)) %>% filter(n()>1) %>%
      mutate(TimeInterval = difftime(Date_ActivityStart, lag(Date_ActivityStart), units = "hours"))
    df
  }
  #######################################
  #Port Turnaround Time Calculation

  PT <- ActivityCodeTime("PS", "PE")

  SOF$PT <- PT
  SOF$PTDay <- mean(as.numeric(PT$TimeInterval), na.rm = TRUE) %/% 24
  SOF$PTHour <- round(mean(as.numeric(PT$TimeInterval), na.rm = TRUE) %% 24,2)

  #Operations Count
  OC <- PortAct %>% group_by(Fkey_Dim_Vessel_Id, Fkey_Dim_Voyage_Id) %>%
    filter(PortActionType_Code %in% c("OS", "OE")) %>%
    add_count(PortActionType_Code)

  SOF$OCMin <- min(OC$n, na.rm = TRUE)
  SOF$OCMax <- max(OC$n, na.rm = TRUE)
  SOF$OCAvg <- mean(OC$n, na.rm = TRUE)


  OTT <- ActivityCodeTime("OS","OE")

  SOF$OTT <- OTT
  SOF$OTTDay <- mean(as.numeric(OTT$TimeInterval), na.rm = TRUE) %/% 24
  SOF$OTTHour <- round(mean(as.numeric(OTT$TimeInterval), na.rm = TRUE) %% 24,2)
##################################################################################################
  #Activity Time Function
  ActivityTime <- function(act1, act2){
    string1 <- paste(act1)
    string2 <- paste(act2)
    df <- PortAct %>% group_by(Fkey_Dim_Vessel_Id, Fkey_Dim_Voyage_Id) %>%
      mutate(new = match(string1, ActivityName)) %>% na.omit() %>%
      slice(new:n()) %>%
      distinct(ActivityName, .keep_all = TRUE) %>%
      filter(ActivityName %in% c(string1, string2)) %>% filter(n() > 1) %>%
      mutate(TimeInterval = difftime(Date_ActivityStart, lag(Date_ActivityStart), units = "hours"))
    df
  }
  #ESP to NOR Tendered (first occurence)
  ESPNOR <- ActivityTime("END OF SEA PASSAGE (ESP)", "NOR TENDERED")
  SOF$ESPNORHour <- round(mean(as.numeric(ESPNOR$TimeInterval), na.rm = TRUE) %% 24,2)
  SOF$ESPNORDay <- round(mean(as.numeric(ESPNOR$TimeInterval), na.rm = TRUE) %/% 24,2)


  #NOR to ALL FAST
  NORALL <- ActivityTime("NOR TENDERED", "ALL FAST")
  SOF$NORALLHour <- round(mean(as.numeric(NORALL$TimeInterval), na.rm = TRUE) %% 24,2)
  SOF$NORALLDay <- round(mean(as.numeric(NORALL$TimeInterval), na.rm = TRUE) %/% 24,2)

  #NOR to Hoses Disconnected
  NORHOS <- ActivityTime("NOR TENDERED", "HOSES DISCONNECTED")
  SOF$NORHOSHour <- round(mean(as.numeric(NORHOS$TimeInterval), na.rm = TRUE) %% 24,2)
  SOF$NORHOSDay <- round(mean(as.numeric(NORHOS$TimeInterval), na.rm = TRUE) %/% 24,2)

  #NOR to Free Pratique Granted
  NORFP <- ActivityTime("NOR TENDERED", "FREE PRATIQUE GRANTED")
  SOF$NORFPHour <- round(mean(as.numeric(NORFP$TimeInterval), na.rm = TRUE) %% 24,2)
  SOF$NORFPDay <- round(mean(as.numeric(NORFP$TimeInterval), na.rm = TRUE) %/% 24,2)

  #Anchored to Anchor Up
  ANCH <- ActivityTime("ANCHORED", "ANCHOR UP")
  SOF$ANCHHour <- round(mean(as.numeric(ANCH$TimeInterval), na.rm = TRUE) %% 24,2)
  SOF$ANCHDay <- round(mean(as.numeric(ANCH$TimeInterval), na.rm = TRUE) %/% 24,2)

  #Inward Passage
  IP <- ActivityTime('PILOT ON BOARD', 'ALL FAST')
  SOF$IPHour <- round(mean(as.numeric(IP$TimeInterval), na.rm = TRUE) %% 24,2)
  SOF$IPDay <- round(mean(as.numeric(IP$TimeInterval), na.rm = TRUE) %/% 24,2)

  #Pre-operation Time
  if(input$PortFuncSOF == 5) PREOP <- ActivityTime('ALL FAST', 'COMMENCED LOADING')
  else PREOP <- ActivityTime('ALL FAST', 'COMMENCED DISCHARGING')
  SOF$PREOPHour <- round(mean(as.numeric(PREOP$TimeInterval), na.rm = TRUE) %% 24,2)
  SOF$PREOPDay <- round(mean(as.numeric(PREOP$TimeInterval), na.rm = TRUE) %/% 24,2)

  #Operation Time
  if(input$PortFuncSOF == 5) OP <- ActivityTime('COMMENCED LOADING','COMPLETED LOADING')
  else OP <- ActivityTime('COMMENCED DISCHARGING', 'COMPLETED DISCHARGE')
  SOF$OPHour <- round(mean(as.numeric(OP$TimeInterval), na.rm = TRUE) %% 24,2)
  SOF$OPDay <- round(mean(as.numeric(OP$TimeInterval), na.rm = TRUE) %/% 24,2)

  #Post Operation Time
  if(input$PortFuncSOF == 5) POP <- ActivityTime('COMPLETED LOADING','CLEAR FROM BERTH')
  else POP <- ActivityTime('COMPLETED DISCHARGE', 'CLEAR FROM BERTH')
  SOF$POPHour <- round(mean(as.numeric(POP$TimeInterval), na.rm = TRUE) %% 24,2)
  SOF$POPDay <- round(mean(as.numeric(POP$TimeInterval), na.rm = TRUE) %/% 24,2)

  #Clear from berth
  CFB <- ActivityTime('CLEAR FROM BERTH', 'START SEA PASSAGE (SSP)')
  SOF$CFBHour <- round(mean(as.numeric(CFB$TimeInterval), na.rm = TRUE) %% 24,2)
  SOF$CFBDay <- round(mean(as.numeric(CFB$TimeInterval), na.rm = TRUE) %/% 24,2)

  SOF$ActivityDF <- data.frame(
    Activity = c("STD ACTIVITY 1", "STD ACTIVITY 2", "STD ACTIVITY 3", "FREE PRATIQUE", "ANCHORED TIME",
                 "INWARD PASSAGE", "PRE-OP TIME", "OPERATION TIME","POST-OP TIME",
                 "OUTWARD PASSAGE"),
    FROM = c("EOSP", "NOR TENDERED", "NOR TENDERED", "NOR TENDERED","ANCHORED","PILOT ONBOARD",
             "ALL FAST", "COMMENCE LOADING/DISCHARGE", "COMPLETE LOADING/DISCHARGE", "CLEAR FROM BERTH"),
    TO = c("NOR TENDERED", "ALL FAST", "HOSES DISCONNECTED", "FREE PRATIQUE GRANTED", "ANCHOR UP",
           "ALL FAST","COMMENCE LOADING/DISCHARGE", "COMPLETE LOADING/DISCHARGE", "CLEAR FROM BERTH", "COSP"),
    `AVG DAYS` = c(SOF$ESPNORDay, SOF$NORALLDay, SOF$NORHOSDay, SOF$NORFPDay, SOF$ANCHDay, SOF$IPDay,
                   SOF$PREOPDay, SOF$OPDay, SOF$POPDay, SOF$CFBDay),
    `AVG HOURS` = c(SOF$ESPNORHour, SOF$NORALLHour, SOF$NORHOSHour, SOF$NORFPHour, SOF$ANCHHour, SOF$IPHour,
                    SOF$PREOPHour, SOF$OPHour, SOF$POPHour, SOF$CFBHour)

  )

})
output$VslSOF <- renderUI({
  selectInput('VslSOF', "Select Vessel", choices = unique(SOF$PortAct$VslName))
})

output$VoySOF <- renderUI({
  selectInput('VoySOF', "Select Voyage", choices = SOF$PortAct$voyNum[SOF$PortAct$VslName %in% input$VslSOF])
  
})
###########################################################################################################
observeEvent(input$VslActFilter, {
  req(SOF$PortAct)
  req(input$VslSOF)
  req(input$VoySOF)


  PTVsl <- SOF$PortAct %>% filter(VslName == input$VslSOF) %>% filter(voyNum == input$VoySOF)

  #Vsl Specific Activity Function
  VslActivityTime <- function(act1, act2){
    string1 <- paste(act1)
    string2 <- paste(act2)
    df <- PTVsl %>% group_by(Fkey_Dim_Vessel_Id, Fkey_Dim_Voyage_Id) %>%
      mutate(new = match(string1, ActivityName)) %>% na.omit() %>%
      slice(new:n()) %>%
      distinct(ActivityName, .keep_all = TRUE) %>%
      filter(ActivityName %in% c(string1, string2)) %>% filter(n() > 1) %>%
      mutate(TimeInterval = difftime(Date_ActivityStart, lag(Date_ActivityStart), units = "hours"))
    df
  }

  #ESP to NOR Tendered (first occurence)
  ESPNORVsl <- VslActivityTime("END OF SEA PASSAGE (ESP)", "NOR TENDERED")
  SOF$ESPNORHourVsl <- round(mean(as.numeric(ESPNORVsl$TimeInterval), na.rm = TRUE) %% 24,2)
  SOF$ESPNORDayVsl <- round(mean(as.numeric(ESPNORVsl$TimeInterval), na.rm = TRUE) %/% 24,2)


  #NOR to ALL FAST
  NORALLVsl <- VslActivityTime("NOR TENDERED", "ALL FAST")
  SOF$NORALLHourVsl <- round(mean(as.numeric(NORALLVsl$TimeInterval), na.rm = TRUE) %% 24,2)
  SOF$NORALLDayVsl <- round(mean(as.numeric(NORALLVsl$TimeInterval), na.rm = TRUE) %/% 24,2)

  #NOR to Hoses Disconnected
  NORHOSVsl <- VslActivityTime("NOR TENDERED", "HOSES DISCONNECTED")
  SOF$NORHOSHourVsl <- round(mean(as.numeric(NORHOSVsl$TimeInterval), na.rm = TRUE) %% 24,2)
  SOF$NORHOSDayVsl <- round(mean(as.numeric(NORHOSVsl$TimeInterval), na.rm = TRUE) %/% 24,2)

  #NOR to Free Pratique Granted
  NORFPVsl <- VslActivityTime("NOR TENDERED", "FREE PRATIQUE GRANTED")
  SOF$NORFPHourVsl <- round(mean(as.numeric(NORFPVsl$TimeInterval), na.rm = TRUE) %% 24,2)
  SOF$NORFPDayVsl <- round(mean(as.numeric(NORFPVsl$TimeInterval), na.rm = TRUE) %/% 24,2)

  #Anchored to Anchor Up
  ANCHVsl <- VslActivityTime("ANCHORED", "ANCHOR UP")
  SOF$ANCHHourVsl <- round(mean(as.numeric(ANCHVsl$TimeInterval), na.rm = TRUE) %% 24,2)
  SOF$ANCHDayVsl <- round(mean(as.numeric(ANCHVsl$TimeInterval), na.rm = TRUE) %/% 24,2)

  #Inward Passage
  IPVsl <- VslActivityTime('PILOT ON BOARD', 'ALL FAST')
  SOF$IPHourVsl <- round(mean(as.numeric(IPVsl$TimeInterval), na.rm = TRUE) %% 24,2)
  SOF$IPDayVsl <- round(mean(as.numeric(IPVsl$TimeInterval), na.rm = TRUE) %/% 24,2)

  #Pre-operation Time
  if(input$PortFuncSOF == 5) PREOPVsl <- VslActivityTime('ALL FAST', 'COMMENCED LOADING')
  else PREOPVsl <- VslActivityTime('ALL FAST', 'COMMENCED DISCHARGING')
  SOF$PREOPHourVsl <- round(mean(as.numeric(PREOPVsl$TimeInterval), na.rm = TRUE) %% 24,2)
  SOF$PREOPDayVsl <- round(mean(as.numeric(PREOPVsl$TimeInterval), na.rm = TRUE) %/% 24,2)

  #Operation Time
  if(input$PortFuncSOF == 5) OPVsl <- VslActivityTime('COMMENCED LOADING','COMPLETED LOADING')
  else OPVsl <- VslActivityTime('COMMENCED DISCHARGING', 'COMPLETED DISCHARGE')
  SOF$OPHourVsl <- round(mean(as.numeric(OPVsl$TimeInterval), na.rm = TRUE) %% 24,2)
  SOF$OPDayVsl <- round(mean(as.numeric(OPVsl$TimeInterval), na.rm = TRUE) %/% 24,2)

  #Post Operation Time
  if(input$PortFuncSOF == 5) POPVsl <- VslActivityTime('COMPLETED LOADING','CLEAR FROM BERTH')
  else POPVsl <- VslActivityTime('COMPLETED DISCHARGE', 'CLEAR FROM BERTH')
  SOF$POPHourVsl <- round(mean(as.numeric(POPVsl$TimeInterval), na.rm = TRUE) %% 24,2)
  SOF$POPDayVsl <- round(mean(as.numeric(POPVsl$TimeInterval), na.rm = TRUE) %/% 24,2)

  #Clear from berth
  CFBVsl <- VslActivityTime('CLEAR FROM BERTH', 'START SEA PASSAGE (SSP)')
  SOF$CFBHourVsl <- round(mean(as.numeric(CFBVsl$TimeInterval), na.rm = TRUE) %% 24,2)
  SOF$CFBDayVsl <- round(mean(as.numeric(CFBVsl$TimeInterval), na.rm = TRUE) %/% 24,2)


  SOF$ActivityDFVsl <- data.frame(
    `AVG DAYS` = c(SOF$ESPNORDayVsl, SOF$NORALLDayVsl, SOF$NORHOSDayVsl, SOF$NORFPDayVsl, SOF$ANCHDayVsl, SOF$IPDayVsl,
                   SOF$PREOPDayVsl, SOF$OPDayVsl, SOF$POPDayVsl, SOF$CFBDayVsl),
    `AVG HOURS` = c(SOF$ESPNORHourVsl, SOF$NORALLHourVsl, SOF$NORHOSHourVsl, SOF$NORFPHourVsl, SOF$ANCHHourVsl,
                    SOF$IPHourVsl,SOF$PREOPHourVsl, SOF$OPHourVsl, SOF$POPHourVsl, SOF$CFBHourVsl)

  )
})

###############################################
#Outputs

#O/P Port Name
output$PortName <- renderText({
  input$PortSOF
})
###############################################
#O/P Port Turnaround
output$PTHist <- renderHighchart({
  req(SOF$PT)
  if (length(SOF$PT$TimeInterval) > 0)
  hchist(as.numeric(SOF$PT$TimeInterval)/24, name = "Port Turnaround",
         binWidth = 10)
})

output$PTDay <- renderText({
  req(SOF$PT)
  req(SOF$PTDay)
  if (nrow(SOF$PT) > 0)
  paste(SOF$PTDay, "Days")
  else "No voyages"
})

output$PTHour <- renderText({
  req(SOF$PT)
  req(SOF$PTHour)
  if (nrow(SOF$PT) > 0)
  paste(SOF$PTHour, "Hours")
  else "No voyages"
})
###############################################
#O/P Oprns Count
output$OCMin <- renderText({
  req(SOF$OCMin)
  paste("Min : ", round(SOF$OCMin,2))
})

output$OCMax <- renderText({
  req(SOF$OCMax)
  paste("Max : ", round(SOF$OCMax,2))
})

output$OCAvg <- renderText({
  req(SOF$OCAvg)
  paste("Avg : ", round(SOF$OCAvg,2))
})
###############################################
#O/P OTT
output$OTTDensity <- renderHighchart({
  req(SOF$OTT)
  if (length(SOF$OTT$TimeInterval) > 0)
    hchist(as.numeric(SOF$OTT$TimeInterval)/24, name = "Operations Turnaround",
           color = 'orange')
})

output$OTTDay <- renderText({
  req(SOF$OTT)
  req(SOF$OTTDay)
  if (nrow(SOF$OTT) > 0)
    paste(SOF$OTTDay, "Days")
  else "No voyages"
})

output$OTTHour <- renderText({
  req(SOF$OTT)
  req(SOF$OTTHour)
  if (nrow(SOF$OTT) > 0)
    paste(SOF$OTTHour, "Hours")
  else "No voyages"
})
###############################################

output$Activity <- renderDT({
  req(SOF$ActivityDF)
  datatable(SOF$ActivityDF,options = list(
    dom = 't',
    ordering = FALSE,
    rowCallback = JS("function(r,d) {$(r).attr('height', '65px')}")
  ))
})

output$ActivityVsl <- renderDT({
  req(SOF$ActivityDFVsl)
  datatable(SOF$ActivityDFVsl, options = list(
    dom = 't',
    ordering = FALSE,
    rowCallback = JS("function(r,d) {$(r).attr('height', '65px')}")
  ), rownames = FALSE)
})

output$MiscFrom <- renderUI({
  req(SOF$PortAct)
  selectInput('MiscFrom', "Select Miscellaneous Activity (From)", choices = unique(SOF$PortAct$ActivityName))
})

output$MiscTo <- renderUI({
  req(SOF$PortAct)
  selectInput('MiscTo', "Select Miscellaneous Activity (To)", choices = unique(SOF$PortAct$ActivityName))
})

observeEvent(input$MiscAct, {
  req(input$MiscFrom)
  req(input$MiscTo)


})

###############################################
#O/P Port Turnaround by Vessel
output$PTDayVsl <- renderText({
  req(SOF$PT)
  req(input$VslSOF)
  req(input$VoySOF)
  SOF$PTVsl <- SOF$PT %>% filter(VslName == input$VslSOF) %>% filter(voyNum == input$VoySOF)
  SOF$PTDayVsl <- mean(as.numeric(SOF$PTVsl$TimeInterval), na.rm = TRUE) %/% 24
  SOF$PTHourVsl <- round(mean(as.numeric(SOF$PTVsl$TimeInterval), na.rm = TRUE) %% 24,2)
  if (nrow(SOF$PTVsl) > 0)
    paste(SOF$PTDayVsl, "Days")
  else "No voyages"
})

output$PTHourVsl <- renderText({
  req(SOF$PT)
  req(input$VslSOF)
  req(input$VoySOF)
  SOF$PTVsl <- SOF$PT %>% filter(VslName == input$VslSOF) %>% filter(voyNum == input$VoySOF)
  SOF$PTDayVsl <- mean(as.numeric(SOF$PTVsl$TimeInterval), na.rm = TRUE) %/% 24
  SOF$PTHourVsl <- round(mean(as.numeric(SOF$PTVsl$TimeInterval), na.rm = TRUE) %% 24,2)
  if (nrow(SOF$PTVsl) > 0)
    paste(SOF$PTHourVsl, "Hours")
  else "No voyages"
})

###############################################
#O/P OTT


output$OTTDayVsl <- renderText({
  req(SOF$OTT)
  req(input$VslSOF)
  req(input$VoySOF)
  SOF$OTTVsl <- SOF$OTT %>% filter(VslName == input$VslSOF) %>% filter(voyNum == input$VoySOF)
  SOF$OTTDayVsl <- mean(as.numeric(SOF$OTTVsl$TimeInterval), na.rm = TRUE) %/% 24
  if (nrow(SOF$OTTVsl) > 0)
    paste(SOF$OTTDayVsl, "Days")
  else "No voyages"
})

output$OTTHourVsl <- renderText({
  req(SOF$OTT)
  req(input$VslSOF)
  req(input$VoySOF)
  SOF$OTTVsl <- SOF$OTT %>% filter(VslName == input$VslSOF) %>% filter(voyNum == input$VoySOF)
  SOF$OTTHourVsl <- round(mean(as.numeric(SOF$OTTVsl$TimeInterval), na.rm = TRUE) %% 24,2)
  if (nrow(SOF$OTTVsl) > 0)
    paste(SOF$OTTHourVsl, "Hours")
  else "No voyages"
})

