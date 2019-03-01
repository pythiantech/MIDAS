##########################################################################################
#TC Data Analysis

values <- reactiveValues()

## Handsontable
observe({
  if (!is.null(input$TCData)) {
    values[["previous"]] <- isolate(values[["DF"]])
    DF = hot_to_r(input$TCData)
    DF <- DF[1:nrow(DF),]
    DF[,12] <- DF[,11] %m+% months(DF[,9])
    DF[,15] <- DF[,12] - DF[,10]
    DF[,16] <- DF[,12] + DF[,10]
    DF[,17] <- DF[,15] - DF[,13]
    DF[,14] <- as.numeric(DF[,17] - Sys.Date())
  } else {
    if (is.null(values[["DF"]]))
      DF <- DF
    else
      DF <- values[["DF"]]
  }
  values[["DF"]] <- DF
})

output$TCData <- renderRHandsontable({
  DF <- values[["DF"]]
  if (!is.null(DF))
    rhandsontable(DF) %>% hot_cols(colWidths = 200, fixedColumnsLeft = 1)

})

## Save
observeEvent(input$savetc, {

  finalDF <- isolate(values[["DF"]])
  saveRDS(finalDF, "data/tcdatapythian/TCComb.Rds")

}
)

##########################################
#Calendar Data

output$calData <- renderFullcalendar({
  start <- NULL
  color <- NULL
  DFgather <- gather(DF, "Date", "Value", AnnivDate, EReD, LReD, XO)
  calendarData <- data.frame(
    start = DFgather$Value,
    color = ifelse(DFgather$Date == "AnnivDate", rep('green',nrow(DFgather)),
                   ifelse(DFgather$Date == "EReD",rep('orange', nrow(DFgather)),
                          ifelse(DFgather$Date == "LReD", rep('blue', nrow(DFgather)), rep('red', nrow(DFgather))))),
    title = DF$Vessel
  )
  fullcalendar(calendarData)
})


output$option <- renderUI({
  pickerInput('options', "Select a period", choices = DF$`Period Type`[DF$Vessel == input$calvsl])
})

caldata <- reactive({
  DF %>% filter(Vessel == input$calvsl) %>% filter(`Period Type` == input$options)
})

output$cpdate <- renderText({
  req(caldata)
  as.character(caldata()$`CP Date`)
})
output$period <- renderText({
  req(caldata)
  as.character(caldata()$`Period in Months`)
})
output$deldate <- renderText({
  req(caldata)
  as.character(caldata()$Delivery)
})
output$annivdate <- renderText({
  req(caldata)
  as.character(caldata()$AnnivDate)
})
output$rddate <- renderText({
  req(caldata)
  as.character(caldata()$Redelivery)
})
output$rdarea <- renderText({
  req(caldata)
  as.character(caldata()$`Redelivery Area`)
})
output$rdnot <- renderText({
  req(caldata)
  as.character(caldata()$`Redel Notices`)
})
output$ered <- renderText({
  req(caldata)
  as.character(caldata()$EReD)
})
output$lred <- renderText({
  req(caldata)
  as.character(caldata()$LReD)
})
output$rate <- renderText({
  req(caldata)
  as.character(caldata()$Rate)
})
output$redarea <- renderText({
  req(caldata)
  as.character(caldata()$`Redelivery Area`)
})



#########################################
#TC Analysis
output$vslConstruction <- renderHighchart({
  vesselConstruction
})

output$Builder <- renderHighchart({
  ByBuilder <-   orderbook %>% dplyr::group_by_(input$type) %>% dplyr::summarise(Count = n()) %>%
    filter(Count >= input$minvsl)
  colnames(ByBuilder)[1] <- "test"
  highchart() %>%
    hc_add_series(ByBuilder,hcaes(x = 'test',y = "Count"), type = "pie",
                  dataLabels = list(enabled = TRUE),innerSize = '40%', size = '80%',
                  tooltip = list(pointFormat = paste('{point.y} ships<br/><b>{point.percentage:.1f}%</b>'))) %>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_title(text = paste("Construction by", input$type)) %>%
    hc_subtitle(text = paste("Control the min. number of constructions by the numeric slider"))
})

output$ByAge <- renderHighchart({
  consolid <- consolidated %>% filter(`Vessel Type` == input$vslclass)
  highchart() %>%
    hc_add_series(consolid,hcaes(x = 'AgeClass',y = "Count"), type = "pie",
                  dataLabels = list(enabled = TRUE),innerSize = '40%', size = '80%',
                  tooltip = list(pointFormat = paste('{point.y} ships<br/><b>{point.percentage:.1f}%</b>'))) %>%
    hc_add_theme(hc_theme_smpl()) %>%
    hc_title(text = paste(input$vslclass, "Fleet Profile"))%>%
    hc_subtitle(text = paste("The chart shows the age profile of current and on order different vessel types "))
})

output$ByOwner <- renderHighchart({
  ByOwner
})

output$OBAF <- renderHighchart({
  req(input$OwnerGroup)
  combineds <- combineds %>% filter(OwnerGroup == input$OwnerGroup)
  OBA <- highchart() %>%
    hc_chart(type = "bar") %>%
    hc_xAxis(categories = factor(combineds$VslType),
             title = list(text = NULL)) %>%
    hc_title(text = "Order Book v/s Active Fleet") %>%
    hc_subtitle(text = "Comparison of current fleet and ordered fleet of an Owner Group") %>%
    hc_yAxis(min = 0,
             title = list(
               text = "Count of Vessels",
               align = 'high'
             ),labels = list(overflow = 'justify')) %>%
    hc_plotOptions(bar = list(datalabels = list(enabled = TRUE)),
                   series = list(pointWidth = 25)) %>%
    hc_legend(layout = 'vertical') %>%
    hc_credits(enabled = FALSE) %>%
    hc_add_series(name = '2018',
                  data = combineds$`2018`) %>%
    hc_add_series(name = '2019',
                  data = combineds$`2019`) %>%
    hc_add_series(name = '2020',
                  data = combineds$`2020`) %>%
    hc_add_series(name = '2021',
                  data = combineds$`2021`) %>%
    hc_add_series(name = 'Present',
                  data = combineds$Present) %>%
    hc_add_theme(hc_theme_smpl())
  OBA
})

###################################################
#PNL Summary
#Load pooldata.Rdata

valuesPL <- reactiveValues(SV = NULL, TCV = NULL)
envPP <- reactiveFileReader(
  intervalMillis = 24*60*60*1000,
  session = session,
  filePath = config$ppdata,
  readFunc = LoadToEnvironment
)
PA_TRN_QUARTERLY_EARNING <- reactive( envPP()[[names(envPP())[1]]])
PoolId <- reactive( envPP()[[names(envPP())[2]]])
NonDryVsls <- reactive( envPP()[[names(envPP())[3]]])
PV_MAS_FLEET_DTLS <- reactive( envPP()[[names(envPP())[4]]])
PV_MAS_FLEET_VSL_DTLS <- reactive( envPP()[[names(envPP())[5]]])
PoolAcctSummary <-  reactive( envPP()[[names(envPP())[6]]])
PerformanceBM <-  reactive( envPP()[[names(envPP())[7]]])
finSum <-  reactive( envPP()[[names(envPP())[8]]])

##################################################################
PNLFinal <- reactive({
VFOD <- V_Fact_OnhireDays()
VFOD$VesselCode <- V_Dim_Vessel()$VesselCode[match(VFOD$Fkey_Dim_Vessel_Id, V_Dim_Vessel()$Dim_Vessel_Id)]

#Extract Period, Daily net TCE and Days Net from PoolAccntSummary
PNLTable <- PoolAcctSummary() %>% select(VSL_ID, Pool, VESSEL_NAME, PERIOD, DAYS_NET, DAILY_NET_TCE) %>% arrange(VSL_ID)
#Add PoolID too
PNLTable$POOL_ID <- PoolId()$POOL_ID[match(PNLTable$Pool,PoolId()$POOL_NAME)]

#Create a unique ID by combining VslID and PoolID
PNLTable <- PNLTable %>% unite(UID, VSL_ID, POOL_ID, sep = "_", remove = FALSE)

#Get dates of join and leaving
PV <- PV_MAS_FLEET_VSL_DTLS() %>% unite(UID, VSL_ID, POOL_ID, sep = "_", remove = FALSE)
PNLTable$DateJoined <- PV$DATE_JOINED[match(PNLTable$UID,PV$UID)]
PNLTable$DateLeft <- PV$DATE_LEFT[match(PNLTable$UID,PV$UID)]

#Get Display Name
PVMFD <- PV_MAS_FLEET_DTLS() %>% unite(UID, VSL_ID, POOL_ID, sep = "_", remove = FALSE)
PNLTable$DisplayName <- PVMFD$DISPLAY_NAME[match(PNLTable$UID,PVMFD$UID)]
PNLTable$PoolParticipant <- PVMFD$POOL_PARTICIPANT[match(PNLTable$UID,PVMFD$UID)]

NDVs <- NonDryVsls() %>% unite(UID, VSL_ID,POOL_ID,sep = "_",remove = FALSE)

NDV <- NDVs %>% group_by(POOL_ID) %>% mutate(Factor = TOTAL_POINTS/mean(TOTAL_POINTS, na.rm = TRUE)) %>%
  select(UID,POOL_ID,Factor)
NDV <- NDV[complete.cases(NDV$Factor),]

#Filter for months
##!!!!!!!!!!!!!!ymd_hms doesn't work on the server!!!!!!!!!!!!!!!!!!!!
PATRN <- PA_TRN_QUARTERLY_EARNING()
PATRN$REPORT_START_DATE <- format(PATRN$REPORT_START_DATE,"%b-%y")
Monthly <- PATRN %>% filter(EARNING_TYPE == 'M') %>%
  select(POOL_ID, Month = REPORT_START_DATE, DAILY_NET_TCE)
Monthly <- Monthly[complete.cases(Monthly$DAILY_NET_TCE),]

UID <- character()
PERIOD <- character()
DAILY_NET_TCE <- numeric()

for (i in 1:nrow(NDV)) {
  Monthlyx <- Monthly %>% filter(POOL_ID == NDV$POOL_ID[i])
  if (nrow(Monthlyx) > 0) {
  for (j in 1:nrow(Monthlyx)) {
    DNTx <- NDV$Factor[i]*Monthly$DAILY_NET_TCE[j]
    UIDx <- NDV$UID[i]
    Periodx <- Monthlyx$Month[j]
    UID <- c(UID, UIDx)
    PERIOD <- c(PERIOD,Periodx)
    DAILY_NET_TCE <- c(DAILY_NET_TCE, DNTx)
  }}
}

MonthlyDF <- data.frame(UID,PERIOD,DAILY_NET_TCE)
rm(Monthly,Monthlyx)
PNLFinal <- bind_rows(PNLTable %>% select(UID, PERIOD, DAILY_NET_TCE), MonthlyDF) %>% arrange(UID)

PNLFinal <- left_join(PNLFinal, PNLTable)
PNLFinal$VSL_ID[is.na(PNLFinal$VSL_ID)] <- PNLTable$VSL_ID[match(PNLFinal$UID[is.na(PNLFinal$VSL_ID)],PNLTable$UID)]
PNLFinal$Pool[is.na(PNLFinal$Pool)] <- PNLTable$Pool[match(PNLFinal$UID[is.na(PNLFinal$Pool)],PNLTable$UID)]
PNLFinal$VESSEL_NAME[is.na(PNLFinal$VESSEL_NAME)] <- PNLTable$VESSEL_NAME[match(PNLFinal$UID[is.na(PNLFinal$VESSEL_NAME)],PNLTable$UID)]
PNLFinal$POOL_ID[is.na(PNLFinal$POOL_ID)] <- PNLTable$POOL_ID[match(PNLFinal$UID[is.na(PNLFinal$POOL_ID)],PNLTable$UID)]
PNLFinal$DateJoined[is.na(PNLFinal$DateJoined)] <- PNLTable$DateJoined[match(PNLFinal$UID[is.na(PNLFinal$DateJoined)],PNLTable$UID)]
PNLFinal$DateLeft[is.na(PNLFinal$DateLeft)] <- PNLTable$DateLeft[match(PNLFinal$UID[is.na(PNLFinal$DateLeft)],PNLTable$UID)]
PNLFinal$DisplayName[is.na(PNLFinal$DisplayName)] <- PNLTable$DisplayName[match(PNLFinal$UID[is.na(PNLFinal$DisplayName)],PNLTable$UID)]
PNLFinal$PoolParticipant[is.na(PNLFinal$PoolParticipant)] <- PNLTable$PoolParticipant[match(PNLFinal$UID[is.na(PNLFinal$PoolParticipant)],PNLTable$UID)]
PNLFinal$VesselCode <- PVMFD$VSL_IMOS_CODE[match(PNLFinal$UID, PVMFD$UID)]

PNLFinal$VESSEL_NAME <- as.character(PNLFinal$VESSEL_NAME)
PNLFinal$DisplayName <- as.character(PNLFinal$DisplayName)
PNLFinal$VesselCode <- as.character(PNLFinal$VesselCode)

PNLFinal <- PNLFinal[complete.cases(PNLFinal$DateJoined),]

for (i in 1:nrow(PNLFinal)) {
PNLFinal$UID2[i] <- paste(PNLFinal$VesselCode[i], PNLFinal$PERIOD[i])
PNLFinal$UUID[i] <- paste(PNLFinal$VesselCode[i], paste0(ex_between(PNLFinal$PERIOD[i], "[","]"), "/", substr(PNLFinal$PERIOD[i],3,4)))}

for (i in 1:nrow(VFOD)) {
VFOD$UUID[i] <- paste(VFOD$VesselCode[i], VFOD$Period[i], collapse = "_")}

for (i in 1:nrow(PNLFinal)) {
  if (is.na(PNLFinal$DAYS_NET[i])) {

    monthdays = as.numeric(days_in_month(ymd(paste('01-',PNLFinal$PERIOD[i]))))
    if (month(PNLFinal$DateJoined[i]) == month(ymd(paste('01-',PNLFinal$PERIOD[i])))) {
      monthdays = monthdays - as.numeric(day(PNLFinal$DateJoined[i]))
    }
    ohdays = VFOD$OffhireDays[match(PNLFinal$UID2[i], VFOD$UUID)]
    PNLFinal$DAYS_NET[i] <- monthdays - ohdays
  }
  else{
    PNLFinal$DAYS_NET[i] <- PNLFinal$DAYS_NET[i]
  }
}


PBM <- PerformanceBM() %>% unite(UID, VSL_ID, POOL_ID, sep = "_", remove = FALSE)
PBM$VslCode <- PVMFD$VSL_IMOS_CODE[match(PBM$UID, PVMFD$UID)]
PBM$UUID <- paste(PBM$VslCode, PBM$QUARTER_MONTH)
PNLFinal$DAILY_NET_TCE2 <- PBM$QTR_MONTH_NET_TCE[match(PNLFinal$UUID, PBM$UUID)]
PNLFinal$IMO <- V_Dim_Vessel()$imo_no[match(PNLFinal$VesselCode, V_Dim_Vessel()$VesselCode)]
PNLFinal$VESSEL_NAME <- toupper(PNLFinal$VESSEL_NAME)
saveRDS(PNLFinal, "PNLFinal.Rds")
PNLFinal
})
######################################################################################

output$PNLSummary <- renderDT({
  req(PNLFinal())
  SelVel <- PNLFinal() %>% filter(VESSEL_NAME == input$VName)

  SelVel$PERIOD <- rm_bracket(SelVel$PERIOD)
  for (i in 1:nrow(SelVel)) {
    if (grepl('^[0-9]',SelVel$PERIOD[i])) {
      SelVel$YearDate[i] <- as.character(ymd(paste0(SelVel$PERIOD[i],"-01")))
    }
    else{
      SelVel$YearDate[i] <- as.character(dmy(paste0("01-",SelVel$PERIOD[i])))
    }
  }
  SelVel <- SelVel %>% arrange(YearDate) %>% mutate(Year = year(YearDate)) %>%
    group_by(Year) %>% mutate(NetDays = c(DAYS_NET[1], diff(DAYS_NET)))

  for (i in 1:nrow(SelVel)) {
    SelVel$NetDays[i] <- ifelse(grepl('^[0-9]',SelVel$PERIOD[i]), SelVel$NetDays[i], SelVel$DAYS_NET[i])}
  
  SelVel <- SelVel %>% select(VESSEL_NAME, PERIOD, DAYS_NET, NetDays,DAILY_NET_TCE,DAILY_NET_TCE2,YearDate,IMO,VesselCode)
  
  SelVel$DAILY_NET_TCE2 <- ifelse(grepl('^[0-9]', SelVel$PERIOD),SelVel$DAILY_NET_TCE2, SelVel$DAILY_NET_TCE)
  SelVel <- SelVel[complete.cases(SelVel$DAILY_NET_TCE2),]
  SelVel$PERIOD <- ifelse(grepl('-Mar', SelVel$PERIOD),gsub("-Mar", "/Q1", SelVel$PERIOD),
                          ifelse(grepl('-Jun', SelVel$PERIOD), gsub("-Jun", "/Q2", SelVel$PERIOD),
                                 ifelse(grepl('-Sep',SelVel$PERIOD), gsub("-Sep", "/Q3", SelVel$PERIOD),
                                        gsub("-Dec", "/Q4", SelVel$PERIOD))))


  TCVsl <- TCNew %>% filter(Vessel == input$VName) %>%
    select(Vessel, Delivery, AnnivDate = `Anniversary Date`, Rate,`Add Commission`)
  TCVsl$Delivery <- dmy(TCVsl$Delivery)
  TCVsl$AnnivDate <- dmy(TCVsl$AnnivDate)
  TCVsl$Rate <- gsub("\\$","",TCVsl$Rate)
  TCVsl$Rate <- gsub(",","",TCVsl$Rate)
  TCVsl$Rate <- as.numeric(TCVsl$Rate)

  TCVsl$`Add Commission` <- gsub("%","",TCVsl$`Add Commission`)
  TCVsl$`Add Commission` <- as.numeric(TCVsl$`Add Commission`)
  TCVsl$PERIOD <- paste0(year(TCVsl$Delivery),"/Q",quarter(TCVsl$Delivery))
  TCVsl$Duration <- interval(TCVsl$Delivery,TCVsl$AnnivDate) %/% months(1)
  TCVsl$TCin <- round(TCVsl$Rate*(1 - TCVsl$`Add Commission`/100))
  TCVsl$Interval <- interval(TCVsl$Delivery,TCVsl$AnnivDate)
  TCinCost <- sum(TCVsl$TCin, na.rm = T)

  SelVel <- SelVel %>% ungroup() %>% select(VESSEL_NAME,PERIOD, NetDays, TCE = DAILY_NET_TCE2, YearDate)
  SelVel$YearDate <- ymd(SelVel$YearDate)
  
  for (i in 1:nrow(SelVel)) {
    x = match(SelVel$PERIOD[i], TCVsl$PERIOD)
    if (!is.na(x)) {
      OrigPeriod <- SelVel$PERIOD[i]
      SelVel$PERIOD[i] <- paste(SelVel$PERIOD[i], "upto", TCVsl$Delivery[x])
      SelVel$YearDate[i] <- TCVsl$Delivery[x]
      OrigDays <- as.numeric(SelVel$NetDays[i])
      SelVel$NetDays[i] <- as.numeric(day(TCVsl$Delivery[x]))
      NewRow <- data.frame(SelVel$VESSEL_NAME[x], paste(OrigPeriod, "from", TCVsl$Delivery[x] + 1),OrigDays - SelVel$NetDays[i],
                           SelVel$TCE[i], TCVsl$Delivery[x] + 1)
      names(NewRow) <- c("VESSEL_NAME", "PERIOD", "NetDays", "TCE","YearDate")
      SelVel <- bind_rows(SelVel, NewRow)
    }
  }


  SelVel <- SelVel %>% arrange(YearDate)

  for (i in 1:nrow(SelVel)) {
    x <- which(SelVel$YearDate[i] %within% TCVsl$Interval)
    if (length(x) < 1) SelVel$TCin[i] <- NA
    else SelVel$TCin[i] <- TCVsl$TCin[x]
  }
  SelVel$TCE <- round(SelVel$TCE)
  SelVel$PLDay <- SelVel$TCE - SelVel$TCin
  SelVel$PLVoyage <- SelVel$NetDays*SelVel$PLDay
  SelVel$NetDays <- round(SelVel$NetDays)
  SelVel$PLVoyage <- round(SelVel$PLVoyage)

  valuesPL$SV <- SelVel
  valuesPL$TCV <- TCVsl
  datatable(SelVel, rownames = FALSE,
            options = list(scrollX = TRUE, columnDefs = list(list(visible = FALSE, targets = c(0,4)))))
})


#########################################################################################
#Scenario Analysis

output$Scenario <- renderPlot({
  TradedDays <- sum(valuesPL$SV$NetDays, na.rm = TRUE)
  AvgTCE <- sum(valuesPL$SV$NetDays*valuesPL$SV$TCE, na.rm = TRUE)/TradedDays
  AvgTCin <- mean(valuesPL$TCV$Rate, na.rm = TRUE)

  #Calculate Total Days
  TDays <- valuesPL$TCV %>% select(Delivery, AnnivDate) %>% filter(Delivery < Sys.Date())
  TDays$AnnivDate[TDays$AnnivDate >= Sys.Date()] <- Sys.Date()
  TotalDays <- sum(as.numeric(difftime(TDays$AnnivDate,TDays$Delivery)))

  #Base Calculation
  Base <- input$baseVal
  Good <- input$baseVal + input$goodSc
  Bad <- input$baseVal - input$badSc
  FinalTCEBase <- (AvgTCE*(TradedDays/TotalDays)) + (Base*(1 - (TradedDays/TotalDays)))
  FinalTCEGood <- (AvgTCE*(TradedDays/TotalDays)) + (Good*(1 - (TradedDays/TotalDays)))
  FinalTCEBad <- (AvgTCE*(TradedDays/TotalDays)) + (Bad*(1 - (TradedDays/TotalDays)))

  DailyPLBase <- FinalTCEBase - AvgTCin
  DailyPLGood <- FinalTCEGood - AvgTCin
  DailyPLBad <- FinalTCEBad - AvgTCin

  TotalPLBase <- DailyPLBase*TotalDays
  TotalPLGood <- DailyPLGood*TotalDays
  TotalPLBad <- DailyPLBad*TotalDays

  df <- data.frame(Scenario = c("Good","Base","Bad"),
                   Total = c(TotalPLGood, TotalPLBase, TotalPLBad))
  ggplot(df, aes(x = Scenario, y = Total, fill = Scenario)) + geom_bar(stat = 'identity') +
    scale_y_continuous(labels = scales::comma) + geom_hline(yintercept = AvgTCin)
})




####################
#Bizlem Data TimeCharterReports
#Reactive values for storing information
TCRVal <- reactiveValues()


#Output as dataframe
#https://stackoverflow.com/questions/22650737/empty-data-message-in-rendertable

output$TCR <- renderUI({
  TCR <- req(BizTCR())
  TCRVal$TCR <- TCR
  if (is.null(TCRVal$TCR))
    return("No Data To Show")
  
  DTOutput('TCRReport')
  
})
output$TCRReport <- renderDT({
  req(TCRVal$TCR)
  datatable(TCRVal$TCR, extensions = 'Buttons', rownames = FALSE, 
            options = list(deferRender = TRUE,scroller = FALSE,scrollX = TRUE,#colReorder = TRUE, filter = 'top',
                           dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print',I('colvis')),
                           pageLength = 10,lengthMenu = list(c(10,20, 50,-1), list('10', '20', '50','All')), paging = T))
  
}) 
  
  
  ####################
  #Bizlem Data BrokerTcRate
  #Reactive values for storing information
  BTCRVal <- reactiveValues()
  
  
  output$BTCR <- renderUI({
    BTCR <- req(BizBTCR())
    BTCRVal$BTCR <- BTCR
    if (is.null(BTCRVal$BTCR))
      return("No Data To Show")
    
    DTOutput('BTCRReport')
    
  })
  output$BTCRReport <- renderDT({
    req(BTCRVal$BTCR)
    datatable(BTCRVal$BTCR, extensions = 'Buttons', rownames = FALSE,
              options = list(deferRender = TRUE,scroller = FALSE,scrollX = TRUE, #colReorder = TRUE, filter = 'top',
                             dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print',I('colvis')),
                             pageLength = 10,lengthMenu = list(c(10,20, 50,-1), list('10', '20', '50','All')), paging = T))
  })
