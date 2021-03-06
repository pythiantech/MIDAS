##########################################################################################

valuesPL <- reactiveValues()
PLData <- reactive({
  PL <- req(input$voydaysPL)
  posData <- read_csv('data/PositionList.csv')
  posData$Flag <- as.logical(posData$Flag)
  posData$Comments <- as.character(posData$Comments)
  posData$OpenPortDate <- as.character(posData$OpenPortDate)
  posData <- select(posData, Flag, VesselName, VoyageNo, EmploymentStatus, OpenPort, OpenPortDate,
                    Cargo, RepositionPort, RepositionDate, CurrentStatus, Comments,
                    Flag,DWT, Type, IceClass, CompleteGMT, UpdatedBy,ROWIDT, everything())
 
  posData <- posData %>% filter(CompleteGMT <= (Sys.time() + PL * 24 * 60 * 60))
  posData$CompleteGMT <- as.character(posData$CompleteGMT)
  posData$CommenceGMT <- as.character(posData$CommenceGMT)
  posData

})


##############################################################################
observeEvent(PLData(),{
  data <- PLData()
  valuesPL[["PositionList"]] <- data
})

output$cstatus <- renderUI({
  pickerInput('cstatus',"Filter by current status",choices = levels(as.factor(valuesPL[["PositionList"]]$CurrentStatus)),
              selected = levels(as.factor(valuesPL[["PositionList"]]$CurrentStatus)), multiple = TRUE, options = list(`actions-box` = TRUE))
})
output$vname <- renderUI({
  pickerInput('vname',"Filter by vessel name",choices = levels(as.factor(valuesPL[["PositionList"]]$VesselName)),
              selected = levels(as.factor(valuesPL[["PositionList"]]$VesselName)), multiple = TRUE, options = list(`actions-box` = TRUE))
})
output$vtypePL <- renderUI({
  pickerInput('vtypePL',"Filter by vessel type",choices = levels(as.factor(valuesPL[["PositionList"]]$Type)),
              selected = levels(as.factor(valuesPL[["PositionList"]]$Type)), multiple = TRUE, options = list(`actions-box` = TRUE,`live-search` = TRUE))
})

output$PositionListData <- renderRHandsontable({
  PositionList <- valuesPL[["PositionList"]] %>%
    filter(CurrentStatus %in% input$cstatus) %>%
    filter(Type %in% input$vtypePL) %>%
    filter(VesselName %in% input$vname)
  if (!is.null(PositionList))
    rhandsontable(PositionList,width = '100%', height = 800,rowHeaders = NULL,
                  selectCallback = TRUE, readOnly = FALSE,
                  fillHandle = list(direction = 'vertical', autoInsertRow = FALSE)) %>%
    hot_context_menu(allowRowEdit = FALSE) %>%
    hot_col(col = "Flag", type = "checkbox") %>%
    hot_col(col = "VesselName", type = NULL, readOnly = TRUE) %>%
    hot_col(col = "VoyageNo", type = NULL, readOnly = TRUE, format = "0") %>%
    hot_col(col = "EmploymentStatus", type = "dropdown",source = c("Subjects","Hold","Fixed","Unfixed")) %>%
    hot_col(col = "OpenPort", type = NULL, readOnly = TRUE) %>%
    hot_col(col = "OpenPortDate", type = NULL, readOnly = TRUE) %>%
    hot_col(col = "Cargo", type = NULL, readOnly = TRUE, colWidths = 150) %>%
    hot_col(col = "RepositionPort", type = "dropdown", source = V_Dim_Ports()$PortName) %>%
    hot_col(col = "RepositionDate", type = "date") %>%
    hot_col(col = "CurrentStatus", type = NULL, readOnly = TRUE) %>%
    hot_col(col = "Comments", type = NULL) %>%
    hot_col(col = "DWT", type = NULL, readOnly = TRUE, format = "0") %>%
    hot_col(col = "Type", type = NULL, readOnly = TRUE) %>%
    hot_col(col = "IceClass", type = NULL, readOnly = TRUE) %>%
    hot_col(col = "CompleteGMT", type = NULL, readOnly = TRUE) %>%
    hot_col(col = "UpdatedBy", type = NULL, readOnly = TRUE) %>%
    hot_col(col = "ROWIDT", type = "numeric",readOnly = TRUE, format = "0", colWidths = 0.1) %>%
    hot_col(col = "LastCargoGrade", type = NULL, readOnly = TRUE, colWidths = 0.1) %>% 
    hot_col(col = "CommenceGMT", type = NULL, readOnly = TRUE, colWidths = 0.1) %>% 
    hot_col(col = "Fkey_Dim_Vessel_Id", type = NULL, readOnly = TRUE, colWidths = 0.1) %>% 
    hot_col(col = "Fkey_Dim_Voyage_Id", type = NULL, readOnly = TRUE, colWidths = 0.1) %>% 
    hot_col(col = "Estimate", type = NULL, readOnly = TRUE, colWidths = 0.1) %>% 
    hot_col(col = "UID", type = NULL, readOnly = TRUE, colWidths = 0.1) %>% 
    # hot_cols(colWidths = c(50,100,100,120,150,150,300,120,120,120,150,100,80,80,150,150,  rep(0.1,7))) %>%
    # hot_rows(rowHeights = 30) %>%
    hot_cols(columnSorting = TRUE)


})

edI <- reactiveValues(editedInfoPL = NA)
observeEvent(input$PositionListData$changes$changes, {
  df <- hot_to_r(input$PositionListData)
  
  info <- input$PositionListData$changes$changes
  rowid <- info[[1]][[1]] <- info[[1]][[1]] + 1
  colid <- info[[1]][[2]] <- info[[1]][[2]] + 1
  oldvalue <- info[[1]][[3]]
  newvalue <- info[[1]][[4]]
  username <- user()
  # ROWIDT <- as.integer(valuesPL[["PositionList"]][rowid,17])
  ROWIDT <- as.integer(df[rowid,17])
  
  info <- sapply(info, function(x) ifelse(x == "NULL", NA, x))

  if (all(is.na(edI$editedInfoPL))) {
    edI$editedInfoPL <- data.frame(c(info, username, ROWIDT), stringsAsFactors = FALSE)
    colnames(edI$editedInfoPL) <- c("rowid", "colid", "oldvalue", "newvalue", "username","ROWIDT")
  } else {
    df <- data.frame(c(info,username, ROWIDT), stringsAsFactors = FALSE)
    colnames(df) <- c("rowid", "colid", "oldvalue", "newvalue","username","ROWIDT")
    edI$editedInfoPL <- rbind(edI$editedInfoPL, df)
  }
  colnames(edI$editedInfoPL) <- c("rowid", "colid", "oldvalue", "newvalue","username","ROWIDT")

  print(edI$editedInfoPL)

})

observeEvent(input$savePLData, {
  req(edI$editedInfoPL)
  editedValue <- edI$editedInfoPL %>% 
    group_by(ROWIDT, colid) %>%
    filter(newvalue == dplyr::last(newvalue) | is.na(newvalue)) %>%
    ungroup()
  print(editedValue)
  PL <- read_csv('data/PositionList.csv')
  PL$Flag <- as.logical(PL$Flag)
  PL$Comments <- as.character(PL$Comments)
  PL$OpenPortDate <- as.character(PL$OpenPortDate)
  PL$CompleteGMT <- as.character(PL$CompleteGMT)
  PL$CommenceGMT <- as.character(PL$CommenceGMT)
  PL <- select(PL, Flag, VesselName, VoyageNo, EmploymentStatus, OpenPort, OpenPortDate,
                    Cargo, RepositionPort, RepositionDate, CurrentStatus, Comments,
                    Flag,DWT, Type, IceClass, CompleteGMT, UpdatedBy,ROWIDT, everything())
  
  for (i in 1:nrow(editedValue)) {
    if (editedValue$colid[i] != 9) PL[editedValue$ROWIDT[i],editedValue$colid[i]] <- as.character(editedValue$newvalue[i])
    else PL[editedValue$ROWIDT[i],editedValue$colid[i]] <- mdy(editedValue$newvalue[i])
    PL$UpdatedBy[editedValue$ROWIDT[i]] <- editedValue$username[i]
  }
  write_csv(PL, 'data/PositionList.csv')
  
  PL <- read_csv('data/PositionList.csv')
  PL$Flag <- as.logical(PL$Flag)
  PL$Comments <- as.character(PL$Comments)
  PL$OpenPortDate <- as.character(PL$OpenPortDate)
  
  PL$CommenceGMT <- as.character(PL$CommenceGMT)
  PL <- select(PL, Flag, VesselName, VoyageNo, EmploymentStatus, OpenPort, OpenPortDate,
                    Cargo, RepositionPort, RepositionDate, CurrentStatus, Comments,
                    Flag,DWT, Type, IceClass, CompleteGMT, UpdatedBy,ROWIDT, everything())
  PL <- PL %>% filter(CompleteGMT <= (Sys.time() + voydaysPL() * 24 * 60 * 60))
  PL$CompleteGMT <- as.character(PL$CompleteGMT)
  valuesPL[["PositionList"]] <- PL
  
  #Update PosListData()
  
  PLDv <- valPLD$rv
  UIDChange <- PL$UID[PL$ROWIDT %in% editedValue$ROWIDT]
 # print(UIDChange)
  PLDv$RepositionPort[PLDv$UID %in% UIDChange] <- PL$RepositionPort[PL$UID %in% UIDChange]
  PLDv$EmploymentStatus[PLDv$UID %in% UIDChange] <- PL$EmploymentStatus[PL$UID %in% UIDChange]
  valPLD$rv <- PLDv
  # print(valPLD$rv$EmploymentStatus[1])
  # saveRDS(valPLD$rv, 'data/PLD.Rds')
  edI$editedInfoPL <- NA
  shinyalert("Success!", "The data has been saved", type = "success")
})

observeEvent(input$Refresh, {
  #Refresh Position List
  
  PL <- req(input$voydaysPL)
  posData <- read_csv('data/PositionList.csv')
  posData$Flag <- as.logical(posData$Flag)
  posData$Comments <- as.character(posData$Comments)
  posData$OpenPortDate <- as.character(posData$OpenPortDate)
  posData <- select(posData, Flag, VesselName, VoyageNo, EmploymentStatus, OpenPort, OpenPortDate,
                    Cargo, RepositionPort, RepositionDate, CurrentStatus, Comments,
                    Flag,DWT, Type, IceClass, CompleteGMT, UpdatedBy,ROWIDT, everything())
  
  posData <- posData %>% filter(CompleteGMT <= (Sys.time() + PL * 24 * 60 * 60))
  posData$CompleteGMT <- as.character(posData$CompleteGMT)
  posData$CommenceGMT <- as.character(posData$CommenceGMT)
  valuesPL[["PositionList"]] <- posData
  
  #Refresh main table
  PLD <- readRDS('data/PLD.Rds')
  valPLD$rv <- PLD
})

