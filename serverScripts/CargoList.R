##########################################################################################
observeEvent(input$showFiltersCL, {
  shinyjs::hide(id = "hiddenCL")
  shinyjs::toggle(id = "visibleCL")
  shinyjs::toggle(id = "filtersCL")
})

observeEvent(input$hideFiltersCL, {
  shinyjs::hide(id = "visibleCL")
  shinyjs::show(id = "hiddenCL")
  shinyjs::hide(id = "filtersCL")
})


valuesCL <- reactiveValues()
CLData <- reactive({
  req(input$InformationCL)
  req(input$StatusCL)
  CCL <- input$ChartCL
  BCL <- input$BrokerCL
  OCL <- input$OperatorCL
  req(input$RegionCL)
  req(input$CargoTypeCL)
  CL <- readRDS('data/CargoList.Rds')
  CL <- CL %>% filter(ReportDate >= Sys.Date() - config$validTime)
  
  saveRDS(CL, 'data/CargoList.Rds')
  CL <- readRDS('data/CargoList.Rds')
  CL$VslType <- wvd()$`Vessel Type`[match(CL$Vessel, wvd()$Name)]
  CL$Operator <- wvd()$`Commercial Operator`[match(CL$Vessel, wvd()$Name)]
  CL$Select <- as.logical(CL$Select)
  CL$Region <- toupper(CL$Region)
  
  saveRDS(CL, 'data/CargoList.Rds')
  CL <- readRDS('data/CargoList.Rds')
  CL$VslType <- wvd()$`Vessel Type`[match(CL$Vessel, wvd()$Name)]
  CL$Operator <- wvd()$`Commercial Operator`[match(CL$Vessel, wvd()$Name)]
  CL$Select <- as.logical(CL$Select)
  CL$Region <- toupper(CL$Region)
  
  
  #Filtering starts
  CL$Region <- toupper(CL$Region)
  CL <- CL %>% dplyr::filter(Information %in% c(input$InformationCL,""," ",NA)) %>%
    dplyr::filter(Region %in% c(input$RegionCL, ""," ",NA)) %>%
    dplyr::filter(CargoType %in% c(input$CargoTypeCL,""," ",NA)) %>%
    dplyr::filter(Status %in% c(input$StatusCL,""," ",NA)) %>%
    dplyr::filter(ReportDate >= input$RepDateCL[1] & ReportDate <= input$RepDateCL[2] | is.na(ReportDate))
  
  #Free Text filter
  if (CCL != '' & BCL == '' & OCL == '')
    CL %>%  dplyr::filter(Reduce(`&`, lapply(strsplit(CCL,' ')[[1]], grepl, Charterer,ignore.case = T)))
  else if (CCL != '' & BCL != '' & OCL == '')
    CL %>%  dplyr::filter(Reduce(`&`, lapply(strsplit(CCL,' ')[[1]], grepl, Charterer,ignore.case = T))) %>%
    dplyr::filter(Reduce(`&`, lapply(strsplit(BCL,' ')[[1]], grepl, Broker,ignore.case = T)))
  else if (CCL != '' & BCL != '' & OCL != '')
    CL %>%  dplyr::filter(Reduce(`&`, lapply(strsplit(CCL,' ')[[1]], grepl, Charterer,ignore.case = T))) %>%
    dplyr::filter(Reduce(`&`, lapply(strsplit(BCL,' ')[[1]], grepl, Broker,ignore.case = T))) %>%
    dplyr::filter(Reduce(`&`, lapply(strsplit(OCL,' ')[[1]], grepl, Operator,ignore.case = T)))
  else if (BCL != '' & CCL == '' & OCL == '')
    CL %>%  dplyr::filter(Reduce(`&`, lapply(strsplit(BCL,' ')[[1]], grepl, Broker,ignore.case = T)))
  else if (OCL != '' & CCL == '' & BCL == '')
    CL %>%  dplyr::filter(Reduce(`&`, lapply(strsplit(OCL,' ')[[1]], grepl, Operator,ignore.case = T)))
  else if (CCL != '' & OCL != '' & BCL == '')
    CL %>%  dplyr::filter(Reduce(`&`, lapply(strsplit(CCL,' ')[[1]], grepl, Charterer,ignore.case = T))) %>%
    dplyr::filter(Reduce(`&`, lapply(strsplit(OCL,' ')[[1]], grepl, Operator,ignore.case = T)))
  else if (OCL != '' & BCL != '' & CCL == '')
    CL %>%  dplyr::filter(Reduce(`&`, lapply(strsplit(OCL,' ')[[1]], grepl, Operator,ignore.case = T))) %>%
    dplyr::filter(Reduce(`&`, lapply(strsplit(BCL,' ')[[1]], grepl, Broker,ignore.case = T)))
  else CL
  CL
  
})

observeEvent(CLData(),{
  data <- CLData()
  valuesCL[["CargoList"]] <- data
  # print(valuesCL$CargoList)
})


output$CargoListData2 <- output$CargoListData <- renderRHandsontable({
  CargoList <- valuesCL[["CargoList"]]
  if (!is.null(CargoList)) {
    hot <- rhandsontable(CargoList,width = '100%', height = 400, rowHeaders = NULL,
                         selectCallback = TRUE, readOnly = FALSE,stretchH = "all") %>%
      hot_context_menu(allowRowEdit = FALSE) %>%
      hot_col(col = "Updatedby", type = NULL, readOnly  = TRUE) %>%
      hot_col(col = "Region", type = "dropdown", source = c('NW EUROPE', 'MED', 'BSEA', 'WAF', 'USG/CARIBS', 'USAC', 
                                                            'USWC', 'LATIN EAST', 'LATIN WEST', 'MIDDLE EAST', 'RED SEA/AFRICA', 'IOR', 
                                                            'SOUTH EA', 'FAR EAST', 'OZ/NZ/PACIFIC')) %>%
      hot_col(col = "CargoType", type = "dropdown", source = c("Clean","Dirty","Chemicals")) %>% 
      hot_col(col = "Information", type = "dropdown", source = c("Market","Private")) %>%     
      hot_col(col = "ReportDate", type = "date") %>%
      hot_col(col = "Charterer", type = NULL) %>%
      hot_col(col = "Qty", type = "numeric") %>%
      hot_col(col = "Grade", type = NULL) %>%
      hot_col(col = "Laycan", type = NULL) %>%
      hot_col(col = "LoadPort", type = "dropdown", source = V_Dim_Ports()$PortName) %>%
      hot_col(col = "DischPort", type = "dropdown", source = V_Dim_Ports()$PortName) %>%
      hot_col(col = "Comments", type = NULL) %>%
      hot_col(col = "Status", type = "dropdown", source = c("Fixed","Unfixed","Hold",
                                                            "Subs","Others","Died", "Looking","Open","WDWF")) %>%
      hot_col(col = "Vessel", type = "dropdown", source = wvd()$Name) %>%
      hot_col(col = "RateType", type = "dropdown", source = c("WS","Lumpsum","Frt Rate")) %>%
      hot_col(col = "Rate", type = NULL) %>%
      hot_col(col = "Broker", type = NULL) %>%
      hot_col(col = "VslType", type = NULL) %>%
      hot_col(col = "Operator", type = NULL, readOnly = TRUE) %>%
      
      hot_col(col = "Select", type = "checkbox") %>% 
      hot_col(col = "timestamp", type = "numeric",readOnly = TRUE) %>%
      hot_col(col = "ROWIDT", type = "numeric",readOnly = TRUE) %>%
      
      hot_cols(colWidths = c(50,100,100,100,100,75,75,80,70,100,100,70,90,80,70,100,100,100,80,100,0.1,0.1)) #%>%
      # hot_rows(rowHeights = 30)
    
    hot}
})


edI <- reactiveValues(editedInfoCL = NA)

observeEvent(input$CargoListData$changes$changes, {
  df <- hot_to_r(input$CargoListData)
  info <- input$CargoListData$changes$changes
  rowid <- info[[1]][[1]] <- info[[1]][[1]] + 1
  colid <- info[[1]][[2]] <- info[[1]][[2]] + 1
  oldvalue <- info[[1]][[3]]
  newvalue <- info[[1]][[4]]
  oldtimestamp <- valuesCL[["CargoList"]][rowid,21]
  newtimestamp <- epochTime()
  username <- user()
  # ROWIDT <- as.integer(valuesCL[["CargoList"]][rowid,22])
  ROWIDT <- as.integer(df[rowid,22])
  info <- sapply(info, function(x) ifelse(x == "NULL", NA, x))
  rowsold <- nrow(readRDS('data/CargoList.Rds'))
  if (all(is.na(edI$editedInfoCL))) {
    edI$editedInfoCL <- data.frame(c(info,oldtimestamp, newtimestamp, username, ROWIDT, rowsold), stringsAsFactors = FALSE)
    
    colnames(edI$editedInfoCL) <- c("rowid", "colid", "oldvalue", "newvalue","oldtimestamp","newtimestamp", "username","ROWIDT","rowsold")
  } else {
    df <- data.frame(c(info,oldtimestamp, newtimestamp, username, ROWIDT, rowsold), stringsAsFactors = FALSE)
    colnames(df) <- c("rowid", "colid", "oldvalue", "newvalue","oldtimestamp","newtimestamp", "username","ROWIDT", "rowsold")
    if (ncol(edI$editedInfoCL) == ncol(df)) edI$editedInfoCL <- rbind(edI$editedInfoCL, df)
    
  }
  colnames(edI$editedInfoCL) <- c("rowid", "colid", "oldvalue", "newvalue","oldtimestamp","newtimestamp", "username","ROWIDT", "rowsold")
  
  
  edI$editedInfoCL$oldtimestamp[is.na(edI$editedInfoCL$oldtimestamp)] <- 0
  
  rowids <- vector("numeric")
  for (i in 1:nrow(edI$editedInfoCL)) {
    rowids <- c(rowids, edI$editedInfoCL[i,1])
    if (is.na(edI$editedInfoCL[i,4])) edI$editedInfoCL[i,4] <- edI$editedInfoCL[i,3]
    if (nchar(edI$editedInfoCL[i,6]) < 5) edI$editedInfoCL[i,6] <- edI$editedInfoCL[i,5]
    
    
    if (is.na(edI$editedInfoCL[i,8])) {
      if (i < 2) {
        edI$editedInfoCL[i,8] <- rowsold + 1
        
      }
      else{
        if (edI$editedInfoCL[i,1] %in% rowids[1:(i - 1)]) {
          indices <- match(edI$editedInfoCL[i,1], rowids[1:(i - 1)])
          edI$editedInfoCL[i,8] <- edI$editedInfoCL[indices,8]
        }
        else {
          edI$editedInfoCL[i,8] <- edI$editedInfoCL[i - 1,8] + 1 
          
        }}
    }
    
    
  }
  print(edI$editedInfoCL)
})

############################################################
observeEvent(input$CargoListData2$changes$changes, {
  df <- hot_to_r(input$CargoListData2)
  info <- input$CargoListData2$changes$changes
  rowid <- info[[1]][[1]] <- info[[1]][[1]] + 1
  colid <- info[[1]][[2]] <- info[[1]][[2]] + 1
  oldvalue <- info[[1]][[3]]
  newvalue <- info[[1]][[4]]
  oldtimestamp <- valuesCL[["CargoList"]][rowid,21]
  newtimestamp <- epochTime()
  username <- user()
  # ROWIDT <- as.integer(valuesCL[["CargoList"]][rowid,22])
  ROOWIDT <- aas.integer(df[rowid,22])
  info <- sapply(info, function(x) ifelse(x == "NULL", NA, x))
  rowsold <- nrow(readRDS('data/CargoList.Rds'))
  if (all(is.na(edI$editedInfoCL))) {
    edI$editedInfoCL <- data.frame(c(info,oldtimestamp, newtimestamp, username, ROWIDT, rowsold), stringsAsFactors = FALSE)
    
    colnames(edI$editedInfoCL) <- c("rowid", "colid", "oldvalue", "newvalue","oldtimestamp","newtimestamp", "username","ROWIDT","rowsold")
  } else {
    df <- data.frame(c(info,oldtimestamp, newtimestamp, username, ROWIDT, rowsold), stringsAsFactors = FALSE)
    colnames(df) <- c("rowid", "colid", "oldvalue", "newvalue","oldtimestamp","newtimestamp", "username","ROWIDT", "rowsold")
    if (ncol(edI$editedInfoCL) == ncol(df)) edI$editedInfoCL <- rbind(edI$editedInfoCL, df)
    
  }
  colnames(edI$editedInfoCL) <- c("rowid", "colid", "oldvalue", "newvalue","oldtimestamp","newtimestamp", "username","ROWIDT", "rowsold")
  
  
  edI$editedInfoCL$oldtimestamp[is.na(edI$editedInfoCL$oldtimestamp)] <- 0
  
  rowids <- vector("numeric")
  for (i in 1:nrow(edI$editedInfoCL)) {
    rowids <- c(rowids, edI$editedInfoCL[i,1])
    if (is.na(edI$editedInfoCL[i,4])) edI$editedInfoCL[i,4] <- edI$editedInfoCL[i,3]
    if (nchar(edI$editedInfoCL[i,6]) < 5) edI$editedInfoCL[i,6] <- edI$editedInfoCL[i,5]
    
    
    if (is.na(edI$editedInfoCL[i,8])) {
      if (i < 2) {
        edI$editedInfoCL[i,8] <- rowsold + 1
        
      }
      else{
        if (edI$editedInfoCL[i,1] %in% rowids[1:(i - 1)]) {
          indices <- match(edI$editedInfoCL[i,1], rowids[1:(i - 1)])
          edI$editedInfoCL[i,8] <- edI$editedInfoCL[indices,8]
        }
        else {
          edI$editedInfoCL[i,8] <- edI$editedInfoCL[i - 1,8] + 1 
          
        }}
    }
    
    
  }
  print(edI$editedInfoCL)
})

#############################################################
observeEvent(input$addRow, {
  req(input$CargoListData)
  df <- hot_to_r(input$CargoListData)
  df <- df %>% add_row()
  valuesCL[["CargoList"]] <- df
})

#############################################################
observeEvent(input$addRow2, {
  req(input$CargoListData2)
  df <- hot_to_r(input$CargoListData2)
  df <- df %>% add_row()
  valuesCL[["CargoList"]] <- df
})

#Save data
observeEvent(input$saveCLData, {
  req(edI$editedInfoCL)
  ####################
  #New Logic
  
  editedValue <- edI$editedInfoCL %>% 
    group_by(ROWIDT, colid) %>%
    filter(newvalue == dplyr::last(newvalue) | is.na(newvalue)) %>%
    ungroup()
  
  CL <- readRDS('data/CargoList.Rds')
  CL$VslType <- wvd()$`Vessel Type`[match(CL$Vessel, wvd()$Name)]
  CL$Operator <- wvd()$`Commercial Operator`[match(CL$Vessel, wvd()$Name)]
  CL$Select <- as.logical(CL$Select)
  CL$Region <- toupper(CL$Region)
  rowsnew <- nrow(readRDS('data/CargoList.Rds'))
  
  for (i in 1:nrow(editedValue)) {
    if (editedValue$oldtimestamp[i] == 0) {
      rowdiff <- rowsnew - editedValue$rowsold[i] #Now there could be lesser rows too!
      
      editedValue$ROWIDT[i] <- editedValue$ROWIDT[i] + rowdiff
      
    }
    else {editedValue$ROWIDT[i] <- editedValue$ROWIDT[i]}
    
    rowindex <- which(CL$ROWIDT == editedValue$ROWIDT[i])
    # print(rowindex)
    if (editedValue$colid[i] != 18) CL[rowindex,editedValue$colid[i]] <- as.character(editedValue$newvalue[i])
    else CL[rowindex, editedValue$colid[i]] <- mdy(editedValue$newvalue[i])
    
    CL$ROWIDT[rowindex] <- editedValue$ROWIDT[i]
    CL$timestamp[rowindex] <- editedValue$newtimestamp[i]
    CL$Updatedby[rowindex] <- editedValue$username[i]
  }
  
  saveRDS(CL, 'data/CargoList.Rds')
  
  ##################################
  CL <- readRDS('data/CargoList.Rds')
  CL$VslType <- wvd()$`Vessel Type`[match(CL$Vessel, wvd()$Name)]
  CL$Operator <- wvd()$`Commercial Operator`[match(CL$Vessel, wvd()$Name)]
  CL$Select <- as.logical(CL$Select)
  CL$Region <- toupper(CL$Region)
  # updatePickerInput(session, 'InformationCL',selected = c("Market", "Private"))
  # updatePickerInput(session, 'RegionCL',selected =  c('NW EUROPE', 'MED', 'BSEA', 'WAF', 'USG/CARIBS', 'USAC', 
  #                                                     'USWC', 'LATIN EAST', 'LATIN WEST', 'MIDDLE EAST', 'RED SEA/AFRICA', 'IOR', 
  #                                                     'SOUTH EA', 'FAR EAST', 'OZ/NZ/PACIFIC'))
  # updatePickerInput(session, 'CargoTypeCL', selected = c("Clean","Dirty","Chemicals"))
  # updatePickerInput(session, 'StatusCL', selected = c("Fixed","Unfixed","Hold",
  #                                                     "Subs","Others","Died", "Looking","Open","WDWF"))
  # updateDateRangeInput(session, 'RepDateCL',start = (Sys.Date() - 30), end=Sys.Date() + 30)
  
  CL <- CL %>% dplyr::filter(Information %in% c(input$InformationCL,""," ",NA)) %>%
    dplyr::filter(Region %in% c(input$RegionCL, ""," ",NA)) %>% 
    dplyr::filter(CargoType %in% c(input$CargoTypeCL,""," ",NA)) %>% 
    dplyr::filter(Status %in% c(input$StatusCL,""," ",NA)) %>%
    dplyr::filter(ReportDate >= input$RepDateCL[1] & ReportDate <= input$RepDateCL[2] | is.na(ReportDate))
  valuesCL[["CargoList"]] <- CL
  edI$editedInfoCL <- NA
  shinyalert("Success!", "The data has been saved", type = "success")
})


###################################################
observeEvent(input$saveCLData2, {
  req(edI$editedInfoCL)
  ####################
  #New Logic
  
  editedValue <- edI$editedInfoCL %>% 
    group_by(ROWIDT, colid) %>%
    filter(newvalue == dplyr::last(newvalue) | is.na(newvalue)) %>%
    ungroup()
  
  CL <- readRDS('data/CargoList.Rds')
  CL$VslType <- wvd()$`Vessel Type`[match(CL$Vessel, wvd()$Name)]
  CL$Operator <- wvd()$`Commercial Operator`[match(CL$Vessel, wvd()$Name)]
  CL$Select <- as.logical(CL$Select)
  CL$Region <- toupper(CL$Region)
  rowsnew <- nrow(readRDS('data/CargoList.Rds'))
  
  for (i in 1:nrow(editedValue)) {
    if (editedValue$oldtimestamp[i] == 0) {
      rowdiff <- rowsnew - editedValue$rowsold[i] #Now there could be lesser rows too!
      
      editedValue$ROWIDT[i] <- editedValue$ROWIDT[i] + rowdiff
      
    }
    else {editedValue$ROWIDT[i] <- editedValue$ROWIDT[i]}
    
    rowindex <- which(CL$ROWIDT == editedValue$ROWIDT[i])
    # print(rowindex)
    if (editedValue$colid[i] != 18) CL[rowindex,editedValue$colid[i]] <- as.character(editedValue$newvalue[i])
    else CL[rowindex, editedValue$colid[i]] <- mdy(editedValue$newvalue[i])
    
    CL$ROWIDT[rowindex] <- editedValue$ROWIDT[i]
    CL$timestamp[rowindex] <- editedValue$newtimestamp[i]
    CL$Updatedby[rowindex] <- editedValue$username[i]
  }
  
  saveRDS(CL, 'data/CargoList.Rds')
  
  ##################################
  CL <- readRDS('data/CargoList.Rds')
  CL$VslType <- wvd()$`Vessel Type`[match(CL$Vessel, wvd()$Name)]
  CL$Operator <- wvd()$`Commercial Operator`[match(CL$Vessel, wvd()$Name)]
  CL$Select <- as.logical(CL$Select)
  CL$Region <- toupper(CL$Region)
  # updatePickerInput(session, 'InformationCL',selected = c("Market", "Private"))
  # updatePickerInput(session, 'RegionCL',selected =  c('NW EUROPE', 'MED', 'BSEA', 'WAF', 'USG/CARIBS', 'USAC', 
  #                                                     'USWC', 'LATIN EAST', 'LATIN WEST', 'MIDDLE EAST', 'RED SEA/AFRICA', 'IOR', 
  #                                                     'SOUTH EA', 'FAR EAST', 'OZ/NZ/PACIFIC'))
  # updatePickerInput(session, 'CargoTypeCL', selected = c("Clean","Dirty","Chemicals"))
  # updatePickerInput(session, 'StatusCL', selected = c("Fixed","Unfixed","Hold",
  #                                                     "Subs","Others","Died", "Looking","Open","WDWF"))
  # updateDateRangeInput(session, 'RepDateCL',start = (Sys.Date() - 30), end=Sys.Date() + 30)
  
  CL <- CL %>% dplyr::filter(Information %in% c(input$InformationCL,""," ",NA)) %>%
    dplyr::filter(Region %in% c(input$RegionCL, ""," ",NA)) %>% 
    dplyr::filter(CargoType %in% c(input$CargoTypeCL,""," ",NA)) %>% 
    dplyr::filter(Status %in% c(input$StatusCL,""," ",NA)) %>%
    dplyr::filter(ReportDate >= input$RepDateCL[1] & ReportDate <= input$RepDateCL[2] | is.na(ReportDate))
  valuesCL[["CargoList"]] <- CL
  edI$editedInfoCL <- NA
  shinyalert("Success!", "The data has been saved", type = "success")
})
###############################################
#Render Scorpio Cargo List
output$ScoFixList2 <- output$ScoFixList <- renderDT({
  SFL <- readRDS('data/ScorpioFixtureList.Rds') 
  SFL <- SFL %>% filter(ReportDate >= Sys.Date() - config$validTime)
  SFL <- SFL[!duplicated(SFL),]
  
  valuesCL$SFL <- SFL
  SFL <- valuesCL$SFL
  SFL <- SFL %>% select(Information, Region, VslType, Charterer, CargoType, Qty, Grade,
                        Laycan, LoadPort,DischPort,Status,Vessel, RateType, Rate,Comments,
                        Broker,ReportDate,Updatedby)
  # valuesCL$SFL2 <- SFL
  datatable(SFL, extensions = 'Buttons', rownames = FALSE, filter = 'top',
            options = list(deferRender = TRUE,scroller = FALSE,scrollX = TRUE,
                           dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print',I('colvis')),
                           pageLength = 10,lengthMenu = list(c(10,20, 50,-1), list('10', '20', '50','All')), paging = T))
  
})

# output$ScoFixList2 <- renderDT({
#   SFL2 <- req(valuesCL$SFL2)
#   datatable(SFL2, extensions = 'Buttons', rownames = FALSE, filter = 'top',
#             options = list(deferRender = TRUE,scroller = FALSE,scrollX = TRUE,
#                            dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print',I('colvis')),
#                            pageLength = 10,lengthMenu = list(c(10,20, 50,-1), list('10', '20', '50','All')), paging = T))
# })
#################################################
#Shift Rows
observeEvent(input$shiftcells, {
  req(input$CargoListData)
  req(valuesCL$SFL)
  rhot <- hot_to_r(input$CargoListData)
  
  rowindex <- (which(rhot$Select))
  
  if (length(rowindex) > 0) {
    
    req(rhot)
    req(rowindex)
    SFL <- valuesCL$SFL
    SFL <- bind_rows(SFL, rhot[rowindex,]) %>% filter(ReportDate >= Sys.Date() - config$validTime)
    SFL <- SFL[!duplicated(SFL),]
    
    saveRDS(SFL, 'data/ScorpioFixtureList.Rds')
    
    valuesCL$SFL <- readRDS('data/ScorpioFixtureList.Rds')
    rowvals <- rhot[rowindex,]$ROWIDT
    
    CL <- readRDS('data/CargoList.Rds')
    
    CL <- CL %>% filter(ROWIDT %!in% rowvals)
    
    saveRDS(CL, 'data/CargoList.Rds')
    CL <- readRDS('data/CargoList.Rds')
    CL$VslType <- wvd()$`Vessel Type`[match(CL$Vessel, wvd()$Name)]
    CL$Operator <- wvd()$`Commercial Operator`[match(CL$Vessel, wvd()$Name)]
    CL$Select <- as.logical(CL$Select)
    CL$Region <- toupper(CL$Region)
    CL %>% dplyr::filter(Information %in% c(input$InformationCL,""," ",NA)) %>%
      dplyr::filter(Region %in% c(input$RegionCL, ""," ",NA)) %>% 
      dplyr::filter(CargoType %in% c(input$CargoTypeCL,""," ",NA)) %>% 
      dplyr::filter(Status %in% c(input$StatusCL,""," ",NA)) %>%
      dplyr::filter(ReportDate >= input$RepDateCL[1] & ReportDate <= input$RepDateCL[2] | is.na(ReportDate))
    valuesCL[["CargoList"]] <- CL
  }
})


##########################
observeEvent(input$shiftcells2, {
  req(input$CargoListData2)
  req(valuesCL$SFL)
  rhot <- hot_to_r(input$CargoListData2)
  rowindex <- (which(rhot$Select))
  
  if (length(rowindex) > 0) {
    
    req(rhot)
    req(rowindex)
    SFL <- valuesCL$SFL
    SFL <- bind_rows(SFL, rhot[rowindex,]) %>% filter(ReportDate >= Sys.Date() - config$validTime)
    SFL <- SFL[!duplicated(SFL),]
    
    saveRDS(SFL, 'data/ScorpioFixtureList.Rds')
    
    valuesCL$SFL <- readRDS('data/ScorpioFixtureList.Rds')
    rowvals <- rhot[rowindex,]$ROWIDT
    
    CL <- readRDS('data/CargoList.Rds')
    
    CL <- CL %>% filter(ROWIDT %!in% rowvals)
    
    saveRDS(CL, 'data/CargoList.Rds')
    CL <- readRDS('data/CargoList.Rds')
    CL$VslType <- wvd()$`Vessel Type`[match(CL$Vessel, wvd()$Name)]
    CL$Operator <- wvd()$`Commercial Operator`[match(CL$Vessel, wvd()$Name)]
    CL$Select <- as.logical(CL$Select)
    CL$Region <- toupper(CL$Region)
    CL %>% dplyr::filter(Information %in% c(input$InformationCL,""," ",NA)) %>%
      dplyr::filter(Region %in% c(input$RegionCL, ""," ",NA)) %>% 
      dplyr::filter(CargoType %in% c(input$CargoTypeCL,""," ",NA)) %>% 
      dplyr::filter(Status %in% c(input$StatusCL,""," ",NA)) %>%
      dplyr::filter(ReportDate >= input$RepDateCL[1] & ReportDate <= input$RepDateCL[2] | is.na(ReportDate))
    valuesCL[["CargoList"]] <- CL
  }
})