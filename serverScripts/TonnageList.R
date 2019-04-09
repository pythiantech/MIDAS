    ##########################################################################################
    
    observeEvent(input$showFiltersTL, {
      shinyjs::hide(id = "hiddenTL")
      shinyjs::toggle(id = "visibleTL")
      shinyjs::toggle(id = "filtersTL")
    })
    
    observeEvent(input$hideFiltersTL, {
      shinyjs::hide(id = "visibleTL")
      shinyjs::show(id = "hiddenTL")
      shinyjs::hide(id = "filtersTL")
    })
    
    ################################################
    #Filters dependent on wvd()
    output$vtypeTL <- renderUI({
      pickerInput('vtypeTL',"Filter by Vessel Type", choices = levels(as.factor(wvd()$`Vessel Type`)),
                  options = list(`actions-box` = TRUE),multiple = T, selected = levels(as.factor(wvd()$`Vessel Type`)))
    })
    output$DWTTL <- renderUI({
      sliderInput('DWTTL',"Filter by DWT",value = c(min(wvd()$Dwt, na.rm = T),max(wvd()$Dwt, na.rm = T)),
                  min = min(wvd()$Dwt, na.rm = T),max = max(wvd()$Dwt, na.rm = T))
    })
    output$OprTL <- renderUI({
      pickerInput('OprTL',"Filter by Operator", choices = levels(as.factor(wvd()$`Commercial Operator`)),
                  options = list(`actions-box` = TRUE,`live-search` = TRUE),multiple = T, selected = levels(as.factor(wvd()$`Commercial Operator`)))
    })
    output$CubicTL <- renderUI({
      sliderInput('CubicTL',"Filter by Cubic",value = c(min(wvd()$Cubics, na.rm = T),max(wvd()$Cubics, na.rm = T)),
                  min = min(wvd()$Cubics, na.rm = T),max = max(wvd()$Cubics, na.rm = T))
    })
    output$builtfilterTL <- renderUI({
      sliderInput('builtfilterTL', "Filter by Age of Vessel", min = 0, max = 30, value = 30)
    })
    output$PortsTL <- renderUI({
      pickerInput('PortsTL',"Filter by Opening Port", choices = levels(as.factor(unique(V_Dim_Ports()$PortName))),
                  options = list(`actions-box` = TRUE,`live-search` = TRUE),multiple = T, selected = levels(as.factor(unique(V_Dim_Ports()$PortName))))
    })

valuesTL <- reactiveValues()
TLData <- reactive({
  req(input$vtypeTL)
  TL <- readRDS('data/TonnageList.Rds')
  TL <- TL %>% filter(OpenDate >= Sys.Date() - config$validTime)
  saveRDS(TL, 'data/TonnageList.Rds')
  TL <- readRDS('data/TonnageList.Rds')
  TL$VslType <- wvd()$`Vessel Type`[match(TL$VesselName, wvd()$Name)]
  TL$Cubic <- wvd()$Cubics[match(TL$VesselName, wvd()$Name)]
  TL$DWT <- wvd()$Dwt[match(TL$VesselName, wvd()$Name)]
  TL$IceClass <- wvd()$`Ice Class`[match(TL$VesselName, wvd()$Name)]
  # TL$IMO <- wvd()$`IMO No.`[match(TL$VesselName, wvd()$Name)]
  TL$Built <- wvd()$Built[match(TL$VesselName, wvd()$Name)]
  TL$Operators <- wvd()$`Commercial Operator`[match(TL$VesselName, wvd()$Name)]
  
  saveRDS(TL, 'data/TonnageList.Rds')
  TL <- readRDS('data/TonnageList.Rds')
  TL %>% filter(VslType %in% c(input$vtypeTL, ""," ",NA)) %>% filter(DWT >= input$DWTTL[1] & DWT <= input$DWTTL[2] | DWT %in% c(""," ",NA)) %>%
    filter(Cubic >= input$CubicTL[1] & Cubic <= input$CubicTL[2] | Cubic %in% c(""," ",NA)) %>%
    filter(Operators %in% c(input$OprTL, "", " ", NA)) %>%
    filter(OpenDate >= input$OpenPortDate[1] & OpenDate <= input$OpenPortDate[2] | is.na(OpenDate)) %>%
    filter(EmpStatus %in% c(input$EmpTL, ""," ", NA)) %>% 
    filter(Built >= as.numeric((lubridate::year(Sys.Date()) - input$builtfilterTL))) %>% 
    filter(OpenPort %in% input$PortsTL)
 
})


##############################################################################
observeEvent(TLData(),{
  data <- TLData()
  valuesTL[["TonnageList"]] <- data
})


output$TonnageListData2 <-output$TonnageListData <- renderRHandsontable({
  TonnageList <- valuesTL[["TonnageList"]]
  
  if (!is.null(TonnageList))
    rhandsontable(TonnageList,width = '100%', height = 400,rowHeaders = NULL,
                         selectCallback = TRUE, readOnly = FALSE,stretchH = "all") %>%
      hot_context_menu(allowRowEdit = FALSE) %>%
      hot_col(col = "VesselName", type = "dropdown", source = unique(wvd()$Name)) %>%
      hot_col(col = "EmpStatus", type = "dropdown", source = c("Fixed","Unfixed","Hold",
                                                               "Subs","Others","Died", "Looking","Open","WDWF","")) %>%
      hot_col(col = "OpenPort", type = "dropdown",source = unique(V_Dim_Ports()$PortName)) %>%
      hot_col(col = "OpenDate", type = "date") %>%
      hot_col(col = "Source", type = NULL) %>%
      hot_col(col = "Comments", type = NULL) %>%
      hot_col(col = "VslType", type = NULL, readOnly = F) %>%
      hot_col(col = "Cubic", type = NULL, readOnly = F, format = "0") %>%
      hot_col(col = "DWT", type = NULL, readOnly = F, format = "0") %>%
      hot_col(col = "IceClass", type = NULL, readOnly = F) %>%
      # hot_col(col = "IMO", type = NULL, readOnly = F) %>%
      hot_col(col = "Built", type = NULL, readOnly = F, format = "0") %>%
      hot_col(col = "Operators", type = NULL, readOnly = F) %>%
      hot_col(col = "UpdatedBy", type = NULL, readOnly = TRUE) %>%
      hot_col(col = "timestamp", type = "numeric",readOnly = TRUE, colWidths = 0.1) %>%
      hot_col(col = "ROWIDT", type = "numeric",readOnly = TRUE, colWidths = 0.1) #%>%
    # hot_cols(colWidths = c(rep(150,13), rep(0.1,2)))
      # hot_cols(colWidths = c(70, 150, 100, 150, 100, 180,150,60,60,60,60,120,100,rep(0.1,2))) 


})

edI <- reactiveValues(editedInfoTL = NA)
observeEvent(input$TonnageListData$changes$changes, {
  df <- hot_to_r(input$TonnageListData)
  info <- input$TonnageListData$changes$changes
  rowid <- info[[1]][[1]] <- info[[1]][[1]] + 1
  colid <- info[[1]][[2]] <- info[[1]][[2]] + 1
  oldvalue <- info[[1]][[3]]
  newvalue <- info[[1]][[4]]
  oldtimestamp <- valuesTL[["TonnageList"]][rowid,14]
  newtimestamp <- epochTime()
  username <- user()
  # ROWIDT <- as.integer(valuesTL[["TonnageList"]][rowid,15])
  ROWIDT <- as.integer(df[rowid,15])
  info <- sapply(info, function(x) ifelse(x == "NULL", NA, x))
  rowsold <- nrow(readRDS('data/TonnageList.Rds'))
  if (all(is.na(edI$editedInfoTL))) {
    edI$editedInfoTL <- data.frame(c(info,oldtimestamp, newtimestamp, username, ROWIDT, rowsold), stringsAsFactors = FALSE)
    colnames(edI$editedInfoTL) <- c("rowid", "colid", "oldvalue", "newvalue","oldtimestamp","newtimestamp",
                                    "username","ROWIDT", "rowsold")
  } else {
    df <- data.frame(c(info,oldtimestamp, newtimestamp, username, ROWIDT, rowsold), stringsAsFactors = FALSE)
    colnames(df) <- c("rowid", "colid", "oldvalue", "newvalue","oldtimestamp","newtimestamp", "username","ROWIDT", "rowsold")
    if (ncol(edI$editedInfoTL) == ncol(df)) edI$editedInfoTL <- rbind(edI$editedInfoTL, df)
  }
  colnames(edI$editedInfoTL) <- c("rowid", "colid", "oldvalue", "newvalue","oldtimestamp","newtimestamp",
                                  "username","ROWIDT", "rowsold")
  
  edI$editedInfoTL$oldtimestamp[is.na(edI$editedInfoTL$oldtimestamp)] <- 0
  
  rowids <- vector("numeric")
  for (i in 1:nrow(edI$editedInfoTL)) {
    rowids <- c(rowids, edI$editedInfoTL[i,1])
    if (is.na(edI$editedInfoTL[i,4])) edI$editedInfoTL[i,4] <- edI$editedInfoTL[i,3]
    if (nchar(edI$editedInfoTL[i,6]) < 5) edI$editedInfoTL[i,6] <- edI$editedInfoTL[i,5]
    
    if (is.na(edI$editedInfoTL[i,8])) {
      if (i < 2) {
        edI$editedInfoTL[i,8] <- rowsold + 1
        
      }
      else{
        if (edI$editedInfoTL[i,1] %in% rowids[1:(i - 1)]) {
          indices <- match(edI$editedInfoTL[i,1], rowids[1:(i - 1)])
          edI$editedInfoTL[i,8] <- edI$editedInfoTL[indices,8]
        }
        else {
          edI$editedInfoTL[i,8] <- edI$editedInfoTL[i - 1,8] + 1
          
        }}
    }
    
    
  }
  print(edI$editedInfoTL)
  
})
######################################################
observeEvent(input$TonnageListData2$changes$changes, {
  df <- hot_to_r(input$TonnageListData2)
  info <- input$TonnageListData2$changes$changes
  rowid <- info[[1]][[1]] <- info[[1]][[1]] + 1
  colid <- info[[1]][[2]] <- info[[1]][[2]] + 1
  oldvalue <- info[[1]][[3]]
  newvalue <- info[[1]][[4]]
  oldtimestamp <- valuesTL[["TonnageList"]][rowid,14]
  newtimestamp <- epochTime()
  username <- user()
  # ROWIDT <- as.integer(valuesTL[["TonnageList"]][rowid,15])
  ROWIDT <- as.integer(df[rowid,15])
  info <- sapply(info, function(x) ifelse(x == "NULL", NA, x))
  rowsold <- nrow(readRDS('data/TonnageList.Rds'))
  if (all(is.na(edI$editedInfoTL))) {
    edI$editedInfoTL <- data.frame(c(info,oldtimestamp, newtimestamp, username, ROWIDT, rowsold), stringsAsFactors = FALSE)
    colnames(edI$editedInfoTL) <- c("rowid", "colid", "oldvalue", "newvalue","oldtimestamp","newtimestamp",
                                    "username","ROWIDT", "rowsold")
  } else {
    df <- data.frame(c(info,oldtimestamp, newtimestamp, username, ROWIDT, rowsold), stringsAsFactors = FALSE)
    colnames(df) <- c("rowid", "colid", "oldvalue", "newvalue","oldtimestamp","newtimestamp", "username","ROWIDT", "rowsold")
    if (ncol(edI$editedInfoTL) == ncol(df)) edI$editedInfoTL <- rbind(edI$editedInfoTL, df)
  }
  colnames(edI$editedInfoTL) <- c("rowid", "colid", "oldvalue", "newvalue","oldtimestamp","newtimestamp",
                                  "username","ROWIDT", "rowsold")
  
  edI$editedInfoTL$oldtimestamp[is.na(edI$editedInfoTL$oldtimestamp)] <- 0
  
  rowids <- vector("numeric")
  for (i in 1:nrow(edI$editedInfoTL)) {
    rowids <- c(rowids, edI$editedInfoTL[i,1])
    if (is.na(edI$editedInfoTL[i,4])) edI$editedInfoTL[i,4] <- edI$editedInfoTL[i,3]
    if (nchar(edI$editedInfoTL[i,6]) < 5) edI$editedInfoTL[i,6] <- edI$editedInfoTL[i,5]
    
    if (is.na(edI$editedInfoTL[i,8])) {
      if (i < 2) {
        edI$editedInfoTL[i,8] <- rowsold + 1
        
      }
      else{
        if (edI$editedInfoTL[i,1] %in% rowids[1:(i - 1)]) {
          indices <- match(edI$editedInfoTL[i,1], rowids[1:(i - 1)])
          edI$editedInfoTL[i,8] <- edI$editedInfoTL[indices,8]
        }
        else {
          edI$editedInfoTL[i,8] <- edI$editedInfoTL[i - 1,8] + 1
          
        }}
    }
    
    
  }
  print(edI$editedInfoTL)
  
})
##############################################
observeEvent(input$addRowTL, {
  df <- hot_to_r(input$TonnageListData)
  df <- df %>% add_row()
  valuesTL[["TonnageList"]] <- df
})

observeEvent(input$addRowTL2, {
  df <- hot_to_r(input$TonnageListData2)
  df <- df %>% add_row()
  valuesTL[["TonnageList"]] <- df
})

observeEvent(input$saveTLData, {
  req(edI$editedInfoTL)
  #################
  #New Logic
  editedValue <- edI$editedInfoTL %>% 
    group_by(ROWIDT, colid) %>%
    filter(newvalue == dplyr::last(newvalue) | is.na(newvalue)) %>%
    ungroup()
  
  TL <- readRDS('data/TonnageList.Rds')
  TL$VslType <- wvd()$`Vessel Type`[match(TL$VesselName, wvd()$Name)]
  TL$Cubic <- wvd()$Cubics[match(TL$VesselName, wvd()$Name)]
  TL$DWT <- wvd()$Dwt[match(TL$VesselName, wvd()$Name)]
  TL$IceClass <- wvd()$`Ice Class`[match(TL$VesselName, wvd()$Name)]
  # TL$IMO <- wvd()$`IMO No.`[match(TL$VesselName, wvd()$Name)]
  TL$Built <- wvd()$Built[match(TL$VesselName, wvd()$Name)]
  TL$Operators <- wvd()$`Commercial Operator`[match(TL$VesselName, wvd()$Name)]
  rowsnew <- nrow(readRDS('data/TonnageList.Rds'))
  
  for (i in 1:nrow(editedValue)) {
    if (editedValue$oldtimestamp[i] == 0) {
      rowdiff <- rowsnew - editedValue$rowsold[i]
      editedValue$ROWIDT[i] <- editedValue$ROWIDT[i] + rowdiff
    }
    else {editedValue$ROWIDT[i] <- editedValue$ROWIDT[i]}
    
    if (editedValue$colid[i] != 5) TL[editedValue$ROWIDT[i],editedValue$colid[i]] <- as.character(editedValue$newvalue[i])
    else TL[editedValue$ROWIDT[i],editedValue$colid[i]] <- mdy(editedValue$newvalue[i])
    TL$ROWIDT[editedValue$ROWIDT[i]] <- editedValue$ROWIDT[i]
    TL$timestamp[editedValue$ROWIDT[i]] <- editedValue$newtimestamp[i]
    TL$UpdatedBy[editedValue$ROWIDT[i]] <- editedValue$username[i]
  }
  print(TL)
  saveRDS(TL, 'data/TonnageList.Rds')
  
  ##################################
  
  TL <- readRDS('data/TonnageList.Rds')
  TL$VslType <- wvd()$`Vessel Type`[match(TL$VesselName, wvd()$Name)]
  TL$Cubic <- wvd()$Cubics[match(TL$VesselName, wvd()$Name)]
  TL$DWT <- wvd()$Dwt[match(TL$VesselName, wvd()$Name)]
  TL$IceClass <- wvd()$`Ice Class`[match(TL$VesselName, wvd()$Name)]
  # TL$IMO <- wvd()$`IMO No.`[match(TL$VesselName, wvd()$Name)]
  TL$Built <- wvd()$Built[match(TL$VesselName, wvd()$Name)]
  TL$Operators <- wvd()$`Commercial Operator`[match(TL$VesselName, wvd()$Name)]
  
  TL %>% filter(VslType %in% c(input$vtypeTL, ""," ",NA)) %>% filter(DWT >= input$DWTTL[1] & DWT <= input$DWTTL[2] | DWT %in% c(""," ",NA)) %>%
    filter(Cubic >= input$CubicTL[1] & Cubic <= input$CubicTL[2] | Cubic %in% c(""," ",NA)) %>%
    filter(Operators %in% c(input$OprTL, "", " ", NA)) %>%
    filter(OpenDate >= input$OpenPortDate[1] & OpenDate <= input$OpenPortDate[2] | is.na(OpenDate)) %>%
    filter(EmpStatus %in% c(input$EmpTL, ""," ", NA)) %>% 
    filter(Built >= as.numeric((lubridate::year(Sys.Date()) - input$builtfilterTL))) %>% 
    filter(OpenPort %in% input$PortsTL)
  valuesTL[["TonnageList"]] <- TL
  
  
  edI$editedInfoTL <- NA
  shinyalert("Success!", "The data has been saved", type = "success")
 
})

################################################
observeEvent(input$saveTLData2, {
  req(edI$editedInfoTL)
  #################
  #New Logic
  editedValue <- edI$editedInfoTL %>% 
    group_by(ROWIDT, colid) %>%
    filter(newvalue == dplyr::last(newvalue) | is.na(newvalue)) %>%
    ungroup()
  
  TL <- readRDS('data/TonnageList.Rds')
  TL$VslType <- wvd()$`Vessel Type`[match(TL$VesselName, wvd()$Name)]
  TL$Cubic <- wvd()$Cubics[match(TL$VesselName, wvd()$Name)]
  TL$DWT <- wvd()$Dwt[match(TL$VesselName, wvd()$Name)]
  TL$IceClass <- wvd()$`Ice Class`[match(TL$VesselName, wvd()$Name)]
  # TL$IMO <- wvd()$`IMO No.`[match(TL$VesselName, wvd()$Name)]
  TL$Built <- wvd()$Built[match(TL$VesselName, wvd()$Name)]
  TL$Operators <- wvd()$`Commercial Operator`[match(TL$VesselName, wvd()$Name)]
  rowsnew <- nrow(readRDS('data/TonnageList.Rds'))
  
  for (i in 1:nrow(editedValue)) {
    if (editedValue$oldtimestamp[i] == 0) {
      rowdiff <- rowsnew - editedValue$rowsold[i]
      editedValue$ROWIDT[i] <- editedValue$ROWIDT[i] + rowdiff
    }
    else {editedValue$ROWIDT[i] <- editedValue$ROWIDT[i]}
    
    if (editedValue$colid[i] != 5) TL[editedValue$ROWIDT[i],editedValue$colid[i]] <- as.character(editedValue$newvalue[i])
    else TL[editedValue$ROWIDT[i],editedValue$colid[i]] <- mdy(editedValue$newvalue[i])
    TL$ROWIDT[editedValue$ROWIDT[i]] <- editedValue$ROWIDT[i]
    TL$timestamp[editedValue$ROWIDT[i]] <- editedValue$newtimestamp[i]
    TL$UpdatedBy[editedValue$ROWIDT[i]] <- editedValue$username[i]
  }
  print(TL)
  saveRDS(TL, 'data/TonnageList.Rds')
  
  ##################################
  
  TL <- readRDS('data/TonnageList.Rds')
  TL$VslType <- wvd()$`Vessel Type`[match(TL$VesselName, wvd()$Name)]
  TL$Cubic <- wvd()$Cubics[match(TL$VesselName, wvd()$Name)]
  TL$DWT <- wvd()$Dwt[match(TL$VesselName, wvd()$Name)]
  TL$IceClass <- wvd()$`Ice Class`[match(TL$VesselName, wvd()$Name)]
  # TL$IMO <- wvd()$`IMO No.`[match(TL$VesselName, wvd()$Name)]
  TL$Built <- wvd()$Built[match(TL$VesselName, wvd()$Name)]
  TL$Operators <- wvd()$`Commercial Operator`[match(TL$VesselName, wvd()$Name)]
  
  TL %>% filter(VslType %in% c(input$vtypeTL, ""," ",NA)) %>% filter(DWT >= input$DWTTL[1] & DWT <= input$DWTTL[2] | DWT %in% c(""," ",NA)) %>%
    filter(Cubic >= input$CubicTL[1] & Cubic <= input$CubicTL[2] | Cubic %in% c(""," ",NA)) %>%
    filter(Operators %in% c(input$OprTL, "", " ", NA)) %>%
    filter(OpenDate >= input$OpenPortDate[1] & OpenDate <= input$OpenPortDate[2] | is.na(OpenDate)) %>%
    filter(EmpStatus %in% c(input$EmpTL, ""," ", NA)) %>% 
    filter(Built >= as.numeric((lubridate::year(Sys.Date()) - input$builtfilterTL))) %>%
    filter(OpenPort %in% input$PortsTL)
  valuesTL[["TonnageList"]] <- TL
  
  
  edI$editedInfoTL <- NA
  shinyalert("Success!", "The data has been saved", type = "success")
  
})

#Get Bizlem Tonnage List

#Reactive values for storing information
TonVal <- reactiveValues()

output$BizTon <- renderDT({
  req(input$reptimevalid)
  BT <- BizTon() %>% filter(ReportTimestamp >= (Sys.time() - input$reptimevalid*24*60*60))
  BT <- data.frame(lapply(BT, function(v) {
    if (is.character(v)) return(toupper(v))
    else return(v)
  }))
  BT$ROWIDT <- ""
  # BT <- BT %>% select(VesselName:Comments,VslType:Owner, UpdatedBy, timestamp, ROWIDT)
  BT <- arrange(BT, desc(ReportTimestamp))
  TonVal$BT <- BT

  datatable(BT, rownames = FALSE, extensions = 'Buttons', filter = 'top',
            options = list(deferRender = TRUE,scroller = FALSE,scrollX = TRUE,
                           dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print',I('colvis')),
                           pageLength = 10,lengthMenu = list(c(10,20, 50,-1), list('10', '20', '50','All')), paging = T,
                           columnDefs = list(list(visible = FALSE, targets = c(12,14,15)))))
            
}, server = TRUE)

output$BizTon2 <- renderDT({
  req(input$reptimevalid2)
  BT <- BizTon() %>% filter(ReportTimestamp >= (Sys.time() - input$reptimevalid2*24*60*60))
  BT <- data.frame(lapply(BT, function(v) {
    if (is.character(v)) return(toupper(v))
    else return(v)
  }))
  BT$ROWIDT <- ""
  # BT <- BT %>% select(VesselName:Comments,VslType:Owner, UpdatedBy, timestamp, ROWIDT)
  BT <- arrange(BT, desc(ReportTimestamp))
  TonVal$BT <- BT
  
  datatable(BT, rownames = FALSE, extensions = 'Buttons', filter = 'top',
            options = list(deferRender = TRUE,scroller = FALSE,scrollX = TRUE,
                           dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print',I('colvis')),
                           pageLength = 10,lengthMenu = list(c(10,20, 50,-1), list('10', '20', '50','All')), paging = T,
                           columnDefs = list(list(visible = FALSE, targets = c(12,14,15)))))
  
}, server = TRUE)

proxy = dataTableProxy('BizTon')

observeEvent(input$shiftcellsTL, {
  rowssel <- input$BizTon_rows_selected
  if (length(rowssel)) {
    df <- TonVal$BT[rowssel,]
  }
  rowsnew <- nrow(readRDS('data/TonnageList.Rds'))
  df$ROWIDT <- seq(rowsnew + 1, rowsnew + nrow(df))
  df$timestamp <- epochTime()
  username <- user()
  df$UpdatedBy <- username
  df$VslType <- wvd()$`Vessel Type`[match(df$VesselName, wvd()$Name)]
  df$Cubic <- wvd()$Cubics[match(df$VesselName, wvd()$Name)]
  df$DWT <- wvd()$Dwt[match(df$VesselName, wvd()$Name)]
  df$IceClass <- wvd()$`Ice Class`[match(df$VesselName, wvd()$Name)]
  # df$IMO <- wvd()$`IMO No.`[match(df$VesselName, wvd()$Name)]
  df$Built <- wvd()$Built[match(df$VesselName, wvd()$Name)]
  df$Operators <- wvd()$`Commercial Operator`[match(df$VesselName, wvd()$Name)]
  df <- df %>% select(-ReportTimestamp)
  df$OpenDate <- mdy(as.character(df$OpenDate))
  # valuesTL$TonnageList$OpenDate <- mdy(as.character(valuesTL$TonnageList$OpenDate))
  # print(str(df))
  # print(str(valuesTL$TonnageList))
  valuesTL[["TonnageList"]] <- rbind(valuesTL[["TonnageList"]], df)
  saveRDS(valuesTL[["TonnageList"]], 'data/TonnageList.Rds')
  TL <- readRDS('data/TonnageList.Rds')
  TL$VslType <- wvd()$`Vessel Type`[match(TL$VesselName, wvd()$Name)]
  TL$Cubic <- wvd()$Cubics[match(TL$VesselName, wvd()$Name)]
  TL$DWT <- wvd()$Dwt[match(TL$VesselName, wvd()$Name)]
  TL$IceClass <- wvd()$`Ice Class`[match(TL$VesselName, wvd()$Name)]
  # TL$IMO <- wvd()$`IMO No.`[match(TL$VesselName, wvd()$Name)]
  TL$Built <- wvd()$Built[match(TL$VesselName, wvd()$Name)]
  TL$Operators <- wvd()$`Commercial Operator`[match(TL$VesselName, wvd()$Name)]

  valuesTL[["TonnageList"]] <- TL
  proxy %>% selectRows(NULL)
})

##############################################################
observeEvent(input$shiftcellsTL2, {
  rowssel <- input$BizTon2_rows_selected
  if (length(rowssel)) {
    df <- TonVal$BT[rowssel,]
  }
  rowsnew <- nrow(readRDS('data/TonnageList.Rds'))
  df$ROWIDT <- seq(rowsnew + 1, rowsnew + nrow(df))
  df$timestamp <- epochTime()
  username <- user()
  df$UpdatedBy <- username
  df$VslType <- wvd()$`Vessel Type`[match(df$VesselName, wvd()$Name)]
  df$Cubic <- wvd()$Cubics[match(df$VesselName, wvd()$Name)]
  df$DWT <- wvd()$Dwt[match(df$VesselName, wvd()$Name)]
  df$IceClass <- wvd()$`Ice Class`[match(df$VesselName, wvd()$Name)]
  # df$IMO <- wvd()$`IMO No.`[match(df$VesselName, wvd()$Name)]
  df$Built <- wvd()$Built[match(df$VesselName, wvd()$Name)]
  df$Operators <- wvd()$`Commercial Operator`[match(df$VesselName, wvd()$Name)]
  df <- df %>% select(-ReportTimestamp)
  valuesTL[["TonnageList"]] <- rbind(valuesTL[["TonnageList"]], df)
  saveRDS(valuesTL[["TonnageList"]], 'data/TonnageList.Rds')
  TL <- readRDS('data/TonnageList.Rds')
  TL$VslType <- wvd()$`Vessel Type`[match(TL$VesselName, wvd()$Name)]
  TL$Cubic <- wvd()$Cubics[match(TL$VesselName, wvd()$Name)]
  TL$DWT <- wvd()$Dwt[match(TL$VesselName, wvd()$Name)]
  TL$IceClass <- wvd()$`Ice Class`[match(TL$VesselName, wvd()$Name)]
  # TL$IMO <- wvd()$`IMO No.`[match(TL$VesselName, wvd()$Name)]
  TL$Built <- wvd()$Built[match(TL$VesselName, wvd()$Name)]
  TL$Operators <- wvd()$`Commercial Operator`[match(TL$VesselName, wvd()$Name)]
  
  valuesTL[["TonnageList"]] <- TL
  proxy %>% selectRows(NULL)
})
##################################

observeEvent(input$ResetFilt, {
  TL <- readRDS('data/TonnageList.Rds')
  valuesTL[["TonnageList"]] <- TL
})


