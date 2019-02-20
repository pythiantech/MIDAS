
#Reactive values for storing information
MktVal <- reactiveValues()

#Pull from Bizlem on button click
observeEvent(input$PullMkt, {
  req(input$MktDateRange)
  start <- as.character(format(input$MktDateRange[1], "%d-%m-%Y"))
  end <- as.character(format(input$MktDateRange[2], "%d-%m-%Y"))
  shinyjs::show("page_shield")
  SRD <- GetBizlemData('Spot', start, end)
  shinyjs::hide("page_shield")
  if (is.data.frame(SRD) && ncol(SRD) > 1) {
    SRD$ReportDate <- dmy_hm(SRD$ReportDate)
    MktVal$MktFix <- SRD
    MktVal$Orig <- SRD
  }

})

#Output as dataframe
#https://stackoverflow.com/questions/22650737/empty-data-message-in-rendertable

output$MktFixUI <- renderUI({
  SRD <- req(BizSpot())
  MktVal$MktFix <- SRD
  MktVal$Orig <- SRD
  
  if (is.null(MktVal$MktFix))
    return("No Data To Show")
  
  DTOutput('MktFixReport')
  
})

output$MktFixReport <- renderDT({
  req(MktVal$MktFix)
  df <- data.frame(lapply(MktVal$MktFix, function(v) {
    if (is.character(v)) return(toupper(v))
    else return(v)
  }))
  df <- df %>% select(Vesseltype, Charterer,CargoType, CargoQty, CargoGrade, LCStart, LCEnd, 
                      LoadPort, DiscPort, Status,VesselName,
                      RateType, Rate, Comments, Broker = Source,
                      ReportDate)
  datatable(df, extensions = 'Buttons', rownames = FALSE, filter = 'top',
            options = list(deferRender = TRUE,scroller = FALSE,scrollX = TRUE,
                           dom = 'Blfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print',I('colvis')),
                           pageLength = 10,lengthMenu = list(c(10,20, 50,-1), list('10', '20', '50','All')), paging = T))
})