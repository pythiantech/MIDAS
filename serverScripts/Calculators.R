
output$WFSubAreaStart <- renderUI({
  pickerInput('WFSubAreaStart', "Select Starting Area", choices = unique(V_Dim_Ports()$SubArea))
})

output$WFSubAreaStop <- renderUI({
  pickerInput('WFSubAreaStop', "Select Ending Area", choices = unique(V_Dim_Ports()$SubArea))
})

output$WFVslType <- renderUI({
  pickerInput('WFVslType', "Select Vessel Type", choices = unique(V_Dim_Vessel()$VesselType))
})

output$WFVslState <- renderUI({
  pickerInput('WFVslState', "Select Vessel State", choices = c("Ballast" = 0, "Laden" = 1))
})

VDPAR <- reactive({
  VDP <- V_Dim_Voyage() %>% select(Fkey_Enum_VoyageStatus_Id, Dim_Voyage_Id, oprType) %>%
    filter(oprType != 'RELT',
           oprType != 'OVTO',
           oprType != 'TCTO',
           Fkey_Enum_VoyageStatus_Id != 1)
  VIDs <- unique(VDP$Dim_Voyage_Id)
  VDPA <- V_Fact_VesselItinerary_Vsched() %>% select(Fkey_Dim_Vessel_Id, Fkey_Dim_Voyage_Id, Imos_VoyNo,ord_no_int,Date_Departure, Date_Arrival,Fkey_Dim_Ports_Id,
                                                   Fkey_Dim_PortFunction_Id,Fkey_Enum_BallastLaden_Id,sea_days, spd) %>%
    filter(Fkey_Dim_Voyage_Id != -1) %>% filter(Fkey_Dim_Vessel_Id != -1) %>% filter(Fkey_Dim_Ports_Id != -1) %>%
    filter(Fkey_Dim_Voyage_Id %in% VIDs) %>%
    filter(Fkey_Enum_BallastLaden_Id != -1) %>%
    arrange(Fkey_Dim_Vessel_Id, Fkey_Dim_Voyage_Id, ord_no_int) %>%
    mutate(Miles = round(sea_days*spd*24, 2))

  VDPA$Vessel <- V_Dim_Vessel()$VesselName[match(VDPA$Fkey_Dim_Vessel_Id, V_Dim_Vessel()$Dim_Vessel_Id)]
  VDPA$Port <- V_Dim_Ports()$PortName[match(VDPA$Fkey_Dim_Ports_Id, V_Dim_Ports()$Dim_Ports_Id)]
  VDPA$SubArea <- V_Dim_Ports()$SubArea[match(VDPA$Fkey_Dim_Ports_Id, V_Dim_Ports()$Dim_Ports_Id)]
  VDPA$Function <- portFunction$Function[match(VDPA$Fkey_Dim_PortFunction_Id, portFunction$ID)]
  VDPA <- VDPA %>% select(Vessel, Fkey_Dim_Voyage_Id,VoyNum = Imos_VoyNo, Port, Function, Date_Arrival, Date_Departure,
                          Fkey_Enum_BallastLaden_Id, Miles, SubArea, sea_days)


  VDPA <- VDPA %>% group_by(Vessel,VoyNum) %>%
    mutate(cumMiles = cumsum(Miles), cumSeaDays = cumsum(sea_days)) %>%
    filter(Function %in% c("Commencing", "Loading", "Discharging")) %>%
    mutate(Miles = c(diff(cumMiles - Miles), last(Miles)), SeaDays = c(diff(cumSeaDays - sea_days), last(sea_days)),
           ToPort = lead(Port),
           Month = month(Date_Departure + floor(c(Date_Arrival[-1], 0) - Date_Departure) / 2, label = TRUE)) %>%
    rename(FromPort = Port) %>% slice(-n()) %>%
    select(Vessel, VoyNum, FromPort, ToPort, Miles, SeaDays, BallastLaden = Fkey_Enum_BallastLaden_Id, Month) %>%
    mutate(TheoDays = Miles/(13*24), WF = (SeaDays/TheoDays))



  VDPA$FromSubArea <- V_Dim_Ports()$SubArea[match(VDPA$FromPort, V_Dim_Ports()$PortName)]
  VDPA$ToSubArea <- V_Dim_Ports()$SubArea[match(VDPA$ToPort, V_Dim_Ports()$PortName)]
  VDPA$VesselType <- V_Dim_Vessel()$VesselType[match(VDPA$Vessel, V_Dim_Vessel()$VesselName)]

  VDPA
})


WeatherRV <- reactiveValues()

observeEvent(input$WFFilters, {

  req(input$WFSubAreaStart)
  req(input$WFSubAreaStop)
  req(input$WFVslType)
  req(input$WFVslState)
  req(VDPAR())

  WFactor <- VDPAR() %>% filter(BallastLaden == input$WFVslState) %>%
    filter(VesselType == input$WFVslType) %>%
    filter(FromSubArea == input$WFSubAreaStart) %>%
    filter(ToSubArea == input$WFSubAreaStop) %>%
    filter(is.finite(WF)) %>%
    group_by(Month) %>%
    summarize(WeatherFactor = round(mean(WF, na.rm=TRUE),2))

  WeatherRV$WF <- WFactor
})

output$WFChart <- renderHighchart({
  highchart() %>%
    hc_chart(type = "column") %>%
    hc_title(text = "Weather Factor for Selected Parameters") %>%
    hc_xAxis(categories = WeatherRV$WF$Month) %>%
    hc_add_series(data = WeatherRV$WF$WeatherFactor,
                  name = "Weather Factor")
})
output$WFVDPA <- renderDT({
  datatable(WeatherRV$WF, options = list(deferRender = TRUE, scroller = FALSE, scrollX = TRUE), rownames = FALSE)
})
