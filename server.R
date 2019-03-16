
function(input, output, session) {
  
  ####################################################################################################
  
  #Read working data
  env <- reactiveFileReader(
    intervalMillis = 6*60*60*1000,
    session = session,
    filePath = config$workingdatadir,
    readFunc = LoadToEnvironment
  )
  V_Fact_OnhireDays <- reactive( env()[[names(env())[1]]])
  V_Dim_CargoAndGrade <- reactive( env()[[names(env())[2]]])
  V_Fact_VoyPNL <- reactive( env()[[names(env())[3]]])
  V_Dim_Vessel <- reactive( env()[[names(env())[4]]])
  V_Dim_PortActivity <- reactive( env()[[names(env())[5]]])
  V_Dim_CargoHandlingEvent <- reactive( env()[[names(env())[6]]])
  V_Dim_Ports <- reactive({
    vdp <- env()[[names(env())[7]]]
    vdp$Region <- PortRegion$Region[match(vdp$PortName,PortRegion$PortName)]
    vdp$Area[vdp$Area == 'NW Europe'] <- 'NW EUROPE'
    vdp
  })
  V_Dim_Voyage <- reactive( env()[[names(env())[8]]])
  V_Fact_PortActivity <- reactive(env()[[names(env())[9]]])
  EDW <- reactive( env()[[names(env())[10]]])
  V_Fact_VesselItinerary_Vsched <- reactive( env()[[names(env())[11]]])
  V_Dim_AddressBook_CharterersOnly <- reactive( env()[[names(env())[12]]])
  Mashup <- reactive( env()[[names(env())[13]]])
  V_Dim_Cargo <- reactive( env()[[names(env())[14]]])
  ####################################################################################################
  
  #Reactively load WVD
  envDraft <- reactiveFileReader(
    intervalMillis = 30*60*1000,
    session = session,
    filePath = config$compfleetpath,
    readFunc = LoadToEnvironment
  )
  
  Draft <- reactive({
    Dft <- envDraft()[[names(envDraft())[1]]]
    Dft$LOA <- as.numeric(Dft$LOA)
    Dft$draught <- as.numeric(Dft$draught)
    Dft$lat <- as.numeric(Dft$lat)
    Dft$lon <- as.numeric(Dft$lon)
    Dft$sogKts <- as.numeric(Dft$sogKts)
    Dft$cog <- as.numeric(Dft$cog)
    colnames(Dft)[2] <- "VslType"
    colnames(Dft)[10] <- "Owner"
    Dft$Name <- toupper(Dft$Name)
    for (i in 1:nrow(Dft)) {
      Dft$LegS[i] <- ifelse(Dft$Leg[i] == "Ballast", '<img src="img/Ballast.png" height="20px" width="20px"></img>',
                              '<img src="img/Laden.png" height="20px" width="20px"></img>')
    }
    Dft$Operator <- wvd()$`Commercial Operator`[match(Dft$mmsi, wvd()$MMSI)]
    Dft$IMO <- wvd()$`IMO No.`[match(Dft$mmsi, wvd()$MMSI)]
    Dft$Built <- wvd()$Built[match(Dft$mmsi, wvd()$MMSI)]
    Dft$IMOType <- wvd()$`IMO Type`[match(Dft$mmsi, wvd()$MMSI)]
    Dft$IMOType[Dft$IMOType %!in% c('IMO I', 'IMO II', 'IMO III')] <- "Not Available"
    Dft$CargoType <- wvd()$Type[match(Dft$mmsi, wvd()$MMSI)]
    Dft
  })
  #########################################################################
  #Read Bizlem tonnage data every hour
  envBiz <- reactiveFileReader(
    intervalMillis = 60*60*1000, #Read every hour
    session = session,
    filePath = config$bizton,
    readFunc = read_csv
  )
  BizTon <- reactive({
    BT <- envBiz()
    BT
  })
  #######################################################################
  #########################################################################
  #Read Bizlem Spot data every hour
  envBizSpot <- reactiveFileReader(
    intervalMillis = 60*60*1000, #Read every hour
    session = session,
    filePath = config$bizspot,
    readFunc = read_csv
  )
  BizSpot <- reactive({
    SP <- envBizSpot()
    SP
  })
  #######################################################################
  #Read Bizlem TCR data every hour
  envBizTCR <- reactiveFileReader(
    intervalMillis = 60*60*1000, #Read every hour
    session = session,
    filePath = config$biztcr,
    readFunc = read_csv
  )
  BizTCR <- reactive({
    TCR <- envBizTCR()
    TCR
  })
  #######################################################################
  #Read Bizlem BTCR data every hour
  envBizBTCR <- reactiveFileReader(
    intervalMillis = 60*60*1000, #Read every hour
    session = session,
    filePath = config$bizbtcr,
    readFunc = read_csv
  )
  BizBTCR <- reactive({
    BTCR <- envBizBTCR()
    BTCR
  })
  #########################################################
  envWVD <- reactiveFileReader(
    intervalMillis = 60*60*1000, #Read every hour
    session = session,
    filePath = '/home/data/wvd.Rds',
    readFunc = readRDS
  )
  wvd <- reactive({
    WVD <- envWVD()
    WVD$VesselDetails <- paste0("<div style = 'font-size:12px;float:left'>
            <span style = 'font-size:16px;font-weight:bold'>",WVD$Name,"</span><br/>",
                                "<br/><span style = 'font-size:10px'>Year Built:",WVD$Built,"</span><br/>",
                                "<span style = 'font-size:10px'>DWT:",WVD$Dwt,"</span><br/>",
                                "<span style = 'font-size:10px'>Cubic:",WVD$Cubics,"</span><br/>",
                                "<span style = 'font-size:10px'>Ice Class:",WVD$`Ice Class`,"</span><br/>",
                                "<span style = 'font-size:10px'>IMO:",WVD$`IMO No.`,"</span><br/>",
                                "<span style = 'font-size:10px'>Ship Type:",WVD$`Vessel Type`,"</span><br/>",
                                "<span style = 'font-size:10px'>Owner:",WVD$`Owner Group`,"</span>
        </div>")
    
    WVD$Age <- year(Sys.Date())-WVD$Built
    WVD$AgeClass <- ifelse(WVD$Age<=5, "0-5 Years Old",
                           ifelse(WVD$Age<=10 & WVD$Age>5, "6-10 Years Old",
                                  ifelse(WVD$Age<=15 & WVD$Age>10, "11-15 Years Old",
                                         ifelse(WVD$Age<=20 & WVD$Age>15, "16-20 Years Old", "Above 20 Years Old")) ))
    
    WVD <- WVD %>% filter(`Vessel Type` %!in% c('Kamsarmax', 'Ultramax'))
    WVD <- WVD[!duplicated(WVD$MMSI),]
    WVD$Cubics <- as.numeric(WVD$Cubics)
    WVD$MMSI <- as.numeric(WVD$MMSI)
    WVD
  })
  #######################################################################
  EDWlatest <- reactive(EDW() %>% group_by(ves_code) %>% filter(voy_no_int == max(voy_no_int)) %>%
    filter(ord_no_int == max(ord_no_int)) %>%
    dplyr::select(ves_code,HSFO = bnkr_arr_0, LSIFO = bnkr_arr_1, LSMGO = bnkr_arr_2, MGO = bnkr_arr_3,
                  func, p_depart_date, p_arr_date,ord_no_int) %>% ungroup())

  #######################################################################

  # Join Vessel Info from Stratum with V_Dim_Vessel
  stratum_vdimvessel <- reactive(vessels %>% left_join(V_Dim_Vessel(), by = c("imo" = "imo_no")) %>%
    dplyr::select(boatName,VesselName,VesselType,IceClass,Vessel_LastDryDock_Date,Dim_Vessel_Id,
                  Vessel_NextDryDock_Date,Vessel_NextSurvey_Date,Vessel_Next_Inspection_Date,
                  Vessel_Last_Polish_Date,Vessel_LastCleaning_Date,VesselCode,lat,lon,cog,sog,mmsi))

  ################################################
  
  user <- reactive({
    if (!is.null(session$user)) session$user
    else GetUserName()
  })
  
  UserMeta <- reactiveValues()
  groups <- reactive({
    if (!is.null(session$groups)) groupnames <- session$groups
    else groupnames <- 'normal'
    UserMeta$Group <- groupnames
    groupnames
  })
  
  output$AdminUI <- renderText({
    groups()
      
  })
  ######################################
  # Voyages with no estimates
  CIDs <- c(266,611,761,629,1075,842,671)

    #######################
    # Extract next destination, ROB and ETA from Veslink API & Sea Routes for next destination
    fetch_veslink_metadata <- function(vesselCode) {
      tryCatch({
        if (MDIS_CACHE) {
          file <- paste0(cache_dir, vesselCode, "_fetch_veslink_metadata.Rds")
          if (file.exists(file) && file.info(file)$mtime > yesterday()) {
            return(readRDS(file))
          } else {
            save_result_to <- file
          }
        } else {
          save_result_to <- NULL
        }

        burl <- "https://api.veslink.com/v1/forms"
        apiToken <- config$apiToken
        startDate <- Sys.Date() - 30
        endDate <- Sys.Date()
        formStatus <- "All"
        formsData <- data.frame(PortName = vector("character"),
                                DistToGo = vector("numeric"),
                                ETA = vector("character"),
                                Vessel = vector("character"),
                                HSFO = vector("numeric"),
                                LSIFO = vector("numeric"),
                                LSMGO = vector("numeric"),
                                MGO = vector("numeric"))
        forms <- GET(burl, query = list(
          apiToken = apiToken,
          startDate = startDate,
          endDate = endDate,
          vesselCode = vesselCode,
          formStatus = formStatus)
        )
        parsed <- read_xml(forms) %>%
          xml_find_first(".//UpcomingPort")
        if (length(parsed) > 0) {
          port <- xml_attrs(parsed) %>% as.list() %>% as_tibble() %>%
            dplyr::select(PortName, DistToGo, ETA) %>% mutate(Vessel = vesselCode)

          forob <- xml_find_first(read_xml(forms), './/FOROB')
          oldw <- getOption("warn")
          options(warn = -1)
          robs = xml_find_all(forob, './Robs/Rob') %>%
            xml_attrs() %>% map(~as.data.frame(as.list(.))) %>%
            bind_rows() %>%
            dplyr::select(FuelType, Remaining) %>%
            spread(FuelType, Remaining)
          options(warn = oldw)
          if (length(colnames(robs)) < 4) { #Some vessels don't return MGO
            robs <-  mutate(robs, MGO = 0)
          }
          colnames(robs) <- c("HSFO", "LSIFO", "LSMGO", "MGO")

          # Put it all together
          result <- rbind(formsData, bind_cols(port, robs))

          if (!is.null(save_result_to)) {
            saveRDS(result, file)
          }

          return(result)
        }
      },error = function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }

    ##################################################################
    fetch_stratum_data <- function(ShipID) {
      tryCatch({
        if (MDIS_CACHE) {
          file <- paste0("data/cache/", ShipID, "_fetch_stratum_data.Rds")
          if (file.exists(file) && file.info(file)$mtime > yesterday()) {
            return(readRDS(file))
          } else {
            save_result_to <- file
          }
        } else {
          save_result_to <- NULL
        }

        MMSI <- unique(UniqueVessels() %>% filter(Dim_Vessel_Id == ShipID) %>% pull(mmsi))
        timeframe <- Sys.Date() - 10
        if(config$ds == 'local') {result <- tbl(pool, "WVDList") %>% select(mmsi, timestamp, lat, lon) %>%
          filter(mmsi == MMSI) %>% filter(!is.na(lat)) %>%
          filter(timestamp >= timeframe) %>% collect()
        } else {
          tf <- as.numeric(gsub('-','', as.character(timeframe)))
          result <- tbl(pool, "WVDListFull") %>% select(mmsi, timestamp, lat, lon, date) %>%
            filter(mmsi==MMSI) %>% filter(!is.na(lat)) %>%
            filter(date >= tf) %>% collect()
          result$mmsi <- as.numeric(result$mmsi)
          result$lat <- as.numeric(result$lat)
          result$lon <- as.numeric(result$lon)
          result$timestamp <- ymd_hms(result$timestamp)
        }
        if (!is.null(save_result_to)) {
          saveRDS(result, file)
        }

        return(result)
      },error = function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }

    #########################################################################################

    ##Fetch Voyage Data
    # fetch_voyage_data <- function(ShipId,startperiod, stopperiod) {
    # 
    #   if (MDIS_CACHE) {
    # 
    #     file <- paste0("data/cache/", ShipId, "_fetch_voyage_data.Rds")
    #     if (file.exists(file) && file.info(file)$mtime > yesterday()) {
    #       return(readRDS(file))
    #     } else {
    #       save_result_to <- file
    #     }
    #   } else {
    #     save_result_to <- NULL
    #   }
    # 
    #   MMSI <- unique(stratum_vdimvessel() %>% filter(Dim_Vessel_Id == ShipId) %>% pull(mmsi))
    #   r2 <- GET(paste0(config$stratumurl,MMSI,"/",startperiod,"/",stopperiod),
    #             add_headers(Authorization = auth_competitor))
      # text_content <- content(r2, "text")
      # result <- text_content %>% fromJSON()
      # result$Color <- ifelse(result$sogKts < 3,"red",
      #                        ifelse(result$sogKts < 8,"orange",
      #                               ifelse(result$sogKts < 12,"yellow",
      #                                      ifelse(result$sogKts < 20,"blue","green"))))
      # result$lastColor <- dplyr::lag(result$Color)
      # 
      # 
      # if (!is.null(save_result_to)) {
      #   saveRDS(result, file)
      # }
      # return(result)


    # }
    ###################################
    ###################################
  VOI <- reactive(V_Dim_Voyage() %>% filter(VoyageEstimateExists == 'Voy With No Estimate' & CompleteGMT >= (Sys.Date() - 30) |
                                   VoyageEstimateExists == 'Voy With Estimate' & CompleteGMT > (Sys.Date()-5) & CompleteGMT <= (Sys.Date() + 60),
                                 oprType != 'RELT',
                                 oprType != 'OVTO',
                                 oprType != 'TCTO',
                                 Fkey_Enum_VoyageStatus_Id != 4,
                                 Fkey_Enum_VoyageStatus_Id != 1,
                                 Fkey_Dim_Company_Id %!in% CIDs) %>%
    dplyr::select(VesselName,Dim_Voyage_Id,voyNum,Vsl_Code,
                  Fkey_Dim_Vessel_Id,Fkey_Dim_Ports_FirstLoad,Fkey_Dim_Ports_LastDischarge,
                  VoyageEstimateExists,oprType,PortOfFirstLoad_PortName,PortOfLastDischarge_Portname,
                  Estimate_PortOfFirstLoad_PortName,Estimate_PortOfLastDisch_PortName,
                  Flag_VoyageLoad_And_DischargeAreas_Equals_Estimate,
                  CommenceGMT,CompleteGMT,Fkey_Dim_Company_Id) %>%
    group_by(Fkey_Dim_Vessel_Id) %>% slice(which.max(CommenceGMT)))
  # print(nrow(VOI))
  ########################################
  noEstimate <- reactive(VOI() %>% filter(VoyageEstimateExists == 'Voy With No Estimate') %>% dplyr::select(Fkey_Dim_Vessel_Id) %>%
    left_join(V_Dim_Voyage()) %>%
    dplyr::select(Fkey_Dim_Vessel_Id,Dim_Voyage_Id, voyNum,PortOfFirstLoad_PortName,CompleteGMT,VoyageEstimateExists) %>%
    arrange(Fkey_Dim_Vessel_Id,voyNum) %>% 
    slice(which.max(CompleteGMT)))

  ########################################
  #Get last three cargoes
  Cargo <- reactive({

    CG <- V_Dim_Voyage() %>%
    filter(
      Fkey_Dim_Vessel_Id %in% VOI()$Fkey_Dim_Vessel_Id,
      oprType != 'RELT',
      oprType != 'OVTO',
      oprType != 'TCTO'
    ) %>%  dplyr::select(Fkey_Dim_Vessel_Id, Dim_Voyage_Id, voyNum, Fkey_Dim_Cargo_id) %>%
    group_by(Fkey_Dim_Vessel_Id) %>%
    top_n(6, voyNum) %>%
    arrange(Fkey_Dim_Vessel_Id) %>% group_by(Fkey_Dim_Vessel_Id) %>% arrange(desc(voyNum)) %>%
    left_join(
      V_Dim_CargoHandlingEvent(),
      by = c("Fkey_Dim_Vessel_Id" = "Fkey_Dim_Vessel_Id",
             "Dim_Voyage_Id" = "Fkey_Dim_Voyage_Id",
             "Fkey_Dim_Cargo_id" = "Fkey_Dim_Cargo_Id")
    ) %>%
    ungroup() %>%
    dplyr::select(Fkey_Dim_Vessel_Id, Dim_Voyage_Id,VoyageNo = voyNum, Cargo = cargo_short, qtyBL) 
    # print(nrow(VOI()))
  CG$Cargo <- ifelse(is.na(CG$Cargo) | CG$Cargo == "", "Data Unavailable", CG$Cargo)
  CG
  })
  ################################################################
  CargoGrade <- reactive({
    CGrade <- Cargo() %>% filter(Cargo != "Data Unavailable") %>% ungroup() %>%
      group_by(Fkey_Dim_Vessel_Id) %>% distinct(Fkey_Dim_Vessel_Id,VoyageNo, Cargo,.keep_all = TRUE) %>% top_n(1)
    CGrade$Grade <- V_Dim_CargoAndGrade()$CargoGrade_txt[match(CGrade$Cargo, V_Dim_CargoAndGrade()$Cargo_ShortName)]
    CGrade
  })

  ################################################################
  CargoMulti <- reactive(Cargo() %>% distinct(Fkey_Dim_Vessel_Id,VoyageNo, Cargo) %>%
    group_by(Fkey_Dim_Vessel_Id, VoyageNo) %>% mutate(Mcargo = paste0(Cargo, collapse = "+")) %>%
    distinct(Fkey_Dim_Vessel_Id, VoyageNo, Mcargo))

  ################################################################
  CargoM <- reactive({
    CM <- CargoMulti() %>% group_by(Fkey_Dim_Vessel_Id) %>%
    filter(Mcargo != 'Data Unavailable') %>% top_n(3, VoyageNo) %>%
    mutate(Cargo = paste0(Mcargo, collapse = "/")) %>%
    dplyr::select(Fkey_Dim_Vessel_Id,Cargo) %>% unique()

    CM$LastCargoGrade <- CargoGrade()$Grade[match(CM$Fkey_Dim_Vessel_Id, CargoGrade()$Fkey_Dim_Vessel_Id)]
    CM
  })
  
  ################################################################
  VOI2 <- reactive(VOI() %>% left_join(CargoM()))

  ################################################################

  VslVoy <- reactive(data.frame(Fkey_Dim_Vessel_Id = c(VOI2()$Fkey_Dim_Vessel_Id[VOI2()$VoyageEstimateExists == 'Voy With Estimate'],noEstimate()$Fkey_Dim_Vessel_Id),
                       Fkey_Dim_Voyage_Id = c(VOI2()$Dim_Voyage_Id[VOI2()$VoyageEstimateExists == 'Voy With Estimate'],noEstimate()$Dim_Voyage_Id),
                       Estimate = c(rep(1,length(VOI2()$Fkey_Dim_Vessel_Id[VOI2()$VoyageEstimateExists == 'Voy With Estimate'])),
                                  rep(0,length(VOI2()$Fkey_Dim_Vessel_Id[VOI2()$VoyageEstimateExists == 'Voy With No Estimate'])))))
  ################################################################
  VoyInfo <- reactive({
    VInfo <- inner_join(VslVoy(),V_Fact_VesselItinerary_Vsched()) %>%
    filter(Fkey_Dim_PortFunction_Id != -1) %>%
    left_join(portFunction, by = c("Fkey_Dim_PortFunction_Id" = "ID")) %>%
    left_join(V_Dim_Ports(), by = c("Fkey_Dim_Ports_Id" = "Dim_Ports_Id")) %>%
    dplyr::select(PortName,Function,ord_no_int,Date_Departure,Date_Arrival,BallastLaden = Fkey_Enum_BallastLaden_Id,Imos_VoyNo,sea_days,
                  port_days,Fkey_Dim_Vessel_Id,Fkey_Dim_Voyage_Id,Longitude,Latitude,Estimate) %>%
    arrange(Fkey_Dim_Voyage_Id,ord_no_int) %>% group_by(Fkey_Dim_Vessel_Id) %>%
    slice(which.max(ord_no_int)) #Check This

  for (i in 1:nrow(VInfo)) {
    Year <- year(VInfo$Date_Departure[i])
    VInfo$Date_Departure[i] <- if_else(Year == 2001,
                                         VInfo$Date_Arrival[i], VInfo$Date_Departure[i])
  }

  FullVoyInfo <- inner_join(VslVoy(),V_Fact_VesselItinerary_Vsched()) %>%
    filter(Fkey_Dim_PortFunction_Id != -1) %>%
    left_join(portFunction, by = c("Fkey_Dim_PortFunction_Id" = "ID")) %>%
    left_join(V_Dim_Ports(), by = c("Fkey_Dim_Ports_Id" = "Dim_Ports_Id")) %>%
    dplyr::select(PortName, Function,ord_no_int,Date_Departure,Date_Arrival,BallastLaden = Fkey_Enum_BallastLaden_Id,Imos_VoyNo,sea_days,
                  port_days,Fkey_Dim_Vessel_Id,Fkey_Dim_Voyage_Id,Longitude,Latitude,Estimate) %>%
    arrange(Fkey_Dim_Voyage_Id,ord_no_int) %>% group_by(Fkey_Dim_Vessel_Id)

  VOI3 <- VOI2() %>% dplyr::select(VesselName, Fkey_Dim_Vessel_Id,Cargo, LastCargoGrade, CommenceGMT, CompleteGMT)

  VInfo <- VInfo %>% left_join(VOI3)

  VslDetails <- V_Dim_Vessel() %>% dplyr::select(Dim_Vessel_Id,VesselType,dwt, IceClass,imo_no)

  VInfo <- VInfo %>% left_join(VslDetails, by = c("Fkey_Dim_Vessel_Id" = "Dim_Vessel_Id"))
  
  for (i in 1:nrow(VInfo)) {
    x <- FullVoyInfo %>% filter(Fkey_Dim_Vessel_Id == VInfo$Fkey_Dim_Vessel_Id[i] & Fkey_Dim_Voyage_Id == VInfo$Fkey_Dim_Voyage_Id[i])
    if (all(x$Estimate == 0)) {
      VInfo$CurrentStatus[i] <- "Prompt"
    }
    else{
      Where <- vector()
      for (j in 1:nrow(x)) {
        Where[j] <- ifelse((x$Date_Arrival[j] <= Sys.Date() & Sys.Date() <= x$Date_Departure[j]),1,0)
      }
      if (length(which(Where > 0)) > 0) {
        VInfo$CurrentStatus[i] <- ifelse(x$Function[which(Where > 0)] == 'Loading', "Arrived Load Port", "Arrived Discharge Port")
      }
      else{
        VInfo$CurrentStatus[i] <- ifelse(x$BallastLaden[which.min(abs(difftime(Sys.Date(),x$Date_Arrival)))] == 0, "In Ballast Leg", "In Laden Leg")
      }
    }

  }

  VInfo$`EmploymentStatus` <- "Unfixed"
  VInfo$`RepositionPort` <- VInfo$PortName
  VInfo$`RepositionDate` <- Sys.Date()
  VInfo$Comments <- ""
  VInfo$Flag <- ""
  VInfo
  })
  ################################################################
  
  FinalTable1 <- reactive({
    FinalTable <- VoyInfo() %>% ungroup() %>% dplyr::select( VoyageNo = Imos_VoyNo, `EmploymentStatus`,  OpenPort = PortName,
                                                        `OpenPortDate` = Date_Departure, Cargo, LastCargoGrade, `RepositionPort`, `RepositionDate`,
                                                        CurrentStatus, Comments, Flag,VesselName,DWT = dwt,Type = VesselType,IceClass,
                                                        CommenceGMT,CompleteGMT,Fkey_Dim_Vessel_Id,Fkey_Dim_Voyage_Id,Estimate)

  # FinalTable$UID <- ifelse(FinalTable$Estimate == 0,
  #                          paste(FinalTable$Fkey_Dim_Vessel_Id, as.integer(FinalTable$CommenceGMT), sep = "_"),
  #                          paste(FinalTable$Fkey_Dim_Vessel_Id, as.integer(FinalTable$CompleteGMT), sep = "_"))
    FinalTable$UID <- paste(FinalTable$Fkey_Dim_Vessel_Id, FinalTable$VoyageNo, sep = "_")

  FinalTable$Flag <- "No"
  FinalTable$UpdatedBy <- ""
  # saveRDS(FinalTable, 'FinalTable.Rds')
  FinalTable
  })
  
  ################################################################
  
  UniqueVessels <- reactive({
  UV <- FinalTable1() %>% 
    left_join(V_Dim_Vessel(), by = c('Fkey_Dim_Vessel_Id' = 'Dim_Vessel_Id')) %>% 
    dplyr::select(VesselName = VesselName.x,VesselType,IceClass=IceClass.x,Vessel_LastDryDock_Date,Fkey_Dim_Vessel_Id,
                  Vessel_NextDryDock_Date,Vessel_NextSurvey_Date,Vessel_Next_Inspection_Date,
                  Vessel_Last_Polish_Date,Vessel_LastCleaning_Date,VesselCode,OpenPort,OpenPortDate,
                  CurrentStatus,RepositionPort,UID) %>% 
    left_join(VOI2()) %>% distinct()
  UV$IMO <- V_Dim_Vessel()$imo_no[match(UV$Fkey_Dim_Vessel_Id, V_Dim_Vessel()$Dim_Vessel_Id)]
  
  UV$Vessel_Last_Polish_Date[UV$Vessel_Last_Polish_Date == ymd('2001-01-01')] <- NA
  UV$Vessel_LastCleaning_Date[UV$Vessel_LastCleaning_Date == ymd('2001-01-01')] <- NA
  UV$Vessel_LastDryDock_Date[UV$Vessel_LastDryDock_Date == ymd('2001-01-01')] <- NA
  UV$Vessel_Next_Inspection_Date[UV$Vessel_Next_Inspection_Date == ymd('2001-01-01')] <- NA
  UV$Vessel_NextDryDock_Date[UV$Vessel_NextDryDock_Date == ymd('2001-01-01')] <- NA
  UV$Vessel_NextSurvey_Date[UV$Vessel_NextSurvey_Date == ymd('2001-01-01')] <- NA
  
  UV$OpenPortDate <- UV$OpenPortDate + V_Dim_Ports()$TimeZone[match(UV$OpenPort, V_Dim_Ports()$PortName)]*3600
  #Get Positional Information
  UV$mmsi <- Draft()$mmsi[match(toupper(UV$VesselName), toupper(Draft()$Name))]
  UV$lat <- Draft()$lat[match(UV$mmsi, Draft()$mmsi)]
  UV$lon <- Draft()$lon[match(UV$mmsi, Draft()$mmsi)]
  UV$cog <- Draft()$cog[match(UV$mmsi, Draft()$mmsi)]
  UV$sog <- Draft()$sogKts[match(UV$mmsi, Draft()$mmsi)]
  UV <- UV %>% select(VesselName, VesselType, IceClass, Vessel_LastDryDock_Date, Dim_Vessel_Id=Fkey_Dim_Vessel_Id,
                      Vessel_NextDryDock_Date,Vessel_NextSurvey_Date,Vessel_Next_Inspection_Date,Vessel_Last_Polish_Date,
                      Vessel_LastCleaning_Date, VesselCode, lat, lon, cog, sog, mmsi, Dim_Voyage_Id, voyNum,
                      Vsl_Code, Fkey_Dim_Ports_FirstLoad, Fkey_Dim_Ports_LastDischarge, VoyageEstimateExists,
                      oprType,PortOfFirstLoad_PortName,PortOfLastDischarge_Portname,Estimate_PortOfFirstLoad_PortName,
                      Estimate_PortOfLastDisch_PortName,Flag_VoyageLoad_And_DischargeAreas_Equals_Estimate,
                      CommenceGMT,CompleteGMT,Fkey_Dim_Company_Id,Cargo,LastCargoGrade,IMO,OpenPort,
                      OpenPortDate,CurrentStatus,RepositionPort,UID)
  # saveRDS(UV,'UV.Rds')
  UV
  })

  ################################################################
  FinalTable <- reactive({
    
  # PosList <- read_csv('data/PositionList.csv')  
  # UPID <- unique(PosList$UID)
  # NUUID <- UPID[UPID %!in% FinalTable1()$UID]
  # 
  # if (length(NUUID) > 0) {
  #   PosList <- PosList %>% filter(UID %!in% NUUID)
  #   write_csv(PosList, 'data/PositionList.csv')
  # }
  
  ReadPosList <- read_csv('data/PositionList.csv')
  UPID <- unique(ReadPosList$UID)
  FT <- FinalTable1() %>% dplyr::filter(UID %!in% UPID)
  FT$ROWIDT <- seq(max(ReadPosList$ROWIDT) + 1, nrow(FT))
  FT <- select(FT, Flag, VesselName, VoyageNo, EmploymentStatus, OpenPort, OpenPortDate,
                    Cargo, RepositionPort, RepositionDate, CurrentStatus, Comments,
                    Flag,DWT, Type, IceClass, CompleteGMT, UpdatedBy,ROWIDT, everything())
  write_csv(FT, 'data/PositionList.csv', append = TRUE)
  FT
  })

  ####################################################################################
  ####################################################################################
  
  #Read certificate data
  CD <- reactiveFileReader(
    intervalMillis = 30*24*60*60*1000, #Once in 30 days
    session = session,
    filePath = config$certpath,
    readFunc = readRDS
  )

  certData <- reactive({
    df <- CD()
    df$VesselCode <- UniqueVessels()$VesselCode[match(toupper(df$Vessel), toupper(UniqueVessels()$VesselName))]
    df
  })
  
  #Read vetting data
  VD <- reactiveFileReader(
    intervalMillis = 30*24*60*60*1000, #Once in 30 days
    session = session,
    filePath = config$vettpath,
    readFunc = readRDS
  )
  vettData <- reactive({
    df <- VD()
    df$VesselCode <- UniqueVessels()$VesselCode[match(toupper(df$Vessel), toupper(UniqueVessels()$VesselName))]
    df
  })

  ####################################################################################
  ####################################################################################
  ####################################################################################
  #Update all UIs with dependencies
  output$VslSOF <- renderUI({
    pickerInput('VslSOF','Select Vessel',
                choices = levels(as.factor(UniqueVessels()$VesselName)),
                options = list(`actions-box` = TRUE, `live-search` = TRUE))
  })

  output$PortSOF <- renderUI({
    pickerInput("PortSOF", 'Select Port',
                choices = levels(as.factor(V_Dim_Ports()$PortName)),
                options = list(`actions-box` = TRUE, `live-search` = TRUE))
  })

  output$gradeFilter <- renderUI({
    pickerInput('gradeFilter', label = "Filter by last cargo grade",
                choices = levels(as.factor(V_Dim_CargoAndGrade()$CargoGrade_txt[V_Dim_CargoAndGrade()$CargoClass == "Liquid"])),
                multiple = TRUE, options = list(`actions-box` = TRUE),
                selected = levels(as.factor(V_Dim_CargoAndGrade()$CargoGrade_txt[V_Dim_CargoAndGrade()$CargoClass == "Liquid"])))
  })

  observeEvent(input$showFiltersPosList, {
    shinyjs::hide(id = "hiddenPosList")
    shinyjs::show(id = "visiblePosList")
    shinyjs::show(id = "filtersPosList")
  })

  observeEvent(input$hideFiltersPosList, {
    shinyjs::hide(id = "visiblePosList")
    shinyjs::show(id = "hiddenPosList")
    shinyjs::hide(id = "filtersPosList")
  })

  #No. of days in header
  voydays <- reactive(input$voydays) %>% debounce(650)
  output$num_days_out <- renderText({
    voydays()
  })

  #No. of days in Position List
  voydaysPL <- reactive(input$voydaysPL) %>% debounce(650)

  ShipID <- reactiveVal(NULL)
  observe({
    shinyjs::toggle(selector = ".needs_vessel_selection", condition = !is.null(ShipID()))
  })



  ######################################
  valPLD <- reactiveValues()
  PosListData <- reactive({
    # saveRDS(UniqueVessels(), 'UV.Rds')
    UV <- req(UniqueVessels())
    # print(nrow(UniqueVessels()))
    
    # UV <- UV2 %>% dplyr::filter(CompleteGMT <= (Sys.Date() + voydays()))
    # UV <- UV2[UV2$CompleteGMT <= (Sys.Date() + voydays()),]
    # UV2 <- UV %>% filter(CompleteGMT <= (Sys.Date() + 10))
    UV$OpenDate <- substr(UV$OpenPortDate, 1, 10)
    UV$OpenTime <- substr(UV$OpenPortDate, 12, 19)
    UV$Area <- V_Dim_Ports()$Area[match(UV$RepositionPort, V_Dim_Ports()$PortName)]
    # UV$YearBuilt <- wvd()$Built[match(toupper(UV$VesselName), wvd()$Name)]
    # UV$DWT <- wvd()$Dwt[match(toupper(UV$VesselName), wvd()$Name)]
    # UV$Cubics <- wvd()$Cubics[match(toupper(UV$VesselName), wvd()$Name)]
    UV$YearBuilt <- wvd()$Built[match(UV$IMO, wvd()$`IMO No.`)]
    UV$DWT <- wvd()$Dwt[match(UV$IMO, wvd()$`IMO No.`)]
    UV$Cubics <- wvd()$Cubics[match(UV$IMO, wvd()$`IMO No.`)]
    UV$EmploymentStatus <- ""
    # print(nrow(UniqueVessels()))
    # print(nrow(UV))
    # saveRDS(UV, 'Uv.Rds')
    # print(is.null(valPLD$rv))
    # if (!is.null(valPLD$rv)) {
    #   #Common UIDs which are not to be changed
    #   UIDs <- UV$UID[UV$UID %in% unique(valPLD$rv$UID)]
    #   # print(UIDs)
    #   UVTemp <- UV
    #   UVTemp$RepositionPort[UV$UID %in% UIDs] <- valPLD$rv$RepositionPort[valPLD$rv$UID %in% UIDs]
    #   UVTemp$EmploymentStatus[UV$UID %in% UIDs] <- valPLD$rv$EmploymentStatus[valPLD$rv$UID %in% UIDs]
    #   valPLD$rv <- UVTemp
    #   print(valPLD$rv$EmploymentStatus[1])
    # }
    # else 
    # print(is.null(valPLD$rv))
    valPLD$rv <- UV
    # saveRDS(UV, 'data/PLD.Rds')
    UV
  })

  
  output$vtype <- renderUI({
    req(PosListData())
    pickerInput('vtype','Select Vessel Type',
                choices = levels(as.factor(PosListData()$VesselType)),
                selected = levels(as.factor(PosListData()$VesselType)),
                options = list(`actions-box` = TRUE),
                multiple = TRUE)
                
  })
  ############################
  #Filters dependent on wvd()
  output$DWPTL <- renderUI({
    sliderInput('DWTPL',"Filter by DWT",value = c(min(wvd()$Dwt, na.rm = T),max(wvd()$Dwt, na.rm = T)),
                min = min(wvd()$Dwt, na.rm = T),max = max(wvd()$Dwt, na.rm = T))
  })
  output$CubicsPL <- renderUI({
    sliderInput('CubicsPL',"Filter by Cubics",value = c(min(wvd()$Cubics, na.rm = T),max(wvd()$Cubics, na.rm = T)),
                min = min(wvd()$Cubics, na.rm = T),max = max(wvd()$Cubics, na.rm = T))
  })

  data <- reactive({
    req(valPLD$rv)
    # print(valPLD$rv$EmploymentStatus[1])
    # print(valPLD$rv$RepositionPort[1])
    # saveRDS(valPLD$rv, "PS.Rds")
    # UV <- UV2[UV2$CompleteGMT <= (Sys.Date() + voydays()),]
    # print(UV$EmploymentStatus[1])
    x <- valPLD$rv %>% filter(CompleteGMT <= (Sys.Date() + voydays())) %>% 
      filter(VesselType %in% input$vtype) %>%
           filter(Area %in% c(input$areaFilter, '--')) %>%
           filter(DWT >= input$DWTPL[1] & DWT <= input$DWTPL[2]) %>%
           filter(Cubics >= input$CubicsPL[1] & Cubics <= input$CubicsPL[2] ) %>%
           filter(YearBuilt >= as.numeric((lubridate::year(Sys.Date()) - input$builtFilter))) %>%
           filter(LastCargoGrade %in% c(input$gradeFilter, NA, ""))

  x <- x %>% select(VesselType,VesselName = VesselName, YearBuilt, DWT, Cubics,OpenPort, OpenDate, RepositionPort,
                    Cargo,CurrentStatus, EmploymentStatus, Area,
                    IceClass, LastDryDock = Vessel_LastDryDock_Date,
                    NextDryDock = Vessel_NextDryDock_Date, LastPolish = Vessel_Last_Polish_Date,LastCleaning = Vessel_LastCleaning_Date,
                    VoyNum = voyNum, FirstLoadPort = PortOfFirstLoad_PortName, LastDischargePort = PortOfLastDischarge_Portname,  LastCargoGrade,
                    OpenPortDate, OpenTime,
                      lat, lon, CompleteGMT, cog, Dim_Vessel_Id, VesselCode,UID)
  # saveRDS(x, "x.Rds")
  x
  })

  # #################################################################
  # # Position List
  # #################################################################

  sPosListData <- SharedData$new(data,key = ~lat, group = "pl")
  
  output$posTable <- DT::renderDT({
    req(sPosListData)
  datatable(sPosListData, rownames =  FALSE,
  extensions = 'Buttons', options = list(pageLength = 50,scrollX = TRUE, dom = 'Blfrtip', 
                                       buttons = c('copy', 'csv', 'excel', 'pdf', 'print',I('colvis')),
                                       columnDefs = list(list(visible = FALSE, targets = c(12:29))))

  )
  },server = FALSE)

  output$posTable2 <- DT::renderDT({
  datatable(sPosListData, rownames = FALSE,
  extensions = 'Buttons', options = list(pageLength = 50,scrollX = TRUE, dom = 'Blfrtip', 
                                                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print',I('colvis')),
                                                   columnDefs = list(list(visible = FALSE, targets = c(12:29))))
    )
  },server = FALSE)
  
##############################################################################
  
  output$distribution <- renderHighchart({
    req(sPosListData$origData())
    df <- sPosListData$origData()
    if (nrow(df) < 1) return(NULL)
    distri <- df %>% group_by(CurrentStatus) %>% summarise(Count = n()) %>% arrange(desc(Count))
    
    hchart(df$CurrentStatus, colorByPoint = TRUE, name = "Status")
  })

  #################################################################
  # Plot ship   positions on leaflet map
  output$posListMap <- renderLeaflet({
   req(sPosListData)
    if (nrow(sPosListData$origData()) < 1) return(NULL)
    shipIcons <- icons(
      iconUrl =
        ifelse(data()$CompleteGMT <= Sys.Date(), "www/img/10.png",
        ifelse(data()$CompleteGMT <= (Sys.Date() + 6), "www/img/7.png",
        "www/img/5.png")),
      iconWidth = 11, iconHeight = 24, iconAnchorX = 5, iconAnchorY = 0
    )

    leaflet(sPosListData) %>%
      addTiles() %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      registerPlugin(rotatedMarkerPlugin) %>%
      addMarkers(lng = ~lon, lat = ~lat, label = ~VesselName,
                 labelOptions = labelOptions(
                   textOnly = TRUE,
                   noHide = FALSE
                 ),
                 icon = shipIcons,
                 options = markerOptions(rotationAngle = ~cog),
                 group = "ships_pos", 
                 layerId = ~Dim_Vessel_Id) %>%
      addFullscreenControl() %>%
      addMouseCoordinates(style = 'basic') %>%
      addResetMapButton() %>%
      addSearchFeatures(targetGroups = "ships_pos",
                        options = searchFeaturesOptions(zoom = 10, openPopup = TRUE,
                                                        hideMarkerOnCollapse = TRUE)) 
  })

  output$posListMap2 <- renderLeaflet({
    req(sPosListData)
    if (nrow(sPosListData$origData()) < 1) return(NULL)
    shipIcons <- icons(
      iconUrl =
        ifelse(data()$CompleteGMT <= Sys.Date(), "www/img/10.png",
               ifelse(data()$CompleteGMT <= (Sys.Date() + 6), "www/img/7.png",
                      "www/img/5.png")),
      iconWidth = 11, iconHeight = 24, iconAnchorX = 5, iconAnchorY = 0
    )
    leaflet(sPosListData) %>%
      addTiles() %>%
      addProviderTiles(providers$Esri.WorldTopoMap) %>%
      registerPlugin(rotatedMarkerPlugin) %>%
      addMarkers(lng = ~lon, lat = ~lat, label = ~VesselName,
                 labelOptions = labelOptions(
                   textOnly = TRUE,
                   noHide = FALSE
                 ),
                 icon = shipIcons,
                 options = markerOptions(rotationAngle = ~cog),
                 group = "ships_pos",
                 layerId = ~Dim_Vessel_Id) %>%
      addFullscreenControl() %>%
      addMouseCoordinates(style = 'basic') %>%
      addResetMapButton() %>%
      addSearchFeatures(targetGroups = "ships_pos",
                        options = searchFeaturesOptions(zoom = 10, openPopup = TRUE,
                                                        hideMarkerOnCollapse = TRUE)) 
  })

  #################################################################
  # Extract the unique ID of the ship that was clicked on
  
  observeEvent(input$posListMap_marker_click, {
    ShipID(input$posListMap_marker_click$id)
  })

  observeEvent(input$posListMap2_marker_click, {
    ShipID(input$posListMap2_marker_click$id)
  })

  #################################################################
  # Get the last 3 cargoes of the selected vessel
  ShipCargo <- reactive({
    req(ShipID())
    VDV <- V_Dim_Voyage() %>%
      filter(
        Fkey_Dim_Vessel_Id == ShipID(),
        oprType != 'RELT',
        oprType != 'OVTO',
        oprType != 'TCTO'
      ) %>%
      dplyr::select(Fkey_Dim_Vessel_Id, Dim_Voyage_Id, voyNum, Fkey_Dim_Cargo_id) %>%
      group_by(Fkey_Dim_Vessel_Id) %>%
      top_n(6, voyNum) %>%
      arrange(Fkey_Dim_Vessel_Id) %>%
      left_join(
        V_Dim_CargoHandlingEvent(),
        by = c("Fkey_Dim_Vessel_Id" = "Fkey_Dim_Vessel_Id",
               "Dim_Voyage_Id" = "Fkey_Dim_Voyage_Id",
               "Fkey_Dim_Cargo_id" = "Fkey_Dim_Cargo_Id")
      ) %>%
      ungroup() %>%
      dplyr::select(VoyageNo = voyNum, Cargo = cargo_short, BLCode = blCode, Fkey_Dim_Cargo_id) %>%
      distinct(VoyageNo, Cargo, .keep_all = TRUE) %>%
      arrange(VoyageNo)
    VDV$Charterer <- V_Dim_Cargo()$charterer[match(VDV$Fkey_Dim_Cargo_id, V_Dim_Cargo()$Dim_Cargo_Id)]
    # If Cargo blank or NA
    VDV$Cargo <- ifelse(is.na(VDV$Cargo) | VDV$Cargo == "",
                         "Data Unavailable", VDV$Cargo)
    VDV <- VDV %>% filter(Cargo != "Data Unavailable")
    VDV$CargoGrade <- V_Dim_CargoAndGrade()$CargoGrade_txt[match(VDV$Cargo, V_Dim_CargoAndGrade()$Cargo_ShortName)]
    VDV
  })
#####################################################################
  # Combine all API calls into one process for speed efficiency
  fetch_data_from_api <- reactive({
    req(ShipID())
    id <- ShipID()
    vesselCode <- isolate(data()) %>% filter(Dim_Vessel_Id == id) %>% pull(VesselCode)
    lat <- isolate(data()) %>% filter(Dim_Vessel_Id == id) %>% pull(lat)
    lon <- isolate(data()) %>% filter(Dim_Vessel_Id == id) %>% pull(lon)
    shinyjs::show("page_shield")
    future({
      list(
        data = fetch_stratum_data(id),
        metadata = fetch_veslink_metadata(vesselCode),
        fuelstatus = EDWlatest() %>% filter(ves_code == vesselCode),
        CData = certData() %>% filter(VesselCode == vesselCode) %>% filter(!is.na(IsPublished)) %>%
          filter(Certificate %in% c("USCG Certificate of Compliance (USCGCOC)", "Certificate of Class (COC)")),
        vettData = vettData() %>% filter(VesselCode == vesselCode) %>%
        dplyr::select(Approval, Basis, Status, InspectionDate, ExpireDate, PortName),
        hull = UniqueVessels() %>% filter(Vsl_Code == vesselCode) %>% dplyr::select(`Last Dry Dock` = Vessel_LastDryDock_Date,
                                                                         `Next Dry Dock Date` = Vessel_NextDryDock_Date,
                                                                         `Next Survey Date` = Vessel_NextSurvey_Date,
                                                                         `Next Inspection Date` = Vessel_Next_Inspection_Date,
                                                                           `Last Polish Date` = Vessel_Last_Polish_Date,
                                                                         `Last Cleaning Date` = Vessel_LastCleaning_Date)
      )
    })
  })

  #################################################################
  # Extract next destination, ROB and ETA from Veslink API
  formsData <- reactive({
    then(fetch_data_from_api(),
         onFulfilled = function(value) {
           shinyjs::hide("page_shield")
           value$metadata
         })
  })



  #################################################################
  # Extract ROB on arrival from EDW table
  ROBData <- reactive({
    then(fetch_data_from_api(),
         onFulfilled = function(value) {
           shinyjs::hide("page_shield")
           value$fuelstatus
         })
  })

  #################################################################
  # Extract Certficate Data
  CertData <- reactive({
    then(fetch_data_from_api(),
         onFulfilled = function(value) {
           shinyjs::hide("page_shield")
           value$CData
         })
  })

  #################################################################
  # Extract Vetting Data
  VettData <- reactive({
    then(fetch_data_from_api(),
         onFulfilled = function(value) {
           shinyjs::hide("page_shield")
           value$vettData
         })
  })

  #################################################################
  # Extract Hull Condition Data
  hullData <- reactive({
    then(fetch_data_from_api(),
         onFulfilled = function(value) {
           shinyjs::hide("page_shield")
           value$hull
         })
  })

  #################################################################
  # 5 day data from Stratum of selected vessel
  indVsl <- reactive({
    then(fetch_data_from_api(),
         onFulfilled = function(value) {
           shinyjs::hide("page_shield")
           
           value$data
         })
  })

  #################################################################
  # Show vessel's name
  output$shipName <- renderText({
    req(ShipID())
    req(data())
    isolate({
        shipName <- data() %>%
        filter(Dim_Vessel_Id == ShipID()) %>%
        pull(VesselName)
      shipName
    })
  })
  
  #Show vessel's name on cargo
  output$shipNameCargo <- renderText({
    req(ShipID())
    req(data())
    isolate({
      shipName <- data() %>%
        filter(Dim_Vessel_Id == ShipID()) %>%
        pull(VesselName)
      shipName
    })
  })
  
  output$shipNameCargo2 <- renderText({
    req(ShipID())
    req(data())
    isolate({
      shipName <- data() %>%
        filter(Dim_Vessel_Id == ShipID()) %>%
        pull(VesselName)
      shipName
    })
  })

  ################################################################
  # Render Cargo as a table
  output$cargoDT <- renderDataTable({
    req(ShipCargo())

    CARGO <- ShipCargo() %>%
      group_by(VoyageNo) %>%
      mutate(Cargoes = paste0(Cargo, collapse = "+")) %>%
      dplyr::select(VoyageNo, Cargoes, Charterer, `Addl. Cargo details`=BLCode, CargoGrade) %>%
      unique()
    CARGO <- utils::tail(CARGO, 3)
  datatable(CARGO, rownames = FALSE,options = list(pageLength = 5,scrollX = TRUE, searching = FALSE, lengthChange = FALSE))
  })

  output$cargoDT2 <- renderDataTable({
    req(ShipCargo())

    CARGO <- ShipCargo() %>%
      group_by(VoyageNo) %>%
      mutate(Cargoes = paste0(Cargo, collapse = "+")) %>%
      dplyr::select(VoyageNo, Cargoes, Charterer, `Addl. Cargo details`=BLCode, CargoGrade) %>%
      unique()
    CARGO <- utils::tail(CARGO, 3)
    datatable(CARGO, rownames = FALSE,options = list(pageLength = 5,scrollX = TRUE, searching = FALSE, lengthChange = FALSE))
  })

  #ROB Information

  output$ROBDTArrival <- renderTable({
    req(ROBData())
    then(ROBData(),
         onFulfilled = function(value) {
           value %>% dplyr::select(HSFO, LSIFO, LSMGO, MGO)
         })

  })

  output$CertificateData <- renderDT({
    req(CertData())
    datatable(then(CertData(),
         onFulfilled = function(value) {
           value %>% dplyr::select(Certificate, Expires)
         }), options = list(pageLength = 5,scrollX = TRUE))

  })

  output$VettingData <- renderDT({
    req(VettData())
    datatable(then(VettData(),
                   onFulfilled = function(value) {
                     value
                   }), options = list(pageLength = 5,scrollX = TRUE))


  })

  output$ROB_headed <- renderText({
    req(formsData())
    then(formsData(),
         onFulfilled = function(value) {
           value$PortName
         })

  })
  output$ROB_headed2 <- renderText({
    req(formsData())
    then(formsData(),
         onFulfilled = function(value) {
           value$PortName
         })

  })
  output$ROB_reach <- renderText({
    req(formsData())
    then(formsData(),
         onFulfilled = function(value) {
           as.character(with_tz(ymd_hms(value$ETA))) #Better date time readability
         })
  })
  output$ROB_distance <- renderText({
    req(formsData())
    then(formsData(),
         onFulfilled = function(value) {
           value$DistToGo
         })
  })

  ################################################################

  # Get Hull Condition Details
  output$HullDetails <- renderUI({
      req(hullData())
      then(hullData(),
           onFulfilled = function(value) {
             lapply(
               names(value),
               function(param) {
                 div(tags$strong(param, "-"), value[[param]])
               }
             )
           })
})

  #################################################################
  # Show vessel's path
  output$shipmap <- renderLeaflet({
    req(indVsl())
    req(formsData())
    toLat <- V_Dim_Ports()$Latitude[match(formsData()$PortName, V_Dim_Ports()$PortName)]
    toLon <- V_Dim_Ports()$Longitude[match(formsData()$PortName, V_Dim_Ports()$PortName)]
    fromLat <- indVsl()$lat[indVsl()$timestamp == max(indVsl()$timestamp)]
    fromLon <- indVsl()$lon[indVsl()$timestamp == max(indVsl()$timestamp)]
    routes <- as.data.frame(get_sea_routes(fromLat,fromLon,toLat,toLon))
    
    then(indVsl(),
         onFulfilled = function(prom) {
           # Need to convert the lat-long data to a format that plays well with leafet JS
           data_js <- lapply(seq_along(prom$lon), function(x) c(prom$lat[x], prom$lon[x]))
if (nrow(routes) > 0) {
           leaflet(prom) %>%
             addTiles() %>%
             registerPlugin(rotatedMarkerPlugin) %>%
             registerPlugin(polylineDecoratorPlugin) %>%
             addPolylines(lng = ~lon, lat = ~lat, weight = 4, color = "blue") %>%
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
             addPolylines(lng = routes$lon, lat = routes$lat,dashArray = "5, 5",color = 'red')
}
           else {
             leaflet(prom) %>%
               addTiles() %>%
               registerPlugin(rotatedMarkerPlugin) %>%
               registerPlugin(polylineDecoratorPlugin) %>%
               addPolylines(lng = ~lon, lat = ~lat, weight = 4, color = "blue") %>%
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
                      }", data = data_js)
           }
         })
  })

  output$shipmap2 <- renderLeaflet({
   
    req(indVsl())
    req(formsData())
    toLat <- V_Dim_Ports()$Latitude[match(formsData()$PortName, V_Dim_Ports()$PortName)]
    toLon <- V_Dim_Ports()$Longitude[match(formsData()$PortName, V_Dim_Ports()$PortName)]
    fromLat <- indVsl()$lat[indVsl()$timestamp == max(indVsl()$timestamp)]
    fromLon <- indVsl()$lon[indVsl()$timestamp == max(indVsl()$timestamp)]
    routes <- as.data.frame(get_sea_routes(fromLat,fromLon,toLat,toLon))
   
    then(indVsl(),
         onFulfilled = function(prom) {
           # Need to convert the lat-long data to a format that plays well with leafet JS
           data_js <- lapply(seq_along(prom$lon), function(x) c(prom$lat[x], prom$lon[x]))
           if (nrow(routes) > 0) {
             leaflet(prom) %>%
               addTiles() %>%
               registerPlugin(rotatedMarkerPlugin) %>%
               registerPlugin(polylineDecoratorPlugin) %>%
               addPolylines(lng = ~lon, lat = ~lat, weight = 4, color = "blue") %>%
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
               addPolylines(lng = routes$lon, lat = routes$lat,dashArray = "5, 5",color = 'red')
           }
           else {
             leaflet(prom) %>%
               addTiles() %>%
               registerPlugin(rotatedMarkerPlugin) %>%
               addPolylines(lng = ~lon, lat = ~lat, weight = 4, color = "blue") %>%
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
                      }", data = data_js)
           }
         })
  })

  #########################################################
  #FLIC Filters
  output$vsltypeFLIC <- renderUI({
    pickerInput('vsltypeFLIC',"Filter by vessel type", choices = levels(as.factor(Draft()$VslType)),
                multiple = TRUE, options = list(`actions-box` = TRUE), selected = c("Handy", "LR1",
                                                                                    "LR2", "MR"))
  })
  output$loafilter <- renderUI({
    sliderInput('loafilter',label = "Filter by LOA",min = min(Draft()$LOA, na.rm = T), max = max(Draft()$LOA, na.rm = T),
                value = c(min(Draft()$LOA, na.rm = T),max = max(Draft()$LOA, na.rm = T)))
  })
  output$GTfilter <- renderUI({
    sliderInput('GTfilter',label = "Filter by Gross Tonnage",min = min(Draft()$GT, na.rm = T),
                max = max(Draft()$GT, na.rm = T),
                value = c(min(Draft()$GT, na.rm = T),max(Draft()$GT, na.rm = T)))
  })
  output$Ownerfilter <- renderUI({
    pickerInput('Ownerfilter', "Filter by Commercial Operator", choices = levels(as.factor(Draft()$Operator)),
                multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE),
                selected = "SCORPIO")
  })
  output$IceClassfilter <- renderUI({
    pickerInput('IceClassfilter', "Filter by IceClass", choices = levels(as.factor(Draft()$`Ice Class`)),
                multiple = TRUE, options = list(`actions-box` = TRUE),
                selected = levels(as.factor(Draft()$`Ice Class`)))
  })
  output$dwtfilter <- renderUI({
    sliderInput('dwtfilter',label = "Filter by DWT",min = min(Draft()$Dwt, na.rm = T),
                max = max(Draft()$Dwt, na.rm = T),
                value = c(min(Draft()$Dwt, na.rm = T),max(Draft()$Dwt, na.rm = T)))
  })
  output$legfilter <- renderUI({
    pickerInput('legfilter', "Filter by Leg", choices = levels(as.factor(Draft()$Leg)),
                multiple = TRUE, options = list(`actions-box` = TRUE),
                selected = levels(as.factor(Draft()$Leg)))
  })
  output$IMOfilter <- renderUI({
    pickerInput('IMOfilter', "Filter by IMO Type", choices = levels(as.factor(Draft()$IMOType)),
                multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE),
                selected = levels(as.factor(Draft()$IMOType)))
  })
  output$builtfilter <- renderUI({
    sliderInput('builtfilter', "Filter by Age of Vessel", min = 0, max = 30, value = 30)
  })
  output$CargoTypeFilter <- renderUI({
    pickerInput('CargoTypeFilter', 'Filter by Cargo Type', choices = levels(as.factor(Draft()$CargoType)),
                multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE),
                selected = levels(as.factor(Draft()$CargoType)))
  })
  output$NavStatusFilter <- renderUI({
    # Draft()$navigationStatus[is.na(Draft()$navigationStatus)] <- "Not Available"
    pickerInput('NavStatusFilter', 'Filter by Navigation Status', choices = c(levels(as.factor(Draft()$navigationStatus)),""," ",NA),
                multiple = TRUE, options = list(`actions-box` = TRUE, `live-search` = TRUE),
                selected = c(levels(as.factor(Draft()$navigationStatus)),""," ",NA))
  })
  

####################################################################################
###################################################################################
source("serverScripts/FLICServer.R", local = TRUE)
source("serverScripts/PositionList.R", local = TRUE)
source("serverScripts/CargoList.R", local = TRUE)
source("serverScripts/TonnageList.R", local = TRUE)
source("serverScripts/MktFixReport.R", local = TRUE)
source("serverScripts/TCTab.R", local = TRUE)
source("serverScripts/TradeFlow.R", local = TRUE)
source("serverScripts/Calculators.R", local = TRUE)
source("serverScripts/SOF.R", local = TRUE)
################################################################################################



}


