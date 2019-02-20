#########################################################################
######################### SET OPTIONS FOR RUNNING THE APP
#########################################################################

MDIS_TESTING <- FALSE

############################################

MDIS_ASYNC <- FALSE


############################################

MDIS_CACHE <- TRUE

#########################################################################
######################### END OPTIONS FOR RUNNING THE APP
#########################################################################

library(shiny)
library(shinyalert)
library(shinyBS)
library(httr)
library(XML)
library(xml2)
library(jsonlite)
library(leaflet)
library(leaflet.extras)
library(leaflet.minicharts)
library(highcharter)
library(htmltools)
library(htmlwidgets)
library(sf)
library(mapview)
library(RCurl)
library(lubridate)
library(RSQLite)
library(rhandsontable)
library(DT)
library(shinyjs)
library(config)
library(RecordLinkage)
library(geosphere)
library(glue)
library(pool)
library(fullcalendar)
library(sp)
library(gmailr)
library(crosstalk)
library(shinyWidgets)
library(timevis)
library(qdapRegex)
library(xts)
library(tidyverse)
###################################################################################################
config <- config::get(file  =  "config.yml", config  =  "production")

options(scipen=999)
#For reactiveFileReader
  LoadToEnvironment <- function(Rdata, env = new.env()) {
    load(Rdata, env)
    return(env)
  }
####################################################################################################

  
  
  
####################################################################################################
#Bizlem fullload API function for Tonnage, Spot, TimeCharterReports and BrokerTcRate
  
  GetBizlemData <- function(report, start, end){
    tryCatch({
    BizURL <- "https://dev.bizlem.io:8082/scorpio/ReportApi"
    auth <- '$CorpioReport!ntegration$ystem@*3!2'
    body <- list(reporttype  =  report, from  =  start, to  =  end, Authorization  =  auth)
    r <- POST(BizURL, body  =  body, encode  =  "json")
    text_context <- content(r, "text")
    x <- paste(text_context, collapse  =  "") %>% fromJSON
    df <- x[[1]]
    rm(r, x, text_context)
    return(df)
    },error  =  function(e){cat("ERROR :",conditionMessage(e), "\n")})
  }
   
####################################################################################################
GetUserName <- function() {
  # Returns user name of computer with twist for Unix
  # Args
  #   none
  # Returns
  #  string of user login name

  x <- Sys.info()[["user"]]

  # if blank try other methods
  if (is.null(x) | x  ==  "") {
    # On windows machines
    Sys.getenv("USERNAME")
  } else {
    # from helpfiles for Unix
    Sys.getenv("LOGNAME")
  }

  # Could get something but it is unknown error
  if (identical(x, "unknown")) {
    warning("unknown returned")
  }

  return(x)
}

if (MDIS_ASYNC) {
  library(promises)
  library(future)
  plan(multiprocess)
  future <- future::future
  then <- promises::then
} else {
  future <- base::identity
  then <- function(promise, onFulfilled) { onFulfilled(promise) }
}


if (MDIS_CACHE) {
  cache_dir <- "data/cache/"
  dir.create(cache_dir, showWarnings  =  FALSE)
}



'%!in%' <- function(x,y)!('%in%'(x,y))

yesterday <- function() Sys.time() - 60*60*24
previous2hrs <- function() Sys.time() - 60*60*2

myScale <- function(x){(x - min(x))/(max(x) - min(x))}

# Mapping of port function
portFunction <- data.frame(ID  =  c(1:14),
                           Function  =  c("Commencing","Discharging", "Fuelling","Canal Transit","Loading",
                                      "Other","Passing","Port Call Cancelled","Repair","Sampling",
                                      "Terminating","Waiting","Delivery","Redelivery"))

#######################################################################
ClosestMatch  =  function(string, stringVector){
  if (grepl(",", x  =  string))
    string <- gsub("^(.*?),.*", "\\1", string)
  distance  =  levenshteinSim(tolower(string), tolower(as.character(stringVector)));
  stringVector[distance  ==  max(distance)]

}

#############################################

# https://stats.stackexchange.com/questions/161379/quickly-finding-nearest-time-observation
# Return an array `i` of indexes into `target`, parallel to array `probe`.
# For each index `j` in `target`, probe[i[j]] is nearest to target[j].
#
nearest <- function(probe, target, ends = c(-Inf,Inf)) {
  #
  # Both `probe` and `target` must be vectors of numbers in ascending order.
  #
  glb <- function(u, v) {
    n <- length(v)
    z <- c(v, u)
    j <- i <- order(z)
    j[j > n] <- -1
    k <- cummax(j)
    return(k[i > n])
  }
  y <- c(ends[1], target, ends[2])

  i.lower <- glb(probe, y)
  i.upper <- length(y) + 1 - rev(glb(rev(-probe), rev(-y)))
  y.lower <- y[i.lower]
  y.upper <- y[i.upper]
  lower.nearest <- probe - y.lower < y.upper - probe
  i <- ifelse(lower.nearest, i.lower, i.upper) - 1
  i[i < 1 | i > length(target)] <- NA
  return(i)
}

###############################################################################
# Define pool handler by pool on global level for sqlite
if (config$ds  ==  'local') {
  pool <- pool::dbPool(drv  =  RSQLite::SQLite(),
                       dbname  =  config$dbname)
} else{
  pool <- pool::dbPool(drv = odbc::odbc(),
                       dsn = "SCORPIO_DWH_PROD",
                       uid  =  "pythian_dkh",
                       pwd  =  "Welcome@123")
}

    onStop(function() {
  poolClose(pool)
}) # important!
##############################################################################
revEpoch <- function(x){
  as.POSIXct(x, origin = "1970-01-01")
}
##############################################################################

# Make Icons
shipIcon <- makeIcon("www/img/10.png", iconWidth  =  11, iconHeight  =  24, iconAnchorX  =  5, iconAnchorY  =  0)
ownships <- makeIcon("www/img/6.png", iconWidth  =  11, iconHeight  =  24, iconAnchorX  =  5, iconAnchorY  =  0)
redIcon <- makeIcon("www/img/10.png", iconWidth  =  11, iconHeight  =  24, iconAnchorX  =  5, iconAnchorY  =  0)
orangeIcon <- makeIcon("www/img/7.png", iconWidth  =  11, iconHeight  =  24, iconAnchorX  =  5, iconAnchorY  =  0)
yellowIcon <- makeIcon("www/img/5.png", iconWidth  =  11, iconHeight  =  24, iconAnchorX  =  5, iconAnchorY  =  0)

letterC <- makeIcon("www/img/letter-c.png")
letterD <- makeIcon("www/img/letter-d.png")
letterF <- makeIcon("www/img/letter-f.png")
letterI <- makeIcon("www/img/letter-i.png")
letterL <- makeIcon("www/img/letter-l.png")
letterO <- makeIcon("www/img/letter-o.png")
letterP <- makeIcon("www/img/letter-p.png")
letterQ <- makeIcon("www/img/letter-q.png")
letterR <- makeIcon("www/img/letter-r.png")
letterS <- makeIcon("www/img/letter-s.png")
letterT <- makeIcon("www/img/letter-t.png")
letterW <- makeIcon("www/img/letter-w.png")
letterY <- makeIcon("www/img/letter-y.png")
letterZ <- makeIcon("www/img/letter-z.png")

# this is taken from: https://gist.github.com/jcheng5/c084a59717f18e947a17955007dc5f92
registerPlugin <- function(map, plugin) {
  map$dependencies <- c(map$dependencies, list(plugin))
  map
}
# For rotating ship icon based on course on ground(cog)
rotatedMarkerPlugin <- htmlDependency(
  "Leaflet.rotatedMarker",
  "0.1.2",
  src  =  normalizePath(path  =  getwd()),
  script  =  "www/js/leaflet.rotatedMarker.js"
)
# For adding directional arrows on leaflet path
polylineDecoratorPlugin <- htmlDependency(
  "Leaflet.PolylineDecorator",
  "1.6.0",
  src  =  normalizePath(path  =  getwd()),
  script  =  "www/js/leaflet.PolylineDecorator.js"
)

# For adding weather maps in leaflet
openWeatherPlugin <- htmlDependency(
  "Leaflet.OpenWeather",
  "1.6.0",
  src  =  normalizePath(path  =  getwd()),
  script  =  "www/js/leaflet.openweathermap.js"
)


#################################################################################
username_competitor <- config$username_competitor
password_competitor <- config$password_competitor
auth_competitor <- paste("Basic",base64(paste(username_competitor, password_competitor, sep = ":")))
searoutesurl <- config$searoutesurl
searouteskey <- config$searouteskey

################################################
# Data preparation

# Get sea routes position

get_sea_routes <- function(lat1, lon1, lat2, lon2){
  tryCatch({
  from <- paste0('lon:',lon1,'lat:',lat1)
  to <- paste0('lon:',lon2,'lat:',lat2)
  r <- GET(paste(searoutesurl,from,to, sep = "/"), add_headers('x-api-key' = searouteskey,
                                                        'application' = "application/json"))
  text_content <- content(r, "text")
  json_content <- text_content %>% fromJSON()
  lon <- as.data.frame(json_content$getRouteJson$routepoints)$lon
  lat <- as.data.frame(json_content$getRouteJson$routepoints)$lat
  routes <- cbind(lon,lat)
  return(routes)},error = function(e){cat("ERROR :",conditionMessage(e), "\n")})
}


# Get vessels present location data from Stratum
get_vessels_data <- function() {
  if (MDIS_CACHE) {
    file <- paste0(cache_dir, "vessels.Rds")
    if (file.exists(file) && file.info(file)$mtime > previous2hrs()) {
      return(readRDS(file))
    } else {
      save_result_to <- file
    }
  } else {
    save_result_to <- NULL
  }

  baseurl_own <- config$baseurl_own
  username_own <- config$username_own
  password_own <- config$password_own
  auth_own <- paste("Basic",base64(paste(username_own, password_own, sep = ":")))

  # Get vessels' IMOs
  imos_get <- GET(paste0(baseurl_own,"vessels"),add_headers(Authorization = auth_own))
  text_content <- content(imos_get, "text")
  json_content <- text_content %>% fromJSON()
  imos <- json_content$obj
  rm(imos_get,text_content,json_content)

  # Loop through IMOs
  counter <- 1
  vessels <- data.frame(matrix(ncol  =  12, nrow  =  0))
  vars <- c("imo", "boatName", "vesselType", "callSign", "mmsi", "gpsTimeStamp",
            "lat", "lon", "cog", "sog", "pollCategory", "pollMessage")
  colnames(vessels) <- vars

  while (counter < (length(imos) %/% 30)*30) {
    testimo <- paste(as.character(imos[counter:(counter + 29)]), collapse = ',')
    vesdetails <- GET(paste0(baseurl_own,"vessels","/",testimo),add_headers(Authorization = auth_own))
    vessels <- bind_rows(vessels,as.data.frame(fromJSON(content(vesdetails, "text"))$obj))
    counter <- counter + 30

  }
  testimo <- paste(as.character(imos[counter:length(imos)]), collapse = ',')
  vesdetails <- GET(paste0(baseurl_own,"vessels","/",testimo),add_headers(Authorization = auth_own))
  vessels <- rbind(vessels,as.data.frame(fromJSON(content(vesdetails, "text"))$obj))
  rm(counter,vars,testimo,vesdetails)
  vessels$lon <- as.numeric(vessels$lon)
  vessels$lat <- as.numeric(vessels$lat)
  vessels$vesselType <- as.factor(vessels$vesselType)

  if (!is.null(save_result_to)) {
    saveRDS(vessels, file)
  }

  return(vessels)
}

vessels <- get_vessels_data()


########################################################
########################################################


##############################################################################
#Comp Fleet Icons

shipIcons <- iconList(Torm = makeIcon("www/img/1.png", iconWidth  =  11, iconHeight  =  24, iconAnchorX  =  5, iconAnchorY  =  0),
                      Damico = makeIcon("www/img/2.png", iconWidth  =  11, iconHeight  =  24, iconAnchorX  =  5, iconAnchorY  =  0),
                      Norient = makeIcon("www/img/4.png", iconWidth  =  11, iconHeight  =  24, iconAnchorX  =  5, iconAnchorY  =  0),
                      UPT = makeIcon("www/img/3.png", iconWidth  =  11, iconHeight  =  24, iconAnchorX  =  5, iconAnchorY  =  0),
                      Maersk = makeIcon("www/img/7.png", iconWidth  =  11, iconHeight  =  24, iconAnchorX  =  5, iconAnchorY  =  0),
                      Hafnia = makeIcon("www/img/9.png", iconWidth  =  11, iconHeight  =  24, iconAnchorX  =  5, iconAnchorY  =  0))

#########################################################

wvd <- readRDS("data/wvd.Rds")


wvd$VesselDetails <- paste0("<div style = 'font-size:12px;float:left'>
            <span style = 'font-size:16px;font-weight:bold'>",wvd$Name,"</span><br/>",
                            "<br/><span style = 'font-size:10px'>Year Built:",wvd$Built,"</span><br/>",
                            "<span style = 'font-size:10px'>DWT:",wvd$Dwt,"</span><br/>",
                            "<span style = 'font-size:10px'>Cubic:",wvd$Cubics,"</span><br/>",
                            "<span style = 'font-size:10px'>Ice Class:",wvd$`Ice Class`,"</span><br/>",
                            "<span style = 'font-size:10px'>IMO:",wvd$`IMO No.`,"</span><br/>",
                            "<span style = 'font-size:10px'>Ship Type:",wvd$`Vessel Type`,"</span><br/>",
                            "<span style = 'font-size:10px'>Owner:",wvd$`Owner Group`,"</span>
        </div>")

wvd$Age <- year(Sys.Date())-wvd$Built
wvd$AgeClass <- ifelse(wvd$Age<=5, "0-5 Years Old",
                       ifelse(wvd$Age<=10 & wvd$Age>5, "6-10 Years Old",
                              ifelse(wvd$Age<=15 & wvd$Age>10, "11-15 Years Old",
                                     ifelse(wvd$Age<=20 & wvd$Age>15, "16-20 Years Old", "Above 20 Years Old")) ))

wvd <- wvd %>% filter(`Vessel Type` %!in% c('Kamsarmax', 'Ultramax'))
wvd <- wvd[!duplicated(wvd$MMSI),]


# AllRegions <- readRDS("data/editedRegions.Rds")
AllRegions <- readRDS('data/AllRegions.Rds')
PortRegion <- readRDS("data/PortRegion.Rds")

epochTime <- function() {
  as.integer(Sys.time())
}

humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")



##############

DF <- readRDS("data/tcdatapythian/TCComb.Rds")
TCNew <- read_csv("data/tcdatapythian/TC.csv")
source(file = "globalScripts/orderbooks.R", local  =  TRUE)



