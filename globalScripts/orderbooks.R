orderbook <- read_csv('data/tcdatapythian/Orderbook.csv')
colnames(orderbook)[2] <- "VslType"
colnames(orderbook)[15] <- "OwnerGroup"

#Convert Built to date
#Assume Jun for missing months
orderbook$Built <- ifelse(nchar(orderbook$Built)<7, paste0(orderbook$Built,"06-01"), orderbook$Built)
orderbook$Built <- ifelse(nchar(orderbook$Built)<10, paste0(orderbook$Built,"-01"), orderbook$Built)
orderbook$Built <- lubridate::ymd(orderbook$Built)

#Convert contract date to date format
orderbook$`Contract Date` <- as.Date(lubridate::fast_strptime(orderbook$`Contract Date`, "%Y-%b-%d"))


require(highcharter)

  orderbook %>% dplyr::group_by(Builder) %>% dplyr::summarise(Count=n()) %>%
  arrange(desc(Count)) %>% filter(Count>10) %>%
  hchart(type = "pie",innerSize= '40%', showInLegend= F,
           hcaes(x = Builder, y = Count, color = -Count)) %>%
  hc_tooltip(pointFormat = paste('{point.y} ships<br/><b>{point.percentage:.1f}%</b>')) %>%
  hc_title(text = "Order Book - By Builder") %>%
  hc_subtitle(text = "This chart only includes builders with over 10 vessels") %>%
  hc_add_theme(hc_theme_ffx())


######################################################################
  #from this beautiful kernel
  #https://www.kaggle.com/pranav84/the-most-dangerous-places-to-work-in-the-usa

#ByVslType
vsltypeOB <- orderbook %>% dplyr::group_by(VslType) %>%
    dplyr::summarise(Count=n()) %>% arrange(desc(Count))

#Deliveries
deliveries <- orderbook %>% dplyr::group_by(Year=lubridate::year(Built)) %>%
  dplyr::summarise(Count=n())

#Spread
obsm <- orderbook %>% group_by(VslType, Year=year(Built)) %>% summarise(Count=n())


vesselConstruction <- hchart(obsm, "column", hcaes(x=VslType,y = Count, group = Year)) %>%
  hc_title(text = "Vessel Construction") %>%
  hc_subtitle(text = "The graph shows the construction of different categories
              of vessels over the next 4 years. You can click on years to enable/disable a particular year") %>%
  hc_plotOptions(column = list(
    dataLabels = list(enabled = FALSE),
    stacking = "normal",
    enableMouseTracking = TRUE)
  ) %>%
  hc_xAxis(title = list(text = "")) %>%
    hc_add_theme(hc_theme_smpl())



##########################################################################################################
#Existing Active fleet

# Active <- wvd %>% group_by(OwnerGroup=`Owner Group`,VslType=`Vessel Type`) %>% summarise(Count=n()) %>% arrange(OwnerGroup) %>%
#   mutate(Year = "Present")
# Active <- select(Active, OwnerGroup, VslType, Year, Count)
# Ordered <- orderbook %>% group_by(OwnerGroup, VslType, Year=year(Built)) %>% summarise(Count=n()) %>% arrange(OwnerGroup)
# Ordered$Year <- as.character(Ordered$Year)
# combineds <- bind_rows(Active,Ordered) %>% arrange(OwnerGroup)
# combineds <- spread(combineds, Year, Count)
# combineds[is.na(combineds)] <- 0
# rm(Active,Ordered)

#########################################
#On Order
# orderbook$Age <- year(orderbook$Built)-year(Sys.Date())
# 
# 
# OnOrder <- orderbook %>% group_by(VslType) %>% summarise(Count=n())
# OnOrder$AgeClass <- rep("On Order", nrow(OnOrder))
# OnOrder <- OnOrder %>% select(`Vessel Type`=VslType, AgeClass, Count)
# PresentVsls <- wvd() %>% group_by(`Vessel Type`, AgeClass) %>% summarise(Count=n())
# 
# consolidated <- bind_rows(PresentVsls,OnOrder) %>% arrange(`Vessel Type`)

