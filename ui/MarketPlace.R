
tabPanel(
  title = "Market Place",
  icon = icon('list'),
  div(
    id = "poslist_page",
    div(
      id = "logo_subheader",
      img(src = "img/ScorpioLogo.png", height = "100%")
    ),
    tabsetPanel(
      id = "poslist_nav",
      tabPanel(
        title = "Vessel Position",
        icon = icon('list'),

        fluidRow(
          hidden(
          column(
            12, id = "hiddenPosList",
            div(
              class = "search__grid header-search-container",
              actionButton('showFiltersPosList', "View Controls",class = "toolbar__btn", icon = icon("sliders"))

            ),
            div(
              class = "section_box",
              div(
                class = "section_box_title",
                "All Vessels"
              ),
              div(
              #   class = "section_box_body",
              #   tags$strong(
              #     "The vessels shown below are likely to be unfixed in next",
              #     textOutput("num_days_out", inline = TRUE), "days."
              #   ), br(),
              #   "Please use the numeric input slider in the sidebar to adjust the number",
              #   "of days for unfixing.", br(), br(),
              #   "Vessels shown in 'red' colour require immediate attention.",
              #   "The ones in 'orange' have voyages expiring in a week's time",
              #   "and vessels displayed in 'green' color have voyages expiring from a week to 30 days.",br(),br(),
                leafletOutput('posListMap', height = 600, width = "100%")
              )
              # bsModal("leafMap", "Vessel History", "show", size = 'large',
              #         leafletOutput("shipmap", width = "100%", height = 400) )
            ),
            ###########################################
            div(
              class = "section_box",
              div(
                class = "section_box_title",
                "Last Three Cargoes - ",
                textOutput("shipNameCargo2", inline = TRUE)
              ),
              div(
                class = "section_box_body",
                DT::dataTableOutput("cargoDT")
              )
            ),
            ###############################################
###########################################
            div(
              class = "section_box",
              div(
                class = "section_box_title",
                "Position List"
              ),
              div(
                class = "section_box_body",
               DT::dataTableOutput("posTable")
              )
            )
###############################################
          )),

        # hidden(
          column(
            9, id = "visiblePosList",
            div(
              class = "search__grid header-search-container",
              actionButton('hideFiltersPosList', "Hide Controls",class = "toolbar__btn", icon = icon("sliders"))

            ),
            div(
              class = "section_box",
              div(
                class = "section_box_title",
                "All Vessels"
              ),
              div(
                #   class = "section_box_body",
                #   tags$strong(
                #     "The vessels shown below are likely to be unfixed in next",
                #     textOutput("num_days_out", inline = TRUE), "days."
                #   ), br(),
                #   "Please use the numeric input slider in the sidebar to adjust the number",
                #   "of days for unfixing.", br(), br(),
                #   "Vessels shown in 'red' colour require immediate attention.",
                #   "The ones in 'orange' have voyages expiring in a week's time",
                #   "and vessels displayed in 'green' color have voyages expiring from a week to 30 days.",br(),br(),
                leafletOutput('posListMap2', height = 600, width = "100%")
              ),
              bsModal("leafMap2", "Vessel History", "show", size = 'large',
                      leafletOutput("shipmap2", width = "100%", height = 400) )
            ),
            ###########################################
            div(
              class = "section_box",
              div(
                class = "section_box_title",
                "Last Three Cargoes - ",
                textOutput("shipNameCargo", inline = TRUE)
              ),
              div(
                class = "section_box_body",
                DT::dataTableOutput("cargoDT2")
              )
            ),
            ###############################################
            ###########################################
            div(
              class = "section_box",
              div(
                class = "section_box_title",
                "Position List"
              ),
              div(
                class = "section_box_body",
                DT::dataTableOutput("posTable2")
              )
            )
            ###############################################
          ),
        # ),
          # hidden(
          column(
            3, id = "filtersPosList",
            div(
              class = "section_box",
              div(
                class = "section_box_title",
                "Select Days to Unfix"
              ),
              div(
                class = "section_box_body",

                sliderInput(
                  "voydays",
                  NULL,
                  min = 1,
                  max = 60,
                  value = 10
                ),
                h6(
                  div(style = "display:inline-block;vertical-align:top;",img(src = "img/10.png",style = "left;")),
                  div(style = "display: inline-block;vertical-align:top;","Prompt")
                ),
                h6(
                  div(style = "display:inline-block;vertical-align:top;",img(src = "img/7.png",style = "left;")),
                  div(style = "display: inline-block;vertical-align:top;","Opening in a week")
                ),
                h6(
                  div(style = "display:inline-block;vertical-align:top;",img(src = "img/5.png",style = "left;")),
                  div(style = "display: inline-block;vertical-align:top;","Opening in 7+ days")
                ),
                highchartOutput("distribution")
              )
            ),

            div(
              class = "section_box",
              div(
                class = "section_box_title",
                "Select Vessel Filters"
              ),
              div(
                class = "section_box_body",
                uiOutput('vtype'),
                div(class = "section_box_divider"),
                pickerInput('areaFilter',label = "Select Area Filter",
                            choices = c("FAR EAST", "IOR", "LATIN AMERICA", "MED", "NORTH AMERICA",
                                       "NW EUROPE", "OTHERS", "WAF"),
                            multiple = TRUE, options = list(`actions-box` = TRUE),
                            selected = c("FAR EAST", "IOR", "LATIN AMERICA", "MED", "NORTH AMERICA",
                                        "NW EUROPE", "OTHERS", "WAF", NA, ""," ")),
                div(class = "section_box_divider"),
                uiOutput('gradeFilter'),
                div(class = "section_box_divider"),
                uiOutput('DWPTL'),
                
                div(class = "section_box_divider"),
                uiOutput('CubicsPL'),
                
                div(class = "section_box_divider"),
                sliderInput('builtFilter', "Filter by Age of Vessel", min = 0, max = 30, value = 30)
              )
            ),

            div(
              class = "section_box needs_vessel_selection",
              id = "section_selected_vessel",
              div(
                class = "section_box_title",
                "Selected Vessel",
                textOutput("shipName", inline = TRUE)
              ),
              div(
                class = "section_box_body",
                # div(class = "section_box_subtitle", "Vessel Location over the last 5 Days"),
                # leafletOutput("shipmap", width = "100%", height = 200),
                # div(class = "section_box_subtitle", "View Vessel History"),
                # actionButton("show", "Click to view history"),
                # div(class = "section_box_divider"),
                # div(class = "section_box_subtitle", "Last three cargoes"),
                # DT::dataTableOutput("cargoDT"),
                # div(class = "section_box_divider"),
                "The vessel is headed to",
                tags$strong(textOutput("ROB_headed", inline = TRUE)), br(),
                "Expected to reach on",
                tags$strong(textOutput("ROB_reach", inline = TRUE)), br(),
                "Distance to go:",
                tags$strong(textOutput("ROB_distance", inline = TRUE)),
                "miles",
                div(class = "section_box_subtitle", "ROB on Arrival at", textOutput("ROB_headed2", inline = TRUE)),
                tableOutput("ROBDTArrival"),
                div(class = "section_box_subtitle", "Inspection Details"),
                uiOutput("HullDetails"),
                div(class = "section_box_divider"),
                ###########################################
                div(class = "section_box_subtitle", "Certificate Data"),
                DT::dataTableOutput("CertificateData"),
                div(class = "section_box_divider"),
                ###########################################
                div(class = "section_box_subtitle", "Vetting Data"),
                DT::dataTableOutput("VettingData")
                ###############################################
                
              )
            )
          )
        # )
        )
      ),




      tabPanel(
        title = "Position List",
        fluidRow(
          column(9,
                 rHandsontableOutput("PositionListData"),
                 useShinyalert()
        ),
        column(3,
               div(
                 class = "section_box",
                 div(
                   class = "section_box_title",
                   "Position List Table"
                 ),
                 div(
                   class = "section_box_body",
                   "Please use this form to add details on tonnage information.
                      You can also use filters below to customize your view.", br(),br(),
                   actionBttn('savePLData',"Save",style = "jelly",
                              color = "success",
                              icon = icon("thumbs-up")),
                   useShinyalert(),
                   br(), br(),
                   actionBttn('Refresh',"Refresh",style = "jelly",
                              color = "success",
                              icon = icon("sync")),
                   div(class = "section_box_divider"),
                   uiOutput('cstatus'),
                   uiOutput('vtypePL'),
                   uiOutput('vname')
                 )),

                 div(
                   class = "section_box",
                   div(
                     class = "section_box_title",
                     "Select Days to Unfix"
                   ),
                   div(
                     class = "section_box_body",
                     sliderInput(
                       "voydaysPL",
                       NULL,
                       min = 1,
                       max = 60,
                       value = 10
                     )
                   )
                 )
               )
      )),
#############################################################
#Tonnage List
tabPanel(
  title = "Tonnage List",
  fluidRow(
    hidden(
      column(
        12, id = 'hiddenTL',
        div(
          class = "search__grid header-search-container",
          actionButton('showFiltersTL', "Show Filters",class = "toolbar__btn", icon = icon("filter"))
          
        ),
        div(
          class = "section_box",
          div(
            class = "section_box_title",
            "Master Tonnage List"
          ),
          div(
            class = "section_box_body",
            rHandsontableOutput("TonnageListData2"),
            br(), br(),
            actionBttn('addRowTL2',"Add New Row",style = "jelly",
                       color = "success",
                       icon = icon("plus")),
            br(),
            br(),
            actionBttn('saveTLData2',"Save",style = "jelly",
                       color = "success",
                       icon = icon("thumbs-up")),
            useShinyalert()
          )
        ),
        div(
          class = "section_box",
          div(
            class = "section_box_title",
            "Market Tonnage Table"
          ),
          div(
            class = "section_box_body",
            
            DTOutput('BizTon2'),
            br(),
            h4("Please select rows by clicking on them and then press the shift button to copy
                  the rows to the table above"),
            actionButton("shiftcellsTL2", "Shift selected rows", class = "btn-primary")
          )
        )
      )
    ),
    
    column(9, id = 'visibleTL',
           div(
             class = "search__grid header-search-container",
             actionButton('hideFiltersTL', "Hide Filters",class = "toolbar__btn", icon = icon("filter"))
           ),
           
           div(
             class = "section_box",
             div(
               class = "section_box_title",
               "Master Tonnage List"
             ),
             div(
               class = "section_box_body",
               rHandsontableOutput("TonnageListData"),
               br(), br(),
               actionBttn('addRowTL',"Add New Row",style = "jelly",
                          color = "success",
                          icon = icon("plus")),
               br(),
               br(),
               actionBttn('saveTLData',"Save",style = "jelly",
                          color = "success",
                          icon = icon("thumbs-up")),
               useShinyalert()
             )
           ),
           div(
             class = "section_box",
             div(
               class = "section_box_title",
               "Market Tonnage Table"
             ),
             div(
               class = "section_box_body",
               
               DTOutput('BizTon'),
               br(),
               h4("Please select rows by clicking on them and then press the shift button to copy
                  the rows to the table above"),
               actionButton("shiftcellsTL", "Shift selected rows", class = "btn-primary")
             )
           )
           
    ),
    column(3, id = 'filtersTL',
           div(
             class = "section_box",
             div(
               class = "section_box_title",
               "Tonnage List"
             ),
             div(
               class = "section_box_body",
               "Please use this form to add details on tonnage information.
               You can also use filters below to customize your view.", br(),br(),
               uiOutput('vtypeTL'),
               
               pickerInput('EmpTL',"Filter by Status", choices = c("Fixed","Unfixed","Hold",
                                                                   "Subs","Others","Died", "Looking","Open","WDWF",""),
                           options = list(`actions-box` = TRUE),multiple = T, selected = c("Fixed","Unfixed","Hold",
                                                                                           "Subs","Others","Died", "Looking","Open","WDWF","")),
               uiOutput('DWTTL'),
               uiOutput('OprTL'),
               uiOutput('CubicTL'),
               
               dateRangeInput('OpenPortDate',"Filter by Open Port Date", start = (Sys.Date() - 30), end = Sys.Date() + 30),
               br(), br(),
               actionBttn('ResetFilt',"Reset Filters",style = "jelly",
                          color = "success",
                          icon = icon("filter"))
             )

           ))
    )
),
#############################################################
#Cargo List
tabPanel(
  title = "Cargo List",
  fluidRow(
    hidden(
      column(12, id = "hiddenCL",
             div(
               class = "search__grid header-search-container",
               actionButton('showFiltersCL', "Show Filters",class = "toolbar__btn", icon = icon("filter"))
               
             ),
             div(
               class = "section_box",
               div(
                 class = "section_box_title",
                 "Cargo List Master Table"
               ),
               div(
                 class = "section_box_body",
                 
                 rHandsontableOutput("CargoListData2"),
                 br(),
                 actionBttn('addRow2',"Add New Row",style = "jelly",
                            color = "success",
                            icon = icon("plus")),
                 br(),
                 br(),
                 actionBttn('saveCLData2',"Save",style = "jelly",
                            color = "success",
                            icon = icon("thumbs-up")),
                 useShinyalert(),
                 br(),
                 h4("Please make sure to save the table prior to shifting selected rows to Scorpio Fixture Table"),
                 actionButton("shiftcells2", "Shift selected rows", class = "btn-primary")
               )
             ),
             div(
               class = "section_box",
               div(
                 class = "section_box_title",
                 "Scorpio Cargo List Table"
               ),
               div(
                 class = "section_box_body",
                 
                 DTOutput('ScoFixList2')
               )
             )
             
      )
    ),
    column(9,id = "visibleCL",
           div(
             class = "search__grid header-search-container",
             actionButton('hideFiltersCL', "Hide Filters",class = "toolbar__btn", icon = icon("filter"))
             
           ),
           div(
             class = "section_box",
             div(
               class = "section_box_title",
               "Cargo List Master Table"
             ),
             div(
               class = "section_box_body",
               
               rHandsontableOutput("CargoListData"),
               br(),
               actionBttn('addRow',"Add New Row",style = "jelly",
                          color = "success",
                          icon = icon("plus")),
               br(),
               br(),
               actionBttn('saveCLData',"Save",style = "jelly",
                          color = "success",
                          icon = icon("thumbs-up")),
               useShinyalert(),
               br(),
               h4("Please make sure to save the table prior to shifting selected rows to Scorpio Fixture Table"),
               actionButton("shiftcells", "Shift selected rows", class = "btn-primary")
             )
           ),
           div(
             class = "section_box",
             div(
               class = "section_box_title",
               "Scorpio Cargo List Table"
             ),
             div(
               class = "section_box_body",
               
               DTOutput('ScoFixList')
             )
           )
           
    ),
    column(3, id = "filtersCL",
           # div(
           #   class = "section_box",
           #   div(
           #     class = "section_box_title",
           #     "Cargo List"
           #   ),
           #   div(
           #     class = "section_box_body",
           #     "Please use this form to add details on available cargos.
           #     You can also use filters below to customize your view.", br(),br(),
           #     materialSwitch(inputId = "viewFilterCL",label = "View Filters",value = TRUE,status = "success"),
           #     
           #     br(),
           #     br()
           #     
           #   )
           # 
           # ),
           # conditionalPanel(condition = "input.viewFilterCL",
             div(
               class = "section_box",
               div(
                 class = "section_box_title",
                 "Filters"
               ),
               div(
                 class = "section_box_body",
                 pickerInput('InformationCL',"Filter by Information", choices = c("Market", "Private"),
                             options = list(`actions-box` = TRUE),multiple = T, selected = c("Market", "Private",""," ", NA)),
                 pickerInput('RegionCL', 'Filter by Region', choices = c('NW EUROPE', 'MED', 'BSEA', 'WAF', 'USG/CARIBS', 'USAC', 
                                                                         'USWC', 'LATIN EAST', 'LATIN WEST', 'MIDDLE EAST', 'RED SEA/AFRICA', 'IOR', 
                                                                         'SOUTH EA', 'FAR EAST', 'OZ/NZ/PACIFIC'),
                             options = list(`actions-box` = TRUE), multiple = TRUE, selected = c('NW EUROPE', 'MED', 'BSEA', 'WAF', 'USG/CARIBS', 'USAC', 
                                                                                                 'USWC', 'LATIN EAST', 'LATIN WEST', 'MIDDLE EAST', 'RED SEA/AFRICA', 'IOR', 
                                                                                                 'SOUTH EA', 'FAR EAST', 'OZ/NZ/PACIFIC',""," ", NA)),
                 pickerInput('CargoTypeCL', "Filter by Cargo Type", choices = c("Clean","Dirty","Chemicals"),
                             options = list(`actions-box` = TRUE),multiple = T, selected = c("Clean","Dirty","Chemicals",""," ", NA)),
                 pickerInput('StatusCL', "Filter by Status", choices = c("Fixed","Unfixed","Hold",
                                                                         "Subs","Others","Died", "Looking","Open","WDWF"),
                             options = list(`actions-box` = TRUE),multiple = T, selected = c("Fixed","Unfixed","Hold",
                                                                                             "Subs","Others","Died", "Looking","Open","WDWF",""," ", NA)),
                 dateRangeInput('RepDateCL',"Filter by Reported Date", start = (Sys.Date() - 30), end = Sys.Date() + 30),
                 textInput("ChartCL","Type in the Charterer name to filter the table",NULL),
                 textInput("BrokerCL","Type in the Broker name to filter the table",NULL),
                 textInput("OperatorCL","Type in the Operator name to filter the table",NULL)
               )
             )
           # )
           )
    )
),
####################################################################################
#Market Fixture List
tabPanel(
  title = "Spot Fixture List",
  fluidRow(
    column(
      12,
      div(
        class = "section_box",
        div(
          class = "section_box_title",
          "Spot Fixture Report"
        ),
        div(
          class = "section_box_body",
          dateRangeInput('MktDateRange',"Select Date Range", start = (Sys.Date() - 2), end = Sys.Date()),
          br(),
          actionBttn('PullMkt',"Pull Data", style = 'jelly',
                     color = "success", icon = icon("thumbs-up")),
          br(), br(),
          uiOutput('MktFixUI')
        )
      )
    )
    # column(
    #   3,
    #   div(
    #     class = "section_box",
    #     div(
    #       class = "section_box_title",
    #       'Parameters'
    #     ),
    #     div(
    #       class = 'section_box_body',
    #       dateRangeInput('MktDateRange',"Select Date Range", start=(Sys.Date()-2), end = Sys.Date()),
    #       br(), br(),
    #       actionBttn('PullMkt',"Pull Data", style = 'jelly',
    #                  color = "success", icon = icon("thumbs-up"))
    #     )
    #   )
      # div(
      #   class = "section_box",
      #   div(
      #     class = "section_box_title",
      #     'Filters'
      #   ),
      #   div(
      #     class = 'section_box_body',
      #     uiOutput('FixType'),
      #     div(class = "section_box_divider"),
      #     uiOutput('LoadPort'),
      #     div(class = "section_box_divider"),
      #     uiOutput('DiscPort'),
      #     div(class = "section_box_divider"),
      #     uiOutput('Status'),
      #     div(class = "section_box_divider"),
      #     uiOutput('Charterer'),
      #     div(class = "section_box_divider"),
      #     uiOutput('Source'),
      #     div(class = "section_box_divider"),
      #     actionBttn('MktFilters',"Filter Data", style = 'jelly',
      #                color = "success", icon = icon("filter")),
      #     br(),br(),
      #     actionBttn('ResetFilters',"Reset filters", style = 'jelly',
      #                color = "success", icon = icon("filter"))
      #     
      #   )
      # )
    # )
  )
)
####################################################################################################
####################################################################################################
      # tabPanel(
      #   title="Competitor Analysis",
      #   icon = icon('map-marker'),
      #   # tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
      #   leafletOutput('cmap',height=800),
      #   div(
      #     class = "section_box",
      #     div(
      #       class = "section_box_title",
      #       "Current Location - Competitors"
      #     ),
      #     div(
      #       class= "section_box_body",
      #       DTOutput('cmapTable')
      #     )
      #   )
      # )
    )
  )
)
