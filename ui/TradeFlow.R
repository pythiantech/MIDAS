tabPanel(
  title = "Trade Flow",
  icon = icon('ship'),
  div(
    id = "tradeflow_page",
    div(
      id = "logo_subheader",
      img(src = "img/ScorpioLogo.png", height = "100%")
      ),
      tabsetPanel(
        id = "poslist_nav",
        tabPanel(
          title = "Vessel History",
          icon = icon('history'),
          fluidRow(
            column(
              12,
              div(
                class = "section_box",
                div(
                  class = "section_box_title",
                  "Vessel History"
                ),
                div(
                  class = "section_box_body",
                  div(style="display: inline-block;vertical-align:top; width: 200px;",uiOutput('FleetType')),
                  div(style="display: inline-block;vertical-align:top; width: 30px;",HTML("<br>")),
                  div(style="display: inline-block;vertical-align:top; width: 200px;",uiOutput('WVDVslType')),
                  div(style="display: inline-block;vertical-align:top; width: 30px;",HTML("<br>")),
                  div(style="display: inline-block;vertical-align:top; width: 200px;",uiOutput('VesselSelect')),
                  div(style="display: inline-block;vertical-align:top; width: 30px;",HTML("<br>")),
                  div(style="display: inline-block;vertical-align:top; width: 200px;",uiOutput('ScorpioDateRange')),
                  br(),
                  actionBttn('ScorExec','Get History'),
                  leafletOutput('shipmaps', height = '700px'),
                  DTOutput('voyages')
                )
              )
            )
            # column(
            #   3,
            #   div(
            #     class = "section_box",
            #     div(
            #       class = 'section_box_title',
            #       'Voyage Details'
            #     ),
            #     div(
            #       class = 'section_box_body',
            #       DTOutput('TripDetails'),
            #       DTOutput('PNLVoyage')
            #     )
            #   )
            # )
          )
        ),
        ####################################
        # tabPanel(
        #   title = "Scorpio Fleet",
        #   icon = icon('map'),
        # 
        #   fluidRow(
        #     column(
        #       9,
        #       div(
        #         class = "section_box",
        #         div(
        #           class = "section_box_title",
        #           "Voyages in the Selected Period"
        #         ),
        #         div(
        #           class = "section_box_body",
        #           DTOutput("voyages")
        #         )
        #       ),
        #       
        #       column(
        #         6,
        #         div(
        #           class = "section_box",
                  # div(
                  #   class = "section_box_title",
                  #   "Top 10 Performing Routes"
                  # ),
                  # div(
                  #   class = "section_box_body",
                  #   DTOutput("RegRev")
                  # )
              #   )
              # ),
              # column(
              #   6,
              #   div(
              #     class = "section_box",
              #     div(
              #       class = "section_box_title",
              #       "Longest Voyage Durations"
              #     ),
              #     div(
              #       class = "section_box_body",
              #       DTOutput("AvgDays")
              #     )
              #   )
              # )
              # 
              # ),
            # column(
            #   3,
            #   div(
            #     class = "section_box",
            #     div(
            #       class = "section_box_title",
            #       "Select Date Range"
            #     ),
            #     div(
            #       class = "section_box_body",
            #       "Please select the date range below to view voyages",
            #       dateRangeInput(
            #         'dateRange',
            #         label = 'Date range input: yyyy-mm-dd',
            #         start = Sys.Date()-30,
            #         end = Sys.Date()
            #       )
              #   )),
              # div(
              #   section = "section_box",
              #   div(
              #     class = "section_box_title",
              #     "Vessel Type Selection"
              #   ),
              #   div(
              #     class = "section_box_body",
              #     "Please the type of vessel to view",
              #     selectInput(
              #       "vsltype",
              #       "Select Vessel Type",
              #       choices="All"
              #     )
              #   )),
              # 
              # div(
              #   section = "section_box",
              #   div(
              #     class = "section_box_title",
              #     "Filter by Area"
              #   ),
            #     div(
            #       class = "section_box_body",
            #       uiOutput("CArea"),
            #       uiOutput("CRegion"),
            #       uiOutput("CSubArea"),
            #       div(class = "section_box_divider"),
            #       uiOutput("LArea"),
            #       uiOutput("LRegion"),
            #       uiOutput("LSubArea"),
            #       div(class = "section_box_divider"),
            #       uiOutput("DArea"),
            #       uiOutput("DRegion"),
            #       uiOutput("DSubArea")
            #       # div(class = "section_box_divider"),
            #       # h4(tags$strong("View by Top Results")),
            #       # sliderInput('results','Select number of top results',min = 5,max = 25,value=10),
            #       # tags$p("Select appropriate numeric column to view the top
            #       #         results."),
            #       # pickerInput('top10', "Select", choices = c("Net Voyage Days"="NetVoyDays",
            #       #                                            "Average TCE"="AvgTCE", "Total Voyages"="TotalVoys",
            #       #                                            "Revenue" = "NetRev")),
            #       # actionBttn('calculate',"Calculate",icon = icon('calculator'),color = "primary")
            # 
            #   )
            # )
        #   )
        # )
        # br(),br(),
        # fluidRow(
        #   column(
        #     6,
        #     div(
        #       class = "section_box_title",
        #       "Aggregated Data"
        #     ),
        #     div(
        #           class = "section_box_body",
        #           # highchartOutput('sankeys', height="900px")
        #           DTOutput('ByRoute')
        #         )
        #   ),
        #   column(
        #     6,
        #     div(
        #       class = "section_box",
        #       div(
        #         class = "section_box_title",
        #         "Top 10 Output"
        #       ),
        #       div(
        #         class = "section_box_body",
        #         leafletOutput("topmap", height="600px")
        #       )
        #     )
        #   )
        # )
      # ),
      ###############################################################################################
      tabPanel(
        title = "World Fleet",
        icon = icon('map'),
        fluidRow(
          column(
            9,

            div(
              conditionalPanel(
                condition = "input.maptype == 'Fleet Distribution'",
              class = "section_box",
              div(
                class = "section_box_title",
                "Fleet Distribution"
              ),
              div(
                class = "section_box_body",
                leafletOutput("fleetdistribution", width = "100%", height = "600px"),
                absolutePanel(top = 100, right = 50,
                              sliderInput("animation", "Time:",
                                          min = Sys.Date()-30,
                                          max = Sys.Date(),
                                          value = Sys.Date()))
                # uiOutput("animation"))

              )
            )),

              div(
                conditionalPanel(
                  condition = "input.maptype == 'Loading Areas'",
                class = "section_box",
                div(
                  class = "section_box_title",
                  "Loading Areas"
                ),
                div(
                  class = "section_box_body",
                  leafletOutput("SCLP", width = "100%", height = "600px")

                )
              )),
            div(
              conditionalPanel(
                condition = "input.maptype == 'Discharging Areas'",
                class = "section_box",
                div(
                  class = "section_box_title",
                  "Discharging Areas"
                ),
                div(
                  class = "section_box_body",
                  leafletOutput("DisPort", width = "100%", height = "600px")
                  
                )
              )),
            div(
              conditionalPanel(
                condition = "input.maptype == 'Fleet Distribution'",
              class = "section_box",
              div(
                class= "section_box_title",
                "Regionwise Distribution of Fleet"
                # textOutput("vslName", inline = TRUE)
              ),
              div(
                class = "section_box_body",
                DTOutput("ARData")
                # leafletOutput("VslPath", height="600px")
              )
            )
          )),
          column(
            3,
            div(
              class = "section_box",
              div(
                class = "section_box_title",
                "Select Fleet of Interest"
              ),
              div(
                class = "section_box_body",
                p("Select fleet of interest by selecting one or more Commercial Operators.
                  Also, select the date range over which you would like to view the fleet distribution."),
                
                br(), br(),
                selectInput('maptype',"Select map type to view",
                            choices = c("Fleet Distribution", "Loading Areas","Discharging Areas"), selected = "Fleet Distribution"),
                dateRangeInput('WorldDateRange', 'Select a date range', start = Sys.Date()-30,
                                  end=Sys.Date()),
                uiOutput('Operator'),
                
                # pickerInput('Owner',"Select Vessel Owner", choices = levels(as.factor(wvd()$`Owner Group`)),
                #             options = list(`actions-box` = TRUE,`live-search`=TRUE),multiple = T),
                uiOutput('VesselT'),
                uiOutput('CargoTypeWF'),
                
                actionBttn('DBexecute',"Execute Query",style = "jelly",
                           color = "primary",
                           icon = icon("rocket"))
                # div(class = "section_box_divider"),
                # uiOutput("selectVsl")

              )
            )
          )
        )
      )
    )
  )
)

