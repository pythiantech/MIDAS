tabPanel(
  title = "Calculators",
  icon = icon('calculator'),
  div(
    id = "poslist_page",
      div(
        id = "logo_subheader",
        img(src = "img/ScorpioLogo.png", height = "100%")
      ),
    tabsetPanel(
      id = "poslist_nav",
      ##################################
      #Weather Factor
      tabPanel(
        title = "Weather Factor",
        icon = icon('umbrella'),

        fluidRow(
          column(
            3,
            div(
              class = "section_box",
              div(
                class = "section_box_title",
                "Parameters"
              ),
              div(
                class = "section_box_body",
                uiOutput('WFSubAreaStart'),
                uiOutput('WFSubAreaStop'),
                uiOutput('WFVslType'),
                uiOutput('WFVslState'),
                actionBttn('WFFilters',"Apply Filters",style = "jelly",
                           color = "primary",
                           icon = icon("rocket"))
              )
            )
          ),
          column(
            9,
            div(
              class = "section_box",
              div(
                class = "section_box_title",
                "Weather Factor"
              ),
              div(
                class = "section_box_body",
                # DTOutput('WFVDPA')
                highchartOutput('WFChart')
              )
            )
          )
        )
      ),
      ##########################################
      #SOF Analysis
      # tabPanel(
      #   title = "SOF Analysis",
      #   icon = icon('clock-o'),
      #   fluidRow(
      #
      #     column(
      #       9,
      #       fluidRow(
      #       div(
      #         class = "section_box",
      #         div(
      #           class = "section_box_title",
      #           textOutput('PortName')
      #         ),
      #         div(
      #           class = "section_box_body",
      #           textOutput('PTDay',inline = TRUE), "days and", textOutput('PTHour',inline = TRUE), "hours"
      #
      #         )
      #       ))
      #     ),
      #     column(
      #       3,
      #       div(
      #         class = "section_box",
      #         div(
      #           class = "section_box_title",
      #           "Activity Filters"
      #         ),
      #         div(
      #           class = "section_box_body",
      #
      #           uiOutput('VslTypeSOF'),
      #           uiOutput('PortSOF'),
      #           uiOutput('CargoSOF'),
      #           uiOutput('PortFuncSOF'),
      #           actionBttn('SOFFilters',"Apply Filters",style = "jelly",
      #                      color = "primary",
      #                      icon = icon("rocket"))
      #         )
      #       )
      #     )
      #   )
      #   #############End of First Fluid Row
      # ),
      tabPanel(
        title = "SOF Analysis",
        icon = icon('clock-o'),

        fluidRow(
          column(
            9,
            div(
              class = 'section_box_title',
              textOutput('PortName')
            ),
            fluidRow(
              column(
                4,
                div(
                  class = "section_box",
                  div(
                    class = "section_box_title",
                    "Port Turnaround Time"

                  ),
                  div(style = "height:328px;",
                    class = "section_box_body",
                    h4(textOutput('PTDay',inline = TRUE)), h4(textOutput('PTHour',inline = TRUE)),
                    br(), br(),
                    highchartOutput('PTHist', height = '180px')
                  )
                )
              ),
              column(
                4,
                div(
                  class = "section_box",
                  div(
                    class = "section_box_title",
                    "Operations Count"
                  ),
                  div(style = "height:328px;",
                    class = "section_box_body",
                    h4(textOutput('OCMin')),br(),
                    h4(textOutput('OCMax')), br(),
                    h4(textOutput('OCAvg'))
                  )
                )
              ),
              column(
                4,
                div(
                  class = "section_box",
                  div(
                    class = "section_box_title",
                    "Operations Turnaround Time"
                  ),
                  div(style = "height:328px;",
                    class = "section_box_body",

                        h4(textOutput('OTTDay',inline = TRUE)), h4(textOutput('OTTHour',inline = TRUE)),
                        br(), br(),
                        highchartOutput('OTTDensity', height = '180px')


                  )
                )
              )
              # column(
              #   3,
              #   div(
              #     class = "section_box",
              #     div(
              #       class = "section_box_title",
              #       "Laytime"
              #     ),
              #     div(style = "height:300px;",
              #       class = "section_box_body"
              #       # textOutput('PTDay',inline = TRUE), "days and", textOutput('PTHour',inline = TRUE), "hours"
              #
              #     )
              #   )
              # )
            )
          ),
          column(
           3,
           div(
             class = "section_box",
             div(
               class = "section_box_title",
               "Activity Filters"
             ),
             div(
               class = "section_box_body",

               uiOutput('VslTypeSOF'),
               uiOutput('PortSOF'),
               uiOutput('CargoSOF'),
               uiOutput('PortFuncSOF'),
               actionBttn('SOFFilters',"Apply Filters",style = "jelly",
                          color = "primary",
                          icon = icon("rocket"))
             )
           )
          )
        ),#End of first fluid Row
        fluidRow(
          column(
            6,
            div(
              class = 'section_box',
              div(
                class = 'section_box_title',
                "Average Activity Breakdown - Port"
              ),
              div(
                class = 'section_box_body',
                DTOutput('Activity')
              )
            )
          ),
          column(
            3,
            div(
              class = 'section_box',
              div(
                class = 'section_box_title',
                "Vessel - Voyage Specific"
              ),
              div(
                class = 'section_box_body',
                DTOutput('ActivityVsl')
              )
            )
          ),
          column(
            3,
            div(
              class = 'section_box',
              div(
                class = 'section_box_title',
                "Activity by Vessel"
              ),
              div(
                class = 'section_box_body',
                uiOutput('VslSOF'),
                uiOutput('VoySOF'),
                actionBttn('VslActFilter',"View Vessel Activities",style = "jelly",
                           color = "primary",
                           icon = icon("exchange")),
                h4("Port Turnaround  Time"),
                h5(textOutput('PTDayVsl',inline = TRUE)), h5(textOutput('PTHourVsl',inline = TRUE)),
                div(class="section_box_divider"),
                h4("Operations Turnaround Time"),
                h5(textOutput('OTTDayVsl',inline = TRUE)), h5(textOutput('OTTHourVsl',inline = TRUE))
              )
            )
          )
        )#, #End of second fluid Row
        # fluidRow(
        #   div(
        #     class = 'section_box',
        #     div(
        #       class = 'section_box_title',
        #       "Average Miscellaneous Activities Timeline - Port"
        #     ),
        #     div(
        #       class = 'section_box_body',
        #       div(style="display:inline-block",uiOutput('MiscFrom')),
        #       div(style="display:inline-block",uiOutput('MiscTo')),br(),
        #       actionBttn('MiscAct',"View Duration",style = "jelly",
        #                                                    color = "success",
        #                                                    icon = icon("clock-o"))
        #     )
        #   )
        # )
        ###########################################################
      )
    )
  )
)
