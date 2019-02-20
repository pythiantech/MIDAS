tabPanel(
  title = "Projects Desk",
  icon = icon('desktop'),
  div(
    id = "poslist_page",
    div(
      id = "logo_subheader",
      img(src = "img/ScorpioLogo.png", height = "100%")
    ),
  tabsetPanel(
    id = "poslist_nav",
    ##############################
    tabPanel(
      title = "Data",
      fluidRow(
        column(
          3,
          div(
            class = "section_box",
            div(
              class = "section_box_title",
              "TC Data"
              ),
            div(
              class = "section_box_body",
              "The table shows data on vessels on Time Charter.
              You can edit the table and once done, press the save button to save the data.",
              actionButton('savetc',"Save",class = "btn-primary")
            )
            
          )
        ),
        column(
          9,
          div(
            class = "section_box",
            div(
              class = "section_box_title",
              "Time Charter Data"
            ),
            div(
              class = "section_box_body",
              rHandsontableOutput("TCData")
            )
          )
        )
      )
    ),
    #############################
    tabPanel(
      title = "Calendar",
      fluidRow(
        column(
          3,
          div(
            class = "section_box",
            div(
              class = "section_box_title",
              "Cardinal Dates"
            ),
            div(
              class = "section_box_body",
              "The calendar displays important dates for vessels on time charter.
              The name of the vessels appear on calendar dates and are colour coded as per the following list :",
              br(),br(),
              tags$li("Green - Anniversary Date"),
              tags$li("Orange - Earliest Re-delivery"),
              tags$li("Blue - Latest Re-delivery"),
              tags$li("Red - Exercise Operation"),
              div(class = "section_box_divider"),
              pickerInput('calvsl', "Select a vessel", choices = levels(as.factor(DF$Vessel)),
                          selected = DF$Vessel[1]),
              uiOutput('option'),
              "CP Date: ",
              tags$strong(textOutput("cpdate", inline = TRUE)), br(),
              "Period: ",
              tags$strong(textOutput("period", inline = TRUE)), br(),
              "Delivery Date: ",
              tags$strong(textOutput("deldate", inline = TRUE)), br(),
              "Anniversary Date: ",
              tags$strong(textOutput("annivdate", inline = TRUE)), br(),
              "Redelivery Date: ",
              tags$strong(textOutput("rddate", inline = TRUE)), br(),
              "Redelivery Notices: ",
              tags$strong(textOutput("rdnot", inline = TRUE)), br(),
              "Redelivery Area: ",
              tags$strong(textOutput("redarea", inline = TRUE)), br(),
              "Earliest Redelivery: ",
              tags$strong(textOutput("ered", inline = TRUE)), br(),
              "Latest Redelivery: ",
              tags$strong(textOutput("lred", inline = TRUE)), br(),
              "Rate: ",
              tags$strong(textOutput("rate", inline = TRUE)), br()
            # pickerInput("cdate","Choose date type", multiple=F,
            #             choices= c("Anniversary Date (Green)"="AnnivDate","Earliest Re-delivery (Orange)"="EReD",
            #                        "Latest Re-delivery (Blue)"="LReD","Exercise Option (Red)"="XO"),
            #             selected = "AnnivDate")
            )
          )
        ),
        column(
          9,
          div(
            class = "section_box",
            div(
              class = "section_box_title",
              "Cardinal Dates"
            ),
          div(
              class = "section_box_body",
              fullcalendarOutput("calData",width = "100%", height='auto')
              # timevisOutput('calData',width = "100%", height = "600px"),
              # div("Selected Vessel: ", tags$strong(textOutput("selected", inline = TRUE))),
              # div("Redelivery Area: ", tags$strong(textOutput('redelarea', inline = TRUE))),
              # div("Redelivery Notices: ", tags$strong(textOutput('redelnotice', inline = TRUE)))
            )
          )
        )
      )
    ),
    #############################
    tabPanel(
      title = "Fleet Mapping",
      fluidRow(
        column(
          3,
          div(
            class = "section_box",
            div(
              class = "section_box_title",
              "Order Books and Active Fleet Analysis"
            ),
            div(
              class = "section_box_body",
              "This tab allows you to view sumary statistics on order books and active fleets from the
              world vessel database compiled in-house."
            )
          )
        ),
        column(
          9,
          div(
            class = "section_box",
            div(
              class = "section_box_title",
              "Summary Statistics"
            ),
            div(
              class = "section_box_body",
              fluidRow(
                column(
                  6,
                  highchartOutput('vslConstruction',height = '600px')
                ),
                column(
                  6,
                  fluidRow(
                    column(6,selectInput('type','Select Builder/Owner',
                                         choices = c('Builder'="Builder",'Owner Group'="OwnerGroup"))),
                    column(6, sliderInput('minvsl',"Select minimum number of vessels",min=1,value=10,max=100))
                  ),
                  highchartOutput('Builder',width='100%', height = '600px')
                )
              ),
              fluidRow(
                div(class = "section_box_divider"),br(),
                column(
                  6,
                  fluidRow(column(6,selectInput('OwnerGroup','Select Owner Group',
                                                choices = levels(as.factor(combineds$OwnerGroup)),
                                                selected = "Scorpio Group"))),
                  highchartOutput('OBAF', width = '100%', height = '600px')
                ),
                column(
                  6,
                  fluidRow(

                    column(6, selectInput('vslclass', 'Select vessel type',
                                          choices = levels(as.factor(consolidated$`Vessel Type`)),
                                          selected = "Handy"))
                  ),
                  highchartOutput('ByAge', width = '100%', height = '600px')
                )



                )

              )
            )
          )
        )
      ),
      # fluidRow(
      #   column(6, selectInput('OwnerGroup','Select Owner Group',
      #                         choices = levels(as.factor(combineds$OwnerGroup))),
      #          highchartOutput('OBAF', width = '100%', height = '600px'))
      # )
      #
    # ),
    #############################
    tabPanel(
      title = "PNL Summary",
      fluidRow(
        column(6,


      div(
        class = "section_box",
        div(
          class = "section_box_title",
          "TC Vessel PNL Summary"
        ),
        div(
          class = "section_box_body",
          selectInput('VName',"Select Vessel on Time Charter",
                      choices = levels(as.factor(TCNew$Vessel)),
                      selected = "GAN-TRUST"),
          DTOutput('PNLSummary')
        )
      )
      ),
      column(6,

             div(
               class = "section_box",
               div(
                 class = "section_box_title",
                 "Scenario Analysis"
               ),
               div(
                 class = "section_box_body",
                 sliderInput('baseVal', "Select Base Cost",min = 10000,
                             max = 20000,value = 13000, step = 100),
                 div(style="display: inline-block;vertical-align:top; width: 250px;",sliderInput('goodSc',"Select Good Scenario value (relative to base)",
                             min=1000, max=4000, val=2000, step = 100)),
                 div(style="display: inline-block;vertical-align:top; width: 250px;",sliderInput('badSc',"Select Bad Scenario value (relative to base)",
                             min=1000, max=4000, val=2000, step = 100)),
                 plotOutput('Scenario')
               )
             )
             )
      )
    ),
    #######################################
    tabPanel(
      title = "Time Charter Report",
      fluidRow(
      column(12,         
      
      div(
        class = "section_box",
        div(
          class = "section_box_title",
          "Time Charter Report"
        ),
        div(
          class = "section_box_body",
          uiOutput('TCR')
        )
      )
      ))
    ),
    #############################################
    #######################################
    tabPanel(
      title = "Broker Time Charter Report",
      fluidRow(
        column(12,         
               
               div(
                 class = "section_box",
                 div(
                   class = "section_box_title",
                   "Broker Time Charter Report"
                 ),
                 div(
                   class = "section_box_body",
                   uiOutput('BTCR')
                 )
               )
        ))
    )
    #############################################
  )
 )
)
