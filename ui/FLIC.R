###############FLIC##################
tabPanel(
  title = "Fleet Info Center",
  icon = icon('desktop'),
  div(
    id = "logo_subheader",
    img(src = "img/ScorpioLogo.png", height = "100%")
  ),
  fluidRow(
    hidden(
    column(
      12, id="hiddenT",
      div(
        class = "search__grid header-search-container",
        actionButton('showFilters', "Show Filters",class = "toolbar__btn", icon = icon("filter"))

      ),
      div(
        class = "section_box",

        div(
          class = "section_box_title",
          "Current Vessel Locations"
        ),
        div(
          class= "section_box_body",
          leafletOutput('cmap',height=600, width='100%')
        )
      ),
      div(
        class = "section_box",
        div(
          class = "section_box_title",
          "Current Vessel Details"
        ),
        div(
          class= "section_box_body",
          DTOutput('cmapTable')
        )
      )
    )),

    ###################
    
    column(
      9, id="visibleT",
      div(
        class = "search__grid header-search-container",
        actionButton('hideFilters', "Hide Filters",class = "toolbar__btn", icon = icon("filter"))

      ),
      div(
        class = "section_box",

        div(
          class = "section_box_title",
          "Current Vessel Locations"
        ),
        div(
          class= "section_box_body",
          leafletOutput('cmap2',height=600, width='100%')
        )
      ),
      div(
        class = "section_box",
        div(
          class = "section_box_title",
          "Current Vessel Details"
        ),
        div(
          class= "section_box_body",
          DTOutput('cmapTable2')
        )
      )
    ),

    ######################################
    
    column(
      3, id = "filtersT",
      div(class = "search__grid header-search-container",
          "Custom Filters"),
      div(
        class = "section_box",
        div(
          class = "section_box_title",
          "Watch Lists"
        ),
        div(
          class = "section_box_body",
          materialSwitch(inputId = "wlView",label = "View WatchLists",value = FALSE,status = "success"),
          materialSwitch(inputId = "viewFilter",label = "View Filters",value = TRUE,status = "success"),
          uiOutput('wlControl'),

          div(class = "section_box_divider"),
          uiOutput('delList'),
          actionButton('deleteList', 'Delete Watch List',class = "btn-primary"),
          div(class = "section_box_divider"),
          dropdownButton(
            uiOutput('editList'),
            uiOutput('multi'),
            actionButton('saveEdited', 'Save edited watchlist',class = "btn-primary"),
            circle = TRUE, status = "danger",
            icon = icon("gear"), width = "300px",

            tooltip = tooltipOptions(title = "Click here to edit watchlists")

        )

      )),
      conditionalPanel(condition = "input.viewFilter",
      div(
        class = "section_box",
        div(
          class = "section_box_title",
          "Filters"
        ),
        div(
          class = "section_box_body",
          uiOutput('vsltypeFLIC') ,
          div(class = "section_box_divider"),
          uiOutput('loafilter'),
          div(class = "section_box_divider"),
          uiOutput('GTfilter'),
          div(class = "section_box_divider"),
          uiOutput('Ownerfilter'),
          div(class = "section_box_divider"),
          uiOutput('IceClassfilter'),
          div(class = "section_box_divider"),
          uiOutput('dwtfilter'),
          div(class = "section_box_divider"),
          uiOutput('legfilter'),
          div(class = "section_box_divider"),
          uiOutput('IMOfilter'),
          div(class = "section_box_divider"),
          uiOutput('builtfilter'),
          div(class = "section_box_divider"),
          uiOutput('CargoTypeFilter'),
          div(class = "section_box_divider"),
          uiOutput('NavStatusFilter'),
          div(class = "section_box_divider"),
          textInput("dest","Type in the destination to filter the table", NULL),
          div(class = "section_box_divider"),
          tags$p("You can also save the filtered records as a custom watchlist to monitor. Please enter an appropriate
                 name for the list and press on the 'Save Watch List' button."),
          textInput('wlName',"Give a name to your list"),
          actionButton('saveList', 'Save Watch List',class = "btn-primary"),
          useShinyalert()
          )
        ))

    )
  )
)
