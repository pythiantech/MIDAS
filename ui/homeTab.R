tabPanel(
  title = "Home",
  icon = icon("home"),
  div(
    id = "home_page",
    div(
      id = "home_intro_row",
      img(id = "home_logo", src = "img/ScorpioLogo.png"), 
      div(
        id = "home_intro_text",
        "Welcome to", strong("MIDAS"),
        "- Maritime Intelligence & Data Analytics System is a user friendly web application for visualizing maritime data from",
        "IMOS, Stratum, Veson and other sources, to help you make more informed decisions."
        
      )
    ),

    fluidRow(
      column(
        12,
        div(
          class = "section_box",
          id = "section_faq",
          div(
            class = "section_box_title",
            "How does it work?"
          ),
          div(
            class = "section_box_body",
            tags$h3("Fleet Information Center (FLIC)"),
            tags$p(style = "display: inline-block;vertical-align:top; width: 800px;","With an impetus on global competition, the
            Fleet info center is designed to assist the user with real-time situational awareness to identify and keep track of global
            Competitor fleets.The tab is designed by segregation into 'worldwide' as well as market competitor 'watchlists',
            allowing the user to customize his own fleet list, select and identify location as well as provide some fundamental voyage 
                   details for the watch listed vessels or any worldwide fleet."),
            div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
            tags$img(style = "display: inline-block;vertical-align:top; width: 500px;",src = "img/FLIC.png"),
            # tags$video(style = "display: inline-block;vertical-align:top; width: 500px;",src = "img/FLIC.mp4",
            #            type = "video/mp4", autoplay = "",loop = "", controls = NA,  width = '600px'),
            div(class = "section_box_divider"),
            tags$h3("Market Place"),
            tags$img(style = "display: inline-block;vertical-align:top; width: 500px;",src = "img/MktPlace.png"),
            # tags$video(style="display: inline-block;vertical-align:top; width: 500px;",src = "img/MarketPlace.mp4",
            #            type="video/mp4", autoplay = "",loop="", controls = NA,  width='600px'),
            div(style = "display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
            tags$p(style = "display: inline-block;vertical-align:top; width: 800px;",
                   "A Pre-fixture management tool wherein the chartering teams across various regions can proactively handle and manage Spot fixture opportunities.

Vessel position tab to provide the chartering team with a bird's eye view on unfixed vessels, and also helps to determine optimum fleet distribution.

The Tool also provides with 3 main categories of Tools - Position List for Own Fleet, Tonnage List for Competitor fleet, Cargo List for Market fixture opportunity.

The tables feature an excel like functionality for ease of use with current market information."),

            div(class = "section_box_divider"),
            tags$h3("Trade Flow"),
            tags$p(style = "display: inline-block;vertical-align:top; width: 800px;",
                   "A trade area analysis tool which on a real time basis helps the user identify the most profitable region/area to position their vessel in order to maximize revenue.

The Trade flow comparison of Scorpio as well as competitor fleet would be provided to analyze historical data."),
            div(style = "display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
            tags$img(style = "display: inline-block;vertical-align:top; width: 500px;",src = "img/TradeFlow.png"),
            # tags$video(style = "display: inline-block;vertical-align:top; width: 500px;",src = "img/Trade.mp4",
            #            type = "video/mp4", autoplay = "",loop="", controls = NA,  width='600px'),
            div(class = "section_box_divider"),

            tags$h3("Projects Desk"),
            tags$img(style = "display: inline-block;vertical-align:top; width: 500px;",src = "img/Projects.png"),
            # tags$video(style="display: inline-block;vertical-align:top; width: 500px;",src = "img/Projects.mp4",
            #            type="video/mp4", autoplay = "",loop="", controls = NA,  width='600px'),
            div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
            tags$p(style="display: inline-block;vertical-align:top; width: 800px;",
                   "A Time charter market analysis tool containing an extensive data table and calendar for all vessels which 
                   are Time chartered in and out. A detailed calendar provides the user with an interactive interface which accentuates 
                   cardinal dates and option periods for each charter to better keep a track of the same. This tab highlights 
                   market analysis on time charter revenue by projection on earning trends to analyze time charter performance.")

            # tags$br(),
            # tags$p(style="display: inline-block;vertical-align:top; width: 800px;", "In case you wish to create your own list,
            #         please slect vessels of interest from the 'Select vessels to create custom list' drop dwon option. Remeber
            #        to give a suitable name to your list and then click on the 'Save Watch List' option."),
            # tags$br(),
            # tags$p(style="display: inline-block;vertical-align:top; width: 800px;", "In case you wish to create your own list,
            #         please slect vessels of interest from the 'Select vessels to create custom list' drop dwon option. Remeber
            #        to give a suitable name to your list and then click on the 'Save Watch List' option.")
            # "You can email us at",
            # tags$a("Pythian Technologies", href = "mailto:dhiraj@pythiantech.com"),

          )
        )
      )

    ),
    fluidRow(column(
      12,
      tags$blockquote(
        class = "pull-right",
        tags$p(tags$em("In God we trust. All others must bring data.")),
        tags$small("W. Edwards Deming")
      )
    ))
  )
)
