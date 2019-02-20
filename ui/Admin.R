tabPanel(
  title = "Admin",
  icon = icon('tools'),
  fluidRow(
    column(
      3,
      div(
        class = "section_box",
        div(
          class = "section_box_title",
          "Admin Panel"
        ),
        div(
          class = "section_box_body",
          "Pleaase use the form on the right to edit existing
          credntials for connecting to various data sources through APIs.",
          br(), br()
        )
      )
    ),
    column(
      9,
      div(
        class = "section_box",
        div(
          class = "section_box_body",
          # div(style="display: inline-block;vertical-align:top; width: 300px;",
          #     textInput('searoutesURL', 'URL for Sea Routes API',value = config$searoutesurl)),
          # div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
        
          textInput('searoutesKey', 'Key for Sea Routes API', value = config$searouteskey, width = '300px'),
          br(),
          div(style="display: inline-block;vertical-align:top; width: 300px;",
              textInput('ownUsername', 'Username for Own Vessels on Stratum',value = config$username_own)),
          div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
          div(style="display: inline-block;vertical-align:top; width: 400px;",
              textInput('ownPassword', 'Password for Own Vessels on Stratum', value = config$password_own)),
          br(),
          div(style="display: inline-block;vertical-align:top; width: 300px;",
              textInput('Q88', 'Q88 Authorization String',value = config$q88authstring)),
          div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
          div(style="display: inline-block;vertical-align:top; width: 400px;",
              textInput('veslink', 'Veslink API token', value = config$veslinkapitoken)),
          br(),
          div(style="display: inline-block;vertical-align:top; width: 300px;",
              textInput('mmsiUsername', 'Username for Stratum API for WVD',value = config$username_competitor)),
          div(style="display: inline-block;vertical-align:top; width: 100px;",HTML("<br>")),
          div(style="display: inline-block;vertical-align:top; width: 400px;",
              textInput('mmsiPassword', 'Password for Stratum API for WVD', value = config$password_competitor)),
          br(),
          textInput('validTime', 'Time validity for tonnage and cargo lists in days', value = config$validTime),
          br(), br(),
          actionBttn('editData',"Save Credentials",style = "jelly",
                     color = "success",
                     icon = icon("edit"))
          
        )
      )
    )
  )
)