#################################################

observeEvent(input$editData, {
  req(input$ownUsername)
  req(input$ownPassword)
  req(input$Q88)
  req(input$veslink)
  req(input$mmsiUsername)
  req(input$mmsiPassword)
  req(input$validTime)
  
  config$username_own <- input$ownUsername
  config$password_own <- input$ownPassword
  config$q88authstring <- input$Q88
  config$veslinkapitoken <- input$veslink
  config$username_competitor <- input$mmsiUsername
  config$password_competitor <- input$mmsiPassword
  config$validTime <- input$validTime
})