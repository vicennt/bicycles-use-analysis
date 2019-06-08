  # ------- Tab 2 "Weekly demand " ---------------

  output$api_test <- renderText({
    contract_name <- "Valence"
    station_num <- "1"
    url <- paste0("https://api.jcdecaux.com/vls/v1/stations/",station_num,"/?contract=",contract_name,"&apiKey=",key)
    df_api = jsonlite::fromJSON(url)
    paste0("Number: ", df_api$number,"\nName: ", df_api$name, "\nAddress: ", df_api$address)
  })
