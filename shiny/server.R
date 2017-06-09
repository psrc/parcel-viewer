function(input, output, session) {

  # functions ---------------------------------------------------------------
  
  # reset map
  leaflet.blank <- function() {
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng = -122.008546, lat = 47.549390, zoom = 9)
  }

  # Search ------------------------------------------------------------------ 

  # place holder for parcel_ids
  values <- reactiveValues(prclIds = NULL)
  
  # update place holder with user's parcel_ids
  observeEvent(input$s_goButton, {
    values$prclIds <- input$s_prcl_id
  })
  
  # clear map, table, and values in input text box
  observeEvent(input$s_clearButton, {
    values$prclIds = " "
  })
  
  observeEvent(input$s_clearButton, {
    updateTextInput(session, "s_prcl_id",
                    value = " ")
  })
  
  # filter for selected parcel_ids
  sSelected <- reactive({
    if (is.null(values$prclIds)) return(NULL)
    
    numItems <- scan(text = values$prclIds, sep = ",", quiet = TRUE)
    parcels %>% filter(parcel_id %in% numItems)
  })
  
  # table manipulation
  sTable <- reactive({
    if (is.null(sSelected())) return(NULL)
    
    sSelected <- sSelected()
    
    tbl <- sSelected %>%
      select(COUNTY, parcel_id, Shape_Area, lat, long) %>%
      left_join(attr, by = "parcel_id") %>%
      rename(county = COUNTY,
             bldg_sqft = building_sqft, 
             nonres_bldg_sqft = nonres_building_sqft,
             num_hh = number_of_households,
             num_jobs = number_of_jobs,
             res_units = residential_units) %>%
      mutate(locate = paste('<a class="go-map" href="" data-lat="', lat, '" data-long="', long, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
  })
  
  # display parcel_ids
  output$map <- renderLeaflet({
    if (is.null(sSelected())) {
      leaflet.blank()
    } else if (values$prclIds == " ") {
      leaflet.blank()
    } else {
    sSelected <- sSelected()
    
    marker.popup <- ~paste0("<strong>Parcel ID: </strong>", as.character(parcel_id))
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron, group = "Street Map") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
      addMarkers(data = sSelected,
                 ~long, 
                 ~lat,
                 popup = marker.popup
                 ) %>%
      addLayersControl(
        baseGroups = c("Street Map", "Imagery")
      ) %>%
      addEasyButton(
        easyButton(
          icon="fa-globe", 
          title="Zoom to Region",
          onClick=JS("function(btn, map){ 
                   map.setView([47.549390, -122.008546],9);
                   }"))
      )
    }
  })
  
  # zoom to selected parcel in datatable when 'locate' icon is clicked
  # Adapted from https://github.com/rstudio/shiny-examples/tree/master/063-superzip-example
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      lat <- input$goto$lat
      lng <- input$goto$lng
      map %>% setView(lng, lat, zoom = 12)
    })
  })
  
  output$s_dt <- DT::renderDataTable({
    locate <- DT::dataTableAjax(session, sTable())
    DT::datatable(sTable(), 
                  caption = "Click on icon in 'locate' field to zoom to parcel",
                  options = list(ajax = list(url = locate)), escape = FALSE)
  })
  
}# end server function






