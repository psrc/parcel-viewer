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
  values <- reactiveValues(ids = NULL)
  
  # Query by:
  sQueryBy <- eventReactive(input$s_goButton, {
    input$s_queryBy  
  })
  
  # update place holder with user's parcel_ids
  observeEvent(input$s_goButton, {
    values$ids <- input$s_id
  })
  
  # clear map, table, and values in input text box
  observeEvent(input$s_clearButton, {
    values$ids = " "
  })
  
  observeEvent(input$s_clearButton, {
    updateTextInput(session, "s_id",
                    value = " ")
  })
  
  # filter for selected ids based on sQueryBy()
  sSelected <- reactive({
    if (is.null(values$ids)) return(NULL)
  
    rng <- grep(":", values$ids)

    if (length(rng) > 0) {
      rng.result <- scan(text = values$ids, sep = ":", quiet = TRUE)
      numItems <- rng.result[1]:rng.result[2]
    } else {
      numItems <- scan(text = values$ids, sep = ",", quiet = TRUE)
    }
    expr <- lazyeval::interp(~col %in% numItems, col = as.name(sQueryBy()))
    parcels.attr %>% filter_(expr)
  })
  
  # table manipulation
  sTable <- reactive({
    if (is.null(sSelected())) return(NULL)
    
    sSelected <- sSelected()
    
    tbl <- sSelected %>%
      select(-(OBJECTID_12:PIN), -(MAJOR:Shape_Le_1), -(Shape_Length)) %>%
      rename(county = COUNTY,
             bldg_sqft = building_sqft, 
             nonres_bldg_sqft = nonres_building_sqft,
             num_hh = number_of_households,
             num_jobs = number_of_jobs,
             res_units = residential_units,
             gwthctr_id = growth_center_id,
             area = AREA,
             shape_area = Shape_Area
             ) %>%
      mutate(shape_area = round(shape_area, 10),
             area = round(area, 2),
             max_dua = round(max_dua, 2),
             lat = round(lat, 4),
             long = round(long, 4),
             locate = paste('<a class="go-map" href="" data-lat="', lat, '" data-long="', long, '"><i class="fa fa-crosshairs"></i></a>', sep="")) %>%
      select(locate, county, parcel_id, city_id, zone_id, faz_id, gwthctr_id, area, shape_area, bldg_sqft:res_units, lat,long) 
        
  })
  
  # display parcel_ids
  output$map <- renderLeaflet({
    if (is.null(sSelected())) {
      leaflet.blank()
    } else if (values$ids == " ") {
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
      map %>% setView(lng, lat, zoom = 16)
    })
  })
  
  output$s_dt <- DT::renderDataTable({
    locate <- DT::dataTableAjax(session, sTable())
    DT::datatable(sTable(), 
                  extensions = 'Buttons', 
                  caption = "Click on icon in 'locate' field to zoom to parcel",
                  options = list(ajax = list(url = locate),
                                 dom = 'Bfrtip',
                                 buttons = c('csv', 'excel')
                                 ), 
                  escape = c(1))
  })
  
}# end server function






