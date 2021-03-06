function(input, output, session) {

  # functions ---------------------------------------------------------------
  
  # reset/default map
  leaflet.blank <- function() {
    leaflet() %>%
      #addProviderTiles(providers$CartoDB.Positron, group = "Street Map") %>%
      addProviderTiles(providers$Esri.WorldStreetMap, group = "Street Map") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
      addLayersControl(
        baseGroups = c("Street Map", "Imagery")
      ) %>%
      setView(lng = -122.008546, lat = 47.549390, zoom = 9) %>%
      addEasyButton(
        easyButton(
          icon="fa-globe", 
          title="Zoom to Region",
          onClick=JS("function(btn, map){ 
                     map.setView([47.549390, -122.008546],9);}"))
          )
  }
  
  # show leaflet results
  leaflet.results <- function(proxy, selected.data, popup) {
    proxy %>% 
      clearMarkers() %>%
      addMarkers(data = selected.data,
                 ~long,
                 ~lat,
                 popup = popup
      ) 
  }  
  
  # format table
  format.table <- function(sSelected) {
    sSelected %>%
      select(-(OBJECTID_12:PIN), -(MAJOR:Shape_Le_1), -(Shape_Length)) %>%
      rename(county = COUNTY,
             bldg_sqft = building_sqft, 
             nonres_bldg_sqft = nonres_building_sqft,
             num_hh = number_of_households,
             num_jobs = number_of_jobs,
             num_bldgs = number_of_buildings,
             res_units = residential_units,
             gwthctr_id = growth_center_id,
             area = AREA,
             shape_area = Shape_Area
      ) %>%
      mutate(shape_area = round(shape_area, 10),
             area = round(area, 2),
             max_dua = round(max_dua, 2),
             max_far = round(max_far, 2),
             lat = round(lat, 4),
             long = round(long, 4)
      ) %>%
      select(county, parcel_id, zone_id, faz_id, gwthctr_id, city_id, area, shape_area, parcel_sqft, bldg_sqft:res_units, lat,long)
  }

  # Search by Number -------------------------------------------------------- 

  # place holder for parcel_ids
  values <- reactiveValues(ids = NULL)
  values.cl <- reactiveValues(ids = NULL)
  
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
  
  observeEvent(input$s_clearButton, {
    values$ids = " "
  })
  
  observeEvent(input$s_clearButton, {
    leafletProxy("map") %>% clearMarkers()
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
    parcels.filter <- parcels.attr %>% filter_(expr) 
    
    if (nrow(parcels.filter) > 5000){
      parcels.filter %>% sample_n(5000)
    } else {
      parcels.filter
    }
  })
  
  # table manipulation
  sTable <- reactive({
    if (is.null(sSelected())) return(NULL)
    
    sSelected <- sSelected()
    format.table(sSelected) %>%
      mutate(locate = paste('<a class="go-map" href="" data-lat="', lat, '" data-long="', long, '"><i class="fa fa-crosshairs"></i></a>', sep="")) %>%
      select(locate, everything())
  })
  
  observe({
    sSelected <- sSelected()
    if (is.null(sSelected) || values$ids == " ") return()
    marker.popup <- ~paste0("<strong>Parcel ID: </strong>", as.character(parcel_id))
    leaflet.results(leafletProxy("map"), sSelected, marker.popup)
  })
  
  # display parcel_ids
  output$map <- renderLeaflet({
    leaflet.blank()
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
      map %>% setView(lng, lat, zoom = 18)
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


  # Search by Click ---------------------------------------------------------  

    
  observe({
    event <- input$mapc_click
    if (is.null(event))
      return()
    isolate({
      x <- parcels.attr$lat
      y <- parcels.attr$long
      dist <- sqrt((x-event$lat)^2 + (y-event$lng)^2)
      values.cl$ids <- parcels.attr$parcel_id[which.min(dist)]
    })
  })
  
  sSelectedcl <- reactive({
    if (is.null(values.cl$ids)) return(NULL)
    parcels.attr %>% filter(parcel_id %in% values.cl$ids)
  })
  
  observe({
    sSelected <- sSelectedcl()
    if (is.null(sSelected) || values.cl$ids == " ") return()
    marker.popup <- ~paste0("<strong>Parcel ID: </strong>", as.character(parcel_id))
    leaflet.results(leafletProxy("mapc"), sSelected, marker.popup)
  })
    
  # display parcel_ids by clicks on the map
  output$mapc <- renderLeaflet({
    leaflet.blank()
    })
  
  # table manipulation
  sTablecl <- reactive({
    if (is.null(sSelectedcl())) return(NULL)
    sSelected <- sSelectedcl()
    format.table(sSelected)
  })
  
  output$s_dtc <- DT::renderDataTable({
    DT::datatable(sTablecl(), 
                  extensions = 'Buttons', 
                  options = list(dom = 'Bfrtip',
                                 buttons = c('csv', 'excel')
                  ), 
                  escape = c(1))
  })
  
}# end server function






