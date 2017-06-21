navbarPage(theme = shinytheme("simplex"),
           "Parcel Viewer",
           tabPanel("Search by Number",
                    tags$head(tags$script(src="gomap.js")),
                    fluidPage(
                      fluidRow(
                        column(width = 2,
                               selectInput(inputId = "s_queryBy",
                                           label = h4("Query for parcels by:"),
                                           choices = list("Parcel ID" = "parcel_id",
                                                          "TAZ" = "zone_id",
                                                          "FAZ" = "faz_id",
                                                          "Growth Center ID" = "growth_center_id",
                                                          "City ID" = "city_id"
                                                          ),
                                           width = '100%'
                               ),
                               br(),
                               h4("Enter one or more ids"),
                               helpText("Separate multiple ids with commas or type as range (i.e. 1000:2000)"),
                               textInput(inputId = "s_id",
                                         label = "",
                                         width = '100%'
                                        ),
                               actionButton(inputId = "s_goButton",
                                            label = "Enter"),
                               actionButton(inputId = "s_clearButton",
                                            label = "Clear all"),
                               br(),
                               br(),
                               helpText("Geographic areas containing many parcels will be sampled to 5,000 parcels"),
                               br()
                               ), # end column
                        column(width = 10,
                               leafletOutput("map", height = "725px")
                               ) # end column
                      ), # end fluidRow
                      br(),
                      fluidRow(
                        # column(width = 3),
                        column(width = 12,
                               DT::dataTableOutput("s_dt")
                               ) # end column
                      ) # end fluidRow
                    ) # end fluidPage
           ), # end tabPanel
           tabPanel("Search by Click",
              tags$head(tags$script(src="gomap.js")),
              fluidPage(
                      fluidRow(
                        leafletOutput("mapc", height = "725px")
                      ), # end fluidRow
                      br(),
                      fluidRow(
                        # column(width = 3),
                        #column(width = 12,
                               DT::dataTableOutput("s_dtc")
                        #) # end column
                      ) # end fluidRow
              ) # end fluidPage
           )
) # end navbarPage