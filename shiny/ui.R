navbarPage(theme = shinytheme("simplex"),
           "Parcel Viewer",
           tabPanel("Search",
                    tags$head(tags$script(src="gomap.js")),
                    fluidPage(
                      fluidRow(
                        column(width = 3,
                               h4("Enter one or more parcel_ids"),
                               helpText("Separate multiple ids with commas"),
                               textInput(inputId = "s_prcl_id",
                                         label = "",
                                         width = '100%'
                                        ),
                               actionButton(inputId = "s_goButton",
                                            label = "Enter"),
                               actionButton(inputId = "s_clearButton",
                                            label = "Clear all"),
                               br(),
                               br()
                               ), # end column
                        column(width = 9,
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
           ) # end tabPanel
) # end navbarPage