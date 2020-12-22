library(shinydashboard)
library(shinydashboardPlus)

pages <- unique(posts_raw$name)


shinyUI(
    
    tags$body(class="skin-blue sidebar-mini control-sidebar-open",
              tags$style(type="text/css",
                         ".shiny-output-error {visibility: hidden;}",
                         ".shiny-output-error:before {visibility: hidden;}"),
              
              dashboardPagePlus(
                  header = dashboardHeaderPlus(
                      title = "Romania social media",
                      enable_rightsidebar = TRUE,
                      rightSidebarIcon = "gears"
                      # dropdownBlock(
                      #     id = 'dropdown',
                      #     title = "More dashboards",
                      #     type = 'tasks'
                      # )
                  ),
                sidebar = dashboardSidebar(
                  width = "0px"
                ),
                rightsidebar = rightSidebar(
                    width = 225,
                    title = 'Dashboard controls',
                    background = 'dark',
                    
                    radioGroupButtons(
                      inputId = "time_range",
                      label = "", 
                      choices = c("Week", "Month", 'Year'),
                      status = "primary"
                    ),
                    uiOutput(
                      'date_range'
                    ),
                    
                    radioGroupButtons(
                      inputId = "pages_select",
                      label = "",
                      choices = c("Political parties", "Other pages"),
                      status = "primary"
                    ),
                    uiOutput(
                      'show_pages'
                    ),
                    uiOutput('tab_controls'),
                    uiOutput('compare_elements')
                        
                ),
                body = dashboardBody(
                  tags$script("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';"),
                  fluidRow(
                    column(9,
                           fluidRow(
                             valueBoxOutput('posts_box', 4),
                             valueBoxOutput('most_posts_box', 4),
                             valueBoxOutput('reactions_box', 4),
                           ),
                           fluidRow(
                             valueBoxOutput('media_box', 4),
                             valueBoxOutput('media_engagement', 4),
                             valueBoxOutput('media_mentions', 4)
                           )
                    ),
                    column(3,
                           img(src = "TBP_dash_logo.gif", width = 200, height = 200)
                    )
                  ),
                    fluidRow(
                      tabBox(height = "500px",
                             width = "300px",
                             id = 'box',
                             tabPanel(id = 1,
                                      "Posts and engagement",
                                fluidRow(
                                  column(width = 12,
                                         plotlyOutput("combined_line_chart")
                                         )
                                ),
                                fluidRow(
                                  column(width = 12,
                                         plotlyOutput("posts_line_chart")
                                         )
                                  ),
                                fluidRow(
                                  column(width = 12,
                                         plotlyOutput("engagement_line_chart")
                                         )
                                )
                             ),
                             tabPanel(id = 2,
                                      "Language and sentiment",
                                      fluidRow(
                                        column(width = 5,
                                          plotlyOutput("reactions_radar_chart"),
                                        ),
                                        column(width = 5,
                                          plotlyOutput("engagement_tree_chart")
                                        )
                                      ),
                                      fluidRow(
                                        column(width = 5,
                                               plotOutput('cooccurences')
                                        ),
                                        column(width = 5,
                                               wordcloud2Output("wordcloud")
                                        )
                                      ),
                                      fluidRow(
                                        plotOutput('sentiment_bar')
                                      )
                             ),
                             tabPanel(id = 3,
                                      "Media and polling",
                                      fluidRow(
                                        h3("Media mentions of political parties and figures", align = "center")
                                      ),
                                      sankeyNetworkOutput("media_mention_sankey"),
                                      fluidRow(
                                        h3("Links to external media", align = "center")
                                      ),
                                      sankeyNetworkOutput("account_share_sankey"),
                                      plotlyOutput("polls_line")
                             )
                      )
                    )
                )
            )
          )
)
