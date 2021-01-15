library(shinydashboard)
library(shinydashboardPlus)

pages <- unique(posts_raw$name)


shinyUI(
    tags$body(class="skin-blue sidebar-mini control-sidebar-open",
              tags$style(type="text/css",
                         ".shiny-output-error {visibility: hidden;}",
                         ".shiny-output-error:before {visibility: hidden;}"),
              tags$style(
                type = 'text/css', 
                '.bg-blue {background-color: #374e59!important; }'
              ),
              tags$style(
                HTML(
                  "/* logo */
        .skin-blue .main-header .logo {
                              background-color: #374e59;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #374e59;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #374e59;
                              }        

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #374e59;
                              }
                  "
                )
              ),
              tags$style(
                ".nav-tabs-custom .nav-tabs li.active {
                    border-top-color: #374e59;
                }"
              ),
              tags$style(HTML('[id^=q]{background-color:#374e59}')),
              
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
                    
                    hr(),
                    radioGroupButtons(
                      inputId = "pages_select",
                      label = "",
                      choices = c("Political parties", "Other pages"),
                      status = "primary"
                    ),
                    uiOutput(
                      'show_pages'
                    ),
                    hr(),
                    uiOutput('tab_controls'),
                    hr(),
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
                        fluidRow(
                          img(src = "TBP_dash_logo_trans.png", width = 200, height = 100)
                        ),
                        fluidRow(
                          img(src = 'IRI_colour_trans.png', width = 200, height = 100)
                        )
                    )
                  ),
                    fluidRow(
                      tabBox(height = "500px",
                             width = "300px",
                             id = 'box',
                             tabPanel(id = 1,
                                      "Posts and engagement",
                                fluidRow(
                                  column(width = 11,
                                         div(align = 'center', plotlyOutput("combined_line_chart") %>% withSpinner())
                                  ),
                                  column(width = 1,
                                    bsButton('q1', label = "", icon = icon("question"), style = "info", size = "small"),
                                    bsPopover('q1', 'Posts per day: total', content = paste0('The total posts across all selected ',
                                                                                         'accounts. Hover your cursor ',
                                                                                         'over the chart to view the ',
                                                                                         'total posts on any day.'))
                                  )
                                ),
                                HTML('<hr style="color: #3B3B3B;">'),
                                fluidRow(
                                  shinyBS::bsPopover(id = "test", title = "test", content = "this is a test")
                                ),
                                fluidRow(
                                  column(width = 11,
                                         div(align = 'center', plotlyOutput("posts_line_chart") %>% withSpinner())
                                  ),
                                  column(width = 1,
                                         bsButton('q2', label = "", 
                                                  icon = icon("question"), 
                                                  style = "info", 
                                                  size = "small"),
                                         bsPopover('q2', title = "Posts per day: by account", content = paste0('The total posts separated by account. ',
                                                                                                      'Hover your cursor over the chart to see the name ',
                                                                                                      'and number of posts for each account.'))
                                         )
                                  ),
                                HTML('<hr style="color: #3B3B3B;">'),
                                fluidRow(
                                  column(width = 11,
                                         div(align = 'center', plotlyOutput("engagement_line_chart") %>% withSpinner())
                                         ),
                                  column(width = 1,
                                         bsButton('q3', label = "", icon = icon("question"), style = "info", size = "small"),
                                         bsPopover('q3', title = "Engagement per day: by account", content = paste0('The total engagement (reactions, comments, and shares) ', 
                                                                                                      'on posts ',
                                                                                                      'by selected accounts. ',
                                                                                                      'Hover your cursor over the chart to see the name ',
                                                                                                      'and engagement for each account.')))
                                )
                             ),
                             tabPanel(id = 2,
                                      "Language and sentiment",
                                      fluidRow(
                                        column(width = 5,
                                          plotlyOutput("reactions_radar_chart") %>% withSpinner()
                                        ),
                                        column(width = 1,
                                               bsButton('q4', label = "", icon = icon("question"), style = "info", size = "small"),
                                               bsPopover('q4', title = "Reactions radar", content = paste0('For each selected account, the number of reactions are shown. ',
                                                                                                           'By default, the chart is rescaled, so the values shown ',
                                                                                                           'are relative to the ',
                                                                                                           'other reactions on the same account. Hover over the chart to see ',
                                                                                                           'the name and value of each account.'))
                                        ),
                                        column(width = 5,
                                          plotlyOutput("engagement_tree_chart") %>% withSpinner()
                                        ),
                                        column(width = 1,
                                               bsButton('q5', label = "", icon = icon("question"), style = "info", size = "small"),
                                               bsPopover('q5', title = "Reactions, comments, and shares", content = paste0(
                                                 'For each selected account, the number of reactions are shown. ',
                                                 'By default, the chart is rescaled, so the values shown ',
                                                 'are relative to the ',
                                                 'other reactions on the same account. Hover over the chart to see ',
                                                 'the name and value of each account.'))
                                        ),
                                      ),
                                      HTML('<hr style="color: #3B3B3B;">'),
                                      fluidRow(
                                        column(width = 5,
                                               plotOutput('sentiment_bar') %>% withSpinner()
                                               # plotOutput('cooccurences')
                                        ),
                                        column(width = 1,
                                               bsButton('q6', label = "", icon = icon("question"), style = "info", size = "small"),
                                               bsPopover('q6', title = "Sentiment analysis", content = paste0(
                                                 'For each selected account, the average positivity/negativity of each post is shown. ',
                                                 "The higher the score, the more positive the average post from that account."))
                                        ),
                                        column(width = 5,
                                               wordcloud2Output("wordcloud") %>% withSpinner()
                                        ),
                                        column(width = 1,
                                               bsButton('q7', label = "", icon = icon("question"), style = "info", size = "small"),
                                               bsPopover('q7', title = "Wordcloud", content = paste0(
                                                 'For each selected account, the most commonly used words are shown.'))
                                        ),
                                      ),
                                      # fluidRow(
                                      #   plotOutput('sentiment_bar')
                                      # )
                             ),
                             tabPanel(id = 3,
                                      "Media and polling",
                                      fluidRow(
                                        column(
                                          width = 1,
                                          div(
                                            h4("Media outlets", style = "transform: rotate(90deg)"),
                                            style = "padding-top: 250px; padding-left: 50px",
                                            align = "center")
                                        ),
                                        column(width = 9,
                                               h3(strong("   Media mentions of selected accounts", align = "left", 
                                                         style = "font-family: 'arial'; white-space: pre; font-size: 35px")),
                                               sankeyNetworkOutput("media_mention_sankey") %>% withSpinner()
                                               ),
                                        column(
                                          width = 1,
                                          div(
                                            h4("Selected accounts", style = "transform: rotate(90deg)"),
                                            style = "padding-top: 250px; padding-right: 50px",
                                            align = "center")
                                        ),
                                        HTML('<hr style="color: #3B3B3B;">'),
                                        column(width = 1,
                                               bsButton('q8', label = "", icon = icon("question"), style = "info", size = "small"),
                                               bsPopover('q8', title = "Media focus", content = paste0(
                                                 'This chart shows the frequency with which media outlets have mentioned ',
                                                 'selected accounts in their Facebook posts. Media outlets are shown on the left, and the account they have mentioned ',
                                                 'is shown on the right side.'))
                                               )
                                      ),
                                      fluidRow(
                                        column(
                                          width = 1,
                                          div(
                                            h4("Selected accounts", style = "transform: rotate(90deg)"),
                                            style = "padding-top: 250px; padding-left: 50px",
                                            align = "center")
                                        ),
                                        HTML('<hr style="color: #3B3B3B;">'),
                                        column(width = 9,
                                               h3(strong("   Selected accounts links to external media", align = "left", 
                                                         style = "font-family: 'arial'; white-space: pre; font-size: 35px")),
                                               sankeyNetworkOutput("account_share_sankey") %>% withSpinner()
                                               ),
                                        column(
                                          width = 1,
                                          div(
                                            h4("External website", style = "transform: rotate(90deg)"),
                                            style = "padding-top: 250px; padding-right: 50px",
                                            align = "center")
                                        ),
                                        column(width = 1,
                                               bsButton('q9', label = "", icon = icon("question"), style = "info", size = "small"),
                                               bsPopover('q9', title = "Links to external websites", content = paste0(
                                                 'The number of times a selected account has posted a link to external media. This includes links to ',
                                                 "non-news media, such as a political party website."
                                                 ))
                                        )
                                      ),
                                      HTML('<hr style="color: #3B3B3B;">'),
                                      fluidRow(
                                        column(
                                          width = 11,
                                          plotlyOutput("polls_line") %>% withSpinner()
                                        ),
                                        column(
                                          width = 1,
                                          bsButton('q10', label = "", icon = icon("question"), style = "info", size = "small"),
                                          bsPopover('q10', title = "Polling average", content = paste0(
                                            'Six-month polling average of major political parties.'
                                          ))
                                        )
                                      )
                             ),
                             tabPanel(id = 4,
                                      "Advertising",
                                      fluidRow(
                                        column(width = 11,
                                               plotOutput("ad_scatter")
                                        ),
                                        column(
                                          width = 1,
                                          bsButton('q11', label = "", icon = icon("question"), style = "info", size = "small"),
                                          bsPopover("q11", title = "Spending vs impressions", content = paste0(
                                            "Spending on each ad is visualized with the impressions for each ad. For both spend and impressions, ",
                                            "the range provided is averaged to approximate the actual value of spend or impressions. ",
                                            "Note this data is only collected for political parties ",
                                            "and not politicians or other political entities. Additionally, note that the maximum number of impressions ",
                                            "reported by Facebook is 1,000,000, but the real figure may be higher."
                                          ))
                                        )
                                      ),
                                      
                                      fluidRow(
                                        column(width = 11,
                                               DT::dataTableOutput('ad_table')),
                                        column(
                                          width = 1,
                                          bsButton('q12', label = "", icon = icon("question"), style = "info", size = "small"),
                                          bsPopover("q12", title = "Spending vs impressions", content = paste0(
                                            "Additional information on each advertisement. The table can be sorted by clicking on a column."
                                          ))
                                        )
                                      )
                                      ),
                             tabPanel(id = 5,
                                      "About this dashboard",
                             fluidRow(
                               column(width = 2),
                               column(width = 8,
                                      h5(
                                        paste0(
                                          "IRI’s custom dashboard to give a more in-depth look at the data from the CrowdTangle Live Display. Dash Daily takes daily updates of CrowdTangle data and augments it with Natural Language Processing, available polling data from EuropeElects.eu, and Facebook Ad Library to enable more in-depth monitoring of the conversations and context. The Dashboard updates daily at 20:30 Eastern Europe Standard Time.
*Data from CrowdTangle, a Facebook-owned tool that tracks interactions on public content from Facebook pages and groups, verified profiles, Instagram accounts, and subreddits. It does not include paid ads unless those ads began as organic, non-paid posts that were subsequently “boosted” using Facebook’s advertising tools. It also does not include activity on private accounts, or posts made visible only to specific groups of followers.
*Natural Language Processing is handled using Amazon Translate API, and the UDPipe R package.
"
                                          
                                        ),
                                        align = 'left',
                                        style = "font-size: 15px"
                                      ))
                             )
                             )
                      )
                    )
                )
            )
          )
)
