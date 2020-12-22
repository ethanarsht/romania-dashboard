

shinyServer(function(input, output) {
    
    options(shiny.sanitize.errors = TRUE)
    
    
    observeEvent(input$time_range,
                 {
                     output$date_range <- renderUI({
                         if (input$time_range == 'Week') {
                             minus <- 7
                         } else if (input$time_range == 'Month') {
                             minus <- 30
                         } else {
                             minus <- 365
                         }
                             dateRangeInput(
                                 'date_range',
                                 label = 'Date range',
                                 start = max(posts_raw$date) - days(minus),
                                 end = max(posts_raw$date)
                             )
                     })
                 })
    
    observeEvent(input$pages_select,
                 {
                     
                     output$show_pages <- renderUI({
                         pages <- unique(posts_raw$name)
                         if (input$pages_select == 'Political parties') {
                             selected <- sort(party_names)
                         } else {
                             df_choices <- posts_raw %>% filter(!name %in% party_names)
                             
                             selected <- sort(unique(df_choices$name))
                         }
                         pickerInput(
                             'show_pages',
                             label = 'Pages to show',
                             choices = pages,
                             selected = selected,
                             options = list(
                                 `actions-box` = TRUE,
                                 size = 10,
                                 `selected-text-format` = "static",
                                 title = "Select parties"
                             ),
                             choicesOpt = list(
                                 content = stringr::str_trunc(pages, width = 18)
                             ),
                             multiple = TRUE
                         )
                     })
                 })
    
    output$posts_box <- renderValueBox({
        posts <- nrow(df_posts())
        valueBox(
            posts, "Posts",
            color = "blue",
            icon = icon("comment-dots"),
            width = 2
        )
    })
    output$reactions_box <- renderValueBox({
        reacs <- df_posts() %>%
            rowwise() %>%
            mutate(
                total_reactions = sum(likeCount, loveCount, wowCount, hahaCount, sadCount, angryCount, thankfulCount, careCount)
            ) %>%
            ungroup() %>%
            group_by(short_name) %>%
            summarize(reactions = sum(total_reactions), color = first(as.character(valid_color))) %>%
            ungroup() %>%
            arrange(desc(reactions))
        
        most_reactions <- reacs[[1, 'short_name']]
        color <- reacs[[1, 'color']]
        if (is.null(color)) {
            color <- "blue"
        }
        valueBox(
            most_reactions,
            "Most engaged account",
            color = "blue",
            icon = icon("user-circle"),
            width = 2
        )
    })
    
    output$most_posts_box <- renderValueBox({
        posts <- df_posts() %>%
            group_by(short_name) %>%
            summarize(post_count = n(), color = first(valid_color)) %>%
            ungroup() %>%
            arrange(desc(post_count))
        most_posts <- posts[[1, 'short_name']]
        color <- posts[[1, 'color']]
        if (is.null(color)) {
            color <- 'blue'
        }
        
        valueBox(
            most_posts,
            'Account with most posts',
            color = "blue",
            icon = icon("user-circle"),
            width = 2
        )
    })
    
    output$media_box <- renderValueBox({
        # validate(
        #     need(nrow(df_media_count()) > 1, "No data for current selection")
        # )
        media_temp <- df_media_count() %>%
            group_by(source_name) %>%
            summarize(
                count = n()
            ) %>%
            arrange(desc(count))
        
        most_shared <- media_temp[[1, 'source_name']]
        
        valueBox(
            most_shared,
            "Most shared external media",
            color = 'blue',
            icon = icon("share-square"),
            width = 2
        )  
    })
    
    output$media_engagement <- renderValueBox({
        media_grouped <- df_media() %>%
            group_by(name) %>%
            summarize(
                total_reactions = sum(likeCount, loveCount, wowCount, hahaCount, sadCount, angryCount, thankfulCount, careCount, shareCount, commentCount)
            ) %>%
            arrange(desc(total_reactions))
        
        top_media_engagement <- media_grouped[[1, 'name']]
        
        valueBox(
            top_media_engagement,
            color = "blue",
            "Most engaged media outlet",
            icon = icon("share-square")
        )
    })
    
    output$media_mentions <- renderValueBox({
        df_mentions <- df_media() %>%
            group_by(media_mentions) %>%
            summarize(count = n()) %>%
            ungroup() %>%
            arrange(desc(count))
        
        most_mentions <- df_mentions[[1, 'media_mentions']]
        color <- df_mentions[[1, 'color']]
        if (is.null(color)) {
            color <- "blue"
        }
        
        valueBox(
            most_mentions,
            "Most media mentions",
            color = "blue",
            icon = icon("comment")
        )
    })
    
    output$tab_controls <- renderUI({
        if (input$box == 'Posts and engagement') {
            tagList(
                numericInput(
                    'roll',
                    label = 'Rolling averages',
                    value = 3,
                    min = 0
                ),
                checkboxInput(
                    'compare',
                    label = 'Compare timeframes',
                    value = FALSE
                )
            )
            
        } else if (input$box == 'Language and sentiment') {
            tagList(
                switchInput(
                    'radar_scale',
                    label = 'Radar scale control',
                    value = TRUE,
                    onLabel = 'Scaled',
                    offLabel = 'Raw'
                ),
                switchInput(
                    'wordcloud_lang',
                    label = 'Language',
                    value = TRUE,
                    onLabel = "Romanian",
                    offLabel = "English"
                ),
                pickerInput(
                    'input_pos',
                    label = 'Parts of speech',
                    choices = unique(corpus$upos),
                    selected = unique(corpus$upos),
                    options = list(
                        `actions-box` = TRUE,
                        size = 10,
                        `selected-text-format` = "static",
                        title = "Select parts of speech"
                    ),
                    multiple = TRUE
                ),
                pickerInput(
                    'reaction_select',
                    label = "Reaction",
                    choices = c('All', 'Comment', 'Share', 'Like'),
                    selected = "All",
                    options = list(
                        `actions-box` = TRUE,
                        size = 10,
                        `selected-text-format` = "static",
                        title = "Select parts of speech"
                    )
                )
            )
        }
    })
    
    df_posts <- reactive({
        shiny::validate(
            need(input$show_pages > 0, "No data on current selection")
        )
       
        if (input$compare == TRUE) {
            df_one <- posts_raw %>%
                filter(name %in% input$show_pages) %>%
                filter(date >= input$date_range[1] 
                       & date <= input$date_range[2]) %>%
                mutate(
                    compare = "Primary"
                )
            
            df_two <- posts_raw %>%
                filter(name %in% input$show_pages) %>%
                filter(date >= input$date_range_comp[1] 
                       & date <= input$date_range_comp[2]) %>%
                mutate(
                    compare = "Comparison"
                )
            df_filter <- bind_rows(df_one, df_two) %>%
                mutate(
                    compare = factor(compare, levels = c("Primary", "Comparison"))
                )
        } else {
            df_filter <- posts_raw %>%
                filter(name %in% input$show_pages) %>%
                filter(date >= input$date_range[1] 
                       & date <= input$date_range[2])
        }
        df_filter
        
    })
    
    df_media <- reactive({
        extract_vector <- paste(c(df_posts()$name, party_short_names), collapse = '|')

        test <- media %>%
            filter(date >= today() - 7
                   & date <= today()) %>%
            mutate(
                media_mentions = str_extract_all(message, extract_vector)
            ) %>%
            separate_rows(
                media_mentions, sep = ','
            ) %>%
            drop_na(
                media_mentions
            ) %>%
            filter(media_mentions != "")
    })
    
    df_polls <- reactive({
        polls %>%
            filter(`Fieldwork End` >= input$date_range[1]
                   & `Fieldwork End` <= input$date_range[2])
    })
    
    df_cloud_en <- reactive({
        corpus_en %>%
            filter(name %in% input$show_pages) %>%
            filter(date >= input$date_range[1]
                   & date <= input$date_range[2]) %>%
            filter(upos %in% input$input_pos)
    })
    
    df_cloud <- reactive({
        corpus %>% 
            filter(name %in% input$show_pages) %>%
            filter(date >= input$date_range[1]
                   & date <= input$date_range[2]) %>%
            filter(upos %in% input$input_pos)
    })
    
    df_media_count <- reactive({
        validate(
            need(nrow(df_posts()) > 0, "No data on current selection")
        )
        data <- df_posts() %>%
            mutate(
                source_link = str_extract(link, '//.*?/'),
                source_link = str_remove_all(source_link, '/')
            ) %>%
            select(
                source_link, name
            ) %>%
            filter(source_link != 'www.facebook.com') %>%
            rename(source_name = source_link) %>%
            filter(!is.na(source_name)) %>%
            group_by(source_name, name) %>%
            summarize(
                count = n()
            ) %>%
            ungroup() %>%
            mutate(
                source_name = str_extract(source_name, ".*\\."),
                source_name = str_remove(source_name, "www\\."),
                source_name = str_remove_all(source_name, '\\.')
            ) %>%
            drop_na(source_name)
        data
    })
    
    observeEvent(input$compare,
                 {
                     output$compare_elements <- renderUI({
                         if (input$compare == FALSE) {
                             NULL
                         } else {
                             dateRangeInput(
                                 'date_range_comp',
                                 label = 'Date range for comparison',
                                 start = max(posts_raw$date) - days(7),
                                 end = max(posts_raw$date)
                             )
                         }
                     })
                 })
    
    output$combined_line_chart <- renderPlotly({
        if (input$compare == FALSE) {
            df_temp <- df_posts() %>%
                mutate(day = date(date)) %>%
                group_by(day) %>%
                summarize(count = n()) %>%
                arrange(day) %>%
                ungroup() %>%
                mutate(
                    `Rolling average` = zoo::rollmean(count, k = input$roll, fill = NA, align = 'right'),
                )
        } else {
            df_one <- df_posts() %>%
                filter(compare == "Primary") %>%
                mutate(day = date(date)) %>%
                group_by(day) %>%
                summarize(count = n(),
                          compare = first(compare)) %>%
                arrange(day) %>%
                ungroup() %>%
                mutate(
                    `Rolling average` = zoo::rollmean(count, k = input$roll, fill = NA, align = 'right')
                )
            
            df_two <- df_posts() %>%
                filter(compare == "Comparison") %>%
                mutate(day = date(date)) %>%
                group_by(day) %>%
                summarize(count = n(),
                          compare = first(compare)) %>%
                arrange(day) %>%
                ungroup() %>%
                mutate(
                    `Rolling average` = zoo::rollmean(count, k = input$roll, fill = NA, align = 'right')
                )
            df_temp <- bind_rows(df_one, df_two)
        }
        
        g <- ggplot(df_temp, aes(x = day, y = `Rolling average`)) +
            geom_line() + 
            bbplot::bbc_style() + 
            theme(legend.position = "none") +
            labs(
                title = paste0(
                    "Posts per day"
                )
            )
        
        plotly_output <- ggplotly(g,
                                  tooltip = c('Name', 'Rolling average')) %>%
            layout(
                width = 1000
            ) 
        
    })
    

    output$posts_line_chart <- renderPlotly({
        
        if (input$compare == FALSE) {
            
            df_n_posts <- data.frame()
            for (party in unique(df_posts()$name)) {
                df_temp <- df_posts() %>%
                    filter(name == party) %>%
                    mutate(day = date(date)) %>%
                    group_by(day, short_name) %>%
                    summarize(count = n()) %>%
                    arrange(day) %>%
                    select(day, short_name, count)
                
                alldates <- seq(min(date(df_posts()$date)), max(date(df_posts()$date)), 1)
                
                dates0 <- alldates[!(alldates %in% df_temp$day)]
                if (length(dates0) > 0) {
                    data0 <- data.frame(day = dates0, name = party, count = 0)
                    
                    data <- bind_rows(df_temp, data0)
                    
                    df_n_posts <- bind_rows(df_n_posts, data)
                } else {
                    df_n_posts <- bind_rows(df_n_posts, df_temp)
                }
                
            }
            
            df_n_posts_chart <- df_n_posts %>%
                group_by(short_name) %>%
                arrange(day) %>%
                mutate(
                    `Rolling average` = zoo::rollmean(count, k = input$roll, fill = NA, align = 'right'),
                    Name = short_name
                ) %>%
                drop_na(short_name)
            
            g <- ggplot(df_n_posts_chart, aes(x = day, y = `Rolling average`, color = Name)) +
                geom_line() + 
                bbplot::bbc_style() + 
                theme(legend.position = "none") +
                labs(
                    title = paste0(
                        "Posts per day"
                    )
                )
            
            plotly_output <- ggplotly(g,
                                      tooltip = c('Name', 'Rolling average')) %>%
                layout(
                    width = 1000
                ) 
        } else {
            
            df_n_posts <- data.frame()
            for (party in unique(df_posts()$name)) {
                for (com in unique(df_posts()$compare)) {
                    df_temp <- df_posts() %>%
                        filter(name == party & compare == com) %>%
                        mutate(day = date(date)) %>%
                        group_by(day, short_name, compare) %>%
                        summarize(count = n()) %>%
                        arrange(day) %>%
                        select(day, short_name, count, compare)
                    
                    
                    alldates <- seq(min(date(df_posts()$date)), max(date(df_posts()$date)), 1)
                    
                    dates0 <- alldates[!(alldates %in% df_temp$day)]
                    
                    if (length(dates0) > 0) {
                        data0 <- data.frame(day = dates0, name = party, count = 0, compare = com)
                        
                        data <- bind_rows(df_temp, data0)
                    } else {
                        data <- df_temp
                    }

                    
                    if (com == "Comparison") {
                        data <- data %>%
                            filter(day >= input$date_range_comp[1] &
                                       day <= input$date_range_comp[2])
                    } else {
                        data <- data %>%
                            filter(day >= input$date_range[1] &
                                       day <= input$date_range[2])
                    }
                    
                    df_n_posts <- bind_rows(df_n_posts, data)
                }
            }
            df_n_posts_chart <- df_n_posts %>%
                group_by(short_name, compare) %>%
                arrange(day) %>%
                mutate(
                    `Rolling average` = zoo::rollmean(count, k = input$roll, fill = NA, align = 'right'),
                    Name = short_name
                ) %>%
                drop_na(short_name)
            
            g <- ggplot(df_n_posts_chart, aes(x = day, y = `Rolling average`, color = Name)) + 
                geom_line() +
                bbplot::bbc_style() + 
                theme(legend.position = "none",
                      axis.text = element_text(size = 9),
                      axis.text.x = element_text(angle = 90)) +
                labs(
                    title = paste0(
                        "Posts per day"
                    )
                ) +
                scale_x_date(labels = scales::date_format('%Y-%m-%d')) +
                facet_wrap(~compare)
            
            plotly_output <- ggplotly(g, tooltip = c('Name', 'Rolling average')) %>%
                layout(
                    width = 1000
                )
        }
        plotly_output
        
    })
    output$reactions_radar_chart <- renderPlotly({
        
        if (input$radar_scale == FALSE) {
            df_radar <- df_posts() %>%
                group_by(name) %>%
                summarize_at(
                    vars(ends_with('count'), -likeCount), sum
                ) %>%
                select(-c('subscriberCount', 'shareCount', 'commentCount', 'thankfulCount'))
        } else {
            df_radar <- df_posts() %>%
                group_by(name) %>%
                summarize_at(
                    vars(ends_with('count')), sum
                ) %>%
                select(-c('subscriberCount', 'shareCount', 'commentCount', 'thankfulCount')) %>%
                mutate_at(
                    vars(ends_with('Count')), scales::rescale
                )
        }
        
        fig <- plot_ly(
            type = 'scatterpolar',
            fill = 'toself')
        
        for (i in (1:nrow(df_radar))) {
            fig <- fig %>% add_trace(
                r = as.numeric(df_radar[i, 2:ncol(df_radar)]),
                theta = colnames(df_radar)[colnames(df_radar) != 'name'],
                name = df_radar[[i, 1]]
            )
            
        }
        
        fig <- fig %>% layout(
            polar = list(
                radialaxis = list(
                    visible = F,
                    range = c(0,max(df_radar[, c(2:ncol(df_radar))]))
                )
            ),
            showlegend = FALSE)
        fig
    })
    
    output$engagement_line_chart <- renderPlotly({
        if (input$compare == FALSE) {
            
            df_n_engagement <- data.frame()
            for (party in unique(df_posts()$name)) {
                df_temp <- df_posts() %>%
                    filter(name == party) %>%
                    mutate(day = date(date)) %>%
                    rowwise() %>%
                    mutate(
                        total_reactions = sum(likeCount, loveCount, wowCount, hahaCount, sadCount, angryCount, thankfulCount, careCount)
                    ) %>%
                    ungroup() %>%
                    group_by(day, short_name) %>%
                    summarize(total_reactions = sum(total_reactions)) %>%
                    arrange(day) %>%
                    select(day, short_name, total_reactions)
                
                alldates <- seq(min(date(df_posts()$date)), max(date(df_posts()$date)), 1)
                
                dates0 <- alldates[!(alldates %in% df_temp$day)]
                if (length(dates0) > 0) {
                    data0 <- data.frame(day = dates0, name = party, total_reactions = 0)
                    data <- bind_rows(df_temp, data0)
                } else {
                    data <- df_temp
                }
                
                df_n_engagement <- bind_rows(df_n_engagement, data)
            }
            
            df_n_engagement_chart <- df_n_engagement %>%
                group_by(short_name) %>%
                arrange(day) %>%
                mutate(
                    `Rolling average` = zoo::rollmean(total_reactions, k = input$roll, fill = NA, align = 'right'),
                    Name = short_name
                ) %>%
                drop_na(short_name)
            
            g <- ggplot(df_n_engagement_chart, aes(x = day, y = `Rolling average`, color = Name)) +
                geom_line() + 
                bbplot::bbc_style() + 
                theme(legend.position = "none") +
                labs(
                    title = paste0(
                        "Engagement per day"
                    )
                )
            
            plotly_output <- ggplotly(g, tooltip = c('Name', 'Rolling average')) %>%
                layout(
                    width = 1000
                )
        }
        else {
            df_n_engagement <- data.frame()
            for (party in unique(df_posts()$name)) {
                for (com in unique(df_posts()$compare)) {
                    df_temp <- df_posts() %>%
                        filter(name == party & compare == com) %>%
                        mutate(day = date(date)) %>%
                        rowwise() %>%
                        mutate(
                            total_reactions = sum(likeCount, loveCount, wowCount, hahaCount, sadCount, angryCount, thankfulCount, careCount) 
                        ) %>%
                        group_by(day, short_name, compare) %>%
                        summarize(total_reactions = sum(total_reactions)) %>%
                        arrange(day) %>%
                        select(day, short_name, total_reactions, compare)
                    
                    
                    alldates <- seq(min(date(df_posts()$date)), max(date(df_posts()$date)), 1)
                    
                    dates0 <- alldates[!(alldates %in% df_temp$day)]
                    
                    if (length(dates0) > 0) {
                        data0 <- data.frame(day = dates0, name = party, total_reactions = 0)
                        data <- bind_rows(df_temp, data0)
                    } else {
                        data <- df_temp
                    }
                    
                    if (com == "Comparison") {
                        data <- data %>%
                            filter(day >= input$date_range_comp[1] &
                                       day <= input$date_range_comp[2])
                    } else {
                        data <- data %>%
                            filter(day >= input$date_range[1] &
                                       day <= input$date_range[2])
                    }
                    
                    df_n_engagement <- bind_rows(df_n_engagement, data)
                }
            }
            df_n_engagement_chart <- df_n_engagement %>%
                group_by(short_name, compare) %>%
                arrange(day) %>%
                mutate(
                    `Rolling average` = zoo::rollmean(total_reactions, k = input$roll, fill = NA, align = 'right'),
                    Name = short_name
                ) %>%
                drop_na(compare, short_name)
            

            g <- ggplot(df_n_engagement_chart, aes(x = day, y = `Rolling average`, color = Name)) + 
                geom_line() +
                bbplot::bbc_style() + 
                theme(legend.position = "none",
                      axis.text = element_text(size = 9),
                      axis.text.x = element_text(angle = 90)) +
                labs(
                    title = paste0(
                        "Engagement per day"
                    )
                ) + 
                scale_x_date(labels = scales::date_format('%Y-%m-%d')) +
                facet_wrap(~compare)
            plotly_output <- ggplotly(g, tooltip = c('Name', 'Rolling average')) %>%
                layout(
                    width = 1000
                )
        }
        plotly_output
    })
    
    output$wordcloud <- renderWordcloud2({
        if (input$wordcloud_lang == FALSE) {
            df_cloud <- df_cloud_en() %>%
                group_by(lemma) %>%
                summarize(
                    freq = n()
                ) %>%
                rename(word = lemma) %>%
                ungroup() %>%
                filter(freq > 3)
        } else {
            df_cloud <- df_cloud() %>%
                group_by(lemma) %>%
                summarize(
                    freq = n()
                ) %>%
                rename(word = lemma) %>%
                ungroup() %>%
                filter(freq > 3)
        }
        wordcloud2a(data = df_cloud, shape = 'circle', size = 0.3)
    })
    
    output$sentiment_bar <- renderPlot({
        
        df_sentiment <- df_posts() %>%
            group_by(short_name) %>%
            summarize(mean_sentiment_polarity = sum(sentiment_polarity, na.rm = T) / n()) %>%
            ungroup() %>%
            drop_na(mean_sentiment_polarity)
        
        df_labels <- data.frame(
            label = c('Positive', 'Negative'),
            x = c(0, 0),
            y = c(max(df_sentiment$mean_sentiment_polarity), min(df_sentiment$mean_sentiment_polarity))
        )
        
        g <- ggplot(df_sentiment, aes(x = short_name, y = mean_sentiment_polarity)) + 
            geom_bar(stat = 'identity', fill = "blue") +
            geom_label(data = df_labels, mapping = aes(x = x, y = y, label = label), hjust = -.01, vjust = 1) +
            bbplot::bbc_style() + 
            theme(
                axis.text.x = element_text(angle = 90, size = 10)
            ) +
            labs(
                title = 'Average post sentiment: positivity and negativity'
            )
        g
    })
    
    output$cooccurences <- renderPlot({
        if (input$wordcloud_lang == TRUE) {
            cooc <- cooccurrence(x = subset(corpus, upos %in% c('NOUN', 'ADJ')),
                                 term = 'lemma',
                                 group = c('newId', 'sentence_id'))
            
            
            wordnetwork <- head(cooc, 30)
            wordnetwork <- igraph::graph_from_data_frame(wordnetwork)
            ggraph::ggraph(wordnetwork, layout = "fr") +
                ggraph::geom_edge_link(aes(width = cooc*.001, edge_alpha = cooc), edge_colour = "pink") +
                ggraph::geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
                ggraph::theme_graph(base_family = "Arial Narrow") +
                theme(legend.position = "none") +
                labs(title = "Cooccurrences within sentences", subtitle = "Nouns & Adjective")
        } else {
            cooc <- cooccurrence(x = subset(df_cloud_en(), upos %in% c('NOUN', 'ADJ')),
                                 term = 'lemma',
                                 group = c('newId', 'sentence_id'))
            
            
            wordnetwork <- head(cooc, 30)
            wordnetwork <- igraph::graph_from_data_frame(wordnetwork)
            ggraph::ggraph(wordnetwork, layout = "fr") +
                ggraph::geom_edge_link(aes(width = cooc*.001, edge_alpha = cooc), edge_colour = "pink") +
                ggraph::geom_node_text(aes(label = name), col = "darkgreen", size = 4) +
                ggraph::theme_graph(base_family = "Arial Narrow") +
                theme(legend.position = "none") +
                labs(title = "Cooccurrences within sentence", subtitle = "Nouns & Adjective")
        }
    })
        
        output$polls_line <- renderPlotly({
            df_line <- polls %>%
                filter(`Fieldwork End` >= (today() - years(1)))
            
            
            g <- df_line %>%
                ggplot(aes(x = `Fieldwork End`, y = result, color = party, group = party,
                           text = paste0('<b>Party:</b> ', party, '<br>',
                                         '<b>Result:</b> ', result*100, '%')
                           )) +
                geom_point(aes(alpha = 0.5)) +
                bbplot::bbc_style() +
                labs(
                    title = 'Polling results'
                ) +
                scale_y_continuous(labels = scales::percent) +
                theme(
                    legend.position = "none"
                ) + 
                geom_smooth(se = F)
            plotly_output <- ggplotly(g, tooltip = c("text"))
            
            plotly_output
        })
        
        output$engagement_tree_chart <- renderPlotly({
            engagement_counts <- df_posts() %>%
                group_by(short_name, name) %>%
                summarize(
                    likes = sum(likeCount),
                    comments = sum(commentCount),
                    shares = sum(shareCount)
                ) %>%
                ungroup() %>%
                pivot_longer(cols = matches('likes|comments|shares'), names_to = "reaction", values_to = "count")
            
            if (input$reaction_select == "All") {
                df_tree <- engagement_counts %>%
                    group_by(short_name) %>%
                    summarize(count = sum(count))
                title_add <- "all reactions"
            } else {
                df_tree <- engagement_counts %>%
                    filter(reaction == paste0(str_to_lower(input$reaction_select), 's'))
                title_add <- str_to_lower(paste0(input$reaction_select, 's'))
            }
            
            # p = treemap(
            #     df_tree,
            #     index = "short_name",
            #     vSize = "count",
            #     type = "index",
            #      
            #     
            #     palette = "Dark2",
            #      
            #     border.col = c("black"),
            #     border.lwds = 1,
            #      
            #     fontsize.labels=0.5,
            #     fontcolor.labels="white",
            #     fontface.labels=1,            
            #     bg.labels=c("transparent"),              
            #     align.labels=c("left", "top"),                                  
            #     overlap.labels=0.5,
            #     inflate.labels=T  
            # )
            # 
            # d3treeR::d3tree2(p, rootname = paste0("Proportion of ", title_add))
            
            fig <- plot_ly(
                df_tree,
                type = "treemap",
                labels = ~short_name,
                parents = NA,
                values = ~count
            ) %>%
                layout(title = paste0('Proportion of ', title_add))
                
        })
        
        output$media_mention_sankey <- renderSankeyNetwork({
            links <- df_media() %>%
                group_by(name, media_mentions) %>%
                summarize(count = n()) %>%
                ungroup()
            
            nodes <- data.frame('name' = unique(c(as.character(links$name), as.character(links$media_mentions))))
            
            links$IDsource <- match(links$name, nodes$name)-1
            links$IDtarget <- match(links$media_mentions, nodes$name)-1
            
            p <- networkD3::sankeyNetwork(
                Links = links,
                Nodes = nodes,
                Source = "IDsource",
                Target = "IDtarget",
                Value = "count", NodeID = "name"
            )
            p
        })
        
        output$account_share_sankey <- renderSankeyNetwork({

            media <- df_media_count() %>%
                group_by(name, source_name)%>%
                summarize(count = n()) %>%
                ungroup() %>%
                rename(party = name)
            
            print(media)
            
            nodes <- data.frame('name' = unique(c(as.character(media$party), as.character(media$source_name))))
            
            media$IDsource <- match(media$party, nodes$name)-1
            media$IDtarget <- match(media$source_name, nodes$name)-1
            
            p <- networkD3::sankeyNetwork(
                Links = media,
                Nodes = nodes,
                Source = "IDsource",
                Target = "IDtarget",
                Value = "count",
                NodeID = "name"
            )
            p
        })
})
