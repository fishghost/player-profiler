## Player Profiler

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(lubridate)
  library(tidyr)
  library(stringr)
  library(tibble)
  
  library(shiny)
  library(shinyjs)

  library(googlesheets4)

  library(ggplot2)
  library(ggimage)
})

#### Functions ####

#### Constants ####
## Player Data from previous seasons and current
# old_data <- read_rds(file.path("www", "Data", "old_data.rds"))
# new_data <- read_rds(file.path("www", "Data", "new_data.rds"))
# player_data <- bind_rows(old_data, new_data) %>%
#   # Ensure hero names levels are in alphabetical order
#   mutate(hero_name = reorder(hero_name, match(hero_name, sort(levels(hero_name)))))
player_data <- read_rds(file.path("www", "Data", "new_data.rds")) %>%
  filter(stat_name != "NULL")

## All seasons map data
# map_data <- read_rds(file.path("www", "Data", "map_data.rds"))

## Constants of Team colors, abbreviations, etc. 
team_colours <- readr::read_csv("www/TeamColours.csv", col_types = "cffcccc")

heroes <- readr::read_delim("www/HeroRoles.csv", delim = ";", col_types = c("cccc"))
hero_colours <- heroes %>% 
  select(hero_name, colour) %>%
  deframe()

post_season_matches <- c(
  37382, 37381, 37150, 37149, 37148, 37147, ## MM playins
  37377, 37378, 37379, 37392, 37391, 37380, ## MM tournament
  37394, 37393, 37404, 37403, 37402, 37401, ## JJ playins
  37355, 37356, 37357, 37360, 37359, 37358, ## JJ tournament
  37406, 37405, 37414, 37413, 37412, 37411, ## SS playins 
  37415, 37416, 37417, 37420, 37419, 37418, ## SS tournament
  37408, 37407, 37424, 37423, 37422, 37421, ## CC playins 
  37383, 37384, 37385, 37388, 37387, 37386  ## CC tournament
)

## Input defaults
default_heroes <- c("All Heroes")
default_time <- 45
range_time <- c(0, 90)
value_type <- c("summed", "per_10")[2]
max_player_select <- 5
page_url <- "https://fishghost.shinyapps.io/Profiler/"
match_options <- c("regular season" = "regular", "including playins/tournies" = "post")

#### Data Wrangling ####
player_names <- player_data %>% 
  distinct(player_name) %>%
  arrange(player_name) %>%
  deframe()

default_stats <- player_data %>%
  filter(hero_name %in% default_heroes) %>%
  distinct(stat_name) %>%
  deframe() %>%
  as.character() %>%
  sort()

#### UI Input/Output ####
## Text 
ui_player_select <- selectizeInput("player_select", label = "Select player(s) (max 5)", 
                                   choices = player_names, multiple = T, 
                                   options = list(
                                     maxItems = max_player_select,
                                     plugins = list("drag_drop", "remove_button"))
                                   )
ui_hero_select <- selectInput("hero_select", label = "...which heroes", 
                              choices = default_heroes)

ui_stat_select <- selectizeInput("stat_select", label = "... which stat", 
                                 choices = default_stats, multiple = T, 
                                 options = list(maxItems = 1))

ui_match_select <- radioButtons("match_select", "... from what games", 
                                choices = match_options)

ui_dummy <- selectInput("select_dummy", label = "dummy", choices = c("a", "b"))

## Buttons
test_button <- actionButton("button_test", label = "test")
ui_export_button <- actionButton("export_button", "Export URL", icon = icon("link"))

ui_min_time <- sliderInput("min_time", "... played for at least (minutes)", 
                           min = range_time[1], max = range_time[2], value = default_time)
## UI Outputs 
ui_plot_compare <- plotOutput("plot_compare", height = "800px", 
                              click = "clicked_stat")
ui_plot_stat <- plotOutput("plot_stat", 
                           click = "clicked_player")
ui_user_text <- textOutput("user_text")


#### Shiny UI ####
ui <- fluidPage(
  # theme = "extra.css",
  ## Library initialization
  useShinyjs(), 
  
  titlePanel("2021 OWL Player Profiler"),
  
  sidebarLayout(
    sidebarPanel(
      # fluidRow(test_button), 
      fluidRow(ui_player_select), 
      tags$hr(),
      fluidRow(p("Compared to data from...")), 
      fluidRow(ui_match_select),
      fluidRow(ui_hero_select), 
      fluidRow(ui_min_time), 
      fluidRow(ui_export_button), 
      tags$hr(),
      fluidRow(p("And specific comparison in ...")),
      fluidRow(ui_stat_select), 
      fluidRow(ui_plot_stat)
    ), 
    mainPanel(
      fluidRow(ui_plot_compare),
      fluidRow(ui_user_text)
    )
  )
)

ui_func <- function(req) {
  url_query <- parseQueryString(req$QUERY_STRING)
}

#### Shiny Server ####
server <- function(input, output, session) {
  #### Memory and Delays ####
  rV <- reactiveValues(match_data = NULL, 
                       scoped_data = NULL, 
                       main_player = NULL, 
                       comparison_data = NULL, 
                       available_heroes = default_heroes, 
                       plot_data = NULL, 
                       plot_stat_data = NULL,
                       url_hero = NULL,
                       last_hero = default_heroes)
  
  d_player_select <- debounce(reactive(input$player_select), 200)
  d_match_select <- debounce(reactive(input$match_select), 500)
  d_hero_select <- debounce(reactive(input$hero_select), 500)
  d_min_time <- debounce(reactive(input$min_time), 500)
  
  #### URL Param ####
  observeEvent(session$clientData$url_search, {
    url_query <- parseQueryString(session$clientData$url_search)

    if (!is.null(url_query[['p']])) {
      possible_players <- unlist(str_split(url_query$p, ","))
      found_players <- player_names[str_detect(player_names, 
                                              regex(paste(possible_players, collapse = "|"), 
                                                    ignore_case = T))]
      ordered_players <- found_players[match(tolower(possible_players), tolower(found_players), nomatch = 0)]
      if(length(found_players >= 1)) {
        updateSelectInput(session, "player_select", selected = ordered_players[1:max_player_select])
      }
    }
    
    if (!is.null(url_query[['m']])) {
      found_match_option <- match_options[str_detect(match_options, url_query[['m']])]
      if (length(found_match_option) == 1) {
        updateRadioButtons(session, "match_select", selected = found_match_option)
      }
    }
    
    if (!is.null(url_query[['h']])) {
      heroes <- player_data %>% distinct(hero_name) %>% deframe() %>% unlist()
      found_hero <- heroes[str_detect(heroes, fixed(url_query$h, ignore_case = T))]
      if(length(found_hero) == 1) {
        rV$url_hero <- found_hero
      }
    }
    
    if (!is.null(url_query[['m']])) {
      if (!is.na(suppressWarnings(as.numeric(url_query$m)))) {
        min_time <- round(min(10, max(as.numeric(url_query$m), 1)))
        updateSliderInput(session, "min_time", value = min_time)
      }
    }

  })
  
  #### Reactive Inputs ####
  observeEvent(c(rV$main_player, d_min_time()), {
    updateSelectInput(session, "hero_select", 
                      selected = default_heroes)
    if (is.null(d_player_select())) {
      rV$available_heroes <- default_heroes
      updateVarSelectizeInput(session, "stat_select", 
                              selected = NULL)
    } else {
      rV$available_heroes <- player_data %>%
        filter(player_name == rV$main_player) %>%
        pivot_wider(id_cols = c(team_name, player_name, hero_name), 
                    names_from = stat_name, 
                    values_from = stat_amount, values_fn = sum) %>%
        filter(`Time Played` >= d_min_time()*60) %>%
        # filter(stat_name == "Time Played" & stat_amount >= d_min_time()*60) %>%
        distinct(hero_name) %>%
        arrange(hero_name) %>%
        deframe()
    }
  })
  
  observeEvent(d_hero_select(), {
    rV$last_hero <- d_hero_select()
  })
  
  observeEvent(rV$available_heroes, {
    selected_hero <- default_heroes
    if (!is.null(rV$url_hero)) {
      rV$last_hero <- rV$url_hero
      selected_hero <- rV$url_hero
      rV$url_hero <- NULL
    }
    if (rV$last_hero %in% rV$available_heroes) {
      selected_hero <- rV$last_hero
    }
    updateSelectInput(session, "hero_select", 
                      choices = rV$available_heroes, 
                      selected = selected_hero)
  })
  
  observeEvent(rV$comparison_data, {
    stat_choices <- rV$comparison_data %>% 
      distinct(stat_name) %>%
      deframe() %>%
      as.character() %>%
      sort()
    
    updateSelectizeInput(session, "stat_select", 
                         choices = stat_choices)
  })
  
  #### Data Wrangling ####
  observeEvent(c(d_hero_select(), d_match_select()), {
    match_data <- player_data %>%
      {if (d_match_select() == "post"){
        filter(., !(esports_match_id %in% post_season_matches)) 
      } else .}
    
    rV$scoped_data <- match_data %>%
      filter(hero_name %in% d_hero_select()) %>%
      filter(stat_name != "NULL")
  })
  
  observeEvent(d_player_select(), {
    if (is.null(d_player_select()) | length(d_player_select()) == 0) {
      rV$main_player <- NULL
    } else {
      rV$main_player <- d_player_select()[1]
    }
  })
  
  observeEvent(c(rV$main_player, rV$scoped_data, d_min_time()), {
    if (is.null(d_player_select()) | length(d_player_select()) == 0) {
      rV$comparison_data <- NULL
    } else {
      wide_data <- rV$scoped_data %>%
        {temp_data <<- .} %>%
        filter(stat_name %in% unique(filter(temp_data, player_name == rV$main_player)$stat_name)) %>%
        pivot_wider(id_cols = c(team_name, player_name, hero_name), 
                    names_from = stat_name, 
                    values_from = stat_amount, values_fn = sum)
      if (dim(wide_data)[1] == 0) {
        rV$comparison_data <- NULL
        updateSelectizeInput(session, "hero_select", 
                             selected = default_heroes)
      } else {
        rV$comparison_data <- wide_data %>% 
          filter(`Time Played` >= d_min_time()*60) %>%
          arrange(player_name) %>%
          pivot_longer(cols = -c(team_name, player_name, hero_name, `Time Played`), 
                       names_to = "stat_name", values_to = "summed") %>%
          filter(!is.na(summed)) %>%
          mutate(per_10 = summed/(`Time Played`/600)) %>%
          rename(display_stat = value_type) %>%
          group_by(stat_name) %>%
          arrange(desc(display_stat)) %>%
          mutate(rank_in_stat = 1:n()) %>%
          mutate(stat_mean = mean(display_stat, na.rm = T), stat_sd = sd(display_stat, na.rm = T)) %>% 
          mutate(norm_stat = (display_stat - stat_mean)/stat_sd) %>%
          ungroup()
      }
    }
  })
  
  #### Stat compare Plot ####
  output$plot_compare <- renderPlot({
    ## Only invalidate on rV$comparison_data
    players <- d_player_select()
    if(is.null(rV$comparison_data) | length(players) == 0) {
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n') 
      text(x = 0.5, y = 0.5, paste("Select one or more players"), 
           cex = 1.6, col = "black")
    } else if (!any(rV$comparison_data$player_name == rV$main_player)) {
      plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n') 
      text(x = 0.5, y = 0.5, paste("Invalid selection"), 
           cex = 1.6, col = "black")
    } else {
      rV$plot_data <- rV$comparison_data %>%
        ## Arrange by first selected player
        mutate(prank = ifelse(player_name == rV$main_player, rank_in_stat, 0)) %>%
        arrange(sum(prank), stat_name) %>%
        mutate(stat_name = reorder(stat_name, desc(prank), FUN = sum))
      
      compare_text <- ifelse(
        length(players) == 1, 
        "no one", 
        paste(players[-1], collapse = ", ")
      )
      
      rV$plot_data %>%
        ## Plotting
        ggplot(aes(y = stat_name)) + 
        geom_violin(aes(x = ifelse(!(player_name %in% players), norm_stat, NA)), 
                    alpha = 0.1, na.rm = T) + 
        geom_jitter(aes(x = ifelse(!(player_name %in% players), norm_stat, NA)),
                    size = 0.5, alpha = 0.1, na.rm = T) +
        geom_point(aes(x = ifelse(player_name %in% players, norm_stat, NA),
                       fill = team_name),
                   size = 8, shape = 21, na.rm = T) +
        geom_text(aes(x = ifelse(player_name %in% players, norm_stat, NA), 
                       color = team_name, 
                       label = toupper(substring(player_name,1,1))), 
                   # size = 3,
                   na.rm = T) +
        geom_label(aes(x = ifelse(player_name == rV$main_player, min(norm_stat), NA),
                       color = team_name,
                       fill = team_name, 
                       label = rank_in_stat),
                   size = 4, na.rm = T) +
        scale_color_manual(values = deframe(team_colours[c(1,5)])) + 
        scale_fill_manual(values = deframe(team_colours[c(1,4)])) + 
        xlab("Normalized stat (per 10)") + 
        ggtitle(sprintf("%s's best stats for %s (%d+ mins played)", 
                        rV$main_player, d_hero_select(), d_min_time()), 
                subtitle = sprintf("compared with %s (click a stat for details)", compare_text)) + 
        theme(legend.position = "none", 
              axis.title.y = element_blank())
    }
  })
  
  #### Specific Stat Plot ####
  observeEvent(c(input$stat_select, rV$comparison_data), {
    if (is.null(input$stat_select) | length(input$stat_select) == 0) {
      output$plot_stat <- renderPlot({
        plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n') 
        text(x = 0.5, y = 0.5, paste("Select a stat to see"), 
             cex = 1.6, col = "black")
      })
    } else if (dim(rV$comparison_data)[1] == 0) {
      output$plot_stat <- renderPlot({
        plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n') 
        text(x = 0.5, y = 0.5, paste("Select a stat to see"), 
             cex = 1.6, col = "black")
      })
    } else {
      player_rank <- rV$comparison_data %>%
        filter(stat_name == input$stat_select, player_name == rV$main_player) %>%
        .$rank_in_stat

      rV$plot_stat_data <- rV$comparison_data %>%
        filter(stat_name == input$stat_select) %>%
        ## Filter top or bottom N
        # mutate(n = dense_rank(display_stat)) %>% 
        filter(rank_in_stat %in% c(1:10, player_rank)) %>%
        
        # arrange(desc(total_plot_stat_1)) %>%
        mutate(player_name = reorder(player_name, display_stat)) 
      
      max_stat <- max(rV$plot_stat_data$display_stat)
      
      output$plot_stat <- renderPlot({
        rV$plot_stat_data %>%
          group_by(player_name) %>%
          mutate(player_row = 1:n()) %>%
          ungroup() %>%
          # left_join(select(team_colours, team, logo), by = c("team_name" = "team")) %>%
          ## Plotting
          ggplot(aes(y = player_name)) +
          geom_bar(aes(x = display_stat, fill = team_name, color = team_name), 
                   stat = "identity", position = position_stack(reverse = T)) +
          geom_text(aes(label = rank_in_stat, color = team_name), x = 0.1*max_stat) + 
          # geom_image(aes(image = ifelse(player_row == 1, logo, NA)), 
          #            x = -0.1*max_stat_1, asp = aspr, na.rm = T) + 
          # geom_label(aes(label = player_name, fill = team_name), x = -0.1*max(plot_data$total_stat_1)) + 
          geom_text(aes(x = 1.05*display_stat,
                        label = ifelse(player_row == 1, 
                                       sprintf("%.2f//%1.0f min", display_stat, `Time Played`/60),
                                       # sprintf(signif(display_stat, digits = 2), `Time Played`/60), 
                                       NA)), 
                    hjust = "left", na.rm = T) +
          # ggtitle(sprintf("%s (%s %d) from %d matches", 
          #                 filter_stats[2], 
          #                 input$top_bot, filter_n, 
          #                 length(unlist(input$match_select)))) + 
          xlim(-0.15*max_stat,1.55*max_stat) + 
          xlab(paste0(input$stat_select, " (per 10)")) +
          ggtitle("click to add player") + 
          scale_color_manual(values = deframe(team_colours[c(1,5)])) + 
          scale_fill_manual(values = deframe(team_colours[c(1,4)])) + 
          theme(axis.title.y = element_blank(), 
                legend.position = "none", legend.title = element_blank())
        })
    }
  })
  
  #### Click Events ####
  observeEvent(input$clicked_stat, {
    if (is.null(rV$plot_data)) {
      return()
    }
    stats_ordered <- rV$plot_data %>%
      arrange(stat_name) %>%
      distinct(stat_name) %>%
      .$stat_name %>%
      as.character()
    
    clicked_stat <- stats_ordered[round(input$clicked_stat$y)]
    
    updateSelectizeInput(session, "stat_select", 
                         selected = clicked_stat)
    
    # print(round(input$clicked_stat$y))
    # print(stats_ordered[round(input$clicked_stat$y)])
  })
  
  observeEvent(input$clicked_player, {
    if (is.null(rV$plot_stat_data)) {
      return()
    }
    players_ordered <- rV$plot_stat_data %>%
      arrange(player_name) %>%
      .$player_name %>%
      as.character()
    
    clicked_player <- players_ordered[round(input$clicked_player$y)]
    new_selection <- c(d_player_select(), clicked_player)
    
    # print(new_selection)
    
    updateSelectizeInput(session, "player_select",
                         selected = new_selection)
  })
  
  #### Export Link ####
  observeEvent(input$export_button, {
    if (length(d_player_select()) == 0) {
      link_preview <- disabled(textInput("preview_link",
                                         "Preview:",
                                         width = "100%",
                                         value = "Select a player first"))
    } else {
      export_players <- paste(d_player_select(), collapse = ",")
      url_param <- paste0(page_url,"?p=", export_players)
      if (d_match_select() != match_options[1]) url_param <- paste0(url_param, "&m=", d_match_select())
      if (d_hero_select() != default_heroes) url_param <- paste0(url_param, "&h=", d_hero_select())
      if (d_min_time() != default_time) url_param <- paste0(url_param, "&m=", d_min_time())
      
      link_preview <- disabled(textInput("preview_link",
                                         "Preview:",
                                         width = "100%",
                                         value = url_param))
    }
    
    showModal(
      modalDialog(
        title = "Copy Link to List",
        link_preview,
        easyClose = TRUE,
      )
    )
  })
  
  #### Server stuff ####
  observeEvent(input$select_dummy, {
    ui_user_text <- renderText(input$select_dummy)
  })
  
  #### Debug Test ####
  observeEvent(input$button_test, {
    debug_text <- paste("thing to test")
    updateUserText(debug_text, 10000)
  })
}

#### Shiny App ####
shinyApp(ui = ui, server = server)