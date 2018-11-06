#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(ggthemes)
library(DT)
library(readr)
library(dplyr)
library(scales)
library(grid)
library(gridExtra)
library(forcats)

# load in  our datasets and ttransform applicable data to factors
hero_data <- read_csv('./data/parse_table_pre.csv')
hero_data <- hero_data %>%
  mutate_all(as.factor)

copick_data <- read_csv('./data/pick_with_top5.csv')
copick_data <- copick_data %>%
  mutate_if(is.character, as.factor)

# Defining general structure of the dashboard

ui <- dashboardPage(
  
                    dashboardHeader(title = 'TI8 Hero Pick Analysis'),
                    
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem('About', tabName = 'about', icon = icon('list-alt')),
                        menuItem('General Pick Analysis', tabName = 'general_plot', icon = icon('bar-chart')),
                        menuItem('Detailed Hero Breakdown', tabName = 'hero_plot', icon = icon('users')),
                        menuItem('Data Tables', icon = icon('table'),
                                 menuSubItem('General Pick Data', tabName = 'hero_table'),
                                 menuSubItem('Picked Together Data', tabName = 'copick_table')),
                        menuItem("Code for the app", icon = icon("file-code-o"), 
                                 href = "https://github.com/bnvl/shiny_dota_dashboard/")
                    )
                  ),
                    
                    dashboardBody(
                      tabItems(
                        
                        # Create UI for the about tab
                        
                        tabItem(tabName = 'about',
                                
                                fluidRow(
                                  box(
                                    width = 12,
                                    p("This Dashboard App serves to showcase hero usage statistics from a Dota 2 tournament The International 2018 (abbreviated as TI8)."),
                                    p("Citing", a("WikiPedia:", href = "https://en.wikipedia.org/wiki/Dota_2", target = '_blank')),
                                    em('Dota 2 is a multiplayer online battle arena (MOBA) video game developed and published by Valve Corporation. Dota 2 is played in matches between two teams of five players, with each team occupying and defending their own separate base on the map. Each of the ten players independently controls a powerful character, known as a "hero", who all have unique abilities and differing styles of play. During a match, players collect experience points and items for their heroes to successfully defeat the opposing team\'s heroes in player versus player combat. A team wins by being the first to destroy a large structure located in the opposing team\'s base, called the "Ancient".'),
                                    br(),
                                    br(),
                                    p('Depending ont the side of the map, players will either be playing as The Radiant (bottom-left side) or The Dire (upper-right side).'),
                                    p('During the competitive tournamemts, teams choose the heroes they are going to use during the game using the mode called "Captains Mode". Each team chooses 6 heroes to ban, thus preventing these heroes from being picked by either team, and 5 heroes for the team to use during the game. These picks and bans constitute 3 pick and ban phases.'),
                                    em('General outline of the picking order as seen on', a("gamepedia.com", href = "https://dota2.gamepedia.com/Game_modes", target = '_blank'), 'is provided below. Green cells correspond to the team picking first, red cells - to the team picking second.'),
                                    HTML('<table style="border-collapse: separate; border-spacing: 4px;">
                                         <tr>
                                         <td style="color:#EEE; padding: 3px; background:#42A51C"><b>Ban</b>
                                         </td>
                                         <td style="color:#EEE; padding: 3px; background:#DE3909"><b>Ban</b>
                                         </td>
                                         <td style="color:#EEE; padding: 3px; background:#42A51C"><b>Ban</b>
                                         </td>
                                         <td style="color:#EEE; padding: 3px; background:#DE3909"><b>Ban</b>
                                         </td>
                                         <td style="color:#EEE; padding: 3px; background:#42A51C"><b>Ban</b>
                                         </td>
                                         <td style="color:#EEE; padding: 3px; background:#DE3909"><b>Ban</b>
                                         </td>
                                         <td style="padding: 3px;">
                                         </td>
                                         <td style="color:#EEE; padding: 3px; background:#42A51C"><b>Pick</b>
                                         </td>
                                         <td style="color:#EEE; padding: 3px; background:#DE3909"><b>Pick</b>
                                         </td>
                                         <td style="color:#EEE; padding: 3px; background:#DE3909"><b>Pick</b>
                                         </td>
                                         <td style="color:#EEE; padding: 3px; background:#42A51C"><b>Pick</b>
                                         </td>
                                         <td style="padding: 3px;">
                                         </td>
                                         <td style="color:#EEE; padding: 3px; background:#42A51C"><b>Ban</b>
                                         </td>
                                         <td style="color:#EEE; padding: 3px; background:#DE3909"><b>Ban</b>
                                         </td>
                                         <td style="color:#EEE; padding: 3px; background:#42A51C"><b>Ban</b>
                                         </td>
                                         <td style="color:#EEE; padding: 3px; background:#DE3909"><b>Ban</b>
                                         </td>
                                         <td style="padding: 3px;">
                                         </td>
                                         <td style="color:#EEE; padding: 3px; background:#DE3909"><b>Pick</b>
                                         </td>
                                         <td style="color:#EEE; padding: 3px; background:#42A51C"><b>Pick</b>
                                         </td>
                                         <td style="color:#EEE; padding: 3px; background:#DE3909"><b>Pick</b>
                                         </td>
                                         <td style="color:#EEE; padding: 3px; background:#42A51C"><b>Pick</b>
                                         </td>
                                         <td style="padding: 3px;">
                                         </td>
                                         <td style="color:#EEE; padding: 3px; background:#DE3909"><b>Ban</b>
                                         </td>
                                         <td style="color:#EEE; padding: 3px; background:#42A51C"><b>Ban</b>
                                         </td>
                                         <td style="padding: 3px;">
                                         </td>
                                         <td style="color:#EEE; padding: 3px; background:#42A51C"><b>Pick</b>
                                         </td>
                                         <td style="color:#EEE; padding: 3px; background:#DE3909"><b>Pick</b>
                                         </td></tr></table>'),
                                    br(),
                                    p('All the data for this App was collected from the dotabuff.com webpages containing results of game sets such as', a('this.', href = 'https://www.dotabuff.com/esports/series/1404269-ti8-main-event-grand-finals-psg-lgd-vs-og', target = '_blank')))
                                )
                              ),
                        
                        # Create UI for the genral tab
                        
                        tabItem(tabName = 'general_plot',
                                
                                # Create custom CSS style to display download buttons center aligned 
                                tags$style(".dl {display: block; margin 0 auto"),
                                
                                fluidRow(
                                  
                                  box(title = 'Plot controls',
                                      status = 'primary',
                                      solidHeader = TRUE,
                                      width = 3,
                                      radioGroupButtons('pick_selector', label = 'Select to show pick or ban statistics', choices = c('Pick' = 'pick', 'Ban' = 'ban'), justified = TRUE, status = 'primary'),
                                      br(),
                                      sliderInput('top_n', label = 'Select top n to show', min = 10, max = 30, value = 20, step = 5),
                                      br(),
                                      prettyRadioButtons('g_filter_selector', label = 'Choose the comparative statistic', choices = c('Win\\Loss', 'Radiant\\Dire', 'Team picking first\\second'), shape = 'square'),
                                      br(),
                                      downloadButton('gplot_download', label = 'Download this plot as pdf', class = 'dl')
                                  ),
                                  
                                  box(title = 'Visualization of character usage during the TI8',
                                      status = 'primary',
                                      width = 9,
                                      plotOutput('general_plot', height = '800px')
                                  )
                                )
                              ),
                        
                        # Create UI for the hero tab
                        
                        tabItem(tabName = 'hero_plot',
                                
                                fluidRow(
                                  valueBoxOutput('winpct_vb'),
                                  valueBoxOutput('banpct_vb'),
                                  valueBoxOutput('poppct_vb')
                                ),
                                
                                fluidRow(
                                  
                                  box(title = 'Plot controls',
                                      status = 'primary',
                                      solidHeader = TRUE,
                                      width = 3,
                                      selectInput('hero_selector', label = 'Select a hero you wish to examine', choices = levels(hero_data$hero)),
                                      br(),
                                      prettyRadioButtons('h_filter_selector', label = 'Choose the comparitive statistic', choices = c('Win\\Loss', 'Radiant\\Dire', 'Team picking first\\second'), shape = 'square'),
                                      br(),
                                      downloadButton('hplot_download', label = 'Download these plots as pdf', class = 'dl')
                                  ),
                                  
                                  box(title = 'Detailed hero usage statistics visualization',
                                      status = 'primary',
                                      width = 6,
                                      plotOutput('hero_plot_pick'),
                                      br(),
                                      plotOutput('hero_plot_ban')
                                  ),
                                  
                                  column(width = 3,
                                         
                                    infoBoxOutput('picked_games_ib',
                                                  width = NULL
                                    ),
                                         
                                    box(title = 'Most picked alongside this hero',
                                        width = NULL,
                                        status = 'success',
                                        solidHeader = TRUE,
                                        tableOutput('picked_with_table')
                                    ),
                                    box(title = 'Most picked against this hero',
                                        width = NULL,
                                        status = 'danger',
                                        solidHeader = TRUE,
                                        tableOutput('picked_against_table')
                                    )
                                  )
                                )
                              ),
                        
                        # Create UI for the tables tab
                        
                        tabItem(tabName = 'hero_table',
                                
                                fluidRow(
                                  box(title = 'Data on general picks and bans of heroes',
                                      width = 12,
                                      DT::dataTableOutput('hero_table')
                                  )
                                ),
                                
                                fluidRow(
                                  box(width = 3,
                                    downloadButton('dt_general_download', label = 'Download data as csv', class = 'dl')
                                  )
                                )
                              ),
                        
                        tabItem(tabName = 'copick_table',
                                
                                fluidRow(
                                  box(title = 'Data on heroes most commonly picked against or with other heroes',
                                      width = 12,
                                      DT::dataTableOutput('copick_table')
                                  )
                                ),
                                
                                fluidRow(
                                  box(width = 3,
                                    downloadButton('dt_copick_download', label = 'Download data as csv', class  = 'dl')
                                  )
                                )
                        )
                        
                      )
                    )
                  )


# Define server logic
server <- function(input, output) {
  
  # Creating reactive variables for the general tab

  general_filter <- reactive({
    switch(input$g_filter_selector,
           'Win\\Loss' = quo(win), 
           'Radiant\\Dire' = quo(side), 
           'Team picking first\\second' = quo(first_pick_status))})
  
  general_subset <- reactive({
      hero_data %>%
      filter(pick == input$pick_selector) %>%
      count(hero, !!general_filter()) %>%
      add_count(hero, wt = n) %>%
      arrange(desc(nn)) %>%
      top_n(nn, n = (input$top_n * 2)) %>%
      mutate(hero = fct_reorder(hero, nn))}) 
    
  general_plot <- reactive({
    ggplot(general_subset(), aes(x = hero, y = n, group = !!general_filter(), fill = !!general_filter())) +
      geom_col() +
      geom_text(aes(label = n), position = position_stack(vjust = 0.5), color = 'white') +
      scale_y_discrete(expand = c(0.01,0)) + 
      coord_flip() +
      theme_hc() + 
      scale_fill_hc() +
      labs(
        caption = 'Data collected from dotabuff.com'
      ) + 
      theme(
        axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        panel.grid.major.y = element_blank(),
        legend.text = element_text(margin = margin(r = 10, l = 2, unit = 'pt')),
        legend.title = element_blank())})
  
  # Create a grid plot object to align title to the left border of the whole image and not just the end of x-axis
  
  general_plot_grid <- reactive({
    title <- textGrob(
      paste('Top', input$top_n, ifelse(input$pick_selector == "pick", 'picked', 'banned'), 'heroes during TI8 by amount of games and', tolower(input$g_filter_selector)),
      x = unit(0, "lines"), 
      y = unit(0, "lines"),
      hjust = 0, vjust = 0,
      gp = gpar(fontsize = 14))
    
    arrangeGrob(general_plot(), top = title) 
  })
  
  # Creating outputs for the general tab
  
  output$general_plot <- renderPlot({grid.draw(general_plot_grid())})
  
  output$gplot_download <- downloadHandler(
    filename = function() {paste('plot_general_TI8_top', input$top_n, ifelse(input$pick_selector == "pick", 'picked', 'banned'),'.pdf', sep = "_")},
    content = function(file) {ggsave(file, plot = general_plot_grid())})
  
  # Creating reactive variables for the hero tab
  
  hero_subset <- reactive({
    hero_data %>%
    group_by(hero) %>%
    filter(hero == input$hero_selector)})
    
  hero_pick_data <- reactive({
    hero_subset() %>%
    filter(pick == 'pick') %>%
    mutate(order_relative = factor(order_relative, levels = c(1, 2, 3, 4, 5))) %>%
    count(order_relative, !!hero_filter_pick())})
    
  hero_ban_data <- reactive({
    hero_subset() %>%
    filter(pick == 'ban') %>% 
    count(order_relative, !!hero_filter_pick())})
    
  hero_filter_pick <- reactive({
    switch(input$h_filter_selector,
           'Win\\Loss' = quo(win), 
           'Radiant\\Dire' = quo(side), 
           'Team picking first\\second' = quo(first_pick_status))})
    
  hero_plot_pick <- reactive({
    ggplot(hero_pick_data(), aes(x = order_relative, y = n, group = !!hero_filter_pick(), fill = !!hero_filter_pick())) + 
    geom_col() +
    geom_text(aes(label = n), position = position_stack(vjust = 0.5), color = 'white') +
    theme_hc() + 
    scale_fill_hc(drop = FALSE) +
    scale_y_continuous(expand = c(0.015,0)) + 
    scale_x_discrete(drop = FALSE) +
    labs(
      title = paste('Amount of games', input$hero_selector, "was picked during the TI by pick order and", tolower(input$h_filter_selector)),
      caption = 'Data collected from dotabuff.com'
    ) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.line.x.bottom = element_line(color = 'black'),
      legend.text = element_text(margin = margin(r = 10, l = 2, unit = 'pt')),
      legend.title = element_blank())})
    
  hero_plot_ban <- reactive({
    ggplot(hero_ban_data(), aes(x = order_relative, y = n, group = !!hero_filter_pick(), fill = !!hero_filter_pick())) + 
    geom_col() +
    geom_text(aes(label = n), position = position_stack(vjust = 0.5), color = 'white') +
    theme_hc() + 
    scale_fill_hc(drop = FALSE) +
    scale_y_continuous(expand = c(0.015,0)) +
    scale_x_discrete(drop = FALSE) +
    labs(
      title = paste('Amount of games', input$hero_selector, "was banned during the TI by ban order and", tolower(input$h_filter_selector)),
      caption = 'Data collected from dotabuff.com'
    ) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.y = element_blank(),
      axis.line.x.bottom = element_line(color = 'black'),
      legend.text = element_text(margin = margin(r = 10, l = 2, unit = 'pt')),
      legend.title = element_blank())})
    
  hero_winpct <- reactive({
    hero_data %>%
    filter(pick == 'pick') %>%
    select(game, hero, win) %>%
    group_by(hero) %>%
    count(win) %>%
    add_count(hero, wt = n) %>%
    mutate(win_perc = n/nn) %>%
    filter(hero == input$hero_selector, win == 'Win')})
    
  hero_banpct <- reactive({
    hero_data %>%
    select(game, hero, pick) %>%
    mutate(total_games = nlevels(game)) %>%
    group_by(hero, total_games) %>%
    count(pick) %>%
    mutate(ban_perc = n/total_games) %>%
    filter(hero == input$hero_selector, pick == 'ban')})
    
  hero_poppct <- reactive({
    hero_data %>%
    select(game, hero) %>%
    mutate(total_games = nlevels(game)) %>%
    group_by(hero, total_games) %>%
    count(hero) %>%
    mutate(pop_perc = n/total_games) %>%
    filter(hero == input$hero_selector)})
    
  hero_total_games <- reactive({
    hero_data %>%
    select(game, hero, pick) %>%
    filter(hero == input$hero_selector, pick == 'pick') %>%
    group_by(hero) %>%
    summarize(total_games = n())})
  
  hero_picked_with <- reactive({
    copick_data %>%
      filter(hero_input == input$hero_selector & type == 'with') %>%
      select(hero_copick, pick_rate) %>%
      mutate(pick_rate = percent(round(pick_rate, 3))) %>%
      rename('Hero name' = hero_copick, 'Pick %' = pick_rate) %>%
      head(5)})
  
  hero_picked_against <- reactive({
    copick_data %>%
      filter(hero_input == input$hero_selector & type == 'against') %>%
      select(hero_copick, pick_rate) %>%
      mutate(pick_rate = percent(round(pick_rate, 3))) %>%
      rename('Hero name' = hero_copick, 'Pick %' = pick_rate) %>%
      head(5)})
  
  # Create outputs for the hero tab
  
  output$hero_plot_pick <- renderPlot({hero_plot_pick()})

  output$hero_plot_ban <- renderPlot({hero_plot_ban()})
  
  output$hplot_download <- downloadHandler(
    filename = function() {paste('plot_', input$hero_selector, '_TI8_pick_ban_data.pdf', sep = "")},
    content = function(file) {ggsave(file, plot = marrangeGrob(grob = list(hero_plot_pick(), hero_plot_ban()), nrow = 1, ncol = 1))})
  
  output$winpct_vb <- renderValueBox({
    valueBox(ifelse(!is.null(hero_winpct()$win_perc), percent(round(hero_winpct()$win_perc, 3)), "0"), 
             paste("games won with ", input$hero_selector), icon = icon('trophy'), color = "light-blue")})
  
  output$banpct_vb <- renderValueBox({
    valueBox(ifelse(!is.null(hero_banpct()$ban_perc), percent(round(hero_banpct()$ban_perc, 3)), "0"), 
             "banned out of total games", icon = icon('search-minus'), color = "blue")})
  
  output$poppct_vb <- renderValueBox({
    valueBox(percent(round(hero_poppct()$pop_perc, 3)), 
             "picked or banned of total games", icon = icon("search-plus"), color = "light-blue")})
  
  output$picked_games_ib <- renderInfoBox({
    infoBox("Total games played", paste(input$hero_selector, " played in ", hero_total_games()$total_games, ' out of 193 games of TI8'), icon = icon("gamepad"), color = 'blue')})
  
  output$picked_with_table <- renderTable({
    hero_picked_with()}, width = NULL, striped = TRUE)
  
  output$picked_against_table <- renderTable({
    hero_picked_against()}, width = NULL, striped = TRUE)
  
  # Create outputs for the table tab
  
  output$hero_table <- DT::renderDataTable({
    hero_data})
  
  output$copick_table <- DT::renderDataTable({
    copick_data})
  
  output$dt_general_download <- downloadHandler(
    filename = function() {paste("general_TI8_pick_data.csv")},
    content = function(file) {write_csv(hero_data, file)})
  
  output$dt_copick_download <- downloadHandler(
    filename = function() {"hero_combinations_TI8_pick_data.csv"},
    content = function(file) {write_csv(copick_data, file)})
}

# Run the application 
shinyApp(ui = ui, server = server)