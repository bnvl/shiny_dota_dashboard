library(readr)
library(dplyr)
library(tidyr)
library(janitor)

col_names <- c('game', 'hero', 'win', 'side', 'first_pick_status', 'pick', 'order_absolute')

hero_data <- read_csv('parse_table.csv', col_names = col_names)

df <- hero_data %>%
  filter(pick == 'pick') %>%
  select(game, hero, win) %>%
  spread(hero, win) %>%
  clean_names(case = 'big_camel')

df_sum <- data.frame(type = factor(), hero_input = factor(), hero_copick = factor(), pick_rate = numeric())


for (heroname in colnames(df)[2:111]) {
 
  heroname = as.name(heroname)
  heroname = enquo(heroname)
  
  for (colname in colnames(df)[2:111]) {
  
    colname = as.name(colname)
    colname = enquo(colname)
    
    if (heroname != colname) {
      
      temp <- df %>% count((!!heroname), (!!colname)) %>%
        filter(!is.na(!!heroname)) %>%
        add_count((!!heroname), wt = n) %>%
        mutate(nn = sum(unique(nn))) %>%
        mutate(perc = n/nn) %>%
        filter(!is.na(!!colname)) %>%
        mutate(type = ifelse((!!heroname) == (!!colname), 'with', 'against')) %>%
        group_by(type) %>%
        summarise(hero_input = gsub("([[:lower:]])([[:upper:]][[:lower:]])", "\\1 \\2", as.character(heroname)[2], perl = TRUE),
                  hero_copick = gsub("([[:lower:]])([[:upper:]][[:lower:]])", "\\1 \\2", as.character(colname)[2], perl = TRUE),
                  pick_rate = sum(perc))
      
      df_sum <- rbind(df_sum, temp)
    }
  }
}

df_sum <- df_sum %>%
  arrange(hero_input, type, desc(pick_rate)) %>%
  mutate(hero_input = replace(hero_input, hero_input == 'Anti Mage', 'Anti-Mage'),
            hero_input = replace(hero_input, hero_input == 'Keeper OfThe Light', 'Keeper of the Light'),
            hero_copick = replace(hero_copick, hero_copick == 'Anti Mage', 'Anti-Mage'),
            hero_copick = replace(hero_copick, hero_copick == 'Keeper OfThe Light', 'Keeper of the Light'))

write_csv(df_sum, 'pick_with_total.csv')

df_sum_top5 <- df_sum %>%
  group_by(hero_input, type)  %>%
  top_n(5, wt = pick_rate)

write_csv(df_sum_top5, 'pick_with_top5.csv')

hero_data <- hero_data %>%
  mutate_all(as.factor)

hero_data <- hero_data %>%
  mutate(order_relative = factor(case_when(
    order_absolute == 1 & pick == 'ban' ~ 1,
    order_absolute == 3 & pick == 'ban' ~ 2,
    order_absolute == 5 & pick == 'ban' ~ 3,
    order_absolute == 11 & pick == 'ban' ~ 4,
    order_absolute == 13 & pick == 'ban' ~ 5,
    order_absolute == 20 & pick == 'ban' ~ 6,
    order_absolute == 7 & pick == 'pick' ~ 1,
    order_absolute == 10 & pick == 'pick' ~ 2,
    order_absolute == 16 & pick == 'pick' ~ 3,
    order_absolute == 18 & pick == 'pick' ~ 4,
    order_absolute == 21 & pick == 'pick' ~ 5,
    order_absolute == 2 & pick == 'ban' ~ 1,
    order_absolute == 4 & pick == 'ban' ~ 2,
    order_absolute == 6 & pick == 'ban' ~ 3,
    order_absolute == 12 & pick == 'ban' ~ 4,
    order_absolute == 14 & pick == 'ban' ~ 5,
    order_absolute == 19 & pick == 'ban' ~ 6,
    order_absolute == 8 & pick == 'pick' ~ 1,
    order_absolute == 9 & pick == 'pick' ~ 2,
    order_absolute == 15 & pick == 'pick' ~ 3,
    order_absolute == 17 & pick == 'pick' ~ 4,
    order_absolute == 22 & pick == 'pick' ~ 5,
    TRUE ~ 0)))

write_csv(hero_data, 'parse_table_pre.csv')
