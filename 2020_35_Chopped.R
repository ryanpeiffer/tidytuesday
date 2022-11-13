


#load packages
library(tidyverse)

#load in data
chopped <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-25/chopped.tsv')


#fix column names 
names(chopped) <- sub("chopped.", "", names(chopped))


#first idea... are certain ingredients more popular
#construct a dataset of 1 row per ingredient per episode
#drop episodes without rating
#don't care what round the ingredient comes from
ingreds <- chopped %>%
    select(series_episode, episode_rating, appetizer, entree, dessert) %>%
    filter(!is.na(episode_rating)) %>%
    mutate(ingredients = paste(appetizer, entree, dessert, sep = ", ")) %>%
    select(-appetizer, -entree, -dessert) %>%
    tidytext::unnest_tokens(ingredient, ingredients, token = 'regex',
                            pattern=",", to_lower = TRUE, drop = TRUE)

#find what ingredients have been used at least once
ingreds_count <- ingreds %>%
    count(ingredient, sort = TRUE, name = "appearances") %>%
    filter(appearances > 1)

length(ingreds$ingredient)
length(unique(ingreds$ingredient))
#there have been 5538 total basket ingredients in Chopped episodes with ratings
#3616 unique basket ingredients
#865 ingredients that have been used in at least 2 rounds


#next i want to get the avg rating for each ingredient
#then see if viewers are as excited about rainbow chard as the producers
ingred_ratings <- ingreds %>%
    group_by(ingredient) %>%
    mutate(sum_rating = sum(episode_rating)) %>%
    add_count(ingredient, sort = TRUE, name = "appearances") %>%
    select(ingredient, sum_rating, appearances) %>%
    distinct() %>%
    mutate(avg_rating = sum_rating / appearances)

series_avg_rating <- mean(chopped$episode_rating[!is.na(chopped$episode_rating)])

#the series avg rating is 8.38, do ingredients with >1 appearance have higher ratings?
repeat_ingred_ratings <- ingred_ratings %>%
    filter(appearances > 1) %>%
    mutate(rating_residual = avg_rating - series_avg_rating)

polarized_ingredients <- repeat_ingred_ratings %>%
    filter(abs(rating_residual) > 0.5) %>%
    filter(appearances > 2) %>%
    arrange(rating_residual)


#plot the polarized ingredients
polarized_ingredients$ingredient <- factor(polarized_ingredients$ingredient,
                                     levels = polarized_ingredients$ingredient[order(polarized_ingredients$rating_residual)])

ggplot(polarized_ingredients, aes(ingredient, avg_rating)) +
    geom_dotplot(binaxis = "y", dotsize = 0.65, stackdir = "center") +
    coord_flip () +
    geom_hline(yintercept = series_avg_rating, colour = "blue",
               size = 1.2) +
    scale_y_continuous(breaks = seq(6.5, 9.5, 0.5),
                       labels = seq(6.5, 9.5, 0.5),
                       limits = c(7, 9.25)) + 
    labs(title = "Ingredients most loved & hated by Chopped viewers",
         subtitle = "Ingredients with at least 3 basket appearances. Blue line
         indicates overall average rating of 8.38.",
         caption = "created by @piefur54 for TidyTuesday",
         y = "Average Episode Rating",
         x = "") +
    theme_bw() +
    theme(plot.title = element_text(color = "black", size = 12, face = "bold"),
          plot.subtitle = element_text(color = "gray30", size = 8),
          plot.caption = element_text(color = "gray60", size = 8))
