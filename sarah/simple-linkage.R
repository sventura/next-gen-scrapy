#  linkage code


#  Make plots, do linkage, etc

if (FALSE) {
  
  #  Missingness plot
  missing_plot <- read_csv("../../../CMU/next-gen-scrapy/sarah/pass_and_game_data.txt") %>%
    mutate(na_loc = if_else(is.na(x_coord) | is.na(y_coord), "yes-na", "no-na")) %>%
    group_by(na_loc, season, home_team) %>%
    summarize(count = n()) %>%
    arrange(desc(count)) %>%
    spread(season, count) %>%
    filter(na_loc == "yes-na") %>%
    ggplot(aes(x = `2017`, y = `2018`, label = home_team)) + 
    geom_label_repel() + 
    # geom_smooth(method = "lm") + 
    theme_bw() + 
    labs(
      title = "Number of Passes with Missing Coordinates, by Home Team and Season"
      # caption = "Data from NFL.com Next Gen Stats via next-gen-scraPy"
    )
  
  ggsave(plot = missing_plot, height = 5.5, width = 6.5,
         filename = "/Users/SamVentura/Desktop/CMU/next-gen-scrapy/sarah/next-gen-scraPy-missing-coordinates.pdf")
  
  #  Link next-gen-scraPy to big data bowl data
  bdb <- read_csv("../../../CMU/next-gen-scrapy/sarah/completion_coordinates.csv") %>% 
    janitor::clean_names() %>% 
    filter(!grepl(pattern = "TOUCHDOWN", x = play_description)) %>%
    mutate(x_adj = case_when(
      target_x == 120 ~ x - snap_x,
      target_x == 0 ~ snap_x - x
    )) %>%
    # mutate(y_adj = case_when(
    #   target_x == 0 ~ y - snap_y,
    #   target_x == 120 ~ snap_y - y
    # )) %>%
    mutate(y_adj = case_when(
      target_x == 0 ~ y - 160/6,
      target_x == 120 ~ 160/6 - y
      # target_x == 0 ~ y - snap_y,
      # target_x == 120 ~ snap_y - y
    )) %>%
    # mutate(own_side = possession_team == yardline_side) %>% 
    # mutate(yardline_100 = case_when(
    #   own_side ~ yardline_number, 
    #   !own_side ~ 50 + (50 - yardline_number)
    # )) %>% 
    # ggplot(aes(x = x, y = yardline_100)) + geom_point()
    # mutate(x_adj = x - 10 - yardline_100) %>%
    mutate(x = y_adj, y = x_adj) %>%
    # mutate(x = x - (53 + 1/3) / 2 - (snap_y - (53 + 1/3) / 2)) %>%
    # select(own_side, possession_team, yardline_side, yardline_number, yardline_100, x_adj, x, y)
    # ggplot(aes(x = x, y = y)) + geom_point()
    
    
    mutate(date = substr(game_play, 1, 8)) %>%
    mutate(date = as.Date(date, format = "%Y%m%d")) %>%
    rename(team = posteam) %>%
    mutate(game_id = as.character(game_id)) %>%
    rename(passer = passer_player_name) %>%
    select(game_id, date, team, home_team, away_team, passer, x, y, 
           game_play, play_id) %>%
    arrange(date) %>%
    filter(!is.na(x), !is.na(y)) %>%
    mutate(dataset = "Big Data Bowl")
  
  ngs <- read_csv("../../../CMU/next-gen-scrapy/sarah/pass_and_game_data.txt") %>%
    filter(pass_type == "COMPLETE") %>%
    select(-X, -X1) %>%
    filter(!is.na(x), !is.na(y)) %>%
    mutate(date = substr(game_id, 1, 8)) %>%
    mutate(date = as.Date(date, format = "%Y%m%d")) %>%
    rename(passer = name, 
           x = x_coord, 
           y = y_coord) %>%
    select(-week) %>%
    separate(passer, into = c("first", "last"), by = " ") %>%
    mutate(passer = paste0(substr(first, 1, 1), ".", last)) %>%
    mutate(game_id = as.character(game_id)) %>%
    select(game_id, date, team, home_team, away_team, passer, x, y) %>%
    arrange(date) %>%
    mutate(dataset = "next-gen-scraPy")
  
  
  # bind_rows(bdb, ngs) %>%
  #   filter(game_id == 2017090700) %>%
  #   filter(team == "KC") %>%
  #   ggplot(aes(x = x, y = y, color = dataset)) + 
  #   geom_point(size = 3) + 
  #   theme_bw()
  
  num_passes <- bind_rows(bdb, ngs) %>%
    filter(date <= max(bdb$date)) %>%
    group_by(game_id, date, team, home_team, away_team, passer) %>%
    summarize(n_ngs = sum(dataset == "next-gen-scraPy"), 
              n_bdb = sum(dataset == "Big Data Bowl")) %>%
    ggplot(aes(x = n_bdb, y = n_ngs)) + 
    geom_point(alpha = 0.25, size = 4) + 
    geom_abline() + 
    theme_bw() + 
    labs(
      title = "Number of Completed Passes at the QB-Game Level Across Datasets", 
      subtitle = "Big Data Bowl vs. next-gen-scraPy (jittered)",
      x = "Number of Completions in Big Data Bowl Dataset",
      y = "Number of Completions in next-gen-scraPy"
    )
  ggsave(plot = num_passes, filename = "../../../CMU/next-gen-scrapy/sarah/number-of-passes.pdf", 
         height = 6, width = 6.2)
  
  
  #  Combine the two datasets
  combined <- bind_rows(bdb, ngs) %>%
    filter(date <= max(bdb$date)) %>%
    mutate(date = as.character(date)) %>%
    group_by(game_id, date, team, home_team, away_team, passer) %>%
    mutate(n_ngs = sum(dataset == "next-gen-scraPy"), 
           n_bdb = sum(dataset == "Big Data Bowl")) %>%
    filter(n_ngs != 0, 
           n_bdb != 0) %>%
    # filter(n_ngs == n_bdb) %>%
    ungroup %>%
    group_by(game_id, date, team, home_team, away_team, passer, dataset) %>%
    mutate(index = row_number()) %>%
    mutate(id = paste0(index, "@", dataset)) %>%
    ungroup %>%
    group_by(game_id, date, team, home_team, away_team, passer)
  
  #  Get the groups
  groups <- summarize(combined) %>%
    ungroup
  
  #  Loop through each game-passer and link pairs of records across datasets
  all_links <- vector("list", nrow(groups))
  for (ii in 1:nrow(groups)) {
    print(c(ii, nrow(groups)))
    
    #  Filter to this group
    sub <- combined %>%
      ungroup %>%
      select(-game_play, -play_id) %>%
      filter(game_id == groups$game_id[ii], 
             date == groups$date[ii], 
             team == groups$team[ii], 
             home_team == groups$home_team[ii], 
             away_team == groups$away_team[ii], 
             passer == groups$passer[ii])
    
    #  Make pairwise comparisons and sort by distance
    pairs <- combn(sub$id, m = 2) %>%
      t %>%
      as_data_frame %>%
      separate(V1, into = c("dataset1", "index1"), sep = "@", remove = FALSE) %>%
      separate(V2, into = c("dataset2", "index2"), sep = "@", remove = FALSE) %>%
      rename(id1 = V1, id2 = V2) %>%
      filter(dataset1 != dataset2) %>%
      left_join(., {
        sub %>%
          select(id, x, y) %>%
          rename(x1 = x, y1 = y)
      }, by = c("id1" = "id")) %>%
      left_join(., {
        sub %>%
          select(id, x, y) %>%
          rename(x2 = x, y2 = y)
      }, by = c("id2" = "id")) %>%
      mutate(distance = sqrt((x1 - x2)^2 + (y1 - y2)^2)) %>%
      mutate(angle = atan((y2 - y1) / (x2 - x1))) %>%
      arrange(distance)
    
    #  Initialize while loop condition and empty links object
    links <- NULL
    iteration <- 1
    nrow_pairs <- nrow(pairs)
    
    #  Iterate through pairwise comparisons
    #  link the two closest records in different datasets
    #  and remove other rows with these records
    while (nrow_pairs > 0) {
      # print(c(iteration, nrow_pairs))
      links <- bind_rows(links, pairs[1,])
      pairs <- filter(pairs, 
                      id1 != pairs$id1[1], 
                      id2 != pairs$id2[1])
      # iteration <- iteration + 1
      nrow_pairs <- nrow(pairs)
      # print(c(iteration, nrow_pairs))
    }
    
    #  Bring other information back in
    all_links[[ii]] <- links %>%
      bind_cols(slice(groups[ii,], rep(1, each = nrow(links))))
    
  }
  
  #  And in the darkness, bind them
  all_links <- bind_rows(all_links)
  median(all_links$distance)
  mean(all_links$distance)
  
  #  Make some plots
  
  ##  Difference in angles
  angle_plot <- all_links %>%
    # mutate(side_of_field = if_else(x2 < 0, "Passes To The Left", "Passes To The Right")) %>%
    mutate(side_of_field = case_when(
      x2 < -7.5 ~ "Passes To The Left", 
      x2 > 7.5 ~ "Passes To The Right", 
      TRUE ~ "Passes To The Middle 15 Yards"
    )) %>%
    ggplot(aes(x = angle, fill = ..count..)) + 
    geom_histogram() + 
    coord_polar() + 
    theme_bw() + 
    facet_grid(~ side_of_field) + 
    theme(legend.position = "bottom", 
          axis.ticks.x = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          plot.title = element_text(hjust = 0.5)) + 
    labs(
      title = "Anglular Difference Between Pass Locations, Big Data Bowl vs. next-gen-scraPy",
      x = NULL, y = NULL, 
      fill = "Number of Passes"
    ) + 
    scale_fill_gradient(low = "grey87", high = "seagreen4") + 
    scale_x_continuous(breaks = seq(-pi / 2, pi/2, length.out = 9), 
                       labels = seq(0, 360, length.out = 9))
  
  ggsave(plot = angle_plot, 
         filename = "/Users/SamVentura/Desktop/CMU/next-gen-scrapy/sarah/angle-plot.pdf",
         height = 5, width = 8)
  
  
  ##  Difference in distances
  distance_plot <- all_links %>%
    ggplot(aes(x = distance)) + 
    geom_histogram(fill = "slateblue4", color = "black") + 
    theme_bw() + 
    labs(
      title = "Distance Between Pass Locations, Big Data Bowl vs. next-gen-scraPy",
      x = NULL, y = NULL
      # fill = "Number of Passes"
    ) 
  
  ggsave(plot = distance_plot, 
         filename = "/Users/SamVentura/Desktop/CMU/next-gen-scrapy/sarah/distance-plot.pdf",
         height = 4, width = 8)
  
  
  ##  Game Plots
  game_plots <- combined %>%
    filter(date == "2017-09-07" & passer == "A.Smith" | 
             date == "2017-09-10" & passer == "B.Roethlisberger" | 
             date == "2017-09-17" & passer == "T.Brady" | 
             date == "2017-10-01" & passer == "R.Wilson") %>%
    mutate(fig = case_when(
      passer == "A.Smith" ~ "A:  ",
      passer == "B.Roethlisberger" ~ "B:  ",
      passer == "R.Wilson" ~ "C:  ",
      passer == "T.Brady" ~ "D:  "
    )) %>%
    mutate(faceter = paste0(fig, passer, " on ", date)) %>%
    ggplot(aes(x = x, y = y)) + 
    geom_hline(yintercept = 0, linetype = "dashed") + 
    geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.25) + 
    geom_point(size = 3.5, pch = 1) +
    geom_point(size = 3, aes(color = dataset), alpha = 0.8) +
    # geom_point(aes(color = dataset), size = 3) + 
    facet_wrap(~ faceter, nrow = 2) +
    theme_bw() + 
    theme(legend.position = "bottom") + 
    scale_color_manual(values = c("darkslateblue", "orangered3")) + 
    xlim(-28, 28) + 
    ylim(-6, 40) + 
    labs(
      title = "Comparison of Completed Pass Locations, Big Data Bowl vs. next-gen-scraPy",
      x = "Width of Field, Relative to Center of Field", 
      y = "Length of Field, Relative to Line of Scrimmage", 
      color = "Data Source"
    )
  
  ggsave(plot = game_plots, 
         filename = "/Users/SamVentura/Desktop/CMU/next-gen-scrapy/sarah/game_plots.pdf",
         height = 8, width = 8)
  
  
}

