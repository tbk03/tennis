explanatory_theme_2 <- function(){


  grid_line_colour <- "grey"
  text_colour <- "black"
  background_colour <- "grey98"


  ggplot2::theme_bw() %+replace%

    ggplot2::theme(

      # format text
      text = ggplot2::element_text(family = "Lato", size = 12),
      plot.title = ggplot2::element_text(hjust = 0,size = 16),
      plot.subtitle = ggplot2::element_text(hjust = 0, size = 12),
      plot.caption = ggplot2::element_text(size = 8,
                                           hjust = 0),

      # format legend
      legend.position = "top",
      legend.background = ggplot2::element_blank(),
      legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 10),

      # format axis
      #axis.text = ggplot2::element_text(face = "bold"),
      axis.title.x = ggplot2::element_text(),
      axis.title.y = ggplot2::element_blank(),
      axis.line.y = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_blank(),
      # axis.line.x = ggplot2::element_line(colour = "black", size = 1),
      #axis.ticks.x = ggplot2::element_line(colour = grid_line_colour, size = 1),
      axis.ticks = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_text(margin=ggplot2::margin(t = 1, b = 10)),

      # format plot gridlines
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(colour = grid_line_colour),

      # format plot background
      panel.background = ggplot2::element_blank(),

      # format overall background (i.e. background behind the entire figure
      # including the plot, titles etc.)
      plot.background = ggplot2::element_blank(),

      # facet labels background
      strip.background = ggplot2::element_rect(fill=background_colour),
      strip.text = ggplot2::element_text(colour = text_colour, face = "bold",
                                         size = 12),
      panel.border = ggplot2::element_blank()
    )
}


get_res <- function(year, file_folder, tour){

  # form file location string based on year
  file_name <- glue::glue("{tour}_matches_{year}.csv")
  file_location <- str_c(file_folder, file_name)

  # read in data from csv
  atp_res_year <- read_csv(file_location,
                           col_types = cols()) %>%  # this line suppresses output
    # when guessing column types
    janitor::clean_names() %>%

    # add tour identifier
    mutate(tour = tour)

  return(atp_res_year)
}

tidy_tennis_df <- function(tennis_res_df, tour){

  # tidy up data for match losers
  losers <- tennis_res_df %>%

    # drop columns containing match stats of the winner
    select(-c(starts_with("winner"), starts_with("w_"))) %>%

    # simplify column naming to enable row binding below
    rename_with(.fn = ~str_replace( .x, "loser_", ""),
                .cols = starts_with("loser")) %>%
    rename_with(.fn = ~str_replace( .x, "l_", ""),
                .cols = starts_with("l_")) %>%

    # rename for simplicity
    rename(player_id = id) %>%

    # add result column and match_id
    mutate(result = "loser",
           match_id = 1:n(),
           match_id_tour = str_c(match_id, tour, sep = "_"),
           .after = name) %>%
    select(-match_id)


  # tidy up data for match winners
  winners <- tennis_res_df %>%

    # drop columns containing  match stats of the loser
    select(-c(starts_with("loser"), starts_with("l_"))) %>%

    # simplify column naming to enable row binding below
    rename_with(.fn = ~str_replace( .x, "winner_", ""),
                .cols = starts_with("winner")) %>%
    rename_with(.fn = ~str_replace( .x, "w_", ""),
                .cols = starts_with("w_")) %>%

    # rename for simplicity
    rename(player_id = id) %>%

    # add result column and match id
    mutate(result = "winner",
           match_id = 1:n(),
           match_id_tour = str_c(match_id, tour, sep = "_"),
           .after = name) %>%
    select(-match_id)

  # put winner and loser data back together to give a tidy format
  atp_res_tidy <- winners %>%
    bind_rows(losers) %>%
    arrange(tourney_id, match_num)

  # return the tidy dataframe
  return(atp_res_tidy)
}
