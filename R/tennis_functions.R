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
