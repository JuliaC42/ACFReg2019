

## create a box plot

mybox2 <- function(df){
  ggplot(df, aes(subcategory, buzz_location_pct, fill = category)) +
    geom_boxplot() + 
    theme_bw() + 
    labs(title = "Time taken to buzz, by subcategory", 
         y = "proportion of words of question read", x = NULL) + 
    guides(fill = guide_legend(nrow = 1, reverse = FALSE)) + coord_flip() + 
    scale_fill_viridis(alpha = 0.5, discrete = TRUE, option = "inferno", direction = -1) +
    theme(legend.position = "top", legend.title = element_blank(), 
          legend.box = "horizontal", legend.spacing.x = unit(0.15, "cm"), 
          axis.line = element_blank(), axis.ticks = element_blank(),
          legend.key.size = unit(0.7, "line"), 
          panel.grid.major.x = element_blank())
}


# table

mytab <- function(df) {
  df %>% 
    group_by(category, subcategory) %>% 
    summarise(question_freq = n(), mean_time = round(mean(buzz_location_pct),3), 
              median_time = round(median(buzz_location_pct),3),
              sd_time = round(sd(buzz_location_pct),3), 
              min_time = round(min(buzz_location_pct),3),
              max_time = round(max(buzz_location_pct),3)) %>% 
    ungroup()
}

mybar <- function(df) {
  ggplot(df, aes(subcategory, pc_correct)) + 
  geom_col(aes(fill = category)) + 
  # seemed closest to what I wanted
  theme_classic() + coord_flip() + 
  # following mostly to sort out the legend
  theme(legend.position = "top", legend.title = element_blank(), 
        legend.box = "horizontal", legend.spacing.x = unit(0.15, "cm"), 
        legend.key.size = unit(0.7, "line"), 
        axis.line = element_blank(), axis.ticks = element_blank()) +
  # following puts all legend on one line and corrects the order
  guides(fill = guide_legend(nrow = 1,reverse = FALSE)) +
  labs(title = "Percentage of correct buzzes by subcategory", y = NULL, x = NULL) + 
  # better colour palette
  scale_fill_viridis(discrete = TRUE, option = "inferno", direction = -1) +
  # following is to reduce gap between subcategories and plot
  scale_y_continuous(expand = c(0,0))
}

# counts

mybar2 <- function(df) {
  ggplot(df, aes(full_category, n)) + 
    geom_col(aes(fill = category)) + 
    theme_classic() + coord_flip() + 
    # following mostly to sort out the legend
    theme(legend.position = "top", legend.title = element_blank(), 
          legend.box = "horizontal", legend.spacing.x = unit(0.15, "cm"), 
          legend.key.size = unit(0.7, "line"), 
          axis.line = element_blank(), axis.ticks = element_blank()) +
    guides(fill = guide_legend(nrow = 1,reverse = TRUE)) +
    # following puts all legend on one line and corrects the order
    labs(title = "ACF Regionals 2019, Unique counts of tossups by subcategory", 
         y = "Frequency", x = NULL) +
    scale_fill_viridis(discrete = TRUE, option = "inferno", direction = -1) +
    # following is to reduce gap between subcategories and plot
    scale_y_continuous(expand = c(0,0))
}


