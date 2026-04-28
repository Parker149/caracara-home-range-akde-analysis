# --- Clear all ---
rm( list = ls() ) 
graphics.off()
cat("\014")


library(dplyr)
library(tidyr)
library(ggplot2)


# --- Home range statistics ---
F36312 <- data.frame(
  season = 4.23,
  pre_fledge = 4.27,
  post_fledge = 4.19
)
M36313 <- data.frame(
  season = 3.87,
  pre_fledge = 4.06,
  post_fledge = 2.99
)
F44868 <- data.frame(
  season = 4.07,
  pre_fledge = 3.79,
  post_fledge = NA
)
M68230 <- data.frame(
  season = 7.84,
  pre_fledge = 5.78,
  post_fledge = 8.11
)

df <- bind_rows(
  F36312 %>% mutate(bird_id = "F36312"),
  M36313 %>% mutate(bird_id = "M36313"),
  F44868 %>% mutate(bird_id = "F44868"),
  M68230 %>% mutate(bird_id = "M68230")
)

# --- Define color scheme
color_key_full <- c(
  "F36312" = "red",
  "F44868" = "green",
  "M36313" = "blue",
  "M68230" = "magenta",
  "Pre-Fledge" = "darkgray",
  "Post-Fledge" = "darkorange4"
)

df_long <- df %>%
  pivot_longer(
    cols = c(season, pre_fledge, post_fledge),
    names_to = "period",
    values_to = "area"
  ) %>%
  mutate(period = recode(period,
                         season = "Seasonal",
                         pre_fledge = "Pre-Fledge",
                         post_fledge = "Post-Fledge"
  )) %>%
  mutate(fill_group = case_when(
    period == "Seasonal" ~ bird_id,
    period == "Pre-Fledge" ~ "Pre-Fledge",
    period == "Post-Fledge" ~ "Post-Fledge"
  ))

ggplot(df_long, aes(x = bird_id, y = area, fill = fill_group)) +
  geom_col(aes(group = period),
           position = position_dodge(width = 0.8),
           width = 0.7) +
  scale_fill_manual(values = color_key_full, drop = FALSE,name = NULL) +
  labs(y = "Home Range Area [km^2]", x = NULL) +
  theme_minimal() 

# Save 
setwd("/Users/parkerwilkerson/Desktop/EEB_HW/Caracara/figures/")
ggsave("bar_chart_home_range_compare.png", width = 5, height = 5,units = "in",
       dpi = 300)  










