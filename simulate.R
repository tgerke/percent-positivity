# setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(patchwork)

# simulate data -----------------------------------------------------------

tests <- tibble(
  date = seq(ymd("2020-02-01"), ymd("2020-07-31"), by = 'day'),
)

tests <- tests %>% 
  mutate(
    date = as_datetime(date),
    positives = c(rep(10, 121), 10*1.11^(1:length(122:n()))) %>% as.integer(),
    total_tests = c(rep(3000, 121), 3000 + 10*1.105^(1:length(122:n()))) %>% as.integer(),
    negatives = total_tests - positives,
    percent_pos = positives / (positives + negatives),
    cumulative_percent_pos = cummean(lead(percent_pos,1)) 
  ) 

# plot test counts --------------------------------------------------------

g_tests <- tests %>% 
  pivot_longer(cols = c(positives, negatives), 
               names_to = "status") %>%
  mutate(status = 
           case_when(status == "positives" ~ "Positive",
                     status == "negatives" ~ "Negative",
                     TRUE ~ as.character(status))) %>%
  ggplot() +
  aes(x = date, y = value, fill = status) + 
  geom_bar(stat = "identity", position = "stack") + 
  labs(fill = NULL) +
  scale_fill_manual(
    values = c(
      Negative = "#dddddd",
      Positive = "#440154"
    )
  ) + 
  scale_y_continuous(labels = grkmisc::format_pretty_num()) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b", expand = expansion()) +
  theme_minimal(14) +
  theme(
    strip.text = element_text(face = "bold", size = 18),
    legend.position = c(0.1, .9),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
  ) + 
  plot_annotation(
    title = "Simulated COVID-19 test results",
    subtitle = "New tests from 2020-02-01 through 2020-07-31",
    caption = "github.com/tgerke/percent-positivity",
    theme = theme(
      plot.title = element_text(hjust = 0, size = 18, face = "plain"),
      plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "lines"),
      plot.subtitle = element_text(margin = margin(b = 1.25, unit = "lines")),
      plot.caption = element_text(color = "#444444")
    )
  ) + 
  labs(x = NULL, y = NULL)

ggsave(here::here("plots", "testing.png"), 
       g_tests, width = 5, height = 4, dpi = 150, scale = 1.5)

# plot percent positivity -------------------------------------------------

g_perc_pos <- tests %>% 
  mutate(percent_smooth = slider::slide_dbl(percent_pos, mean, .before = 7, .complete = TRUE)) %>%
  pivot_longer(c(percent_pos, cumulative_percent_pos, percent_smooth), names_to = "type") %>%
  mutate(type = case_when(type == "percent_pos" ~ "Previous day",
                          type == "percent_smooth" ~ "7-day average",
                          type == "cumulative_percent_pos" ~ "Cumulative (DOH)")) %>%
  mutate(type = fct_relevel(type, "Previous day", "7-day average")) %>%
  ggplot() +
  aes(date, y = value, color = type, linetype = type) +
  geom_line() +
  scale_color_manual(values = c("#440154", "#6baa75", "#3e78b2")) + 
  scale_linetype_manual(values = c("solid", "dashed", "twodash")) + 
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b", expand = expansion()) +
  scale_y_continuous(labels = scales::percent_format(1), limits = c(0, .8)) +
  theme_minimal(14) +
  theme(
    strip.text = element_text(face = "bold", size = 18),
    legend.position = c(0.15, .86),
    legend.title = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
  ) + 
  plot_annotation(
    title = "COVID-19 test positivity",
    subtitle = "Daily reports if using previous day, 7-day average, or cumulative total",
    caption = "github.com/tgerke/percent-positivity",
    theme = theme(
      plot.title = element_text(hjust = 0, size = 18, face = "plain"),
      plot.margin = margin(0.5, 0.5, 0.5, 0.5, unit = "lines"),
      plot.subtitle = element_text(margin = margin(b = 1.25, unit = "lines")),
      plot.caption = element_text(color = "#444444")
    )
  ) + 
  labs(x = NULL, y = NULL)

ggsave(here::here("plots", "perc_pos.png"), 
       g_perc_pos, width = 5, height = 4, dpi = 150, scale = 1.5)