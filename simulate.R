# setup -------------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(ggtext)
#library(patchwork)

theme_set(
  firasans::theme_ipsum_fsc(
    axis_text_family = "Fira Sans Condensed",
    axis_text_size = 10, 
    axis_title_size = 14,
    axis_title_just = "cc") +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.title = element_markdown(face = "plain"),
          plot.subtitle = element_markdown(),
          plot.caption = element_markdown(),
          plot.title.position = "plot")
)

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

test_colors <- c('Positive' = "#440154", 
                 'Negative' = "#a9a9a9")

g_tests <- 
  tests %>% 
  pivot_longer(cols = c(positives, negatives), 
               names_to = "status") %>%
  mutate(status = 
           case_when(status == "positives" ~ "Positive",
                     status == "negatives" ~ "Negative",
                     TRUE ~ as.character(status))) %>%
  ggplot() +
  aes(x = date, y = value, fill = status) + 
  geom_bar(stat = "identity", position = "stack", show.legend = FALSE) + 
  labs(fill = NULL) +
  scale_fill_manual(values = test_colors) + 
  scale_y_continuous(labels = grkmisc::format_pretty_num()) +
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b", expand = expansion()) + 
  labs(
    title = "Simulated COVID-19 test results",
    subtitle = glue::glue(
      "New ",
      "<strong style = 'color:{test_colors['Positive']}'>positive</strong>",
      " and ",
      "<strong style = 'color:{test_colors['Negative']}'>negative</strong>",
      " tests (Feb-Jul)"),
    caption = glue::glue(
      "Code: github.com/tgerke/percent-positivity<br>",
      "Twitter: @travisgerke"), 
    x = NULL, y = NULL) + 
  theme(
    plot.title = element_markdown(face = "plain", margin=margin(70,0,-25,0)),
    plot.subtitle = element_markdown(face = "plain", margin=margin(30,0,-30,0)),
    plot.margin = margin(-4, 0.5, 0.5, 0.5, unit = "lines"))

ggsave(here::here("plots", "testing.png"), 
       g_tests, width = 5, height = 4, dpi = 300, scale = 1.5)

# plot percent positivity -------------------------------------------------

type_colors <- c('Previous day' = "#440154", 
                 '7-day average' = "#6baa75",
                 'Cumulative (DOH)' = "#3e78b2")

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
  scale_color_manual(values = type_colors) + 
  scale_linetype_manual(values = c("solid", "dashed", "twodash")) + 
  scale_x_datetime(date_breaks = "1 month", date_labels = "%b", expand = expansion()) +
  scale_y_continuous(labels = scales::percent_format(1), limits = c(0, .8)) +
  theme(
    legend.position = c(0.15, .86),
    legend.title = element_blank()) + 
  labs(
    title = "COVID-19 test percent positivity",
    subtitle = glue::glue(
      "Daily positivity reports if using ",
      "<strong style = 'color:{type_colors['Previous day']}'>previous day</strong>",
      ", ",
      "<strong style = 'color:{type_colors['7-day average']}'>7-day average</strong>",
      ", or ",
      "<strong style = 'color:{type_colors['Cumulative (DOH)']}'>cumulative average</strong>"),
    caption = glue::glue(
      "Code: github.com/tgerke/percent-positivity<br>",
      "Twitter: @travisgerke")
    ) +
  labs(x = NULL, y = NULL)

ggsave(here::here("plots", "perc_pos.png"), 
       g_perc_pos, width = 5, height = 4, dpi = 300, scale = 1.5)