library(tidyverse)
library(ggforce)
library(extrafont)
library(paletteer)
loadfonts(device = "win", quiet = T)
extrafont::fonts()
extrafont::font_import()
library(hrbrthemes)
hrbrthemes::update_geom_font_defaults(family = "Roboto Condensed")
library(ggtext)
library(ggrepel)

wdr <- getwd()
my_caption <- "Roland Schmidt | @zoowalk | http://werk.statt.codes"



# Get data  ---------------------------------------------------------------------

df <- readr::read_csv(
  file = "https://data.ipu.org/api/women-ranking.csv?load-entity-refs=taxonomy_term%2Cfield_collection_item&max-depth=2&langcode=en&month=9&year=2019",
  skip = 5
)

names(df) <- c(
  "rank", "country",
  "lower_elections", "lower_seats", "lower_women_abs", "lower_women_rel",
  "upper_elections", "upper_seats", "upper_women_abs", "upper_women_rel"
)

df <- df %>%
  mutate_at(vars(contains("lower"), contains("upper")), .funs = list(num = ~ as.numeric(.))) %>%
  mutate(country = stringr::str_remove_all(country, regex("\\(.*\\)") %>% str_trim())) %>% 
  filter(!is.na(lower_women_rel_num)) #1 country/Sudan with na

# data arrangement --------------------------------------------------------

top <- df %>%
  top_n(., 3, wt = lower_women_rel_num)

lower <- df %>%
  top_n(., -2, wt = lower_women_rel_num)


df <- df %>% 
  select(country, lower_women_rel_num) %>%
  mutate(lower_women_rel_num = case_when(
    str_detect(country, "Austria") ~ 39.34,
    TRUE ~ as.numeric(lower_women_rel_num)
  )) %>%
  mutate(indicator = case_when(
    str_detect(country, "Austria") ~ "Austria",
    str_detect(country, "Bosnia and Herzegovina|United States") ~ "select",
    str_detect(country, paste(top$country, collapse = "|")) ~ "top",
    str_detect(country, paste(lower$country, collapse = "|")) ~ "bottom",
    TRUE ~ "other"
  )) %>%
  mutate(country = case_when(
    str_detect(country, "Bosnia") ~ "Bosnia",
    str_detect(country, "United States") ~ "USA",
    TRUE ~ as.character(country)
  ))

others <- df %>%
  filter(country %in% c("Austria", "Bosnia", "USA"))


# Dotplot with jitter -----------------------------------------------------

pos <- position_jitter(width = 0.2, seed = 1)

plot_dot <- df %>%
  mutate(label2 = paste(country, scales::percent(lower_women_rel_num / 100,
    accuracy = .1
  ))) %>%
  mutate(label2 = case_when(
    indicator == "other" ~ "",
    TRUE ~ as.character(label2)
  )) %>%
  ggplot(., aes(
    x = .5,
    y = lower_women_rel_num,
    color = indicator,
    label = label2,
    group = country
  )) +
  geom_point(
    position = pos
  ) +
  geom_rect(
    xmin = 0.75,
    xmax = Inf,
    ymin = -Inf,
    ymax = Inf,
    color = "white",
    fill = "white"
  ) +
  geom_text_repel(
    xlim = c(.75, 1.2),
    position = pos,
    # segment.size = 0.3,
    fontface = "italic",
    color = "grey30",
    segment.color = "grey30",
    hjust = 0
  ) +
  labs(
    title = "Women in Parliaments",
    subtitle = "Share of women in lower houses; top and bottom 3 \ncountries plus Austria, Bosnia, and USA.",
    caption = c("Data: Inter-Parliamentary Union (data.ipu.org) as of end of 2018, Austria as of <br>23 Oct 2019.", "<br><br><br>Roland Schmidt | @zoowalk | <span style='color:black'>**werk.statt.codes**</span>")
  ) +
  scale_y_continuous(
    limits = c(-5, 100),
    breaks = seq(0, 100, 25),
    minor_breaks = NULL,
    expand = expansion(mult = c(0.01, 0)),
    labels = scales::percent_format(scale = 1, accuracy = 1)
  ) +
  scale_x_continuous(
    expand = expansion(mult = 0),
    limits = c(0.15, 1.2)
  ) +
  scale_color_manual(values = c(
    "Austria" = "red",
    "other" = "grey30",
    "top" = "green",
    "bottom" = "steelblue",
    "select" = "orange"
  )) +
  hrbrthemes::theme_ipsum_rc() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    plot.subtitle = element_text(color = "grey30"),
    legend.position = "none",
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption = element_markdown(color = "grey30", hjust = c(0, 1)),
    axis.title = element_blank(),
    axis.text.x = element_blank()
  )

plot_dot

name <- "Women_in_Parliament_dot"
format <- ".png"
time <- Sys.Date()
folder <- paste0(wdr, "/graphs/")
height <- 18
width <- 14
ggsave(
  filename = paste0(folder, time, name, format),
  device = "png",
  type = "cairo",
  height = height,
  width = width,
  scale = 1,
  unit = c("cm")
)

# Distribution plot -------------------------------------------------------

df_interval <- df %>% 
  mutate(women_interval=cut(x=lower_women_rel_num, 
                            breaks=seq(0,100,10),
                            right=F)) %>% 
  mutate(women_interval=forcats::fct_explicit_na(women_interval, na_level=">70")) %>% 
  mutate(indicator = case_when(
    str_detect(country, "Austria") ~ "Austria",
    TRUE ~ "other"
  ))

plot_distribution <- df_interval %>% 
  ggplot()+
  geom_bar(aes(x=women_interval,
               fill=indicator),
           stat="count") +
  geom_text(aes(x=women_interval, 
                label=..count..),   
            stat="count",
            nudge_y=5)+
  labs(title="Number of parliaments per share of female MPs",
       subtitle=paste("Only lower houses.","Position of <span style='color:red'>Austria</span> indicated in red."),
       caption = c("Data: Inter-Parliamentary Union (data.ipu.org) as of end of 2018, Austria as of <br>23 Oct 2019.", "<br><br><br>Roland Schmidt | @zoowalk | <span style='color:black'>**werk.statt.codes**</span>"),
       x="% of female MPs",
       y="Number of lower houses")+
  scale_y_continuous(expand=expansion(mult=c(0.01,0.1)))+
  scale_fill_manual(values=c("Austria"="red", "other"="steelblue"))+
  hrbrthemes::theme_ipsum_rc()+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.subtitle = element_markdown(color= "grey30"),
        plot.caption = element_markdown(color = "grey30", hjust = c(0, 1)),
        )

plot_distribution
  
name <- "Women_in_Parliament_distribution"
format <- ".png"
time <- Sys.Date()
folder <- paste0(wdr, "/graphs/")
height <- 18
width <- 14
ggsave(plot_distribution,
  filename = paste0(folder, time, name, format),
  device = "png",
  type = "cairo",
  height = height,
  width = width,
  scale = 1,
  unit = c("cm")
)


# Combine plots -----------------------------------------------------------

library(patchwork)

(plot_dot+
    theme(plot.caption = element_markdown(color="grey30", hjust=0))+
    labs(caption="Data: Inter-Parliamentary Union (data.ipu.org) as of end of 2018, Austria as of 23 Oct 2019."))+

(plot_distribution + 
   theme(plot.caption = element_markdown(color="grey30", hjust=1))+
   labs(caption="Roland Schmidt | @zoowalk | <span style='color:black'>**werk.statt.codes**</span>"))

name <- "Women_in_Parliament_combined"
format <- ".png"
time <- Sys.Date()
folder <- paste0(wdr, "/graphs/")
height <- 18
width <- 28
ggsave(filename = paste0(folder, time, name, format),
       device = "png",
       type = "cairo",
       height = height,
       width = width,
       scale = 1,
       unit = c("cm")
)

