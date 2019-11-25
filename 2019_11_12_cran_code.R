library(tidyverse)
library(ggridges)
library(ggtext)

cran_code <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-11-12/loc_cran_packages.csv")

# looking at the comment to code ratio. just an idea I saw on twitter

cran <- cran_code %>%
  select(pkg_name, everything()) %>%
  mutate(comment_code_ratio = comment/code) %>%
  group_by(language) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  mutate(rank = dense_rank(desc(count))) %>%
  filter(rank <= 10 & 
           is.finite(comment_code_ratio) & 
                       code > median(.$code) & 
                          comment_code_ratio <= 1) %>%
  mutate(language = paste0("> ", language) %>%
  fct_reorder(.,comment_code_ratio, mean, .desc = TRUE))


# time to plot out some ridges

cran_code_ridges <- cran %>%
  ggplot(aes(comment_code_ratio, language)) +
  geom_density_ridges(color = "white", fill = "white", alpha = 0.8) +
  scale_x_continuous(
    limits = c(0,1),
    expand = c(0,0)
  ) +
  labs(
    x = "Comment:Code Ratio",
    y = "Language",
    title = "Comment to Code Ratio of R Packages, by Language **_**",
    subtitle = paste0("#On average, R files have the highest comment:code ratio, but also have a\n#platykurtic distribution. ",
               "In contrast, Markdown, HTML, and CSS files have\n#relatively few comments ",
               "in relation to their lines of code."),
    caption = paste0("Data: Phillip Massicotte | Viz: Jim Felps")
  ) +
  theme(
    rect = element_rect(fill = "black"),
    text = element_text(family = "mono", color = "white"),
    panel.background = element_rect(fill = NA),
    axis.text = element_text(color = "white", family = "mono", size = 10),
    panel.grid = element_blank(),
    plot.title = element_markdown(size = 17),
    axis.title = element_text(size = 13),
    plot.margin = unit(c(.5, 1, .5, 1), "cm"),
    plot.subtitle = element_text(color = "gray45", face = "italic")
  )

ggsave("~/R/Git/Project/TidyTuesday/cran_code_ridges.png", cran_code_ridges, width = 16, height = 9, dpi = 320)  
  
