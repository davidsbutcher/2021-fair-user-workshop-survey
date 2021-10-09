
# Load packages

library(tidyverse)
library(ggplot2)
library(magrittr)

# Load survey results

survey <- 
  readxl::read_xlsx(
    "data/211008 1600 Registered UW attendees and panelists DEIDENTIFIED.xlsx"
  )

# Get the column names

col_names <- 
  names(survey)


# Col 18 ----

# Please rate your familiarity with the FAIR principles (research data should be
# made Findable, Accessible, Interoperable, and Reusable) on scale of 1 (no 
# familiarity) to 5 (very familiarity). - Drag slider

col18 <- 
  dplyr::pull(survey, 18) %>% 
  .[!is.na(.)]

# Make results into a DF for plotting

col18_df <- 
  table(col18) %>% 
  tibble::enframe() %>% 
  dplyr::mutate(
    value = as.integer(value),
    fraction = value/sum(value)
  )

# Make plot

col18_plot <- 
  col18_df %>% 
  ggplot() +
  geom_bar(
    aes(
      x = name,
      y = value
    ),
    stat = "identity",
    fill = 
      c("#FFA300", "#D94BF7", "#12D2FF", "#FFD039", "#7BCC8C")
  ) +
  theme_minimal() +
  labs(
    x = "Response",
    y = "Count"
  ) +
  theme(
    text = element_text(size = 20)
  )

ggsave("col18_plot.png", col18_plot, dpi = 300, height = 8, width = 12)

# Col 19 ----

# Do you (or members of your group) disseminate data via publicly accessible 
# repositories (e.g. Materials Cloud, Protein Data Bank, Marine Geosciences Data
# System, figshare, etc. )?

col19 <- 
  dplyr::pull(survey, 19)

# Make results into a DF for plotting

col19_df <- 
  table(col19) %>% 
  tibble::enframe() %>% 
  dplyr::mutate(
    value = as.integer(value),
    fraction = value/sum(value)
  )

# Make plot

col19_plot <- 
  col19_df %>% 
  ggplot() +
  geom_bar(
    aes(
      x = name,
      y = value
    ),
    stat = "identity",
    fill = 
      c("#FFA300", "#D94BF7", "#12D2FF")
  ) +
  theme_minimal() +
  labs(
    x = "Response",
    y = "Count"
  ) +
  theme(
    text = element_text(size = 20)
  )

ggsave("col19_plot.png", col19_plot, dpi = 300, height = 8, width = 12)

## Col 20 -26

# Why not? (Check all that apply.)

col20to26 <- 
  survey[20:26] %>% 
  unlist() %>% 
  table()


# Col 27 ----

# Please complete the following sentence: \"I think that ensuring that the data 
# I produce are FAIR _______.\"

col27 <- 
  dplyr::pull(survey, 27)

# Make results into a DF for plotting

col27_df <- 
  table(col27) %>% 
  tibble::enframe() %>% 
  dplyr::mutate(
    value = as.integer(value),
    fraction = value/sum(value),
    name = stringr::str_wrap(name, 20)
  )

# Make plot

col27_plot <- 
  col27_df %>% 
  ggplot() +
  geom_bar(
    aes(
      x = name,
      y = value
    ),
    stat = "identity",
    fill = 
      c("#FFA300", "#D94BF7", "#12D2FF", "#FFD039")
  ) +
  theme_minimal() +
  labs(
    x = "Response",
    y = "Count"
  ) +
  theme(
    text = element_text(size = 20)
  )

ggsave("col27_plot.png", col27_plot, dpi = 300, height = 8, width = 12)

# Col 33 ----

# Please complete the following sentence: \"I think that ensuring that the data 
# I produce are FAIR _______.\"

col33 <- 
  dplyr::pull(survey, 33)

# Make results into a DF for plotting

col33_df <- 
  table(col33) %>% 
  tibble::enframe() %>% 
  dplyr::mutate(
    value = as.integer(value),
    fraction = value/sum(value),
    name = stringr::str_wrap(name, 20)
  )

# Make plot

col33_plot <- 
  col33_df %>% 
  ggplot() +
  geom_bar(
    aes(
      x = name,
      y = value
    ),
    stat = "identity",
    fill = 
      c("#FFA300", "#D94BF7", "#12D2FF")
  ) +
  theme_minimal() +
  labs(
    x = "Response",
    y = "Count"
  ) +
  theme(
    text = element_text(size = 20)
  )

ggsave("col33_plot.png", col33_plot, dpi = 300, height = 8, width = 12)