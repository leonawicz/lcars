library(rtrek)
library(trekcolors)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)

scriptData <- st_transcripts()

pat <- "('s\\s|\\s\\(|\\sV\\.).*"
x <- filter(scriptData, format == "episode" & series == "TNG") %>%
  unnest(text) %>%
  select(season, title, character, line) %>%
  mutate(character = gsub(pat, "", character)) %>%
  group_by(season, title, character) %>%
  summarize(lines = n(), words = length(unlist(strsplit(line, " "))))

x <- mutate(x, character = toupper(case_when(
  character == "Beverly" ~ "Crusher",
  character == "Geordi" ~ "La Forge",
  TRUE ~ character
)))

totals <- group_by(x, character) %>%
  summarize(lines = sum(lines), words = sum(words)) %>%
  arrange(desc(words)) %>% top_n(8)

id <- totals$character
chr <- factor(totals$character, levels = id)
uniform_colors <- trek_pal("starfleet")
ulev <- c("Command", "Operations", "Science")
uniform <- factor(ulev[c(1, 2, 1, 2, 3, 3, 2, 3)], levels = ulev)
totals <- mutate(totals, character = chr, uniform = uniform)

biggest <- filter(x, character %in% id) %>%
  mutate(character = factor(character, levels = id)) %>%
  group_by(season, character) %>%
  summarize(title = title[which.max(words)], words = max(words)) %>%
  arrange(character)

biggest <- mutate(biggest, winner = character[which.max(words)],
                  ymn = min(words), ymx = max(words)) %>% ungroup() %>%
  mutate(uniform = factor(uniform[match(biggest$character, id)], levels = ulev))

plot_theme <- theme_rtrek_dark
save(totals, biggest, uniform, uniform_colors, plot_theme, file = "inst/shiny/demo/demoApp.RData")
