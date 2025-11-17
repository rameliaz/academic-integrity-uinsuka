## Exploring RW Database

library(dplyr)
library(ggplot2)
library(lubridate)
library(wordcloud)

rw_ds <- read.csv("retraction_watch.csv")

# Filter for cases that include Indonesia
idn_retractions <- rw_ds |>
  filter(grepl("Indonesia", Country, fixed = TRUE))

# Extract year and count retractions per year
retractions_per_year <- idn_retractions |>
  mutate(
    date_parsed = mdy_hm(RetractionDate),  # Parse mm/dd/yy h:mm format
    year = year(date_parsed)
  ) |>
  count(year) |>
  filter(!is.na(year))

# Create A line plot
line <- ggplot(retractions_per_year, aes(x = year, y = n)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +
  labs(
    title = "Number of Retractions per Year in Indonesia",
    x = "Year",
    y = "Number of Retractions"
  ) +
  theme_minimal()

# Creating a wordcloud of the reason of retractions
# Split subjects by semicolon and count frequencies
wordcloud_reason <- idn_retractions |>
  pull(Reason) |>
  strsplit(";\\s*") |>
  unlist() |>
  trimws() |>
  table() |>
  as.data.frame() |>
  setNames(c("word", "freq")) |>
  arrange(desc(freq))

png("wordcloud_reason.png", width = 800, height = 600)
wordcloud(
  words = wordcloud_reason$word,
  freq = wordcloud_reason$freq,
  min.freq = 1,
  max.words = 100,
  random.order = FALSE,
  colors = brewer.pal(8, "Dark2"),
  scale = c(3, 0.5)
)
dev.off()