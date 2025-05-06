# Code to generate cover art for r4np -------------------------------------


# Load packages -----------------------------------------------------------

library(tidyverse)
library(tidytext)
library(here)
library(showtext)


# Data generation and preparation -----------------------------------------

# Specify the directory where your .qmd files are located
qmd_dir <- here::here()

# Identify all .qmd files in the directory
qmd_files <- list.files(
  path = qmd_dir, 
  pattern = "\\.qmd$", 
  full.names = TRUE
)

# Read and combine files into a single data frame
# Each row will represent a single line of text, retaining the file name and line number.
all_text <- map_df(qmd_files, function(file) {
  lines <- readLines(file)
  tibble(
    file_name = basename(file),
    line_number = seq_along(lines),
    text = lines
  )
})

# Remove empty rows with no content
df_text <-
  all_text |>
  filter(text != "")


# Creating the corpus for plotting ----------------------------------------

df_words <-
  df_text |>
  unnest_tokens(word, text)

df_words_clean <- anti_join(df_words, stop_words, by = "word")

# remove digits

df_words_clean <-
  df_words_clean |>
  filter(!str_detect(word, "\\d"))

df_words_clean |> count(word, sort = TRUE)

# Manual removal of certain words

words_to_remove <-
  tibble(word = c(
    "i.e",
    "0",
    "1",
    "2",
    "3",
    "4",
    "5",
    "6",
    "7",
    "8",
    "9",
    "true",
    "sec",
    "fig",
    "https"
  ))

df_words_clean <- anti_join(df_words_clean, words_to_remove, by = "word")

# Creating the plot -------------------------------------------------------

## Add special font
font_add("Source Code Pro Regular", regular = "/Users/danieldauber/Library/Fonts/SourceCodePro-Regular.ttf")

showtext_auto()

cover_art <- 
  df_words_clean |>
  count(word, sort = TRUE) |>
  # filter(n > 10) |>
  ggplot(aes(x = word,
             y = n)
         ) +
  geom_col(aes(alpha = n),
           width = 7,
           fill = "#000000") +
  geom_point(aes(alpha = n),
             size = 1) +
  geom_point(aes(alpha = n),
             shape = 21,
             size = 3) +
  geom_text(aes(label = word,
                size = n,
                alpha = n),
            family = "Source Code Pro Regular",
            vjust = -2) +
  scale_size_continuous(range = c(7, 30)) +
  scale_alpha_continuous(range = c(0.4, 1)) +
  theme_void() +
  
  # Theming the plot
  theme(
    # aspect.ratio = 9/6,
    plot.background = element_rect(fill = "#ffffff"),
    legend.position = "none"
  )

# cover_art

ggsave("_cover_art/r4np_cover_art.png",
       plot = cover_art,
       width = 6000,
       height = 9000,
       units = "px",
       dpi = 600)
