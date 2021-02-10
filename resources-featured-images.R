# -------------------------------- #
# - Take screenshot of resources - #
# -------------------------------- #

library(tidyverse)
library(janitor)

# Screenshot --------------------------------------------------------------
# list of resources + comparing to missing - delete the screenshot to generate a new one
resources <- read_csv("data/Resources-For export.csv") %>%
  clean_names() %>%
  mutate(filename = paste0("plots/", make_clean_names(title, sep_out = "-"), ".png")) %>% 
  filter(!filename %in% list.files("plots", full.names = TRUE))

# screenshot and resizing - webshot is vectorized but it generates some errors
walk2(resources$url,
      resources$filename,
      function(url, filename) {
        webshot2::webshot(
          url = url,
          file = filename,
          cliprect = "viewport",
          delay = 3,
          vwidth = 400 * 5,
          vheight = 225 * 5
        ) %>%
          webshot::resize("400x")
      })

# Create spreadsheet ------------------------------------------------------
resources_featured_images <- resources %>%
  mutate(
    featured_image = paste0(
      "https://raw.githubusercontent.com/rfortherestofus/rru-website/master/plots/",
      make_clean_names(title, sep_out = "-"),
      ".png"
    )
  ) %>%
  select(title, featured_image) %>% 
  write_csv("data/Resources-screenshot_names.csv")
