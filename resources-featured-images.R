# -------------------------------- #
# - Take screenshot of resources - #
# -------------------------------- #

library(tidyverse)
library(janitor)
library(magick)
library(rsvg)

# Import ------------------------------------------------------------------
# list of resources + comparing to missing - delete the screenshot to generate a new one
# from Airtable
resources <- read_csv("data/Resources-Grid view.csv") %>%
  clean_names() %>%
  mutate(filename = paste0("plots/", make_clean_names(title, sep_out = "-"), ".png")) %>%
  filter(!filename %in% list.files("plots", full.names = TRUE))

# Screenshot --------------------------------------------------------------
# filtering on no logo
resources_to_screenshot <- resources %>%
  filter(is.na(logo))

# screenshot and resizing - webshot is vectorized but it generates some errors
walk2(resources_to_screenshot$url,
      resources_to_screenshot$filename,
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

# Logo --------------------------------------------------------------------
# filtering on logo
resources_to_logo <- resources %>%
  filter(!is.na(logo))

# loading a white square
white_square <-
  image_read_svg("white-square.svg") %>%
  image_crop("212x212+50")

# download, resize, center, save
walk2(
  resources_to_logo$logo,
  resources_to_logo$filename,
  possibly(function(logo, filename) {
    if (str_detect(logo, ".svg")) {
      logo_img <- image_read_svg(logo) %>%
        image_scale("400x")
    } else {
      logo_img <- image_read(logo) %>%
        image_scale("400x")
    }
    
    image_append(c(white_square, logo_img, white_square)) %>%
      image_scale("400x225") %>%
      image_transparent("white") %>%
      image_background("white") %>%
      image_write(filename)
  }, otherwise = NULL)
)

# Create spreadsheet ------------------------------------------------------
resources_featured_images <- resources %>%
  mutate(
    featured_image = paste0(
      "https://raw.githubusercontent.com/rfortherestofus/rru-website/master/plots/",
      make_clean_names(title, sep_out = "-"),
      ".png"
    )
  ) %>%
  write_csv("data/resources-to-import.csv",
            na = "")
