library(tidyverse)
library(janitor)
library(webshot)
library(magick)

resources <- read_csv("data/Resources-For export.csv") %>% 
  clean_names()

create_screenshot <- function(resource_title) {
  
  resources_filtered <- resources %>% 
    filter(title == resource_title)
  
  url <- resources_filtered$url
  
  filename <- resources_filtered$title %>% 
    make_clean_names(sep_out = "-") %>% 
    paste0("plots/", ., ".png")
  
  
  webshot(url = url,
          file = filename,
          cliprect = "viewport",
          delay = 3,
          vwidth = 400 * 5,
          vheight = 225 * 5) 
  
  image_read(filename) %>% 
    image_scale("400") %>% 
    image_write(path = filename)
}

resource_titles <- resources %>% 
  pull(title)

walk(resource_titles, create_screenshot)


# Create spreadsheet ------------------------------------------------------

# resources_featured_images <- 
  resources %>% 
  mutate(featured_image = title) %>% 
  mutate(featured_image = make_clean_names(featured_image, sep_out = "-")) %>% 
  mutate(featured_image = paste0("plots/", featured_image, ".png")) %>% 
  view()

