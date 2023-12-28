library(rvest)
library(xml2)
library(tidyverse)
library(leaflet)
library(viridis)
library(ggmap)

link <- "https://gelmir.com/wp-sitemap-posts-compendium_item-1.xml"
page <- read_html(link)

# Extract URLs using xpath
urls <- xml_text(xml_find_all(page, "//loc"))

# Create data frame
df <- data.frame(URL = urls)

# Getting info for each item
get_info <- function(item_url) {
  item_page <- read_html(item_url)
  item_name <- item_page %>% html_nodes(".wp-block-post-title") %>%
    html_text()
  item_type <- item_page %>% html_nodes(".taxonomy-object_type a") %>%
    html_text() %>% paste(collapse = ",")
  item_style <- item_page %>% html_nodes(".taxonomy-style a") %>%
    html_text() %>% paste(collapse = ",")
  item_material <- item_page %>% html_nodes(".taxonomy-material a") %>%
    html_text() %>% paste(collapse = ",")
  item_collection <- item_page %>% html_nodes(".taxonomy-collection a") %>%
    html_text()
  item_location <- paste(stringr::str_match(item_page %>% html_nodes(".wp-block-group-is-layout-flex+ p") %>% html_text(),
                                            "Find[ -]?spot: (.*?)(?:,|\\.)\\s*(.*?)(?:,|\\.|$)")[, c(2, 3)],
                         collapse = ", ")
  
  # Check if any field is empty and assign NA if true
  if (length(item_name) == 0) item_name <- NA
  if (length(item_style) == 0) item_style <- NA
  if (length(item_material) == 0) item_material <- NA
  if (length(item_collection) == 0) item_collection <- NA
  if (length(item_location) == 0) item_collection <- NA
  
  # Create data frame
  item_info <- data.frame(Name = item_name, Style = item_style, Material = item_material, Collection = item_collection, Location = item_location)
  
  return(item_info)
}

# Getting info for each item
info_list <- lapply(df$URL, FUN = get_info)

# Convert the list of data frames to a single data frame
items <- do.call(rbind, info_list)

# Add URL column to the data frame
items$URL <- df$URL

# API key
register_google(key = "AIzaSyB2w_v2pM2YEphZ8NCOtOJ9zsQFIplIeD8")

# Assuming 'Location' is the column with artifact locations
items$Location <- as.character(items$Location)

# Remove rows with missing or empty locations
items <- items[!is.na(items$Location) & items$Location != "", ]

# Geocode locations
locations <- geocode(items$Location)

# Add latitude and longitude columns to the 'items' data frame
items$Latitude <- locations$lat
items$Longitude <- locations$lon

# Get unique values of 'Style'
unique_styles <- unique(items$Style)

# Create a color palette with hexadecimal codes
style_colors <- c(
  "Early Animal Style" = "#FF0000",   # red
  "Early Animal Style, Style I" = "#FFA500",   # orange
  "Style I" = "#FFFF00",   # yellow
  "Style I, Style II/B" = "#008000",   # green
  "Style II/B" = "#0000FF",   # blue
  "Style II/B, Style II/C" = "#4B0082",   # indigo
  "Style II/C" = "#EE82EE",   # violet
  "Style II/C, Style II/D" = "#800080",   # purple
  "Style II/D" = "#FFC0CB",   # pink
  "Style III E (Broa Style)" = "#A52A2A",   # brown
  "Broa Style" = "#808000",   # olive
  "Broa Style, Oseberg Style" = "#00CED1",   # cyan
  "Oseberg Style" = "#008080",   # teal
  "Borre Style, Oseberg Style" = "#ADD8E6",   # lightblue
  "Borre Style" = "#006400",   # darkgreen
  "Borre Style, Jelling Style" = "#8B0000",   # darkred
  "Jelling Style" = "#FFD700",   # gold
  "Jelling Style, Mammen Style" = "#00FF00",   # lime
  "Mammen Style" = "#FF8C00",   # darkorange
  "Mammen Style, Ringerike Style" = "#DA70D6",   # orchid
  "Ringerike Style" = "#008B8B",   # darkcyan
  "Urnes Style" = "#808080"   # grey
)

# Ensure that the factor levels match the color names
items$Style <- factor(items$Style, levels = names(style_colors))

# Create a color palette
style_palette_manual <- colorFactor(
  palette = style_colors,
  domain = items$Style
)

# Create a leaflet map with colored markers
artifact_map <- leaflet(items) %>%
  addTiles() %>%
  addCircleMarkers(
    ~Longitude, ~Latitude,
    popup = ~paste("Name: ", Name, "<br>",
                   "Style: ", Style, "<br>",
                   "Material: ", Material, "<br>",
                   "Collection: ", Collection),
    fillColor = ~style_palette_manual(Style),
    label = ~as.character(Style),
    fillOpacity = 0.8,
    stroke = FALSE,
    clusterOptions = markerClusterOptions()
  ) %>%
  # Add legend
  addLegend(
    position = "bottomright",
    colors = style_colors,
    labels = names(style_colors),
    title = "Artifact Styles"
  )

# Display the map
artifact_map