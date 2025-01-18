library(tidyverse)
library(dplyr)
bart_data <- read.csv("https://www.barttorvik.com/getadvstats.php?year=2025&csv=1", header = FALSE)

colnames(bart_data) = c("player_name",	"team",	"conf",	"GP",	"Min_per",	"ORtg",	"usg",	
                        "eFG",	"TS_per",	"ORB_per",	"DRB_per",	"AST_per",	"TO_per",	
                        "FTM",	"FTA",	"FT_per",	"twoPM",	"twoPA",	"twoP_per",	"TPM",	
                        "TPA",	"TP_per",	"blk_per",	"stl_per",	"ftr",	"yr",	"height",	
                        "num",	"porpag",	"adjoe",	"pfr",	"year",	"pid",	"type",	"Rec_Rank",	 
                        "ast_tov",	 "rimmade",	 "totalrimshots",	 "midmade",	 "totalmidshots",	 
                        "rim_make_percent",	 "mid_make_percent",	 "dunksmade",	 "dunkattempts",	 
                        "dunk_make_percentage", "pick",	 "drtg",	"adrtg",	 "dporpag",	 "stops",	 
                        "bpm",	 "obpm",	 "dbpm",	 "gbpm",	"mp",	"ogbpm",	"dgbpm",	"oreb",	"dreb",	
                        "treb",	"ast",	"stl",	"blk",	"pts",	"position",	"no_idea")

bart_data <- bart_data %>%
  group_by(player_name) %>%
  mutate(player_name = ifelse(duplicated(player_name) | duplicated(player_name, fromLast = TRUE),
                              paste(player_name, "(", team, ")", sep = ""), player_name)) %>%
  ungroup()

bart_data$new_position <- ifelse(bart_data$position %in% c("Pure PG", "Combo G", "Scoring PG"), "Guard",
                                 ifelse(bart_data$position %in% c("Wing G", "Wing F"), "Wing",
                                        ifelse(bart_data$position %in% c("Stretch 4", "PF/C", "C"), "Big", NA)))

bart_data <- bart_data[bart_data$position != "", ]

numeric_columns <- bart_data %>%
  select(where(is.numeric)) %>%
  names()

bart_data <- bart_data %>% filter(Min_per > 20)

bart_data_percentiles <- bart_data

for (col in numeric_columns) {
  bart_data_percentiles[[paste0(col, "_percentile")]] <- ntile(bart_data[[col]], 100)
}



# Select only relevant columns
final_df <- bart_data_percentiles %>%
  select(player_name, new_position, ends_with("_percentile"))

final_df <- final_df %>%
  rename(position = new_position) %>% select(-GP_percentile)

final_df <- final_df %>%
  select(-c(
    "treb_percentile",
    "ast_percentile", "stl_percentile", "blk_percentile",
    "pts_percentile", "no_idea_percentile",
    "pid_percentile", "year_percentile", "pfr_percentile"
  ))

final_df$drtg_percentile <- 100 - final_df$drtg_percentile
final_df$adrtg_percentile <- 100 - final_df$adrtg_percentile
final_df$TO_per_percentile <- 100 - final_df$TO_per_percentile

final_df$Class <- bart_data_percentiles$yr

final_df$Team <- bart_data_percentiles$team


final_df$oreb <- bart_data_percentiles$oreb
final_df$dreb <- bart_data_percentiles$dreb
final_df$totalrimshots <- bart_data_percentiles$totalrimshots
final_df$totalmidshots <- bart_data_percentiles$totalmidshots
final_df$TPA <- bart_data_percentiles$TPA


guard_df <- final_df %>% filter(final_df$position == "Guard")
wing_df <- final_df %>% filter(final_df$position == "Wing")
big_df <- final_df %>% filter(final_df$position == "Big")


calculate_attribute_scores <- function(player_data) {
  if (nrow(player_data) == 0) {
    return(NULL)  # Player not found
  }
  
  if (player_data$position[1] == "Guard"){
    max_attempts <- max(guard_df$totalrimshots, na.rm = TRUE)
    max_mid_attempts <- max(guard_df$totalmidshots, na.rm = TRUE)
    max_tp_attempts <- max(guard_df$TPA, na.rm = TRUE)
    max_oreb_attempts <- max(guard_df$oreb, na.rm = TRUE)
    max_dreb_attempts <- max(guard_df$dreb, na.rm = TRUE)
  }
  else if (player_data$position[1] == "Wing"){
    max_attempts <- max(wing_df$totalrimshots, na.rm = TRUE)
    max_mid_attempts <- max(wing_df$totalmidshots, na.rm = TRUE)
    max_tp_attempts <- max(wing_df$TPA, na.rm = TRUE)
    max_oreb_attempts <- max(wing_df$oreb, na.rm = TRUE)
    max_dreb_attempts <- max(wing_df$dreb, na.rm = TRUE)
  }
  else if (player_data$position[1] == "Big"){
    max_attempts <- max(big_df$totalrimshots, na.rm = TRUE)
    max_mid_attempts <- max(big_df$totalmidshots, na.rm = TRUE)
    max_tp_attempts <- max(big_df$TPA, na.rm = TRUE)
    max_oreb_attempts <- max(big_df$oreb, na.rm = TRUE)
    max_dreb_attempts <- max(big_df$dreb, na.rm = TRUE)
  }
  else {
    max_attempts <- max(final_df$totalrimshots, na.rm = TRUE)
    max_mid_attempts <- max(final_df$totalmidshots, na.rm = TRUE)
    max_tp_attempts <- max(final_df$TPA, na.rm = TRUE)
    max_oreb_attempts <- max(final_df$oreb, na.rm = TRUE)
    max_dreb_attempts <- max(final_df$dreb, na.rm = TRUE)
  }
  
  # Calculate scores
  
  max_make_percentage <- 100  # Assuming percentages are out of 100
    # Maximum attempts from the dataset
  
  # Calculate the raw score
  raw_score <- player_data$rim_make_percent_percentile * log(player_data$totalrimshots + 1)
  
  # Normalize to a 0 to 100 scale
  scaled_score <- (raw_score / (max_make_percentage * log(max_attempts + 1))) * 100
  
  # Round the final score
  inside_scoring_grade <- round(scaled_score, 1)
  
  max_mid_make_percentage <- 100  # Assuming percentages are out of 100
  #max_mid_attempts <- max(final_df$totalmidshots_percentile, na.rm = TRUE)  # Maximum mid attempts
  #max_tp_attempts <- max(final_df$TPA_percentile, na.rm = TRUE)  # Maximum three-point attempts
  
  # Calculate raw scores for mid-range and three-point shooting
  raw_mid_score <- player_data$mid_make_percent_percentile * log(player_data$totalmidshots + 1)
  raw_tp_score <- player_data$TP_per_percentile * log(player_data$TPA + 1)
  
  # Combine raw scores
  combined_raw_score <- raw_mid_score + 1.5*raw_tp_score
  
  # Normalize to a 0 to 100 scale
  max_combined_score <- max_mid_make_percentage * (log(max_mid_attempts + 1) + 1.5*log(max_tp_attempts + 1))
  scaled_jumpshooting_grade <- (combined_raw_score / max_combined_score) * 100
  
  # Round the final score
  jumpshooting_grade <- round(scaled_jumpshooting_grade, 1)
  
  playmaking_grade <- (player_data$AST_per_percentile + player_data$ast_tov_percentile) / 2
  
  max_oreb_percentage <- 100
  
  #max_oreb_attempts <- max(final_df$oreb_percentile, na.rm = TRUE)
  #max_dreb_attempts <- max(final_df$dreb_percentile, na.rm = TRUE)
  
  raw_oreb_score <- player_data$ORB_per_percentile * log(player_data$oreb + 1)
  raw_dreb_score <- player_data$DRB_per_percentile * log(player_data$dreb + 1)
  
  combined_raw_score_reb <- raw_oreb_score + raw_dreb_score
  
  max_combined_score_reb <- max_oreb_percentage * (log(max_oreb_attempts + 1) + log(max_dreb_attempts + 1))
  rebounding_grade <- (combined_raw_score_reb / max_combined_score_reb) * 100
  
  defense_grade <- (
    player_data$stl_per_percentile + player_data$blk_per_percentile +
      player_data$dbpm_percentile + player_data$dgbpm_percentile + 
      player_data$dporpag_percentile + player_data$adrtg_percentile
  ) / 6
  
  advanced_metrics_grade <- (
    player_data$bpm_percentile + player_data$gbpm_percentile + 
      player_data$porpag_percentile
  ) / 3
  
  player_name = player_data$player_name
  
  position = player_data$position
  
  # Create a dataframe to return the attribute scores
  attribute_scores <- data.frame(
    player_name = player_name,
    position = position,
    inside_scoring_grade = round(inside_scoring_grade, 1),
    jumpshooting_grade = round(jumpshooting_grade, 1),
    playmaking_grade = round(playmaking_grade, 1),
    rebounding_grade = round(rebounding_grade, 1),
    defense_grade = round(defense_grade, 1),
    advanced_metrics_grade = round(advanced_metrics_grade, 1)
  )
  
  return(attribute_scores)
}

attribute_scores <- calculate_attribute_scores(final_df)

numeric_columns <- sapply(attribute_scores, is.numeric)

attribute_scores <- attribute_scores %>%
  mutate(position = ifelse(player_name == "Ace Bailey", "Wing", position))

library(dplyr)

# Function to scale grades by position
scale_grades_by_position <- function(data) {
  # Identify grade columns
  grade_cols <- grep("_grade$", names(data), value = TRUE)
  
  # Group by position and scale each grade column
  scaled_data <- data %>%
    group_by(position) %>%
    mutate(across(
      all_of(grade_cols),
      ~ ifelse(!is.na(.), (./max(., na.rm = TRUE)) * 100, NA)
    )) %>%
    ungroup()
  
  return(scaled_data)
}

attribute_scores <- scale_grades_by_position(attribute_scores)


average_score <- rowMeans(attribute_scores[, numeric_columns], na.rm = TRUE)

attribute_scores$average_grade = average_score

attribute_scores$Min_per = final_df$Min_per_percentile

attribute_scores[, grep("_grade$", names(attribute_scores))] <- 
  round(attribute_scores[, grep("_grade$", names(attribute_scores))], 1)

library('rvest')
library('httr')
library('jpeg')

attribute_scores$Class <- final_df$Class
attribute_scores$Team <- final_df$Team

image_urls <- read.csv("image_urls.csv")

drops <- c('X')
image_urls <- image_urls[ , !(names(image_urls) %in% drops)]

attribute_scores <- merge(attribute_scores, image_urls, by = "player_name", all.x = TRUE)


# 
# query <- "Christian Anderson Texas Tech"
# search_url <- paste0("https://www.google.com/search?hl=en&tbm=isch&q=", URLencode(query))
# 
# # Send a GET request
# response <- GET(search_url)
# 
# # Read the HTML of the page
# page <- read_html(response)
# 
# # Extract image URLs from 'src' attribute of <img> tags
# img_urls <- page %>%
#   html_nodes("img") %>%
#   html_attr("src")
# 
# image_url <- img_urls[2]  # Replace with the actual URL
# 
# # Download the image data
# img_data <- content(GET(image_url), "raw")
# 
# temp_file <- tempfile(fileext = ".jpg")  # Create a temporary file with .jpg extension
# writeBin(img_data, temp_file)
# 
# # Read the image data
# img <- readJPEG(temp_file)
# 
# # Display the image
# plot(1:2, type = "n", xlab = "", ylab = "", axes = FALSE)  # Create an empty plot area
# rasterImage(img, 1, 1, 2, 2)

get_image_url <- function(name, team) {
  # Create the Google Images search URL

  query <- paste(name, team, "Headshot ESPN")
  search_url <- paste0("https://www.google.com/search?hl=en&tbm=isch&q=", URLencode(query))

  # Send GET request to Google Images search page
  response <- GET(search_url)

  # Read the HTML of the page
  page <- read_html(response)

  # Extract image URLs from the 'src' attribute of <img> tags
  img_urls <- page %>%
    html_nodes("img") %>%
    html_attr("src")

  # Return the second image URL (the first is usually the Google logo)
  if(length(img_urls) > 1) {
    return(img_urls[2])  # First image (skipping the logo)
  } else {
    return(NA)  # Return NA if no images found
  }
}

missing_players <- setdiff(attribute_scores$player_name, image_urls$player_name)
# 
images <- rep(NA, length(missing_players))

if (length(missing_players) > 0) {
  for (i in 1:length(missing_players)) {
    
    # Get the player_name and team for the current player
    player_name <- missing_players[i]
    team <- attribute_scores$Team[attribute_scores$player_name == player_name]
    
    # Get the image URL for each player
    images[i] <- get_image_url(player_name, team)
    
    # Pause for 5 seconds to avoid hitting the rate limit
    Sys.sleep(5)  # Sleep for 5 seconds between requests
    
    # Print out the status
    print(paste("Processed", player_name, "from team", team))
  }
  
}



missing_images <- data.frame(Image_URL = images)
missing_images$player_name <- missing_players

final_image_urls <- rbind(image_urls, missing_images)
write.csv(final_image_urls, 'image_urls.csv')
# # 
# for (i in 1:nrow(attribute_scores)) {
#   # Check if the Image_URL is NA
#   if (is.na(images[i])) {
#     # Get the image URL for each player
#     images[i] <- get_image_url(attribute_scores$player_name[i], attribute_scores$Team[i])
# 
#     # Pause for 5 seconds to avoid hitting the rate limit
#     Sys.sleep(5)  # Sleep for 5 seconds between requests
#   } else {
#     # Skip to the next iteration if the Image_URL is not NA
#     print(paste("skipped ", attribute_scores$player_name[i]))
#     next
#   }
# }
# 
#attribute_scores$Image_URL <- image_urls$Image_URL
# 
# 
# image_urls$player_name <- attribute_scores$player_name
# 

#images <- read.csv('image_urls.csv')

#attribute_scores$Image_URL <- images

#images_df = data.frame(Image_URL = images)
#images_df$player_name = attribute_scores$player_name
#write.csv(images_df, 'image_urls.csv')

library(tidyverse)
library(paletteer)
library(gt)
library(gtExtras)
library(gtUtils)

gt_theme_f5 <- function(gt_object, ...) {
  
  gt_object %>%
    opt_table_font(
      font = list(
        google_font("Roboto"),
        default_fonts()
      ),
      weight = 400
    ) %>%
    tab_style(
      locations = cells_title("title"),
      style = cell_text(
        font = google_font("Roboto"),
        weight = 700
      )
    ) %>%
    tab_style(
      locations = cells_title("subtitle"),
      style = cell_text(
        font = google_font("Roboto"),
        color = "gray65",
        weight = 400
      )
    ) %>%
    tab_style(
      style = list(
        cell_borders(
          sides = "top", color = "black", weight = px(0)
        ),
        cell_text(
          font = google_font("Roboto"),
          #transform = "uppercase",
          v_align = "bottom",
          size = px(14),
          weight = 'bold'
        )
      ),
      locations = list(
        gt::cells_column_labels(),
        gt::cells_stubhead()
      )
    ) %>%
    tab_options(
      column_labels.background.color = "floralwhite",
      data_row.padding = px(7.5),
      heading.border.bottom.style = "none",
      table.border.top.style = "none", # transparent
      table.border.bottom.style = "none",
      column_labels.font.weight = "bold", 
      column_labels.border.top.style = "none",
      column_labels.border.bottom.width = px(2),
      column_labels.border.bottom.color = "black",
      row_group.border.top.style = "none",
      row_group.border.top.color = "black",
      row_group.border.bottom.width = px(1),
      row_group.border.bottom.color = "floralwhite",
      stub.border.color = "floralwhite",
      stub.border.width = px(0),
      source_notes.font.size = 12,
      source_notes.border.lr.style = "none",
      table.font.size = 16,
      heading.align = "left",
      table.background.color = "floralwhite",
      table_body.hlines.color = 'gray90',
      ...
    )
}

custom_palette <- as.character(paletteer::paletteer_d("Redmonder::dPBIRdGn"))[3:9]


drops <- c("Min_per")
attribute_scores <- attribute_scores[ , !(names(attribute_scores) %in% drops)]

attribute_scores <- attribute_scores %>% 
  relocate(Image_URL, .before = position) %>% 
  relocate(Class, .before = inside_scoring_grade) %>% 
  relocate(Team, .before = position)

attribute_scores$average_grade <- round(attribute_scores$average_grade, 2)

attribute_scores <- attribute_scores %>%
  mutate(Image_URL = ifelse(grepl("encrypted-tbn0.gstatic.com", Image_URL), Image_URL, NA))

attribute_scores <- attribute_scores[order(attribute_scores$average_grade, decreasing = TRUE),]

attribute_scores$Class[attribute_scores$player_name == "Jaylen Crocker-Johnson"] <- "So"
attribute_scores$Class[attribute_scores$player_name == "Lazar Djokovic"] <- "So"
attribute_scores$Class[attribute_scores$player_name == "Cam Manyawu"] <- "So"
attribute_scores$Class[attribute_scores$player_name == "Scotty Middleton"] <- "So"



attribute_scores$Image_URL[attribute_scores$player_name == "Braden Smith"] <- "https://www.proballers.com/media/cache/resize_600_png/ul/player/backup/braden-smith-1eef688e-2777-60de-91e6-154720c595f7.png"
attribute_scores$Image_URL[attribute_scores$player_name == "Jacob Cofie"] <- "https://s3media.247sports.com/Uploads/Assets/355/715/12715355.jpg"
attribute_scores$Image_URL[attribute_scores$player_name == "Kjay Bradley Jr."] <- "https://thumbs.dreamstime.com/b/default-profile-picture-avatar-photo-placeholder-vector-illustration-default-profile-picture-avatar-photo-placeholder-vector-189495158.jpg"
attribute_scores$Image_URL[attribute_scores$player_name == "Brady Hardewig"] <- "https://thumbs.dreamstime.com/b/default-profile-picture-avatar-photo-placeholder-vector-illustration-default-profile-picture-avatar-photo-placeholder-vector-189495158.jpg"
attribute_scores$Image_URL[attribute_scores$player_name == "Jaxson Ford"] <- "https://thumbs.dreamstime.com/b/default-profile-picture-avatar-photo-placeholder-vector-illustration-default-profile-picture-avatar-photo-placeholder-vector-189495158.jpg"
attribute_scores$Image_URL[attribute_scores$player_name == "Jayden Quaintance"] <- "https://www.proballers.com/media/cache/resize_600_png/https---www.proballers.com/ul/player/backup/jayden-quaintance-1efa76ef-5250-6a92-a4e7-29afe45891eb.png"
attribute_scores$Image_URL[attribute_scores$player_name == "Ishan Sharma"] <- "https://cdn.prod.website-files.com/5d3752ecc5e950deedb7ab2b/62bee407411e955fa8b3ec19_Untitled-5-Recovered_0005_20220625-MPH120705.jpg"
attribute_scores$Image_URL[attribute_scores$player_name == "Joson Sanon"] <- "https://www.proballers.com/media/cache/resize_600_png/https---www.proballers.com/ul/player/backup/joson-sanon-1efa76ef-ad5b-6d7e-9fc2-0518f0fe1d35.png"
attribute_scores$Image_URL[attribute_scores$player_name == "Tre Johnson"] <- "https://www.usab.com/imgproxy/xaloomaBsXRZM8ior0OyWbzl6YyonNgud_XdZkvG_VA/rs:fit:3000:0:0:g:ce/aHR0cHM6Ly9zdG9yYWdlLmdvb2dsZWFwaXMuY29tL3VzYWItY29tLXByb2QvdXBsb2FkLzIwMjQvMDQvMTEvNjJkNjQ5NjAtMjU1Ni00MzY4LTgzNzEtNWU1ZDhiNWVhNTBhLmpwZw.png"
attribute_scores$Image_URL[attribute_scores$player_name == "Azmar Abdullah"] <- "https://thumbs.dreamstime.com/b/default-profile-picture-avatar-photo-placeholder-vector-illustration-default-profile-picture-avatar-photo-placeholder-vector-189495158.jpg"
attribute_scores$Image_URL[attribute_scores$player_name == "Blake Harper"] <- "https://thumbs.dreamstime.com/b/default-profile-picture-avatar-photo-placeholder-vector-illustration-default-profile-picture-avatar-photo-placeholder-vector-189495158.jpg"
attribute_scores$Image_URL[attribute_scores$player_name == "Jack Whitbourn"] <- "https://thumbs.dreamstime.com/b/default-profile-picture-avatar-photo-placeholder-vector-illustration-default-profile-picture-avatar-photo-placeholder-vector-189495158.jpg"
attribute_scores$Image_URL[attribute_scores$Team == "West Georgia"] <- "https://thumbs.dreamstime.com/b/default-profile-picture-avatar-photo-placeholder-vector-illustration-default-profile-picture-avatar-photo-placeholder-vector-189495158.jpg"
attribute_scores$Image_URL[attribute_scores$player_name == "Somto Cyril"] <- "https://www.madehoops.com/Images/Logos/d9b8c2aa-fb2d-44a8-8973-243401f38dbd.png"


#attribute_scores$Image_URL[attribute_scores$player_name == "James Jones"] <- "https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/5241698.png&w=350&h=254"
#attribute_scores$Image_URL[attribute_scores$player_name == "Brit Harris"] <- "https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/5241698.png&w=350&h=254"
#attribute_scores$Image_URL[attribute_scores$player_name == "Nic Book"] <- "https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/5241698.png&w=350&h=254"
#attribute_scores$Image_URL[attribute_scores$player_name == "Daniel Helterhoff"] <- "https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/5241698.png&w=350&h=254"
#attribute_scores$Image_URL[attribute_scores$player_name == "Breylin Garcia"] <- "https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/5241698.png&w=350&h=254"
#attribute_scores$Image_URL[attribute_scores$player_name == "Sean Smith"] <- "https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/5177533.png&w=350&h=254"
#attribute_scores$Image_URL[attribute_scores$player_name == "Jordan Mason"] <- "https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/5106453.png&w=350&h=254"
#attribute_scores$Image_URL[attribute_scores$player_name == "Tre White"] <- "https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/5105643.png"
#attribute_scores$Image_URL[attribute_scores$player_name == "Max Jones"] <- "https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/5108104.png"
#attribute_scores$Image_URL[attribute_scores$player_name == "Jaden Bradley"] <- "https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/4432737.png&w=350&h=254"
#attribute_scores$Image_URL[attribute_scores$player_name == "Micah Robinson"] <- "https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/5037876.png&w=350&h=254"
attribute_scores$Image_URL[attribute_scores$player_name == "Dylan Harper"] <- "https://d208p2yjo1bsmq.cloudfront.net/images/2024/10/10/20241009_RUMBB_DMR30638-Edit.jpg?width=300"
#attribute_scores$Image_URL[attribute_scores$player_name == "Josh Harris"] <- "https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/5239996.png&w=350&h=254"
#attribute_scores$Image_URL[attribute_scores$player_name == "Jeremiah Williams"] <- "https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/4701222.png&w=350&h=254"
#attribute_scores$Image_URL[attribute_scores$player_name == "Pharrel Payne"] <- "https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/5105823.png&w=350&h=254"
attribute_scores$Image_URL[attribute_scores$player_name == "Dayton Forsythe"] <- "https://thumbs.dreamstime.com/b/default-profile-picture-avatar-photo-placeholder-vector-illustration-default-profile-picture-avatar-photo-placeholder-vector-189495158.jpg"
attribute_scores$Image_URL[attribute_scores$player_name == "Elijah Moore"] <- "https://thumbs.dreamstime.com/b/default-profile-picture-avatar-photo-placeholder-vector-illustration-default-profile-picture-avatar-photo-placeholder-vector-189495158.jpg"
attribute_scores$Image_URL[attribute_scores$player_name == "Asa Newell"] <- "https://www.usab.com/imgproxy/c-pvTlK2EMV08_srrCRUg09WzGLx47GwlDtsEFVhzlo/rs:fit:3000:0:0:g:ce/aHR0cHM6Ly9zdG9yYWdlLmdvb2dsZWFwaXMuY29tL3VzYWItY29tLXByb2QvdXBsb2FkLzIwMjQvMDQvMTEvNmM3NzNhY2MtYmY5Yy00MzYzLWE4NzMtMTA4YTcxZTA2MTA4LmpwZw.png"
attribute_scores$Image_URL[attribute_scores$player_name == "Del Jones"] <-'https://s3media.247sports.com/Uploads/Assets/623/823/11823623.jpeg'
attribute_scores$Image_URL[attribute_scores$player_name == "Jermahri Hill"] <-'https://ballstatesports.com/images/2024/7/9/Jermahri_Hill.png?width=300'
attribute_scores$Image_URL[attribute_scores$player_name == "Will Sydnor"] <-'https://gojaspers.com/images/2024/10/9/WillSydnor.JPG'
attribute_scores$Image_URL[attribute_scores$player_name == "Jaden Winston"] <-'https://gojaspers.com/images/2024/10/9/BF7T0438.JPG'
attribute_scores$Image_URL[attribute_scores$player_name == "Masiah Gilyard"] <-'https://gojaspers.com/images/2024/10/9/Swishgilardheadshot.JPG'
attribute_scores$Image_URL[attribute_scores$player_name == "Wesley Robinson"] <-'https://gojaspers.com/images/2024/10/9/WesleyRobinsonheadshot.JPG'
attribute_scores$Image_URL[attribute_scores$player_name == "Shaquil Bender"] <-'https://gojaspers.com/images/2024/10/9/ShaqBenderheadshot.JPG'
attribute_scores$Image_URL[attribute_scores$player_name == "James Patterson"] <- 'https://a.espncdn.com/i/headshots/mens-college-basketball/players/full/5241495.png'
attribute_scores$Image_URL[attribute_scores$player_name == "Damarion Dennis"] <- 'https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/5243127.png'
attribute_scores$Image_URL[attribute_scores$player_name == "Wade Taylor IV"] <- 'https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/4683833.png&w=350&h=254'
attribute_scores$Image_URL[attribute_scores$player_name == "Sheldon Williams"] <- 'https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/5243128.png'
attribute_scores$Image_URL[attribute_scores$player_name == "Henry Coleman III"] <- 'https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/4432186.png'
attribute_scores$Image_URL[attribute_scores$player_name == "Scooter Williams Jr."] <- 'https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/5243118.png'
attribute_scores$Image_URL[attribute_scores$player_name == "Hayden Hefner"] <-'https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/4433279.png'
attribute_scores$Image_URL[attribute_scores$player_name == "Solomon Washington"] <-'https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/5105562.png&w=350&h=254'
attribute_scores$Image_URL[attribute_scores$player_name == "Andersson Garcia"] <-'https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/4702431.png'
attribute_scores$Image_URL[attribute_scores$player_name == "Taye Fields"] <-'https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/5239799.png&w=350&h=254'
attribute_scores$Image_URL[attribute_scores$player_name == "Donte Bacchus"] <-'https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/5239797.png&w=350&h=254'
attribute_scores$Image_URL[attribute_scores$player_name == "Jordy Barnes"] <-'https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/5241264.png&w=350&h=254'
attribute_scores$Image_URL[attribute_scores$player_name == "Bismark Nsiah"] <-'https://a.espncdn.com/combiner/i?img=/i/headshots/mens-college-basketball/players/full/5241774.png'

attribute_scores$player_name <- gsub("\\s*\\(.*\\)", "", attribute_scores$player_name)


attribute_scores %>% 
  gt() %>%
  opt_interactive(use_search = TRUE,
                  use_filters = TRUE,
                  use_resizers = TRUE,
                  use_highlight = TRUE,
                  use_compact_mode = TRUE,
                  use_text_wrapping = TRUE,
                  use_page_size_select = TRUE) %>%
  # add title
  tab_header(
    title = "College Basketball Player Grades", 
    subtitle = paste("As of", format(Sys.Date(), "%B %d, %Y"))
  ) %>%
  # format labels
  cols_label(player_name="Player",
             Image_URL = "Headshot",
             position="Position",
             inside_scoring_grade="Inside Scoring Grade",
             jumpshooting_grade="Jumpshooting Grade",
             playmaking_grade="Playmaking Grade",
             rebounding_grade="Rebounding Grade",
             defense_grade="Defense Grade",
             advanced_metrics_grade="Advanced Metrics Grade",
             average_grade="Overall Grade") %>% 
  # add tab spanners
  tab_spanner(label = "Scoring", 
              columns = inside_scoring_grade:jumpshooting_grade) %>% 
  tab_spanner(label = "Passing", 
              columns = playmaking_grade) %>% 
  tab_spanner(label = "Rebounding", 
              columns = rebounding_grade) %>%
  tab_spanner(label = "Defense", 
              columns = defense_grade) %>% 
  tab_spanner(label = "Metrics", 
              columns = advanced_metrics_grade) %>%
  # add borders
  tab_style(style = cell_borders(sides = "left", weight = px(2)),
            locations = cells_body(columns = c(inside_scoring_grade, playmaking_grade, rebounding_grade, defense_grade, advanced_metrics_grade, average_grade)))%>% 
  tab_style(style = cell_borders(sides = c("left"), color = "black", weight = px(2)),
            locations = cells_column_spanners(spanners = c("Scoring", "Passing", "Rebounding", "Defense", "Metrics")))  %>% 
  tab_style(style = cell_borders(sides = c("left"), color = "black", weight = px(2)),
            locations = cells_column_labels(columns = c(inside_scoring_grade, playmaking_grade, rebounding_grade, defense_grade, advanced_metrics_grade, average_grade)))%>%
  fmt_image(Image_URL, height = 50)   %>%
  # gt_img_rows(Image_URL, img_source = 'web')%>% 
  # color ranks
  gt_color_rows(ends_with("_grade"), palette = custom_palette, direction = 1, domain = c(0, 100))  

