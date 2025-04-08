# Libraries and Utilities -------------------------------------------------

install.packages("tidyverse")
library(tidyverse)
library(RColorBrewer)
library(patchwork)
library (scales) #re scales data

spotify_songs <-
  tibble(
    readr::read_csv(
      'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-01-21/spotify_songs.csv'
    )
  )

view(spotify_songs)


# Downloaded the drake file from Kaggle
drake_data <- read_csv('drake_data_Daphne_220286086.csv')


# Coding and Rearranging  -------------------------------------------------

# Cleaning out the spotify data set
spotify_songs <- spotify_songs %>%
  filter(track_artist == "Drake") %>% # Only keeping drake songs to make code more tidy
  mutate(
    track_album_release_date = as.Date(track_album_release_date), #changes class of track_album_release_date into numeric value
    title = track_name %>% #in the track_name column
      str_to_lower() %>% #removes all capitals 
      str_squish() %>% #removes all extra spaces
      str_remove_all("[^a-z0-9 ]")# removing extra letters and numbers this makes it easier to compare with other dataset
  ) 

# Removing duplicates and unnecessary songs
spotify_clean <- spotify_songs %>%
  group_by(track_name) %>%  # groups all track_names together by name-> makes it easier to modify
  slice_min(order_by = track_album_release_date,
            n = 1,
            with_ties = FALSE) %>%
  #chooses track by earliest release date and keeps the first track
  #with_ties ignores if some of the tracks have the same release date and only chooses one
  ungroup()


drake_clean <- drake_data %>%
  mutate(
    title = lyrics_title %>% # removing the word lyric after the clean_title
      as.character () %>%
      str_replace("(?i)lyrics$", "") %>% # "" replaces lyrics with empty space
      str_to_lower () %>%
      str_squish () %>% #removes any extra gaps 
      str_remove_all("[^a-z0-9 ]"),
    # replacing views into numeric values -> this will help when creating averages 
    views = case_when(
      str_detect(track_views, "K") ~ parse_number(track_views) * 1e3,
      str_detect(track_views, "M") ~ parse_number(track_views) * 1e6,
      TRUE ~ parse_number(track_views) #parse ignores everything but the necessary info
    )
  )


#Compare song titles in both data sets to see if they match
setequal(drake_clean$title, spotify_clean$title) #returns TRUE if x and y contain the same rows (ignoring order).
uncommon <- union( 
  setdiff(drake_clean$title, spotify_clean$title), #setdiff() finds differences between the 2 data sets, in this case only in the selected columns -> and we join them together with union
  setdiff(spotify_clean$title, drake_clean$title)
)
uncommon # just to see all of the different albums incase there was an error in the identification

# Filter both data sets to keep only rows with common titles
drake <- drake_clean %>% filter(!(title %in% uncommon))
spotify <- spotify_clean %>% filter(!(title %in% uncommon))#filtering the songs from uncommon variable out of both data sets

drake <- drake %>%
  filter(!album %in% c("So Far Gone (EP)", "Scary Hours")) %>%
  slice(-c(13)) #manually removed extra columns (duplicates)

spotify <- spotify %>%
  arrange(match(title, drake$title)) # arranging both data sets so they can be matched
spotify$track_album_name <- drake$album #copy and pasting all the album names into the spotify data set

spotify$track_album_name == drake$album # making sure that it all transferred correctly



# Summarizing data  ---------------------------------------------

song_summary <- spotify %>%
  group_by(track_album_name) %>% # grouping all albums together so the data is summarized by its data and not by song
  summarise(
    avg_popularity = mean(track_popularity, na.rm = TRUE), #na.rm ignores any missing values
    avg_speechiness = mean(speechiness, na.rm = TRUE),
    num_songs = n() #shows how many songs are in each album since they have been compacted 
  )
view(song_summary)

drake_summary <- drake %>%
  group_by(album) %>%
  summarise(avg_track_views = mean(views, na.rm = TRUE),
            num_songs = n())
view(drake_summary)

summary_all <- left_join (drake_summary, song_summary, by = c("album" = "track_album_name")) %>% #merging both data summaries into one using the album column
  arrange(desc(avg_track_views))#ordering them by track views, this will help with the graph plotting


# Plotting data  ----------------------------------------------------------


# Popularity : Spotify vs Lyrics in a Heat map
p1 <- summary_all %>%
  pivot_longer(
    cols = c (avg_track_views, avg_popularity),
    names_to = "metric",
    values_to = "value"
  ) %>% #separates data by their value and column name
  group_by(metric) %>% #by grouping them we make sure they keep their scale of data
  mutate(value_scaled = rescale(value)) %>% #re scale changes max and min
  ggplot(aes(x = album, y = metric)) +
  geom_raster(aes(fill = value_scaled)) + #creates a heat map for the track view
  scale_fill_gradient(low = "lightblue", high = "blue") + #changing gradient scale as it seems more logical
  theme_minimal() + #removes grey from background
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # angle the album names so they can be seen more clearly
  plot_annotation(title = "") %>%
  labs(
    title = "Spotify Popularity vs. Genius Views in Drake Albums",
    subtitle = "when the colours match they have a closer correlation",
    x = NULL, #no title for the x axis as it took too much space and it is repeated in the second plot
    y = "Metric",
    fill = "Scaled values"
  ) #adding captions and formatting

#closest in color have highest correlation


# Speechiness vs Track Views in a linear model
p2 <- summary_all %>%
  ggplot(mapping = aes(x = avg_speechiness, y = avg_track_views)) +
  geom_point(aes(colour = album)) +
  geom_smooth( 
    method = "lm", #lm= linear model
    se = FALSE, #removes shadow of lm
    colour = 'lightblue',
    linetype = 6 #creates the dotted line
  ) + #created a linear model to fit the trend
  xlim (0.1, 0.4) + #changed x axis length to fit furthest data
  scale_y_continuous(
    limits = c(1000000, 3500000),
    breaks = c(1000000, 1500000, 2000000, 2500000, 3000000, 3500000, 4000000),
    # breaks are the numeric vectors of positions
    labels = c('1', '1.5', '2', '2.5', '3', '3.5', '4')
  ) + # adds names to the breaks, this is also to change limit and to make the y axis label make more sense
  theme_linedraw() + #changing background to a nicer layout
  labs(
    title = "Track views vs. Overall speechiness in Drake Albums",
    x = "Presence of Spoken Words (0 = Musical, 1 = Spoken)",
    y = "Genius Views in millions",
    colour = "Albums",
    caption = "Data from Kaggle https://www.kaggle.com/code/thomaskonstantin/drake-lyrics-eda-and-views-prediction?select=drake_data.csv"
  ) +
  theme(plot.caption = element_text(
    face = "italic",
    hjust = 0, #hjust determines position of text in the plot horizontaly 
    size = 10,
    colour = 'grey35'
  ))
# downward curve ? negative relationship = less speechiness has more track views

final_plot <- p1 / p2 + #mergin both plots into one graph and adding it into its own variable 
  plot_annotation(
    title = 'How Drakes Music Performs on Streaming vs Lyric Platforms',
    subtitle = 'Daphne Soldaini 220286086',
    theme = theme(
      plot.title = element_text(face = "bold", hjust = 0.5, size = 20),
      plot.subtitle = element_text(
        face = "italic",
        hjust = 1,
        size = 10,
        colour = 'maroon'
      )
    )
  )

final_plot 

#Saving the graph !!!
ggsave(
  "drake_lyric_vs_song_Daphne_220286086.png",
  width = 20,
  height = 10,
  dpi = 300
) #dpi is the quality it would save as
