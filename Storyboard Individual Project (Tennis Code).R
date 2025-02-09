### Frame 1: Treemap of Nationalities
### Putting My Country's Name Out There!

# import all the libraries needed
library(ggplot2)
library(dplyr)
library(plotly)
library(countrycode)

# read in data sets
matches <- read.csv("KaggleMatches.csv")
players <- read.csv("KagglePlayers.csv")

# group the players by country and get the number of players from each country
country_count <- players %>%
  group_by(country) %>%
  summarise(count = n())

# remove the missing values 
country_count <- country_count[country_count$country != "", ]

# Use countrycode() function to get a new column with the full name of the
# three character countries. However, some country codes are not available.
country_count$country_full <- countrycode(country_count$country, "iso3c", 
                                          "country.name", nomatch = NULL)

# change the three character country codes to full country name
country_count$country_full[2] <- "Netherlands Antilles"
country_count$country_full[4] <- "Algeria"
country_count$country_full[6] <- "Angola"
country_count$country_full[7] <- "Antigua and Barbuda"
country_count$country_full[8] <- "Australasia"
country_count$country_full[11] <- "Aruba"
country_count$country_full[12] <- "American Samoa"
country_count$country_full[16] <- "Bahamas"
country_count$country_full[29] <- "Bangladesh"
country_count$country_full[18] <- "Barbados"
country_count$country_full[22] <- "Bermuda"
country_count$country_full[26] <- "Belize"
country_count$country_full[29] <- "Botswana"
country_count$country_full[31] <- "Great Britain"
country_count$country_full[32] <- "Bahrain"
country_count$country_full[33] <- "Brunei"
country_count$country_full[34] <- "Bulgaria"
country_count$country_full[35] <- "Burkina Faso"
country_count$country_full[37] <- "New Caledonia"
country_count$country_full[38] <- "Cambodia"
country_count$country_full[40] <- "Caribbean/West Indies"
country_count$country_full[41] <- "Cayman Islands"
country_count$country_full[42] <- "Ceylon"
country_count$country_full[43] <- "Republic of the Congo"
country_count$country_full[45] <- "Chile"
country_count$country_full[53] <- "Costa Rica"
country_count$country_full[54] <- "Croatia"
country_count$country_full[59] <- "Denmark"
country_count$country_full[63] <- "Eastern Caribbean"
country_count$country_full[67] <- "El Salvador"
country_count$country_full[71] <- "Fiji"
country_count$country_full[74] <- "West Germany"
country_count$country_full[78] <- "Germany"
country_count$country_full[80] <- "Greece"
country_count$country_full[81] <- "Grenada"
country_count$country_full[82] <- "Guatemala"
country_count$country_full[83] <- "Guadeloupe"
country_count$country_full[86] <- "Haiti"
country_count$country_full[87] <- "Hawaii"
country_count$country_full[89] <- "Honduras"
country_count$country_full[92] <- "Indonesia"
country_count$country_full[94] <- "Iran"
country_count$country_full[99] <- "Virgin Islands"
country_count$country_full[101] <- "Italy"
country_count$country_full[109] <- "Saudi Arabia"
country_count$country_full[110] <- "Kuwait"
country_count$country_full[112] <- "Latvia"
country_count$country_full[113] <- "Libya"
country_count$country_full[117] <- "Lesotho"
country_count$country_full[118] <- "Lebanon"
country_count$country_full[123] <- "Madagascar"
country_count$country_full[125] <- "Malaysia"
country_count$country_full[128] <- "Mongolia"
country_count$country_full[134] <- "Monaco"
country_count$country_full[136] <- "Mauritius"
country_count$country_full[137] <- "Martinique"
country_count$country_full[138] <- "Mauritania"
country_count$country_full[139] <- "Myanmar"
country_count$country_full[141] <- "Nicaragua"
country_count$country_full[142] <- "Netherlands"
country_count$country_full[143] <- "Nepal"
country_count$country_full[144] <- "Nigeria"
country_count$country_full[145] <- "Niger"
country_count$country_full[147] <- "Northern Mariana Islands"
country_count$country_full[150] <- "Oman"
country_count$country_full[153] <- "Paraguay"
country_count$country_full[155] <- "Philippines"
country_count$country_full[159] <- "Pacific Oceania"
country_count$country_full[161] <- "Portugal"
country_count$country_full[163] <- "Puerto Rico"
country_count$country_full[166] <- "Rhodesia"
country_count$country_full[168] <- "South Africa"
country_count$country_full[171] <- "Samoa"
country_count$country_full[172] <- "Serbia and Montenegro"
country_count$country_full[174] <- "Seychelles"
country_count$country_full[176] <- "Singapore"
country_count$country_full[178] <- "Slovenia"
country_count$country_full[180] <- "Solomon Islands"
country_count$country_full[182] <- "Sri Lanka"
country_count$country_full[183] <- "Sudan"
country_count$country_full[184] <- "Switzerland"
country_count$country_full[190] <- "Tanzania"
country_count$country_full[191] <- "Czechoslovakia"
country_count$country_full[196] <- "Turks & Caicos Islands"
country_count$country_full[197] <- "Togo"
country_count$country_full[198] <- "Chinese Taipei"
country_count$country_full[199] <- "Trinidad & Tobago"
country_count$country_full[203] <- "United Arab Emirates"
country_count$country_full[207] <- "Soviet Union"
country_count$country_full[208] <- "Uruguay"
country_count$country_full[211] <- "Vanuatu"
country_count$country_full[213] <- "Vietnam"
country_count$country_full[214] <- "Saint Vincent and the Grenadines"
country_count$country_full[216] <- "Yugoslavia"
country_count$country_full[218] <- "Zambia"
country_count$country_full[219] <- "Zimbabwe"

# remove unknown countries
country_count <- country_count[!(country_count$country_full %in% c("UNK")), ]

# for those countries that appear twice, get the sum (due to usage of older 
# three character code)
country_count <- country_count %>%
  group_by(country_full, country) %>%
  summarise(count = sum(count))

# plot the tree map with label as the three character country, and hover has
# full country name (if available) and the count of players
plot_ly(country_count, labels = ~ country, parents = NA, values = ~ count,
        type = 'treemap',
        hovertemplate = "Country: %{customdata}<br>No. of Players: %{value}<extra></extra>",
        customdata = ~ country_full) %>%
  layout(title = "Treemap of Tennis Players' Nationalities")

#_______________________________________________________________________________

### Frame 2: Winning and Losing Percentage for Players from the Top 10 Countries 
### with The Most Tennis Players

### From Sweet Success to Bitter Defeat Across Countries

library(shiny)

# get the count of matches won for each country
matches_win <- matches %>%
  group_by(winner_ioc) %>%
  summarise(matches_won = n())

# get the count of matches lost for each country
matches_lose <- matches %>%
  group_by(loser_ioc) %>%
  summarise(matches_lost = n())

# join the two data frames together by country code
matches_win_lose <- full_join(matches_win, matches_lose, by = c("winner_ioc" = "loser_ioc"))

# deal with Singapore's redundant appearance
matches_win_lose$matches_won[123] <- matches_win_lose$matches_won[123] + 
  matches_win_lose$matches_won[124]
matches_win_lose$matches_lost[123] <- matches_win_lose$matches_lost[123] + 
  matches_win_lose$matches_lost[124]
matches_win_lose <- subset(matches_win_lose, winner_ioc != "SIN")

# get the count of total matches for each country
matches_win_lose$total_matches <- matches_win_lose$matches_won + 
  matches_win_lose$matches_lost

# percentage of matches won
matches_win_lose$won_percentage <- round(matches_win_lose$matches_won/
                                           matches_win_lose$total_matches*100, 2)

# percentage of matches lost
matches_win_lose$lost_percentage <- round(matches_win_lose$matches_lost/
                                            matches_win_lose$total_matches*100, 2)

# Use countrycode() function to get a new column with the full name of the
# three character countries. However, some country codes are not available.
matches_win_lose$country_full <- countrycode(matches_win_lose$winner_ioc, "iso3c", 
                                             "country.name", nomatch = NULL)

# get only the number of matches won and lost, percentage of matches won 
# and lost, besides the full country name
matches_win_lose <- matches_win_lose[, c(4:7)]

# change the three character country codes to full country name
matches_win_lose$country_full[1] <- "Netherlands Antilles"
matches_win_lose$country_full[2] <- "Algeria"
matches_win_lose$country_full[4] <- "Angola"
matches_win_lose$country_full[5] <- "Antigua and Barbuda"
matches_win_lose$country_full[11] <- "Bahamas"
matches_win_lose$country_full[12] <- "Bangladesh"
matches_win_lose$country_full[13] <- "Barbados"
matches_win_lose$country_full[16] <- "Bermuda"
matches_win_lose$country_full[20] <- "Botswana"
matches_win_lose$country_full[22] <- "Bahrain"
matches_win_lose$country_full[23] <- "Bulgaria"
matches_win_lose$country_full[25] <- "Caribbean/West Indies"
matches_win_lose$country_full[26] <- "Chile"
matches_win_lose$country_full[31] <- "Costa Rica"
matches_win_lose$country_full[32] <- "Croatia"
matches_win_lose$country_full[36] <- "Denmark"
matches_win_lose$country_full[38] <- "Eastern Caribbean"
matches_win_lose$country_full[41] <- "El Salvador"
matches_win_lose$country_full[45] <- "Fiji"
matches_win_lose$country_full[48] <- "West Germany"
matches_win_lose$country_full[51] <- "Germany"
matches_win_lose$country_full[53] <- "Greece"
matches_win_lose$country_full[54] <- "Guatemala"
matches_win_lose$country_full[57] <- "Haiti"
matches_win_lose$country_full[59] <- "Honduras"
matches_win_lose$country_full[61] <- "Indonesia"
matches_win_lose$country_full[63] <- "Iran"
matches_win_lose$country_full[76] <- "Saudi Arabia"
matches_win_lose$country_full[77] <- "Kuwait"
matches_win_lose$country_full[78] <- "Latvia"
matches_win_lose$country_full[80] <- "Lesotho"
matches_win_lose$country_full[81] <- "Lebanon"
matches_win_lose$country_full[85] <- "Madagascar"
matches_win_lose$country_full[87] <- "Malaysia"
matches_win_lose$country_full[90] <- "Mongolia"
matches_win_lose$country_full[95] <- "Monaco"
matches_win_lose$country_full[96] <- "Mauritius"
matches_win_lose$country_full[99] <- "Netherlands"
matches_win_lose$country_full[100] <- "Nigeria"
matches_win_lose$country_full[101] <- "Niger"
matches_win_lose$country_full[104] <- "Oman"
matches_win_lose$country_full[107] <- "Paraguay"
matches_win_lose$country_full[109] <- "Philippines"
matches_win_lose$country_full[111] <- "Pacific Oceania"
matches_win_lose$country_full[113] <- "Portugal"
matches_win_lose$country_full[114] <- "Puerto Rico"
matches_win_lose$country_full[116] <- "Rhodesia"
matches_win_lose$country_full[118] <- "South Africa"
matches_win_lose$country_full[120] <- "Samoa"
matches_win_lose$country_full[121] <- "Serbia and Montenegro"
matches_win_lose$country_full[123] <- "Singapore"
matches_win_lose$country_full[124] <- "Slovenia"
matches_win_lose$country_full[126] <- "Solomon Islands"
matches_win_lose$country_full[128] <- "Sri Lanka"
matches_win_lose$country_full[129] <- "Switzerland"
matches_win_lose$country_full[133] <- "Czechoslovakia"
matches_win_lose$country_full[137] <- "Togo"
matches_win_lose$country_full[138] <- "Chinese Taipei"
matches_win_lose$country_full[139] <- "Trinidad & Tobago"
matches_win_lose$country_full[142] <- "United Arab Emirates"
matches_win_lose$country_full[145] <- "Soviet Union"
matches_win_lose$country_full[146] <- "Uruguay"
matches_win_lose$country_full[150] <- "Vietnam"
matches_win_lose$country_full[151] <- "Yugoslavia"
matches_win_lose$country_full[152] <- "Zambia"
matches_win_lose$country_full[153] <- "Zimbabwe"

# remove nan and unknown countries, besides rows with NA values
matches_win_lose <- matches_win_lose[!(matches_win_lose$country_full %in% c("nan", "UNK")), ]
matches_win_lose <- matches_win_lose[!is.na(matches_win_lose$total_matches), ]

# create a drop-down menu for users to select the country they want to observe
# the pie chart of
# create a drop-down menu for users to select the country they want to observe
# the pie chart of
selectInput("countries", "Select Country:", 
            choices = unique(matches_win_lose$country_full), 
            selected = "United States")

# render the pie chart based on the country selected by the user
renderPlotly({
  chosen_pie <- matches_win_lose[matches_win_lose$country_full == input$countries, ]
  
  won_percentage <- chosen_pie$won_percentage
  lost_percentage <- chosen_pie$lost_percentage
  total_matches <- chosen_pie$total_matches
  
  current_pie <- data.frame(Category = c("Winning Percentage", "Losing Percentage"),
                            Percentage = c(won_percentage, lost_percentage))
  
  plot_ly(data = current_pie, labels = ~ Category, values = ~ Percentage, type = "pie",
          textposition = "inside", textinfo = "label+percent", hoverinfo = "text",
          text = ~ paste("Total Matches:", total_matches)) %>%
    layout(title = paste("Pie Chart of the Winning and Losing Percentages for", 
                         input$countries),
           width = 1100, height = 500,
           margin = list(l = 50, r = 10, b = 50, t = 50, pad = 2))
})
#_______________________________________________________________________________

### Frame 3: Line Chart of Average Age for Men and Women from 1949 to 2021

### Evolution of the Ages

library(gganimate)

# filter out rows with NA tournament dates
matches3 <- matches[!is.na(matches$tourney_date), ]

# get separate data frames for males and females
players3_m <- players[players$gender == "male",]
players3_f <- players[players$gender == "female",]

# left join the matches data frame with players data frame for winners only
data_win_m <- left_join(matches3, players3_m, by = c("winner_id" = "player_id"))
data_win_f <- left_join(matches3, players3_f, by = c("winner_id" = "player_id"))

# extract the year of the tournament
data_win_m$year <- as.numeric(format(as.Date(data_win_m$tourney_date, format = "%Y-%m-%d"), "%Y"))
data_win_f$year <- as.numeric(format(as.Date(data_win_f$tourney_date, format = "%Y-%m-%d"), "%Y"))

# get the relevant columns
data_win_m <- data_win_m[, c(8,15,56,57)]
data_win_f <- data_win_f[, c(8,15,56,57)]

# group by id and year to make sure the same player does not appear 
# twice in one year
data_m <- data_win_m %>%
  group_by(winner_id, year) %>%
  distinct() 

data_f <- data_win_f %>%
  group_by(winner_id, year) %>%
  distinct()

# remove rows with NA gender and age
data_m <- data_m[!is.na(data_m$gender), ]
data_m <- data_m[!is.na(data_m$winner_age), ]
data_f <- data_f[!is.na(data_f$gender), ]
data_f <- data_f[!is.na(data_f$winner_age), ]

# calculate the average winner's age over the years 
avg_age_m <- data_m %>%
  group_by(year) %>%
  summarise(avg_age = mean(winner_age))

avg_age_f <- data_f %>%
  group_by(year) %>%
  summarise(avg_age = mean(winner_age))

# add in a column for gender
avg_age_m$gender <- "male"
avg_age_f$gender <- "female"

# combined both data frames into one
avg_age_combined <- rbind(avg_age_m, avg_age_f)

# plot the line chart
gif <- ggplot(avg_age_combined, aes(x = year, y = avg_age, group = gender, 
                                    colour = factor(gender))) + 
  geom_line() +
  geom_point() +
  labs(x = "Year", y = "Average Age", color = "Gender",
       title = "Line Chart of the Average Age of Tennis Players
       Across Gender from 1949 to 2021") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.box.background = element_rect(colour = "black")) +
  transition_reveal(year) 

# animate the chart
animate(gif, fps = 5, height = 5, width = 7, units = "in", res = 150)

#_______________________________________________________________________________

### Frame 4: Histogram of Match Duration Based on Grand Slam Surfaces, Faceted 
### by Gender

### Battle of the Courts

# filter out only Grand Slam level tournaments
matches_g <- matches[matches$tourney_level == "G", ]

# Create separate data frames for males and females. Note that best of 3 is 
# for females and best of 5 is for males.
matches_g_m <- matches_g[matches_g$best_of == 5, ]
matches_g_f <- matches_g[matches_g$best_of == 3, ]

# get only the relevant columns, which are surface and minutes
matches_g_m <- matches_g_m[, c(3, 27)]
matches_g_f <- matches_g_f[, c(3, 27)]

# remove NA values in the minute column, which may not be available due to
# older data
matches_g_m <- matches_g_m[!is.na(matches_g_m$minutes),]
matches_g_f <- matches_g_f[!is.na(matches_g_f$minutes),]

# add gender column
matches_g_m$Gender <- "Male"
matches_g_f$Gender <- "Female"

# combine the two data frames into one
matches_g_all <- rbind(matches_g_m, matches_g_f)

# allow users to select the type of surface
radioButtons("radio", h3("Select Surface Type:"), 
             choices = unique(matches_g_all$surface), selected = "Clay",
             inline = TRUE)

renderPlotly({
  # filter out the rows containing only the surface chosen by users
  matches_chosen <- matches_g_all[matches_g_all$surface == input$radio, ]
  
  # plot histograms for match duration distribution according to type of court
  # chosen by users, faceted by gender
  p <- ggplot(matches_chosen, aes(x = minutes, fill = Gender,
                                  text = paste("Duration:", x, "minutes",
                                               "<br>Count:", stat(count))))+
    geom_histogram(binwidth = 12, color = "black") +
    scale_fill_manual(values = c("hotpink", "blue")) +
    labs(title = paste("Histogram of the Match Duration Distribution for", 
                       input$radio, "Courts"),
         x = "Match Duration (minutes)", y = "Frequency") +
    facet_wrap(~ Gender) +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))
  
  ggplotly(p, tooltip = c("text")) %>%
    layout(width = 800, height = 450,
           margin = list(l = 50, r = 10, b = 50, t = 50, pad = 2))
}) 

#_______________________________________________________________________________

### Frame 5: Box Plots of Aces and Double Faults Across Dominant
### Hand and Gender

### Dominant Hand at Play in the Power of the Swing

# remove rows with NA values in winner_hand column
matches5 <- matches[!is.na(matches$winner_hand), ]

# get separate data frames for males and females
players5_m <- players[players$gender == "male",]
players5_f <- players[players$gender == "female",]

# left join the matches data frame with players data frame for winners only
data_m5 <- left_join(matches5, players5_m, by = c("winner_id" = "player_id"))
data_f5 <- left_join(matches5, players5_f, by = c("winner_id" = "player_id"))

# get the relevant columns
data_m5 <- data_m5[, c(12, 28, 29, 56)]
data_f5 <- data_f5[, c(12, 28, 29, 56)]

# create a new column for the four categories we want
data_m5$hand_gender <- paste0(data_m5$winner_hand, "_", data_m5$gender)
data_f5$hand_gender <- paste0(data_f5$winner_hand, "_", data_f5$gender)

# ensure that NA values are not kept in the data frames
data_m5 <- data_m5[data_m5$hand_gender %in% c("L_male", "R_male"), ]
data_f5 <- data_f5[data_f5$hand_gender %in% c("L_female", "R_female"), ]

# rename the categories for easier interpretation
data_m5$hand_gender <- ifelse(data_m5$hand_gender == "L_male", 
                              "Left-Handed Male", "Right-Handed Male")
data_f5$hand_gender <- ifelse(data_f5$hand_gender == "L_female", 
                              "Left-Handed Female", "Right-Handed Female")

# combine the two data frames
data_5 <- rbind(data_m5, data_f5)

# rename the columns
names(data_5) <- c("Dominant Hand", "Aces", "Double Faults", "Gender",
                   "Category")

# allow user to choose if they want to observe the distribution of 
# aces or distribution of double faults
radioButtons("AceOrDf", "Distribution of Aces or Double Faults?", 
             choices = c("Aces", "Double Faults"), selected = "Aces",
             inline = TRUE)

# render the interactive boxplots
renderPlotly({
  
  plot_ly(data_5, x = ~ .data[[input$AceOrDf]], y = ~ factor(Category), 
          color = ~ Category, showlegend = FALSE) %>% 
    add_boxplot() %>%
    layout(title = paste("Box Plot Distribution of", input$AceOrDf, 
                         "Across Gender and Dominant Hand"), 
           xaxis = list(title = "Gender and Dominant Hand"), 
           yaxis = list(title = input$AceOrDf), 
           width = 1100, height = 500,
           margin = list(l = 50, r = 10, b = 50, t = 50, pad = 2))
})

#_______________________________________________________________________________

### FRAME 6: Scatter Plot of Winning Percentage Against Height 

### Height...Is It A Factor?

library(stringr)

# get the count of matches won for each player
matches_win6 <- matches %>%
  group_by(winner_name, winner_ht) %>%
  summarise(matches_won = n())

# get the count of matches lost for each player
matches_lose6 <- matches %>%
  group_by(loser_name, loser_ht) %>%
  summarise(matches_lost = n())

# join the two data frames together by the player names 
matches_win_lose6 <- full_join(matches_win6, matches_lose6, 
                               by = c("winner_name" = "loser_name"))

# remove extra whitespace for the names
matches_win_lose6$winner_name <- trimws(matches_win_lose6$winner_name)

# get the count of total matches for each player
matches_win_lose6$total_matches <- matches_win_lose6$matches_won + 
  matches_win_lose6$matches_lost

# percentage of matches won by each player
matches_win_lose6$won_percentage <- round(matches_win_lose6$matches_won/
                                            matches_win_lose6$total_matches*100, 2)

# get the relevant columns
matches_win_lose6 <- matches_win_lose6[, c(1,2,7)]

# remove rows with NA values for the height
matches_win_lose6 <- matches_win_lose6[!is.na(matches_win_lose6$winner_ht), ]

# get the full names of the players
players6 <- players
players6$full_name <- paste(players6$name_first, players6$name_last)

# join matches_win_lose6 and players6 together to obtain the gender of each player
joined_data6 <- left_join(matches_win_lose6, players6, 
                          by = c("winner_name" = "full_name"))

# get the relevant columns
joined_data6 <- joined_data6[, c(1,2,3,10)]

# allow users to select whether they want to observe the scatter plot 
# of male or female
radioButtons("gender", "Select Gender:", choices = c("Male", "Female"), 
             selected = "Male", inline = TRUE)

# render the interactive scatter plot
renderPlotly({
  
  # filter out the data frame based on the gender selected by the user
  data_chosen6 <- joined_data6[joined_data6$gender == tolower(input$gender), ]
  
  plot_ly(data_chosen6, x = ~ winner_ht, y = ~ won_percentage,
          hovertext = ~paste0("Name: ", winner_name, 
                              "<br>", "Height: ", winner_ht, " cm",
                              "<br>", "Winning Percentage: ", won_percentage, 
                              "%")) %>%
    add_markers(color = I("limegreen")) %>%
    layout(title = paste("Scatter Plot of Winning Percentage Against Height of", 
                         str_to_sentence(input$gender), "Tennis Players"),
           xaxis = list(title = "Height (cm)"),
           yaxis = list(title = "Winning Percentage"),
           width = 900, height = 450,
           margin = list(l = 50, r = 10, b = 50, t = 50, pad = 2))
})

#_______________________________________________________________________________

### Frame 7: Top 20 Players with the Most Grand Slam Titles Obtained

### It's Raining Grand Slams!
# subset for Grand Slam tournaments only
matches7 <- matches[matches$tourney_level == "G", ]
# subset for final rounds only
matches7 <- matches7[matches7$round == "F", ]
# get the relevant columns
matches7 <- matches7[, c(2,11)]

players7 <- players
# make a column for full_name
players7$full_name <- paste(players7$name_first, players7$name_last)
# get the relevant columns
players7 <- players7[, c(7,8)]

# join the two data frames by the winner's name (to get gender)
gs_data <- left_join(matches7, players7, by = c("winner_name" = "full_name"))

# remove any rows with NA values for gender
gs_data <- gs_data[!is.na(gs_data$gender), ]

# calculate the number of Grand Slams won by each player
gs_data <- gs_data %>%
  group_by(winner_name, gender) %>%
  summarise(number_gs = n()) 

# arrange the number of Grand Slams in descending order and get the top 25
gs_data_top25 <- head(arrange(gs_data, desc(number_gs)), 25)

# create the bar chart 
plot_ly(gs_data_top25, x = ~ number_gs, y = ~ winner_name) %>%
  add_bars(color = ~ gender, colors = c("hotpink", "royalblue"),
           showlegend = TRUE) %>%
  layout(title = "Bar Chart of the Top 25 Players with the Most Grand Slam Titles from 1949 to 2021",
         xaxis = list(title = "Number of Grand Slam Titles"),
         yaxis = list(title = "Player Name",
                      categoryorder = "array",
                      categoryarray = rev(gs_data_top25$winner_name)),
         width = 900, height = 500,
         margin = list(l = 50, r = 10, b = 50, t = 50, pad = 2))

#_______________________________________________________________________________

### Frame 8: Heatmap of the Top 10 Player's Winning Percentage In Each Round Across
### Grand Slams

### Winning is my Middle Name, or Is It?

library(tidyr)

# subset for Grand Slam tournaments only
matches8 <- matches[matches$tourney_level == "G", ]

# remove rows with round Q4
matches8 <- matches8[!matches8$round == "Q4", ]

# rename all the same tournaments as Australian Open
matches8$tourney_name <- 
  ifelse(matches8$tourney_name %in% c("Australian Chps.", "Australian Open-2",
                                      "Australian Championships", 
                                      "Australian Open 2"),
         "Australian Open", matches8$tourney_name)

# rename all the same tournaments as French Open
matches8$tourney_name <- 
  ifelse(matches8$tourney_name == "Roland Garros", "French Open", 
         matches8$tourney_name)

# rename all the same tournaments as US Open
matches8$tourney_name <- 
  ifelse(matches8$tourney_name %in% c("Us Open", "US National Championships"),
         "US Open", matches8$tourney_name)

# get the top 10 players based on number of Grand Slams obtained
gs_data_top10 <- gs_data_top25$winner_name[1:10]

# filter out rows with the top 10 players as winners and get the 
# relevant columns
matches8_win <- matches8[matches8$winner_name %in% gs_data_top10, ]
matches8_win <- matches8_win[, c(2,11,26)]

# filter out rows with the top 10 players as losers and get the 
# relevant columns
matches8_lose <- matches8[matches8$loser_name %in% gs_data_top10, ]
matches8_lose <- matches8_lose[, c(2,19,26)]

# get the count of matches won for each player across tournaments and rounds
matches8_win <- matches8_win %>%
  group_by(winner_name, tourney_name, round) %>%
  summarise(matches_won = n())

# get the count of matches lost for each player across tournaments and rounds
matches8_lose <- matches8_lose %>%
  group_by(loser_name, tourney_name, round) %>%
  summarise(matches_lost = n())

# rename the data frame columns
names(matches8_win) <- c("name", "tourney_name", "round", "matches_won")
names(matches8_lose) <- c("name", "tourney_name", "round", "matches_lost")

# combine the two data frames by name, tourney_name and round
combined_data8 <- full_join(matches8_win, matches8_lose, 
                            by = c("name", "tourney_name", "round"))

# replace NA values in matches_lost with 0 to indicate they have never lost
combined_data8$matches_lost <- ifelse(is.na(combined_data8$matches_lost), 0, 
                                      combined_data8$matches_lost)

# replace NA values in matches_won with 0 to indicate they have never won
combined_data8$matches_won <- ifelse(is.na(combined_data8$matches_won), 0, 
                                     combined_data8$matches_won)

# get total matches
combined_data8$total_matches <- combined_data8$matches_won + combined_data8$matches_lost

# compute winning percentage for each round in each Grand Slam
combined_data8$winning_percentage <- 
  combined_data8$matches_won/combined_data8$total_matches*100

# get the relevant columns
combined_data8 <- combined_data8[, c(1:3,7)]

# plot the heatmap
gg8 <- ggplot(combined_data8, aes(round, name, fill = winning_percentage,
                                  text = paste("Name:", name,
                                               "<br>Round:", round,
                                               "<br>Winning Percentage:", 
                                               round(winning_percentage,2), "%")))+
  geom_tile(color= "white", lwd = 0.5) + 
  scale_x_discrete(limits = c("R128", "R64", "R32", "R16", "QF", "SF", "F")) +
  scale_fill_gradient2(midpoint = 50, low = "red", mid = "orange", 
                       high = "limegreen") +
  facet_grid(~ tourney_name) + 
  labs(title = "Heatmap of the Winning Percentage for the Top 10 Players Across Each Round of the Four Grand Slams",
       x = "Round", y = "Player Name", fill = "Winning Percentage") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2, size = 10, margin = margin(b = 20)))

# make it interactive
ggplotly(gg8, tooltip = c("text"))

#_______________________________________________________________________________

### Frame 9: Bar Chart of Players Who can Beat the Top 10 Players

### Oh No!...Haunting of the Bogey Players 

# subset for Grand Slam tournaments only
matches9 <- matches[matches$tourney_level == "G", ]

# filter out rows with the top 10 players as winners and get the 
# relevant columns
matches9_win <- matches9[matches9$winner_name %in% gs_data_top10, ]
matches9_win <- matches9_win[, c(11, 19)]

# filter out rows with the top 10 players as losers and get the 
# relevant columns
matches9_lose <- matches9[matches9$loser_name %in% gs_data_top10, ]
matches9_lose <- matches9_lose[, c(11, 19)]

# group by the winner and loser names, count the number times each of the pairs
# met, and get a new column of who plays with who
matches9_win <- matches9_win %>%
  group_by(winner_name, loser_name) %>%
  summarise(matches_won = n()) %>%
  mutate(Match = paste(winner_name, "vs", loser_name))

# group by the loser and winner names, count the number times each of the pairs
# met, and get a new column of who plays with who
matches9_lose <- matches9_lose %>%
  group_by(loser_name, winner_name) %>%
  summarise(matches_lost = n()) %>%
  mutate(Match = paste(loser_name, "vs", winner_name))

# join the two data frames by Match and get the relevant columns
matches9_combined <- full_join(matches9_win, matches9_lose, by = c("Match"))
matches9_combined <- matches9_combined[, c(1,2,3,4,7)]

# if there are NA values in the matches won and matches lost column, replace
# them with 0 to indicate never winning or never losing a match against 
# a particular player
matches9_combined$matches_won <- ifelse(is.na(matches9_combined$matches_won), 
                                        0 , matches9_combined$matches_won)
matches9_combined$matches_lost <- ifelse(is.na(matches9_combined$matches_lost), 
                                         0 , matches9_combined$matches_lost)

# rename the columns
names(matches9_combined) <- c("Player", "Bogey_Player", "Players_Wins", 
                              "Match", "Players_Losses")

# get the total number of matches played by each pair of players
matches9_combined$Total_Meet <-
  matches9_combined$Players_Wins + matches9_combined$Players_Losses

# compute the percentage of bogey players winning 
matches9_combined$Bogey_Win_Percentage <- 
  round(matches9_combined$Players_Losses/matches9_combined$Total_Meet*100, 2)

# get rows where the percentage is more than 0.5, to show consistent
# winning of the bogey player over the top player
matches9_bogey <- matches9_combined[matches9_combined$Bogey_Win_Percentage > 50,]

# remove rows with NA values for the Player
matches9_bogey <- matches9_bogey[!is.na(matches9_bogey$Player), ]

# create a new column for the record of the pair of players
matches9_bogey$Record <- paste0(matches9_bogey$Players_Wins, "-",
                                matches9_bogey$Players_Losses)

# get the relevant columns
matches9_bogey <- matches9_bogey[, c(1,2,8,7)]

# create the bar plot, where the bogey player for each top player is plotted
# horizontally, with the bogey player's win percentage as the height of the bars
gg9 <- ggplot(matches9_bogey, aes(x = Bogey_Win_Percentage, y = Player, 
                                  fill = Player,
                                  text = paste("Player:", Player,
                                               "<br>Bogey Player:", Bogey_Player,
                                               "<br>Record:", Record,
                                               "<br>Bogey Player Win Percentage:", 
                                               Bogey_Win_Percentage, "%"))) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), 
           width = 0.6, color = "black") +
  labs(title = "Bar Chart of the Winning Percentages for 
         Bogey Players that Have Toppled Top Players",
       x = "Bogey Player Win Percentage", y = "Player") +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, vjust = 2, size = 10, margin = margin(b = 20))) +
  guides(fill = FALSE) +
  geom_text(aes(x = 30, label = Bogey_Player), 
            position = position_dodge(width = 0.8), size = 3)

# interactive horizontal bar plot
ggplotly(gg9, tooltip = "text")

#_______________________________________________________________________________

### Frame 10: Bubble Plot of the Best Player Who Has Never Won A Grand Slam

### Errr...Let's Not Think Too Much About Grand Slam Titles Shall We?

# First, get the winning percentage for players who have never won a Grand Slam

# get matches that are Grand Slam finals only
matches10 <- matches[matches$tourney_level == "G", ]
matches10 <- matches10[matches10$round == "F", ]

# the names of players who have won Grand Slams before
gs_winners <- unique(matches10$winner_name)

# group by the winner names to get the number of times a player has won a title
matches10_win <- matches %>%
  group_by(winner_name) %>%
  summarise(matches_won = n()) 

# group by the loser names to get the number of times a player has lost a title
matches10_lose <- matches %>%
  group_by(loser_name) %>%
  summarise(matches_lost = n()) 

# join the two data frames together
matches10_win_lose <- full_join(matches10_win, matches10_lose, 
                                by = c("winner_name" = "loser_name"))

# trim the white space in winner_name column
matches10_win_lose$winner_name <- trimws(matches10_win_lose$winner_name)

# replace NA values with 0 for matches_won column
matches10_win_lose$matches_won <- ifelse(is.na(matches10_win_lose$matches_won), 
                                         0, matches10_win_lose$matches_won)

# replace NA values with 0 for matches_lost column
matches10_win_lose$matches_lost <- ifelse(is.na(matches10_win_lose$matches_lost), 
                                          0, matches10_win_lose$matches_lost)

# compute the total matches
matches10_win_lose$total_matches <- matches10_win_lose$matches_won +
  matches10_win_lose$matches_lost

# remove players who have won Grand Slams before
matches10_win_lose <- matches10_win_lose[!(matches10_win_lose$winner_name %in% gs_winners),]

# compute the winning percentage
matches10_win_lose$winning_percentage <- round(matches10_win_lose$matches_won/matches10_win_lose$total_matches*100, 2)

#_________

# Secondly, get the number of titles that players who have never won 
# a Grand Slam have

# get only final matches
matches10_titles <- matches[matches$round == "F", ]

# compute the number of titles that a player has won
matches10_win_titles <- matches10_titles %>%
  group_by(winner_name) %>%
  summarise(titles_won = n()) 

# trim whitespace in winner_name
matches10_win_titles$winner_name <- trimws(matches10_win_titles$winner_name)

# # remove players who have won Grand Slams before
matches10_win_titles <- matches10_win_titles[!(matches10_win_titles$winner_name %in% gs_winners),]

#_________

# Thirdly, get the highest ranking for players who have never won a Grand Slam

# get only the winner_name and winner_rank
matches10_rank <- matches[, c(11,46)]

# obtain the highest rank for each winner
matches10_rank <- matches10_rank %>%
  group_by(winner_name) %>%
  mutate(highest_rank = min(winner_rank)) 

# remove the winner_rank column
matches10_rank <- matches10_rank[, c(1,3)]

# remove duplicate rows
matches10_rank <- distinct(matches10_rank)

# remove players who have won Grand Slams before
matches10_rank <- matches10_rank[!(matches10_rank$winner_name %in% gs_winners),]

#_____

# Join all the data frames together to get the final data set. They are joined
# according to matches10_win_titles, to ensure that only players who have won at
# least one title before are considered to be in the plot. 

matches10_plot <- left_join(matches10_win_titles, matches10_win_lose, 
                            by = "winner_name")

matches10_plot <- left_join(matches10_plot, matches10_rank, 
                            by = "winner_name")

# remove rows with NA values in highest_rank column
matches10_plot <- matches10_plot[!is.na(matches10_plot$highest_rank), ]

# involve players.csv to obtain gender information for each player
players10 <- players 
players10$full_name <- paste(players10$name_first, players10$name_last)
players10 <- players10[, c(8,7)]

matches10_plot <- left_join(matches10_plot, players10, 
                            by = c("winner_name" = "full_name"))

# remove rows with NA values for gender
matches10_plot <- matches10_plot[!is.na(matches10_plot$gender), ]

# ensure "male" and "female" are "Male" and "Female" for legend 
# plotting purposes
matches10_plot$gender <- str_to_sentence(matches10_plot$gender)

#_____

# Lastly, customise the legend and plot an interactive bubble plot

lgnd <- list(font=list(size = 12, color = "darkgreen"),
             bordercolor = "darkgreen", borderwidth = 2, 
             orientation = "h")

plot_ly(matches10_plot, x = ~ titles_won, y = ~ winning_percentage, 
        hoverinfo = "text",
        colors = c("hotpink", "royalblue"),
        text = ~ paste("<b>Player:", winner_name,
                       "<br>Titles Won:", titles_won,
                       "<br>Total Matches Played:", total_matches,
                       "<br>Winning Percentage:", winning_percentage, "%",
                       "<br>Highest Rank:", highest_rank)) %>%
  add_markers(color = ~ gender, size = ~ highest_rank) %>%
  layout(legend = lgnd,
         title = "Bubble Plot of Winning Percentage Against Number of Titles
  Won for Players Who Have Never Won A Grand Slam",
         xaxis = list(title = "Number of Titles Won"),
         yaxis = list(title = "Winning Percentage"),
         showlegend = TRUE,
         width = 900, height = 500,
         margin = list(l = 50, r = 10, b = 50, t = 50, pad = 2),
         hoverlabel = list(bgcolor="lightgreen", bordercolor="darkgreen",
                           font=list(family="Arial", color="darkgreen")))

