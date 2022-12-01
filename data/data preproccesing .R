require(readr)
require(dplyr)
require(fastDummies)
require(stringi)


life_data <- read_csv("data/life-expectancy-raw.csv")
life_data %>%
  group_by(Country) %>%
  summarise(n = n())

# There are 193 countries, and I assume each country has 16 entries

life_data <- life_data %>%
  dummy_cols('Status',remove_first_dummy = TRUE, remove_selected_columns = TRUE) %>%
  select(-Country,-Year)



life_clean <- na.omit(life_data)
write_csv(life_clean,"data/life_clean.csv")
