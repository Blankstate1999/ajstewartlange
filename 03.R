#Data wrangling ----------------------------------------------------------------

library(tidyverse)

head(mpg,10)
str(mpg)

mpg %>% 
  select(manufacturer)

mpg %>%
  distinct(manufacturer) %>%  #Identifies unique entries
  count()                     #Counts unique entries

mpg %>%
  filter(manufacturer == "honda") #Extracts all 'Honda' entries

mpg %>%
  filter(manufacturer == "honda" | manufacturer == "audi")  # | = 'or'

mpg %>%
  filter(manufacturer == "honda" & year == "1999") %>%
  select(manufacturer, year, cty, hwy)

mpg %>%
  mutate(manufacturer = str_to_title(manufacturer), 
         model = str_to_title(model)) %>%
  select(manufacturer, model, year, trans, hwy)

my_messy_data <- read_csv("https://raw.githubusercontent.com/ajstewartlang/03_data_wrangling/master/data/my_data.csv")

head(my_messy_data)

my_tidied_data <- my_messy_data %>%
  mutate(condition = recode(condition,
                            "1" = "PrimeA_TargetA",
                            "2" = "PrimeA_TargetB",
                            "3" = "PrimeB_TargetA",
                            "4" = "PrimeB_TargetB")) %>%
  separate(col = "condition", into = c("Prime", "Target"), sep = "_") %>%
  mutate(Prime = factor(Prime), Target = factor(Target))


#Data summarizing --------------------------------------------------------------

library(tidyverse)

mpg %>%
  group_by(manufacturer) %>%
  summarise(mean_hwy = mean(hwy), sd_hwy = sd(hwy), number = n()) %>%
  arrange(-mean_hwy)

mpg %>%
  group_by(manufacturer) %>%
  summarise_at(c("displ", "cty", "hwy"), mean, na.rm = T)

mpg %>%
  group_by(manufacturer) %>%
  summarise_if(is.numeric, mean, na.rm = T)

mpg %>%
  group_by(manufacturer) %>%
  mutate(mean_hwy = mean(hwy), sd_hwy = sd(hwy)) %>%
  select(-class, -trans)

mpg_with_mean <- mpg %>%
  group_by(manufacturer) %>%
  mutate(mean_hwy = mean(hwy), sd_hwy = sd(hwy)) %>%
  select(-class, -trans)

head(mpg_with_mean)
str(mpg_with_mean)

head(starwars)
str(starwars)

starwars %>%
  filter(!is.na(height)) %>%
  filter(species == "Human") %>%
  summarise(mean_height = mean(height))

starwars %>%
  filter(!is.na(height)) %>%
  filter(species == "Human") %>%
  summarise(mean_height = mean(height))

  

