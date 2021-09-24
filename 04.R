library(tidyverse)
library(ggplot2)


mpg %>%
  mutate(manufacturer = str_to_title(manufacturer)) %>% #Changes to title case
  ggplot2::ggplot(aes(x=manufacturer, y=cty)) +
  geom_jitter(width = .2, alpha = .75, size = 2) +
  geom_point() +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=45, vjust=0.5, hjust=.5)) +
  theme(text = element_text(size = 13)) +
  labs(title = "City Fuel Economy by Car Manufacturer",
       x = "Manufacturer", 
       y = "City Fuel Economy (mpg)")

#Improving the plot-------------------------------------------------------------

library(Hmisc)

mpg %>%
  mutate(manufacturer = str_to_title(manufacturer)) %>% #Changes to title case
  ggplot2::ggplot(aes(x=manufacturer, y=cty)) +
  stat_summary(fun.data = mean_cl_boot, colour = "black", size = 1) +
  geom_jitter(alpha = .25) +
  theme_minimal() +
  theme(axis.text.x=element_text(angle=45, vjust=0.5, hjust=.5)) +
  theme(text = element_text(size = 13)) +
  labs(title = "City Fuel Economy by Car Manufacturer",
       x = "Manufacturer", 
       y = "City Fuel Economy (mpg)")

#The finished plot?-------------------------------------------------------------

mpg %>%
  mutate(manufacturer = str_to_title(manufacturer)) %>%
  ggplot(aes(x = fct_reorder(manufacturer, .fun = mean, cty), y = cty, colour = manufacturer)) +
  stat_summary(fun.data = mean_cl_boot, size = 1) +
  geom_jitter(alpha = .25) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(title = "Manufacturer by City Fuel Economy",
       x = "Manufacturer", 
       y = "City Fuel Economy (mpg)") +
  guides(colour = FALSE) +
  coord_flip()

#Using facet_wrap()-------------------------------------------------------------

mpg %>%
  filter(class != "suv") %>%
  mutate(class = str_to_title(class)) %>%
  ggplot(aes(x = displ, y = cty, colour = class)) + 
  geom_jitter(width = .2) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  labs(title = "City Fuel Economy by Engine Displacement",
       x = "Engine Displacement (litres)", 
       y = "City Fuel Economy (mpg)") +
  guides(colour = FALSE) +
  facet_wrap(~ class)

#Scatterplots-------------------------------------------------------------------

mpg %>%
  mutate(class = str_to_upper(class)) %>%
  ggplot(aes(x = cty, y = displ)) +
  geom_point(aes(colour = class)) +
  geom_smooth(se = FALSE) +
  theme(text = element_text(size = 13)) +
  theme_minimal() + 
  labs(x = "City Fuel Economy (mpg)",
       y = "Engine Displacement (litres)", 
       colour = "Vehicle Class")

#Plotting with histograms-------------------------------------------------------

mpg %>%
  ggplot(aes(x = displ)) +
  geom_histogram(binwidth = .5, fill = "grey") +
  labs(title = "Histogram of Engine Displacement",
       x = "Engine Displacement (litres)",
       y = "Count")

#The ggridges package-----------------------------------------------------------

install.packages("ggridges")
library(ggridges)

mpg %>%
  mutate(class = str_to_title(class)) %>%
  ggplot(aes(x = displ, y = fct_reorder(class, .fun = mean, displ))) +
  geom_density_ridges(height = .5, aes(fill = class)) +
  theme_minimal() +
  theme(text = element_text(size = 13)) +
  guides(fill = FALSE) + 
  labs(x = "Engine Displacement (litres)",
       y = NULL)

#The NHANES dataset-------------------------------------------------------------

install.packages("NHANES")
library(NHANES)

ncol(NHANES)
nrow(NHANES)


