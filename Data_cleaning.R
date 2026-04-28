# Install relevant packages -------
install.packages("pacman") # this needs to run only at the first instance. once it has run, comment this out

pacman::p_load("tidyverse", "lubdridate", "janitor")




blood <- read.csv("blood_donation.csv")
ggplot(blood, aes(x=Blood_Group)) +
  geom_bar(binwidth = 0.5)
ggplot(blood, aes(x=Weight_kg, y=Hemoglobin_g_dL)) +
  geom_point()
