# Install relevant packages -------
#install.packages("pacman") # this needs to run only at the first instance. once it has run, comment this out

pacman::p_load(tidyverse, 
               lubdridate, 
               janitor,
               stringr,
               skimr)   #  Don't kill fish




blood_raw <- read.csv("blood_donation.csv")
ggplot(blood, aes(x=Blood_Group)) +
  geom_bar(binwidth = 0.5)
ggplot(blood, aes(x=Weight_kg, y=Hemoglobin_g_dL)) +
  geom_point()


key_cols = c("Donor_ID", "Full_Name", "Gender", "Age", "Blood_Group", "Contact_Number",
             "Email" , "City" , "State", "Country", "Last_Donation_Date", 
             "Total_Donations" , "Eligible_for_Donation" , "Medical_Condition" , 
             "Weight_kg","Hemoglobin_g_dL", "Donation_Center", 
             "Registration_Date")
blood <- blood_raw %>% 
 # mutate(key_completeness = rowSums(!is.na(.[,key_cols]))/length(key_cols)) %>% 
  # this identifies the any missing data
  rename(
    donor_id = Donor_ID,
    full_name = Full_Name,
    gender = Gender,
    age = Age ,
    blood_group = Blood_Group,
    contact_number = Contact_Number,
    email = Email,
    city = City,
    state = State,
    country = Country,
    last_donation_date = Last_Donation_Date,
    total_donations = Total_Donations,
    eligible_for_donation = Eligible_for_Donation,
    hemoglobin = Hemoglobin_g_dL,
    donation_center = Donation_Center,
    registration_date = Registration_Date,
    medical_condition = Medical_Condition,
    weight = Weight_kg
         ) %>% 
#  mutate(blood$last_donation_date <- as.Date(blood$last_donation_date)) %>% 
  mutate(
    eligible_for_donation = case_when(
      age > 18 & age < 60 &
        (is.na(medical_condition) | medical_condition == "" | medical_condition == "None") &
        (is.na(last_donation_date) | last_donation_date <= Sys.Date() - months(3)) &
        weight > 50 &
        (
          (gender == "Male" & hemoglobin > 14) |
            (gender == "Female" & hemoglobin > 12)
        )
      ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>% 
  janitor::clean_names()
setdiff(key_cols, names(blood))

skim(blood)




