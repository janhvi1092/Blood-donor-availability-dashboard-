library(shiny)
library(bslib)
library(tidyverse)
library(plotly) # Added for interactive scatter plots
library(DT)
library(janitor)
library(lubridate)
library(stringr)



# Load data
blood_raw <- read.csv("blood_donation.csv")

# Clean and transform the dataset
blood_clean <- blood_raw %>% 
  # Standardize all column names to snake_case first to avoid manual renaming
  janitor::clean_names() %>%
  
  # Ensure dates are properly formatted
  mutate(
    last_donation_date = as.Date(last_donation_date),
    registration_date = as.Date(registration_date)
  ) %>%
  
  # Create age groups for the demographic pyramid
  mutate(
    age_group = cut(
      age, 
      breaks = c(-Inf, 19, seq(24, 64, by = 5), Inf), 
      labels = c("<20", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65+"),
      right = TRUE
    )
  ) %>%
  
  # Determine eligibility
  mutate(
    eligible_for_donation = case_when(
      age > 18 & age < 60 &
        (is.na(medical_condition) | medical_condition %in% c("", "None")) &
        (is.na(last_donation_date) | last_donation_date <= Sys.Date() - days(90)) & # days(90) is safer date math
        weight_kg > 50 &
        (
          (gender == "Male" & hemoglobin_g_d_l > 14) |
            (gender == "Female" & hemoglobin_g_d_l > 12)
        ) ~ TRUE,
      TRUE ~ FALSE
    )
  )

# Calculate key completeness based on the newly cleaned names
key_cols <- c("donor_id", "full_name", "gender", "age", "blood_group", 
              "contact_number", "email", "city", "state", "country", 
              "last_donation_date", "total_donations", "eligible_for_donation", 
              "medical_condition", "weight_kg", "hemoglobin_g_d_l", 
              "donation_center", "registration_date")

blood_clean <- blood_clean %>%
  mutate(key_completeness = rowSums(!is.na(select(., all_of(key_cols)))) / length(key_cols))



ui <- page_sidebar(
  title = "Blood Donation Service Monitoring",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  sidebar = sidebar(
    title = "Global Filters",
    selectInput("blood_group", "Blood Group", 
                choices = c("All", unique(blood_clean$blood_group)), 
                selected = "All"),
    selectInput("city", "City", 
                choices = c("All", unique(blood_clean$city)), 
                selected = "All")
  ),
  
  layout_columns(
    col_widths = c(6, 6, 12),
    card(
      card_header("Donor Demographic Pyramid"),
      plotOutput("pyramid_plot")
    ),
    card(
      card_header("Donor Health & Center Landscape"),
      plotlyOutput("donor_scatter") # Replaced leafletOutput
    ),
    card(
      card_header("Filtered Donor Database"),
      DTOutput("donor_table")
    )
  )
)

server <- function(input, output, session) {
  
  # 1. Reactive Data Filter
  filtered_data <- reactive({
    data <- blood_clean
    if (input$blood_group != "All") {
      data <- data %>% filter(blood_group == input$blood_group)
    }
    if (input$city != "All") {
      data <- data %>% filter(city == input$city)
    }
    data
  })
  
  # 2. Demographic Pyramid Plot (Unchanged)
  output$pyramid_plot <- renderPlot({
    req(nrow(filtered_data()) > 0)
    
    pyramid_df <- filtered_data() %>%
      group_by(age_group, gender) %>%
      summarise(
        total_donors = n(),
        eligible_donors = sum(eligible_for_donation == TRUE, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      mutate(
        total_donors = ifelse(gender == "Male", -total_donors, total_donors),
        eligible_donors = ifelse(gender == "Male", -eligible_donors, eligible_donors)
      )
    
    max_val <- max(abs(pyramid_df$total_donors), na.rm = TRUE)
    
    ggplot(pyramid_df, aes(x = age_group, fill = gender)) +
      geom_bar(aes(y = total_donors), stat = "identity", alpha = 0.5, width = 0.8) +
      geom_bar(aes(y = eligible_donors), stat = "identity", width = 0.4) +
      scale_y_continuous(labels = abs, limits = c(-max_val * 1.1, max_val * 1.1)) +
      scale_fill_manual(values = c("Male" = "#2c3e50", "Female" = "#e74c3c")) +
      coord_flip() +
      theme_minimal() +
      labs(x = "Age Group", y = "Number of Donors (Solid = Eligible, Faded = Total)")
  })
  
  # 3. Interactive Scatter Plot 
  output$donor_scatter <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    
    # Create a ggplot with custom text mapping for the Plotly tooltip
    p <- ggplot(filtered_data(), aes(
      x = age, 
      y = hemoglobin_g_d_l, 
      color = donation_center,
      size = total_donations,
      # The 'text' aesthetic creates a custom HTML tooltip
      text = paste0(
        "<b>Name:</b> ", full_name, "<br>",
        "<b>ID:</b> ", donor_id, "<br>",
        "<b>City:</b> ", city, "<br>",
        "<b>Center:</b> ", donation_center, "<br>",
        "<b>Eligibility:</b> ", ifelse(eligible_for_donation, "Eligible", "Not Eligible"), "<br>",
        "<b>Total Donations:</b> ", total_donations
      )
    )) +
      # Use jitter to prevent overplotting if multiple donors share exact age/hemoglobin
      geom_jitter(alpha = 0.7, width = 0.5, height = 0.2) +
      scale_size_continuous(range = c(2, 8)) + # Control dot size scaling
      theme_minimal() +
      labs(
        x = "Age",
        y = "Hemoglobin (g/dL)",
        color = "Donation Center"
      ) +
      theme(legend.position = "right")
    
    # Convert ggplot to an interactive plotly object, specifying 'text' for the tooltip
    ggplotly(p, tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "white", font = list(family = "Arial")))
  })
  
  # 4. Data Table (Unchanged)
  output$donor_table <- renderDT({
    datatable(
      filtered_data() %>% 
        select(donor_id, full_name, age, gender, blood_group, city, eligible_for_donation, donation_center),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE,
      class = "display nowrap"
    )
  })
}

shinyApp(ui, server)
