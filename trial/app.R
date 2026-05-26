# Load Required Libraries -------------------------------------------------
library(shiny)
library(bslib)
library(tidyverse)
library(lubridate)
library(janitor)
library(plotly)
library(DT)

# Data Preprocessing ------------------------------------------------------
# This runs once when the app starts
blood_raw <- read.csv("blood_donation.csv")

blood_clean <- blood_raw %>% 
  janitor::clean_names() %>%
  mutate(
    last_donation_date = as.Date(last_donation_date),
    registration_date = as.Date(registration_date),
    # Pre-calculate age groups for the pyramid
    age_group = cut(
      age, 
      breaks = c(-Inf, 19, seq(24, 64, by = 5), Inf), 
      labels = c("<20", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65+"),
      right = TRUE
    ),
    # Determine eligibility
    eligible_for_donation = case_when(
      age > 18 & age < 60 &
        (is.na(medical_condition) | medical_condition %in% c("", "None")) &
        (is.na(last_donation_date) | last_donation_date <= Sys.Date() - days(90)) &
        weight_kg > 50 &
        (
          (gender == "Male" & hemoglobin_g_d_l > 14) |
            (gender == "Female" & hemoglobin_g_d_l > 12)
        ) ~ TRUE,
      TRUE ~ FALSE
    )
  )

# Extract choices for UI
available_blood_groups <- unique(blood_clean$blood_group)
available_cities <- unique(blood_clean$city)
min_age <- min(blood_clean$age, na.rm = TRUE)
max_age <- max(blood_clean$age, na.rm = TRUE)

# UI Definition -----------------------------------------------------------
ui <- page_sidebar(
  title = "Blood Donation Service Monitoring",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  # Sidebar with filters
  sidebar = sidebar(
    title = "Global Filters",
    p("Leave blank to select all."),
    selectInput("blood_group", "Blood Group(s)", 
                choices = available_blood_groups, 
                selected = available_blood_groups, # Select all by default
                multiple = TRUE),
    
    selectInput("city", "City/Cities", 
                choices = available_cities, 
                selected = available_cities, # Select all by default
                multiple = TRUE),
    
    hr(),
    
    # Table-specific filter
    title = "Table Filters",
    sliderInput("age_range", "Age Range (Table Only)",
                min = min_age, max = max_age, 
                value = c(min_age, max_age), step = 1)
  ),
  
  # Main body layout
  # col_widths = 12 forces each card to take the full width, making them large and central
  layout_columns(
    col_widths = 12,
    
    card(
      full_screen = TRUE,
      card_header("Donor Demographic Pyramid"),
      plotOutput("pyramid_plot", height = "500px")
    ),
    
    card(
      full_screen = TRUE,
      card_header("Donor Health & Center Landscape"),
      plotlyOutput("donor_scatter", height = "600px")
    ),
    
    card(
      full_screen = TRUE,
      card_header("Filtered Donor Database"),
      DTOutput("donor_table")
    )
  )
)

# Server Logic ------------------------------------------------------------
server <- function(input, output, session) {
  
  # 1. Base Reactive Data (Plots Only)
  # Filters by Blood Group and City
  base_data <- reactive({
    data <- blood_clean
    
    # If the user clears the box, treat it as "All" to prevent the app from crashing/emptying
    if (!is.null(input$blood_group)) {
      data <- data %>% filter(blood_group %in% input$blood_group)
    }
    if (!is.null(input$city)) {
      data <- data %>% filter(city %in% input$city)
    }
    
    data
  })
  
  # 2. Table Reactive Data
  # Inherits the base filters and ADDS the age filter
  table_data <- reactive({
    base_data() %>%
      filter(age >= input$age_range[1] & age <= input$age_range[2])
  })
  
  # 3. Demographic Pyramid Plot
  output$pyramid_plot <- renderPlot({
    req(nrow(base_data()) > 0)
    
    pyramid_df <- base_data() %>%
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
      theme_minimal(base_size = 14) + # Increased base font size for larger plot
      labs(x = "Age Group", y = "Number of Donors (Solid = Eligible, Faded = Total)")
  })
  
  # 4. Interactive Scatter Plot 
  output$donor_scatter <- renderPlotly({
    req(nrow(base_data()) > 0)
    
    p <- ggplot(base_data(), aes(
      x = age, 
      y = hemoglobin_g_d_l, 
      color = donation_center,
      size = total_donations,
      text = paste0(
        "<b>Name:</b> ", full_name, "<br>",
        "<b>ID:</b> ", donor_id, "<br>",
        "<b>City:</b> ", city, "<br>",
        "<b>Center:</b> ", donation_center, "<br>",
        "<b>Eligibility:</b> ", ifelse(eligible_for_donation, "Eligible", "Not Eligible"), "<br>",
        "<b>Total Donations:</b> ", total_donations
      )
    )) +
      geom_jitter(alpha = 0.7, width = 0.5, height = 0.2) +
      scale_size_continuous(range = c(3, 10)) + 
      theme_minimal(base_size = 14) +
      labs(
        x = "Age",
        y = "Hemoglobin (g/dL)",
        color = "Donation Center"
      ) +
      theme(legend.position = "right")
    
    ggplotly(p, tooltip = "text") %>%
      layout(hoverlabel = list(bgcolor = "white", font = list(family = "Arial")))
  })
  
  # 5. Data Table
  output$donor_table <- renderDT({
    datatable(
      table_data() %>% # Note: This uses the age-filtered reactive dataset
        select(donor_id, full_name, age, gender, blood_group, city, eligible_for_donation, donation_center),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE,
      class = "display nowrap"
    )
  })
}

# Run App -----------------------------------------------------------------
shinyApp(ui, server)



