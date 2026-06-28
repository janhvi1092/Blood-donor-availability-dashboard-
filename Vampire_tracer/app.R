install.packages("pacman")
pacman::p_load(shiny,
               bslib,
               tidyverse,
               DT,
               janitor)


# Load data
blood_raw <- read.csv("blood_donation.csv")

# Clean and transform the dataset
blood_clean <- blood_raw %>% 
  janitor::clean_names() %>%
  mutate(
    last_donation_date = as.Date(last_donation_date),
    registration_date = as.Date(registration_date),
    
    # Calculate exact duration
    days_since_last_donation = as.numeric(difftime(Sys.Date(), last_donation_date, units = "days")),
    
    # Create generalized categories for duration
    duration_category = case_when(
      is.na(days_since_last_donation) ~ "Unknown / Never Donated",
      days_since_last_donation <= 90 ~ "0-90 Days",
      days_since_last_donation <= 180 ~ "91-180 Days",
      days_since_last_donation <= 365 ~ "181-365 Days",
      TRUE ~ "> 365 Days"
    ),
    
    age_group = cut(
      age, 
      breaks = c(-Inf, 19, seq(24, 64, by = 5), Inf), 
      labels = c("<20", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65+"),
      right = TRUE
    ),
    
    eligible_for_donation = case_when(
      age > 18 & age < 60 &
        (is.na(medical_condition) | medical_condition %in% c("", "None")) &
        (is.na(last_donation_date) | last_donation_date <= Sys.Date() - days(90)) & 
        weight_kg > 50 &
        ((gender == "Male" & hemoglobin_g_d_l > 14) | (gender == "Female" & hemoglobin_g_d_l > 12)) ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
  # Rename column as requested
  rename(last_donation_center = donation_center)

# Extract unique choices for UI
blood_groups <- unique(blood_clean$blood_group[!is.na(blood_clean$blood_group)])
cities <- unique(blood_clean$city[!is.na(blood_clean$city)])
duration_cats <- c("0-90 Days", "91-180 Days", "181-365 Days", "> 365 Days", "Unknown / Never Donated")
age_range <- range(blood_clean$age, na.rm = TRUE)


ui <- page_sidebar(
  title = "Blood Donation Service Monitoring",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  sidebar = sidebar(
    title = "Global Filters",
    
    # Checkbox groups for easy, single-click multiple selections
    checkboxGroupInput("blood_group", "Blood Group", 
                       choices = blood_groups, 
                       selected = blood_groups),
    
    checkboxGroupInput("city", "City", 
                       choices = cities, 
                       selected = cities),
    
    checkboxGroupInput("duration_cat", "Time Since Last Donation",
                       choices = duration_cats,
                       selected = duration_cats),
    
    # Age Slider remains the same
    sliderInput("age", "Age Range", 
                min = age_range[1], max = age_range[2], value = age_range)
  ),
  
  layout_columns(
    col_widths = c(6, 6, 12),
    card(
      card_header("Donor Demographic Pyramid"),
      plotOutput("pyramid_plot")
    ),
    card(
      card_header(
        div(class = "d-flex justify-content-between align-items-center",
            "Center Eligibility Density (Ring)",
            selectInput("donut_center", NULL, choices = c("All Centers" = "All", unique(blood_clean$last_donation_center)), width = "150px")
        )
      ),
      plotOutput("donut_chart")
    ),
    card(
      card_header("Filtered Donor Database"),
      DTOutput("donor_table")
    )
  )
)

server <- function(input, output, session) {
  
  # 1. Cleaned up Reactive Filter Logic
  filtered_data <- reactive({
    # We require inputs to be present before filtering to avoid transient errors during app load
    # If a user unchecks ALL boxes in a category, the charts will temporarily clear out safely
    req(input$blood_group, input$city, input$duration_cat, input$age)
    
    blood_clean %>%
      filter(
        blood_group %in% input$blood_group,
        city %in% input$city,
        duration_category %in% input$duration_cat,
        age >= input$age[1] & age <= input$age[2]
      )
  })
  
  # 2. Demographic Pyramid Plot 
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
  
  # 3. Eligible vs Ineligible Ring Display
  output$donut_chart <- renderPlot({
    d <- filtered_data() 
    
    if (input$donut_center != "All") {
      d <- d %>% filter(last_donation_center == input$donut_center)
    }
    
    req(nrow(d) > 0)
    
    donut_data <- d %>%
      count(eligible_for_donation) %>%
      mutate(
        status = ifelse(eligible_for_donation, "Eligible", "Ineligible"),
        fraction = n / sum(n),
        ymax = cumsum(fraction),
        ymin = c(0, head(ymax, n = -1)),
        label_pos = (ymax + ymin) / 2
      )
    
    ggplot(donut_data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = status)) +
      geom_rect(color = "white", linewidth = 1) +
      geom_text(aes(x = 3.5, y = label_pos, label = paste0(round(fraction * 100, 1), "%")), 
                color = "white", size = 5, fontface = "bold") +
      coord_polar(theta = "y") +
      xlim(c(2, 4)) +
      theme_void() +
      scale_fill_manual(values = c("Eligible" = "#27ae60", "Ineligible" = "#95a5a6")) +
      labs(fill = "Donation Status") +
      annotate("text", x = 2, y = 0, label = paste("Total Donors:\n", sum(donut_data$n)), 
               size = 5, fontface = "bold", color = "#2c3e50")
  })
  
  # 4. Data Table
  output$donor_table <- renderDT({
    datatable(
      filtered_data() %>% 
        select(donor_id, full_name, age, gender, blood_group, city, 
               duration_category, eligible_for_donation, last_donation_center),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE,
      class = "display nowrap"
    )
  })
}

shinyApp(ui, server)