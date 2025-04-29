library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)
library(stringr)

bourbon_data <- read_csv("BourbonData.csv")
bourbon_data$Flavor_Profile <- tolower(bourbon_data$Flavor_Profile)

# Rename column first
bourbon_data <- bourbon_data %>%
  rename(Release_Year = Year_Made)

# Clean up Release_Year values
bourbon_data <- bourbon_data %>%
  mutate(
    Release_Year = case_when(
      is.na(Release_Year) ~ "Made Every Year",
      str_trim(as.character(Release_Year)) == "" ~ "Made Every Year",
      str_detect(tolower(as.character(Release_Year)), "every year|non-vintage|n/a|na") ~ "Made Every Year",
      TRUE ~ as.character(as.integer(Release_Year))
    )
  )



flavor_groups <- list(
  Sweet = c("vanilla", "caramel", "honey", "maple", "sugar", "brown sugar", "toffee", "fudge", "candy", "cupcake"),
  Spicy = c("spice", "cinnamon", "pepper", "clove", "ginger", "nutmeg", "heat", "zest", "jalapeno"),
  Fruity = c("fruit", "cherry", "apple", "berry", "orange", "citrus", "peach", "raisin", "grape", "lemon", "banana", "fig"),
  Nutty = c("nut", "almond", "pecan", "hazelnut", "walnut", "cashew", "macadamia"),
  Woody = c("oak", "wood", "smoke", "char", "cedar", "leather", "earth", "tobacco", "ash"),
  Rich = c("chocolate", "molasses", "butter", "cream", "espresso", "coffee", "syrup", "cocoa", "fudge")
)
flavor_terms_unique <- sort(unique(unlist(flavor_groups)))

bourbon_data <- bourbon_data %>%
  mutate(
    Corn = str_extract(Mash_Bill, "\\d+(?=% corn)") %>% as.numeric() %>% replace_na(0),
    Rye = str_extract(Mash_Bill, "\\d+(?=% rye)") %>% as.numeric() %>% replace_na(0),
    Barley = str_extract(Mash_Bill, "\\d+(?=% malted barley|% barley)") %>% as.numeric() %>% replace_na(0),
    Wheat = str_extract(Mash_Bill, "\\d+(?=% wheat)") %>% as.numeric() %>% replace_na(0),
    Sorghum = str_extract(Mash_Bill, "\\d+(?=% sorghum)") %>% as.numeric() %>% replace_na(0),
    Millet = str_extract(Mash_Bill, "\\d+(?=% millet)") %>% as.numeric() %>% replace_na(0),
    Oats = str_extract(Mash_Bill, "\\d+(?=% oats)") %>% as.numeric() %>% replace_na(0),
    Other_Grain = pmax(0, 100 - Corn - Rye - Barley - Wheat - Sorghum - Millet - Oats),
    Mash_Type = case_when(
      Wheat > 15 ~ "Wheated",
      Rye >= 20 ~ "High Rye",
      Corn >= 80 ~ "Traditional Corn",
      Millet > 0 ~ "Experimental",
      TRUE ~ "Other"
    )
  )

mash_options <- c("Corn", "Rye", "Barley", "Wheat", "Sorghum", "Millet", "Oats")

ui <- fluidPage(
  theme = shinytheme("cosmo"),
  titlePanel("Bourbon Explorer"),
  sidebarLayout(
    sidebarPanel(
      radioButtons("price_mode", "Price Selection Mode:",
                   choices = c("Under $250" = "under", "$250+" = "over"),
                   selected = "under"),
      conditionalPanel(
        condition = "input.price_mode == 'under'",
        sliderInput("price_range", "Price Range ($):",
                    min = 12, max = 250, value = c(12, 250), step = 5)
      ),
      sliderInput("rating_range", "Rating Range:",
                  min = min(bourbon_data$Rating, na.rm = TRUE),
                  max = max(bourbon_data$Rating, na.rm = TRUE),
                  value = c(min(bourbon_data$Rating, na.rm = TRUE),
                            max(bourbon_data$Rating, na.rm = TRUE))),
      sliderInput("abv_range", "ABV Range (%):",
                  min = min(bourbon_data$Abv, na.rm = TRUE),
                  max = max(bourbon_data$Abv, na.rm = TRUE),
                  value = c(min(bourbon_data$Abv, na.rm = TRUE),
                            max(bourbon_data$Abv, na.rm = TRUE))),
      sliderInput("age_range", "Aging Period (Years):",
                  min = 0, max = 50, value = c(0, 50)),
      selectInput("release_year", "Filter by Release Year:",
                  choices = c("All", sort(unique(bourbon_data$Release_Year))), selected = "All"),
      selectInput("distillery", "Filter by Distillery:",
                  choices = c("All", sort(unique(bourbon_data$Distillery))), selected = "All"),
      checkboxGroupInput("flavor_terms", "Select Individual Flavors:", choices = flavor_terms_unique),
      checkboxGroupInput("mash_grains", "Select Mash Grains:", choices = mash_options),
      numericInput("top_n", "Top N Bourbons to Display:", 10, min = 1, max = 50)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Top Bourbons", plotOutput("topPlot")),
        tabPanel("Data Table", div(style = "overflow-x: auto;", tableOutput("filteredTable")))
      )
    )
  )
)

server <- function(input, output) {
  filtered_data <- reactive({
    df <- bourbon_data %>%
      filter(
        Rating >= input$rating_range[1],
        Rating <= input$rating_range[2],
        Abv >= input$abv_range[1],
        Abv <= input$abv_range[2],
        `Aging Period` >= input$age_range[1],
        `Aging Period` <= input$age_range[2]
      )

    if (input$price_mode == "under") {
      df <- df %>% filter(Price >= input$price_range[1], Price <= input$price_range[2])
    } else {
      df <- df %>% filter(Price > 250)
    }

    if (input$release_year != "All") {
      df <- df %>% filter(Release_Year == input$release_year)
    }



    if (input$distillery != "All") {
      df <- df %>% filter(Distillery == input$distillery)
    }

    if (length(input$flavor_terms) > 0) {
      df <- df %>% filter(
        sapply(strsplit(tolower(Flavor_Profile), ","), function(words) {
          clean <- trimws(words)
          any(clean %in% input$flavor_terms)
        })
      )
    }

    if (length(input$mash_grains) > 0) {
      df <- df %>% filter(
        rowSums(across(all_of(input$mash_grains), ~ replace_na(.x, 0) > 0)) > 0
      )
    }

    df %>%
      mutate(Proof = Abv * 2,
             Score = (Rating * 0.6) + (Proof * 0.2) - (Price * 0.2)) %>%
      arrange(desc(Score))
  })

  output$topPlot <- renderPlot({
    filtered_data() %>%
      slice_head(n = input$top_n) %>%
      ggplot(aes(x = reorder(Name, Score), y = Score)) +
      geom_col(fill = "darkred") +
      coord_flip() +
      labs(title = paste("Top", input$top_n, "Bourbons by Score"), x = "Bourbon", y = "Score") +
      theme_minimal()
  })

  output$filteredTable <- renderTable({
    filtered_data()
  })
}

shinyApp(ui = ui, server = server)

