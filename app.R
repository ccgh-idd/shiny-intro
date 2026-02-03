library(shiny)
library(tidyverse)

linelist <- readRDS("data/clean/moissala_data.rds")

ui <- fluidPage(
  titlePanel("Moissala measles Outbreak"),

  sidebarLayout(
    sidebarPanel(
      dateRangeInput(
        "date_range",
        "Select Date Range:",
        start = min(linelist$date_onset, na.rm = TRUE),
        end = max(linelist$date_onset, na.rm = TRUE),
        min = min(linelist$date_onset, na.rm = TRUE),
        max = max(linelist$date_onset, na.rm = TRUE)
      ),

      selectInput(
        inputId = "time_unit",
        label = "Time Unit:",
        choices = c("Day", "Week", "Month", "Year"),
        selected = "Day"
      )
    ),

    mainPanel(
      plotOutput("epicurve"),

      tabsetPanel(
        tabPanel("Age Pyramid", plotOutput("age_pyramid")),
        tabPanel("Summary Statistics", tableOutput("summary_table"))
      )
    )
  )
)

server <- function(input, output, session) {
  # here data are filtered
  filtered_data <- reactive({
    linelist |>
      filter(
        date_onset >= input$date_range[1],
        date_onset <= input$date_range[2]
      )
  })

  # count the data
  plot_df <- reactive({
    filtered_data() |>
      mutate(
        agg_date = lubridate::floor_date(
          date_onset,
          unit = tolower(input$time_unit)
        )
      ) |>
      count(agg_date)
  })

  # plot the epicurve
  output$epicurve <- renderPlot({
    n_valid <- nrow(filtered_data() |> filter(!is.na(date_onset)))

    plot_df() |>
      ggplot(aes(x = agg_date, y = n)) +
      geom_col(fill = "#E74C3C", color = "white", linewidth = 0.3) +
      labs(
        title = paste("Cases by", input$time_unit),
        subtitle = paste(
          "Date range:",
          format(input$date_range[1], "%b %d, %Y"),
          "to",
          format(input$date_range[2], "%b %d, %Y")
        ),
        x = paste("Date of Onset (", input$time_unit, ")", sep = ""),
        y = "Number of Cases (n)",
        caption = paste(
          "Displaying",
          n_valid,
          "cases with valid dates"
        )
      ) +
      theme_minimal()
  })

  # data for the age pyramid
  pyramid_df <- reactive({
    filtered_data() |>
      filter(!is.na(age_group) & !is.na(sex)) |>
      mutate(sex = case_match(sex, "m" ~ "male", "f" ~ "female"))
  })

  output$age_pyramid <- renderPlot({
    apyramid::age_pyramid(
      data = pyramid_df(),
      age_group = "age_group",
      split_by = "sex"
    )
  })

  output$summary_table <- renderTable({
    filtered_data() |>
      summarise(
        .by = age_group,
        `Total Cases` = n(),
        `Male (n)` = sum(sex == "m", na.rm = TRUE),
        `Male (%)` = round(sum(sex == "m", na.rm = TRUE) / n() * 100, 1),
        `Female (n)` = sum(sex == "f", na.rm = TRUE),
        `Female (%)` = round(sum(sex == "f", na.rm = TRUE) / n() * 100, 1),
        `CFR (%)` = round(
          digits = 2,
          sum(outcome == "dead", na.rm = TRUE) /
            sum(outcome %in% c("dead", "recovered"), na.rm = TRUE) *
            100
        )
      )
  })
}

shinyApp(ui, server)
?daterangeinput
