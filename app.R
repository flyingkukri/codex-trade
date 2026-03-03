library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(readr)
library(countrycode)

load_vdem_data <- function() {
  if (requireNamespace("vdemdata", quietly = TRUE)) {
    vdem <- vdemdata::vdem

    out <- vdem %>%
      transmute(
        country_name = country_name,
        iso3 = country_text_id,
        year = as.integer(year),
        democracy_index = as.numeric(v2x_polyarchy)
      ) %>%
      filter(!is.na(iso3), !is.na(year), !is.na(democracy_index)) %>%
      group_by(iso3, country_name, year) %>%
      summarize(democracy_index = mean(democracy_index), .groups = "drop")

    return(out)
  }

  message("Package 'vdemdata' not found. Using bundled demo democracy data.")

  years <- 2000:2023
  countries <- c("DEU", "FRA", "USA", "CHN", "POL", "ITA", "ESP", "NLD", "SWE", "JPN")

  expand.grid(iso3 = countries, year = years, stringsAsFactors = FALSE) %>%
    mutate(
      country_name = countrycode(iso3, "iso3c", "country.name"),
      democracy_index = pmin(
        1,
        pmax(
          0,
          case_when(
            iso3 == "DEU" ~ 0.85 + sin(year / 5) * 0.03,
            iso3 %in% c("FRA", "SWE", "NLD") ~ 0.82 + sin(year / 6) * 0.02,
            iso3 %in% c("USA", "JPN", "ITA", "ESP", "POL") ~ 0.74 + sin(year / 7) * 0.04,
            iso3 == "CHN" ~ 0.2 + sin(year / 8) * 0.02,
            TRUE ~ 0.6
          )
        )
      )
    )
}

generate_demo_baci <- function(years, importers) {
  set.seed(42)
  product_sections <- c("Machinery", "Chemicals", "Transport", "Food", "Textiles")

  expand.grid(
    exporter_iso3 = "DEU",
    importer_iso3 = importers,
    year = years,
    product_section = product_sections,
    stringsAsFactors = FALSE
  ) %>%
    mutate(
      trade_value = round(runif(n(), min = 50, max = 3000), 2)
    )
}

ui <- fluidPage(
  titlePanel("Germany Trade Security Dashboard"),
  sidebarLayout(
    sidebarPanel(
      helpText("Compare Germany with selected countries and compute a trade security index."),
      fileInput(
        "baci_file",
        "Upload CEPII BACI trade file (CSV)",
        accept = c(".csv", "text/csv", "text/comma-separated-values")
      ),
      tags$small(
        "Required columns: exporter_iso3, importer_iso3, year, product_section, trade_value"
      ),
      hr(),
      uiOutput("country_selector_ui"),
      uiOutput("year_selector_ui"),
      uiOutput("product_selector_ui")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Democracy comparison", plotlyOutput("democracy_line", height = "420px")),
        tabPanel("Trade security map", plotlyOutput("risk_map", height = "520px")),
        tabPanel("Computed index table", tableOutput("risk_table"))
      )
    )
  )
)

server <- function(input, output, session) {
  democracy_data <- reactive({
    load_vdem_data()
  })

  trade_data <- reactive({
    if (is.null(input$baci_file)) {
      demo_importers <- c("FRA", "USA", "CHN", "POL", "ITA", "ESP", "NLD", "SWE", "JPN")
      years <- 2008:2023
      return(generate_demo_baci(years, demo_importers))
    }

    read_csv(input$baci_file$datapath, show_col_types = FALSE) %>%
      transmute(
        exporter_iso3 = as.character(exporter_iso3),
        importer_iso3 = as.character(importer_iso3),
        year = as.integer(year),
        product_section = as.character(product_section),
        trade_value = as.numeric(trade_value)
      )
  })

  germany_trade <- reactive({
    trade_data() %>%
      filter(exporter_iso3 == "DEU", !is.na(importer_iso3), !is.na(year), !is.na(product_section))
  })

  output$country_selector_ui <- renderUI({
    choices <- germany_trade() %>%
      distinct(importer_iso3) %>%
      arrange(importer_iso3) %>%
      pull(importer_iso3)

    selectizeInput(
      "selected_countries",
      "Countries to compare with Germany",
      choices = choices,
      selected = head(choices, 3),
      multiple = TRUE
    )
  })

  output$year_selector_ui <- renderUI({
    years <- germany_trade() %>% distinct(year) %>% arrange(year) %>% pull(year)

    selectInput(
      "selected_years",
      "Years to include",
      choices = years,
      selected = years,
      multiple = TRUE
    )
  })

  output$product_selector_ui <- renderUI({
    sections <- germany_trade() %>%
      distinct(product_section) %>%
      arrange(product_section) %>%
      pull(product_section)

    selectInput(
      "selected_product",
      "Product section for map/table",
      choices = sections,
      selected = sections[[1]],
      multiple = FALSE
    )
  })

  democracy_comparison <- reactive({
    req(input$selected_countries, input$selected_years)

    selected <- unique(c("DEU", input$selected_countries))

    democracy_data() %>%
      filter(iso3 %in% selected, year %in% as.integer(input$selected_years)) %>%
      mutate(country_name = ifelse(iso3 == "DEU", "Germany", country_name))
  })

  weighted_index <- reactive({
    req(input$selected_years)

    germany_trade() %>%
      filter(year %in% as.integer(input$selected_years)) %>%
      left_join(
        democracy_data() %>% select(iso3, year, democracy_index),
        by = c("importer_iso3" = "iso3", "year" = "year")
      ) %>%
      group_by(year, product_section) %>%
      mutate(trade_share = trade_value / sum(trade_value, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(weighted_component = trade_share * democracy_index)
  })

  country_risk <- reactive({
    weighted_index() %>%
      filter(product_section == input$selected_product) %>%
      group_by(year, exporter_iso3, product_section) %>%
      summarize(country_risk_index = sum(weighted_component, na.rm = TRUE), .groups = "drop")
  })

  output$democracy_line <- renderPlotly({
    dat <- democracy_comparison()

    p <- ggplot(dat, aes(x = year, y = democracy_index, color = country_name, group = iso3)) +
      geom_line(linewidth = 1) +
      geom_point(size = 1.8) +
      labs(
        title = "V-Dem Democracy Index: Germany vs Selected Countries",
        x = "Year",
        y = "Democracy Index"
      ) +
      theme_minimal()

    ggplotly(p)
  })

  output$risk_map <- renderPlotly({
    req(input$selected_product, input$selected_years)

    chosen_year <- max(as.integer(input$selected_years))

    map_data <- weighted_index() %>%
      filter(year == chosen_year, product_section == input$selected_product) %>%
      group_by(importer_iso3) %>%
      summarize(weighted_component = sum(weighted_component, na.rm = TRUE), .groups = "drop") %>%
      mutate(country_name = countrycode(importer_iso3, "iso3c", "country.name"))

    plot_ly(
      data = map_data,
      type = "choropleth",
      locations = ~importer_iso3,
      z = ~weighted_component,
      text = ~paste0(country_name, "<br>Weighted contribution: ", round(weighted_component, 4)),
      colorscale = "Viridis",
      marker = list(line = list(color = "rgb(180,180,180)", width = 0.3))
    ) %>%
      layout(
        title = paste(
          "Trade Security Index Map (Germany exports) -",
          input$selected_product,
          "-",
          chosen_year
        ),
        geo = list(showframe = FALSE, showcoastlines = TRUE)
      )
  })

  output$risk_table <- renderTable({
    country_risk() %>%
      arrange(year) %>%
      mutate(country_risk_index = round(country_risk_index, 4))
  })
}

shinyApp(ui, server)
