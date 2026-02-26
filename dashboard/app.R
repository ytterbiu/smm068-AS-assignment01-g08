# Load necessary libraries
library(shiny)
library(tidyverse)
library(quantmod)
library(bslib)
library(DT)
library(patchwork)
library(plotly)
library(rootSolve)
library(optimx)
library(quadprog)
library(scales)

#------------------------------------------------------------------------------
# GLOBAL DATA SETUP (Runs ONCE when the app starts - High Efficiency)
#------------------------------------------------------------------------------
# Define our 16 U.S. Stocks
stock_choices <- sort(c(
  "MSFT",
  "AAPL",
  "GOOG",
  "NVDA",
  "JPM",
  "GS",
  "V",
  "JNJ",
  "UNH",
  "WMT",
  "PG",
  "XOM",
  "CAT",
  "TRV",
  "NEE",
  "AIG"
))

# Default selection
# default_stocks <- c("MSFT", "JNJ", "XOM", "AIG", "GS")
default_stocks <- c("AIG", "GS", "JNJ", "MSFT", "NEE")

start_date_global <- as.Date("2018-01-01")
end_date_global <- Sys.Date()

# Safely download data into a list to prevent order scrambling and drop failures
price_list <- list()
for (ticker in stock_choices) {
  tryCatch(
    {
      # auto.assign = FALSE returns the object directly instead of using environments
      temp_data <- suppressWarnings(
        getSymbols(
          ticker,
          src = "yahoo",
          from = start_date_global,
          to = end_date_global,
          auto.assign = FALSE
        )
      )
      # Extract just the Adjusted Close
      price_list[[ticker]] <- Ad(temp_data)
    },
    error = function(e) {
      message(paste("Skipping", ticker, "- could not download data."))
    }
  )
}

# Merge all successful downloads into one master xts object
master_prices_xts <- do.call(merge, price_list)

# The column names will currently look like "MSFT.Adjusted". Let's clean that up.
colnames(master_prices_xts) <- gsub(
  "\\.Adjusted",
  "",
  colnames(master_prices_xts)
)

# Convert to a standard dataframe for our ggplot/dplyr pipeline
master_prices <- data.frame(
  Date = index(master_prices_xts),
  coredata(master_prices_xts)
)

# Update stock_choices to ONLY include stocks that successfully downloaded
stock_choices <- intersect(stock_choices, colnames(master_prices))

#------------------------------------------------------------------------------
# UI DEFINITION
#------------------------------------------------------------------------------
ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "united"),
  titlePanel("Portfolio optimisation & efficient frontier dashboard"),

  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Configuration"),

      # Ticker Selection
      selectizeInput(
        "selected_stocks",
        "Select exactly 5 stocks:",
        choices = stock_choices,
        selected = default_stocks,
        multiple = TRUE,
        options = list(maxItems = 5)
      ),
      helpText(
        "Note: To ensure instantaneous loading without API throttling, the universe of available assets is currently limited to 16 pre-loaded US stocks."
      ),

      # Date Range
      dateRangeInput(
        "date_range",
        "Study period:",
        start = "2023-01-01",
        end = "2025-12-31",
        min = start_date_global,
        max = end_date_global
      ),
      helpText(paste(
        "Data are currently available from 2018-01-01 to",
        Sys.Date(),
        " (today)."
      )),

      hr(),

      # Slider for risk tolerance (Only visible/relevant for Tab 3)
      conditionalPanel(
        condition = "input.tabs == '3. Efficient Frontier'",
        h5("Portfolio selection"),
        sliderInput(
          "risk_tolerance",
          "Subjective risk tolerance:",
          min = 0,
          max = 100,
          value = 0,
          step = 10
        ),
        helpText(
          "0 = Global minimum variance (lowest risk). 100 = Maximum expected return (highest risk)."
        )
      )
    ),

    mainPanel(
      tabsetPanel(
        id = "tabs",

        # TAB 1: Performance
        tabPanel(
          "1. Stock Performance",
          br(),
          h4("Rebased performance (start = 100)"),
          plotlyOutput("plot_rebased", height = "350px"),
          br(),
          h4(
            "Performance of selected stocks over time period (adjusted close)"
          ),
          plotlyOutput("plot_absolute", height = "350px"),
        ),

        # TAB 2: Statistics
        tabPanel(
          "2. Statistics",
          br(),
          h4("Annualised return & volatility under MPT"),
          tableOutput("table_summary"),
          br(),
          h4("Covariance matrix (annualised)"),
          tableOutput("table_cov")
        ),

        # TAB 3: Efficient Frontier
        tabPanel(
          "3. Efficient Frontier",
          br(),
          h4("Mean-Variance efficient frontier"),
          plotOutput("plot_frontier", height = "500px"),
          br(),
          fluidRow(
            column(
              width = 8,
              card(
                card_header(
                  "Portfolio weights",
                  class = "bg-primary text-white"
                ),
                tableOutput("table_weights")
              )
            ),
            column(
              width = 4,
              card(
                card_header(
                  "Selected portfolio metrics",
                  class = "bg-primary text-white"
                ),
                tableOutput("table_selected_metrics")
              )
            )
          )
        )
      )
    )
  )
)

#------------------------------------------------------------------------------
# SERVER LOGIC
#------------------------------------------------------------------------------
server <- function(input, output, session) {
  # 1. Reactive Data Filter (Instantaneous slicing of memory)
  filtered_data <- reactive({
    req(length(input$selected_stocks) == 5) # Ensure exactly 5 are selected

    master_prices %>%
      select(Date, all_of(input$selected_stocks)) %>%
      filter(Date >= input$date_range[1] & Date <= input$date_range[2]) %>% #using < to match getsymbols
      drop_na()
  })

  # 2. Reactive Statistics (Returns, Covariance, etc.)
  calc_stats <- reactive({
    prices <- filtered_data()
    req(nrow(prices) > 10) # ensure enough data

    # Calculate discrete daily returns
    df_returns <- prices %>%
      arrange(Date) %>%
      mutate(across(-Date, ~ .x / lag(.x) - 1)) %>%
      drop_na()

    ANN_FACTOR <- 252

    mean_returns_d <- df_returns %>% select(-Date) %>% colMeans()
    mean_returns_a <- mean_returns_d * ANN_FACTOR

    cov_matrix_d <- cov(df_returns %>% select(-Date))
    cov_matrix_a <- cov_matrix_d * ANN_FACTOR

    sd_a <- sqrt(diag(cov_matrix_a))
    cor_matrix_a <- cor(df_returns %>% select(-Date))

    list(
      returns = mean_returns_a,
      cov = cov_matrix_a,
      sd = sd_a,
      cor = cor_matrix_a
    )
  })

  # 3. Reactive Efficient Frontier (Calculates the curve once per stock/date change)
  calc_frontier <- reactive({
    stats <- calc_stats()
    n_sec <- length(stats$returns)

    Dmat <- stats$cov
    # Add a microscopic amount to the diagonal of Dmat to ensure strict positive-definiteness.
    # This stops solve.QP from failing when assets are highly correlated.
    diag(Dmat) <- diag(Dmat) + 1e-8

    dvec <- rep(0, n_sec)
    Amat <- cbind(rep(1, n_sec), stats$returns, diag(n_sec))

    # Back off the target boundaries by a hair to avoid numerical instability
    min_target <- min(stats$returns) + 1e-6
    max_target <- max(stats$returns) - 1e-6

    # Increase point density (from 500 to 2500) to smooth out flat horizontal stretches
    targets <- seq(min_target, max_target, length.out = 500)

    eff_frontier <- map_dfr(targets, function(tar) {
      b0vec <- c(1, tar, rep(0, n_sec))
      sol <- tryCatch(
        solve.QP(Dmat, dvec, Amat, b0vec, meq = 2),
        error = function(e) NULL
      )
      if (is.null(sol)) {
        return(NULL)
      }

      w <- sol$solution
      w[w < 1e-10] <- 0
      w <- w / sum(w)

      tibble(
        Risk = sqrt(as.numeric(t(w) %*% stats$cov %*% w)),
        Return = sum(w * stats$returns),
        Weights = list(w)
      )
    })

    # Guarantee the endpoints are perfectly anchored to the min/max individual assets
    max_idx <- which.max(stats$returns)
    min_idx <- which.min(stats$returns)

    w_max <- rep(0, n_sec)
    w_max[max_idx] <- 1
    w_min <- rep(0, n_sec)
    w_min[min_idx] <- 1

    endpoints <- tibble(
      Risk = c(stats$sd[min_idx], stats$sd[max_idx]),
      Return = c(stats$returns[min_idx], stats$returns[max_idx]),
      Weights = list(w_min, w_max)
    )

    # Combine and sort to ensure a perfectly continuous line
    eff_frontier <- bind_rows(eff_frontier, endpoints) %>%
      arrange(Return) %>%
      distinct(Return, .keep_all = TRUE)

    # Identify Efficient vs Inefficient segments
    gmv_idx <- which.min(eff_frontier$Risk)
    efficient_segment <- eff_frontier[gmv_idx:nrow(eff_frontier), ]
    # Include gmv_idx in BOTH segments so the dashed and solid lines connect flawlessly
    inefficient_segment <- eff_frontier[1:(gmv_idx), ]

    list(
      full = eff_frontier,
      efficient = efficient_segment,
      inefficient = inefficient_segment,
      gmv_point = eff_frontier[gmv_idx, ]
    )
  })

  # --- TAB 1 OUTPUTS ---
  output$plot_absolute <- renderPlotly({
    validate(need(
      length(input$selected_stocks) == 5,
      "Please select exactly 5 stocks."
    ))
    df <- filtered_data()

    df_long <- df %>%
      pivot_longer(-Date, names_to = "Ticker", values_to = "Price")

    p <- ggplot(df_long, aes(x = Date, y = Price, color = Ticker)) +
      geom_line(linewidth = 0.5, alpha = 0.9) +
      scale_color_brewer(palette = "Dark2") +
      theme_bw() +
      labs(x = NULL, y = "Price (USD)")

    ggplotly(p) %>%
      layout(
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2)
      )
  })

  output$plot_rebased <- renderPlotly({
    validate(need(length(input$selected_stocks) == 5, ""))
    df <- filtered_data()

    df_long <- df %>%
      pivot_longer(-Date, names_to = "Ticker", values_to = "Price")
    df_rebased <- df_long %>%
      group_by(Ticker) %>%
      arrange(Date) %>%
      mutate(Rebased = 100 * Price / first(Price)) %>%
      ungroup()

    p <- ggplot(df_rebased, aes(x = Date, y = Rebased, color = Ticker)) +
      geom_line(linewidth = 0.5, alpha = 0.9) +
      scale_color_brewer(palette = "Dark2") +
      theme_bw() +
      labs(x = NULL, y = "Rebased Index (100)")

    ggplotly(p) %>%
      layout(
        legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.2)
      )
  })

  # --- TAB 2 OUTPUTS ---
  output$table_summary <- renderTable(
    {
      stats <- calc_stats()
      tibble(
        Stock = names(stats$returns),
        `Expected Return (Ann.)` = sprintf("%.2f%%", stats$returns * 100),
        `Volatility (Ann.)` = sprintf("%.2f%%", stats$sd * 100)
      )
    },
    align = "lcc"
  )

  output$table_cov <- renderTable(
    {
      stats <- calc_stats()
      df <- as.data.frame(stats$cov)
      df <- mutate(df, across(everything(), ~ sprintf("%.6f", .)))
      cbind(Stock = rownames(df), df)
    },
    align = "lrrrrr"
  )

  # --- TAB 3 OUTPUTS ---
  output$plot_frontier <- renderPlot({
    validate(need(
      length(input$selected_stocks) == 5,
      "Please select exactly 5 stocks."
    ))
    front <- calc_frontier()
    stats <- calc_stats()

    asset_points <- tibble(
      Stock = names(stats$returns),
      Risk = stats$sd,
      Return = stats$returns
    )

    # Map the 0-100 slider to the rows of the efficient segment
    row_idx <- round(scales::rescale(
      input$risk_tolerance,
      to = c(1, nrow(front$efficient)),
      from = c(0, 100)
    ))
    selected_pt <- front$efficient[row_idx, ]

    ggplot() +
      geom_line(
        data = front$efficient,
        aes(x = Risk, y = Return),
        color = "darkgreen",
        linewidth = 1.2
      ) +
      geom_line(
        data = front$inefficient,
        aes(x = Risk, y = Return),
        color = "grey60",
        linetype = "dashed",
        linewidth = 1
      ) +
      geom_point(
        data = asset_points,
        aes(x = Risk, y = Return, color = Stock),
        size = 3
      ) +
      geom_text(
        data = asset_points,
        aes(x = Risk, y = Return, label = Stock, color = Stock),
        hjust = -0.3,
        vjust = 0.5,
        show.legend = FALSE
      ) +
      geom_point(
        data = selected_pt,
        aes(x = Risk, y = Return),
        color = "red",
        size = 5,
        shape = 18
      ) +
      annotate(
        "text",
        x = selected_pt$Risk,
        y = selected_pt$Return + 0.015,
        label = "Selected\nPortfolio",
        color = "red",
        fontface = "bold",
        size = 4
      ) +
      scale_color_brewer(palette = "Dark2") +
      # Added 'expand' to prevent the edges of the plot and labels from getting cut off
      scale_x_continuous(
        labels = scales::percent_format(accuracy = 1),
        expand = expansion(mult = c(0.1, 0.15))
      ) +
      scale_y_continuous(
        labels = scales::percent_format(accuracy = 1),
        expand = expansion(mult = c(0.1, 0.15))
      ) +
      labs(
        x = "Risk (annualised Volatility)",
        y = "Expected Return (annualised)",
        color = "Assets:"
      ) +
      theme_bw() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold")
      )
  })

  # --- TAB 3 TABLES ---
  output$table_weights <- renderTable(
    {
      front <- calc_frontier()
      stats <- calc_stats()

      # Map the 0-100 slider to the rows of the efficient segment
      row_idx <- round(scales::rescale(
        input$risk_tolerance,
        to = c(1, nrow(front$efficient)),
        from = c(0, 100)
      ))
      selected_pt <- front$efficient[row_idx, ]

      weights <- unlist(selected_pt$Weights)

      # Create the transposed table
      tibble(
        Stock = names(stats$returns),
        Weight = sprintf("%.2f%%", weights * 100)
      ) %>%
        pivot_wider(names_from = Stock, values_from = Weight)
    },
    align = "c"
  )

  output$table_selected_metrics <- renderTable(
    {
      front <- calc_frontier()

      # Map the 0-100 slider to the rows of the efficient segment
      row_idx <- round(scales::rescale(
        input$risk_tolerance,
        to = c(1, nrow(front$efficient)),
        from = c(0, 100)
      ))
      selected_pt <- front$efficient[row_idx, ]

      # Create a clean summary table
      tibble(
        Metric = c(
          "Expected return (annualised)",
          "Risk (annualised std dev)"
        ),
        Value = c(
          sprintf("%.2f%%", selected_pt$Return * 100),
          sprintf("%.2f%%", selected_pt$Risk * 100)
        )
      )
    },
    align = "lc"
  )
}

# Run the app
shinyApp(ui = ui, server = server)
