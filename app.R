# app.R ---- CM2 Portfolio Coursework (Q1 + Q2a/Q2b)

library(shiny)
library(DT)
library(quadprog)

library(dplyr)
library(tidyr)
library(purrr)
library(tibble)

library(ggplot2)
library(scales)
library(moments)

ANNUAL <- 252
TICKERS <- c("AIG", "GS", "JNJ", "MSFT", "NEE")

# ---------------- Helpers ----------------

calc_returns <- function(prices){
  prices %>%
    arrange(Date) %>%
    mutate(across(all_of(TICKERS), ~ .x / dplyr::lag(.x) - 1)) %>%
    filter(!is.na(.data[[TICKERS[1]]]))
}

annualised_stats <- function(returns){
  mat <- as.matrix(returns[, TICKERS, drop = FALSE])
  
  mu   <- colMeans(mat, na.rm = TRUE) * ANNUAL
  covm <- stats::cov(mat, use = "pairwise.complete.obs") * ANNUAL
  
  list(
    mu   = mu,
    cov  = covm,
    sd   = sqrt(diag(covm)),
    var  = diag(covm),
    skew = apply(mat, 2, moments::skewness, na.rm = TRUE),
    kurt = apply(mat, 2, moments::kurtosis, na.rm = TRUE) - 3
  )
}

solve_frontier <- function(mu, covm, n_points = 250){
  n <- length(mu)
  
  Dmat <- covm
  dvec <- rep(0, n)
  
  # Constraints: sum(w)=1 , mu'w = target , w >= 0
  Amat <- cbind(rep(1, n), mu, diag(n))
  meq  <- 2
  
  targets <- seq(min(mu), max(mu), length.out = n_points)
  
  res <- purrr::map_dfr(targets, function(tar){
    sol <- tryCatch(
      quadprog::solve.QP(Dmat, dvec, Amat, c(1, tar, rep(0, n)), meq = meq),
      error = function(e) NULL
    )
    if (is.null(sol)) return(NULL)
    
    w <- sol$solution
    w[w < 1e-10] <- 0
    if (sum(w) == 0) return(NULL)
    w <- w / sum(w)
    
    tibble(
      ret = as.numeric(sum(w * mu)),
      risk = as.numeric(sqrt(t(w) %*% covm %*% w)),
      weights = list(w)
    )
  })
  
  validate <- function(ok, msg) if (!ok) stop(msg)
  validate(nrow(res) > 0, "No feasible frontier points found. Check data.")
  
  res %>%
    distinct(round(risk, 8), round(ret, 8), .keep_all = TRUE)
}

efficient_envelope <- function(frontier_df){
  gmv_idx <- which.min(frontier_df$risk)
  gmv_ret <- frontier_df$ret[gmv_idx]
  
  # keep only efficient side
  eff <- frontier_df %>%
    filter(ret >= gmv_ret) %>%
    arrange(ret, risk)
  
  # bin by return to keep minimum-risk point in each bin (smooth curve)
  eff %>%
    mutate(ret_bin = round(ret, 4)) %>%
    group_by(ret_bin) %>%
    slice_min(order_by = risk, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(-ret_bin) %>%
    arrange(risk)
}

pick_nearest <- function(frontier_df, target_ret){
  frontier_df[which.min(abs(frontier_df$ret - target_ret)), , drop = FALSE]
}

# ---------------- UI ----------------

ui <- fluidPage(
  titlePanel("CM2 Portfolio Coursework (Q1 + Q2a/Q2b)"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("datafile", "Upload RDS file", accept = c(".rds")),
      helpText("If no file is uploaded, the app loads: cache_AIG_GS_JNJ_MSFT_NEE.rds (must be in the same folder as app.R)."),
      
      tags$hr(),
      
      sliderInput("frontier_points", "Number of frontier points",
                  min = 150, max = 1000, value = 300, step = 50),
      
      uiOutput("target_slider_ui"),
      
      checkboxInput("show_assets", "Show individual assets on frontier", TRUE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Q1: Data & Plots",
                 h4("Adjusted Close Prices (preview)"),
                 DTOutput("price_table"),
                 tags$hr(),
                 plotOutput("price_plot", height = 320),
                 plotOutput("rebase_plot", height = 320),
                 tags$hr(),
                 plotOutput("returns_plot", height = 420)
        ),
        
        tabPanel("Q2(a): Statistics",
                 h4("Annualised Mean-Variance Inputs (and daily higher moments)"),
                 DTOutput("stats_table"),
                 tags$hr(),
                 h4("Annualised Covariance Matrix"),
                 DTOutput("cov_table"),
                 tags$hr(),
                 h4("Annualised Correlation Matrix"),
                 DTOutput("corr_table")
        ),
        
        tabPanel("Q2(b): Efficient Frontier",
                 plotOutput("frontier_plot", height = 450),
                 tags$hr(),
                 h4("Selected Portfolio (nearest feasible to target return)"),
                 DTOutput("selected_table"),
                 tags$hr(),
                 h4("GMV Portfolio (Global Minimum Variance)"),
                 DTOutput("gmv_table")
        )
      )
    )
  )
)

# ---------------- Server ----------------

server <- function(input, output, session){
  
  prices <- reactive({
    if (!is.null(input$datafile$datapath)) {
      df <- readRDS(input$datafile$datapath)
    } else {
      validate(
        need(file.exists("cache_AIG_GS_JNJ_MSFT_NEE.rds"),
             "Default RDS not found. Put 'cache_AIG_GS_JNJ_MSFT_NEE.rds' in the same folder as app.R or upload it.")
      )
      df <- readRDS("cache_AIG_GS_JNJ_MSFT_NEE.rds")
    }
    
    validate(need("Date" %in% names(df), "Data must contain a Date column."))
    missing <- setdiff(TICKERS, names(df))
    validate(need(length(missing) == 0, paste("Missing columns:", paste(missing, collapse = ", "))))
    
    df %>%
      mutate(Date = as.Date(Date)) %>%
      arrange(Date) %>%
      select(Date, all_of(TICKERS))
  })
  
  returns <- reactive(calc_returns(prices()))
  st <- reactive(annualised_stats(returns()))
  
  frontier_all <- reactive({
    solve_frontier(st()$mu, st()$cov, n_points = input$frontier_points)
  })
  
  frontier_eff <- reactive({
    efficient_envelope(frontier_all())
  })
  
  output$target_slider_ui <- renderUI({
    fe <- frontier_eff()
    rng <- range(fe$ret, na.rm = TRUE)
    sliderInput(
      "target_ret",
      "Target annualised return (E)",
      min = rng[1], max = rng[2],
      value = rng[1] + 0.6 * diff(rng),
      step = diff(rng) / 300
    )
  })
  
  # -------- Q1 --------
  
  output$price_table <- renderDT({
    datatable(
      tail(prices(), 80),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })
  
  output$price_plot <- renderPlot({
    prices() %>%
      pivot_longer(-Date, names_to = "Stock", values_to = "Price") %>%
      ggplot(aes(Date, Price, colour = Stock)) +
      geom_line(linewidth = 0.8) +
      theme_minimal(base_size = 14) +
      theme(panel.grid.minor = element_blank()) +
      labs(title = "Adjusted Close Prices", x = "", y = "Price")
  })
  
  output$rebase_plot <- renderPlot({
    df <- prices()
    base <- df[1, TICKERS, drop = FALSE]
    
    df2 <- df %>%
      mutate(across(all_of(TICKERS), ~ .x / as.numeric(base[[cur_column()]]) * 100))
    
    df2 %>%
      pivot_longer(-Date, names_to = "Stock", values_to = "Index") %>%
      ggplot(aes(Date, Index, colour = Stock)) +
      geom_line(linewidth = 0.8) +
      theme_minimal(base_size = 14) +
      theme(panel.grid.minor = element_blank()) +
      labs(title = "Rebased Prices (first date = 100)", x = "", y = "Index")
  })
  
  output$returns_plot <- renderPlot({
    returns() %>%
      pivot_longer(-Date, names_to = "Stock", values_to = "Return") %>%
      ggplot(aes(Date, Return * 100, colour = Stock)) +
      geom_line(linewidth = 0.55) +
      facet_wrap(~Stock, ncol = 1, scales = "free_y") +
      theme_minimal(base_size = 13) +
      theme(
        legend.position = "right",
        panel.grid.minor = element_blank()
      ) +
      labs(
        title = "Daily Returns (%, faceted by stock)",
        x = "Date",
        y = "Return (%)"
      )
  })
  
  # -------- Q2(a) --------
  
  output$stats_table <- renderDT({
    s <- st()
    tibble(
      Stock = TICKERS,
      Mean_Annualised = percent(as.numeric(s$mu), accuracy = 0.01),
      Variance_Annualised = as.numeric(s$var),
      SD_Annualised = percent(as.numeric(s$sd), accuracy = 0.01),
      Skewness_Daily = as.numeric(s$skew),
      ExcessKurtosis_Daily = as.numeric(s$kurt)
    ) %>%
      datatable(options = list(dom = "t"), rownames = FALSE)
  })
  
  output$cov_table <- renderDT({
    s <- st()
    as.data.frame(round(s$cov, 6)) %>%
      rownames_to_column(var = "Stock") %>%
      datatable(options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  output$corr_table <- renderDT({
    s <- st()
    corrm <- cov2cor(s$cov)
    as.data.frame(round(corrm, 4)) %>%
      rownames_to_column(var = "Stock") %>%
      datatable(options = list(scrollX = TRUE, pageLength = 10), rownames = FALSE)
  })
  
  # -------- Q2(b) --------
  
  output$frontier_plot <- renderPlot({
    req(input$target_ret)
    
    fa <- frontier_all()
    fe <- frontier_eff()
    s <- st()
    
    gmv <- fa[which.min(fa$risk), , drop = FALSE]
    sel <- pick_nearest(fe, input$target_ret)
    
    asset_df <- tibble(
      Stock = TICKERS,
      risk  = as.numeric(s$sd),
      ret   = as.numeric(s$mu)
    )
    
    # "PDF-style" look: smooth red frontier line, clean axes, asset labels
    ggplot() +
      geom_path(data = fe, aes(x = risk, y = ret), linewidth = 1.2, colour = "red") +
      geom_point(data = fe, aes(x = risk, y = ret), size = 1.2, alpha = 0.35, colour = "red") +
      geom_point(data = gmv, aes(x = risk, y = ret), size = 4, colour = "black") +
      geom_point(data = sel, aes(x = risk, y = ret), size = 4, colour = "blue") +
      {if (isTRUE(input$show_assets))
        list(
          geom_point(data = asset_df, aes(x = risk, y = ret), size = 3, colour = "black"),
          geom_text(data = asset_df, aes(x = risk, y = ret, label = Stock),
                    nudge_x = 0.002, size = 4, colour = "black")
        )
      } +
      scale_x_continuous(labels = percent_format(accuracy = 1)) +
      scale_y_continuous(labels = percent_format(accuracy = 1)) +
      labs(
        title = "Efficient Frontier (No short selling)",
        x = "Mean-Var target Risk (annualised SD)",
        y = "Expected Return (annualised)"
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(face = "bold"),
        panel.grid.minor = element_blank()
      )
  })
  
  output$selected_table <- renderDT({
    req(input$target_ret)
    fe <- frontier_eff()
    sel <- pick_nearest(fe, input$target_ret)
    
    w <- unlist(sel$weights[[1]])
    names(w) <- TICKERS
    
    overview_df <- tibble(
      Metric = c("Target return (chosen)", "Return (selected)", "Risk (SD)"),
      Value  = c(
        percent(input$target_ret, accuracy = 0.01),
        percent(sel$ret, accuracy = 0.01),
        percent(sel$risk, accuracy = 0.01)
      )
    )
    
    weights_df <- tibble(
      Stock = TICKERS,
      Weight = percent(as.numeric(w), accuracy = 0.01)
    )
    
    out <- bind_rows(
      overview_df %>% mutate(Section = "Overview") %>% rename(Col1 = Metric, Col2 = Value) %>% select(Section, Col1, Col2),
      weights_df  %>% mutate(Section = "Weights")  %>% rename(Col1 = Stock,  Col2 = Weight) %>% select(Section, Col1, Col2)
    )
    
    datatable(out, options = list(dom = "t", pageLength = 50), rownames = FALSE)
  })
  
  output$gmv_table <- renderDT({
    fa <- frontier_all()
    gmv <- fa[which.min(fa$risk), , drop = FALSE]
    w <- unlist(gmv$weights[[1]])
    names(w) <- TICKERS
    
    summary_df <- tibble(
      Metric = c("Return (annualised)", "Risk (SD, annualised)"),
      Value  = c(
        percent(gmv$ret, accuracy = 0.01),
        percent(gmv$risk, accuracy = 0.01)
      )
    )
    
    weights_df <- tibble(
      Stock = TICKERS,
      Weight = percent(as.numeric(w), accuracy = 0.01)
    )
    
    out <- bind_rows(
      summary_df %>% mutate(Section = "Summary") %>% rename(Col1 = Metric, Col2 = Value) %>% select(Section, Col1, Col2),
      weights_df %>% mutate(Section = "Weights") %>% rename(Col1 = Stock, Col2 = Weight) %>% select(Section, Col1, Col2)
    )
    
    datatable(out, options = list(dom = "t", pageLength = 50), rownames = FALSE)
  })
  
}

shinyApp(ui, server)