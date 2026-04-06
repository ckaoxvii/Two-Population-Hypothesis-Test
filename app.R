library(shiny)
library(bslib)
library(reactable)
library(tidyverse)
library(ggplot2)
library(htmlwidgets)

ui <- page_sidebar(
  title = "Two-Proportion Hypothesis Test",
  theme = bs_theme(
    primary = "#A90533",
    "navbar-bg" = "#A90533",
    "card-header-bg" = "#A90533",
    "card-header-color" = "white"
  ),
  tags$head(
    tags$style(
      HTML(
        ".card-header {
          background-color: #A90533 !important;
          color: white !important;
          font-weight: bold;
        }"
      )
    )
  ),
  sidebar = sidebar(
    radioButtons(
      "input_type",
      "Input Type",
      choices = c(
        "Sample Proportions" = "prop",
        "Number of Successes" = "count"
      ),
      selected = "prop"
    ),
    withMathJax(),
    uiOutput("sample_input_ui"),
    tags$label(HTML("Significance Level, \\(\\alpha\\):"), `for` = "alpha"),
    numericInput(
      "alpha",
      label = NULL,
      value = 0.05,
      min = 0.0001,
      max = 0.5,
      step = 0.001
    ),
    withMathJax(
      radioButtons(
        "alt",
        "Alternative Hypothesis",
        choices = c(
          "\\(p_1 - p_2 \\ne 0\\)" = "two.sided",
          "\\(p_1 - p_2 > 0\\)" = "greater",
          "\\(p_1 - p_2 < 0\\)" = "less"
        ),
        selected = "two.sided"
      )
    )
  ),

  withMathJax(
    layout_columns(
      col_widths = c(6, 6, 12),
      row_heights = c(1, 1, 2),
      card(
        full_screen = TRUE,
        card_header("Hypothesis Test Input Summary"),
        uiOutput("hypothesis_summary")
      ),
      card(
        full_screen = TRUE,
        card_header("Hypothesis Test Results"),
        reactableOutput("results_table")
      ),
      card(
        full_screen = TRUE,
        card_header("\\(p\\)-Value Plot"),
        plotOutput("pvalue_plot", height = "320px")
      )
    )
  )
)

server <- function(input, output, session) {

  fmt <- function(x, d = 4) {
    out <- sprintf(paste0("%.", d, "f"), x)
    sub("^-", "\u2212", out)
  }

  fmtp <- function(p) {
    ifelse(
      p < 1e-4,
      "< 0.0001",
      formatC(p, format = "f", digits = 6)
    )
  }

  output$sample_input_ui <- renderUI({
    if (input$input_type == "prop") {
      tagList(
        tags$label(HTML("Group 1 Proportion, \\(\\hat{p}_1\\):"), `for` = "p1_hat"),
        numericInput(
          "p1_hat",
          label = NULL,
          value = 0.55,
          min = 0,
          max = 1,
          step = 0.001
        ),
        tags$label("Group 1 Sample Size:", `for` = "n1"),
        numericInput(
          "n1",
          label = NULL,
          value = 100,
          min = 1,
          step = 1
        ),
        tags$label(HTML("Group 2 Proportion, \\(\\hat{p}_2\\):"), `for` = "p2_hat"),
        numericInput(
          "p2_hat",
          label = NULL,
          value = 0.45,
          min = 0,
          max = 1,
          step = 0.001
        ),
        tags$label("Group 2 Sample Size:", `for` = "n2"),
        numericInput(
          "n2",
          label = NULL,
          value = 100,
          min = 1,
          step = 1
        )
      )
    } else {
      tagList(
        tags$label("Group 1 Successes:", `for` = "x1"),
        numericInput(
          "x1",
          label = NULL,
          value = 55,
          min = 0,
          step = 1
        ),
        tags$label("Group 1 Sample Size:", `for` = "n1_count"),
        numericInput(
          "n1_count",
          label = NULL,
          value = 100,
          min = 1,
          step = 1
        ),
        tags$label("Group 2 Successes:", `for` = "x2"),
        numericInput(
          "x2",
          label = NULL,
          value = 45,
          min = 0,
          step = 1
        ),
        tags$label("Group 2 Sample Size:", `for` = "n2_count"),
        numericInput(
          "n2_count",
          label = NULL,
          value = 100,
          min = 1,
          step = 1
        )
      )
    }
  })

  calc <- reactive({
    req(input$alt)

    alt <- input$alt

    if (input$input_type == "prop") {
      req(input$p1_hat, input$n1, input$p2_hat, input$n2)

      validate(
        need(input$p1_hat >= 0 && input$p1_hat <= 1,
             "Group 1 proportion must be between 0 and 1."),
        need(input$p2_hat >= 0 && input$p2_hat <= 1,
             "Group 2 proportion must be between 0 and 1."),
        need(input$n1 >= 1, "Group 1 sample size must be at least 1."),
        need(input$n2 >= 1, "Group 2 sample size must be at least 1.")
      )

      p1_hat <- input$p1_hat
      p2_hat <- input$p2_hat
      n1 <- input$n1
      n2 <- input$n2
      x1 <- round(p1_hat * n1)
      x2 <- round(p2_hat * n2)
    } else {
      req(input$x1, input$n1_count, input$x2, input$n2_count)

      validate(
        need(input$x1 >= 0, "Group 1 successes must be nonnegative."),
        need(input$x2 >= 0, "Group 2 successes must be nonnegative."),
        need(input$n1_count >= 1, "Group 1 sample size must be at least 1."),
        need(input$n2_count >= 1, "Group 2 sample size must be at least 1."),
        need(input$x1 <= input$n1_count,
             "Group 1 successes cannot exceed its sample size."),
        need(input$x2 <= input$n2_count,
             "Group 2 successes cannot exceed its sample size.")
      )

      x1 <- input$x1
      x2 <- input$x2
      n1 <- input$n1_count
      n2 <- input$n2_count
      p1_hat <- x1 / n1
      p2_hat <- x2 / n2
    }

    p_pool <- (x1 + x2) / (n1 + n2)
    se <- sqrt(p_pool * (1 - p_pool) * (1 / n1 + 1 / n2))
    diff_hat <- p1_hat - p2_hat
    z <- diff_hat / se

    p_val <- switch(
      alt,
      "two.sided" = 2 * pnorm(-abs(z)),
      "greater"   = 1 - pnorm(z),
      "less"      = pnorm(z)
    )

    list(
      x1 = x1,
      n1 = n1,
      p1_hat = p1_hat,
      x2 = x2,
      n2 = n2,
      p2_hat = p2_hat,
      p_pool = p_pool,
      diff_hat = diff_hat,
      se = se,
      z = z,
      p_val = p_val,
      alt = alt
    )
  })

  output$hypothesis_summary <- renderUI({
    cdat <- calc()

    alt_text <- switch(
      cdat$alt,
      "two.sided" = "\\(H_a: p_1 - p_2 \\ne 0\\)",
      "greater"   = "\\(H_a: p_1 - p_2 > 0\\)",
      "less"      = "\\(H_a: p_1 - p_2 < 0\\)"
    )

    tagList(
      withMathJax(),
      tags$p(HTML("\\(H_0: p_1 - p_2 = 0\\)")),
      tags$p(HTML(alt_text)),
      tags$hr(),
      tags$p(HTML(paste0(
        "Group 1: \\(x_1 = ", cdat$x1, "\\), \\(n_1 = ", cdat$n1,
        "\\), \\(\\hat{p}_1 = ", fmt(cdat$p1_hat, 4), "\\)"
      ))),
      tags$p(HTML(paste0(
        "Group 2: \\(x_2 = ", cdat$x2, "\\), \\(n_2 = ", cdat$n2,
        "\\), \\(\\hat{p}_2 = ", fmt(cdat$p2_hat, 4), "\\)"
      ))),
      tags$p(HTML(paste0(
        "Observed difference: \\(\\hat{p}_1 - \\hat{p}_2 = ",
        fmt(cdat$diff_hat, 4), "\\)"
      )))
    )
  })

  mathjax_render <- function(tbl) {
    htmlwidgets::onRender(
      tbl,
      "function(el,x){
        if(window.MathJax){
          if(MathJax.typesetPromise){
            MathJax.typesetPromise([el]);
          } else if(MathJax.Hub && MathJax.Hub.Queue){
            MathJax.Hub.Queue(['Typeset', MathJax.Hub, el]);
          }
        }
      }"
    )
  }

  output$results_table <- renderReactable({
    cdat <- calc()

    mathjax_render(
      reactable(
        tibble(
          "Group 1 Proportion" = fmt(cdat$p1_hat, 4),
          "Group 2 Proportion" = fmt(cdat$p2_hat, 4),
          "Difference \\((\\hat{p}_1 - \\hat{p}_2)\\)" = fmt(cdat$diff_hat, 4),
          "Standard Error" = fmt(cdat$se, 4),
          "\\(z\\)-Statistic" = fmt(cdat$z, 4),
          "\\(p\\)-Value" = fmtp(cdat$p_val)
        ),
        defaultColDef = colDef(align = "right")
      )
    )
  })

  output$pvalue_plot <- renderPlot({
      cdat <- calc()

      x <- seq(-4.5, 4.5, length.out = 2000)
      df <- tibble(
        x = x,
        y = dnorm(x)
      )

      z_abs <- abs(cdat$z)

      p <- ggplot(df, aes(x, y)) +
        geom_line(linewidth = 1, color = "black") +
        labs(
          x = "z",
          y = "Density"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
        )

      if (cdat$alt == "two.sided") {
        left_tail <- df %>% filter(x <= -z_abs)
        right_tail <- df %>% filter(x >= z_abs)

        left_label <- tibble(
          x = -z_abs - 0.7,
          y = dnorm(-z_abs) * 0.7,
          label = paste0("Area = ", formatC(cdat$p_val / 2, format = "f", digits = 4))
        )

        right_label <- tibble(
          x = z_abs + 0.7,
          y = dnorm(z_abs) * 0.7,
          label = paste0("Area = ", formatC(cdat$p_val / 2, format = "f", digits = 4))
        )

        p <- p +
          geom_area(
            data = left_tail,
            aes(x = x, y = y),
            inherit.aes = FALSE,
            fill = "#A90533",
            alpha = 0.35
          ) +
          geom_area(
            data = right_tail,
            aes(x = x, y = y),
            inherit.aes = FALSE,
            fill = "#A90533",
            alpha = 0.35
          ) +
          geom_vline(
            xintercept = c(-z_abs, z_abs),
            linetype = "dashed",
            linewidth = 1,
            color = "#A90533"
          ) +
          geom_label(
            data = left_label,
            aes(x = x, y = y, label = label),
            inherit.aes = FALSE,
            fill = "white",
            color = "#A90533",
            label.size = 0.25,
            size = 4
          ) +
          geom_label(
            data = right_label,
            aes(x = x, y = y, label = label),
            inherit.aes = FALSE,
            fill = "white",
            color = "#A90533",
            label.size = 0.25,
            size = 4
          )

      } else if (cdat$alt == "greater") {
        right_tail <- df %>% filter(x >= cdat$z)

        right_label <- tibble(
          x = cdat$z + 0.9,
          y = dnorm(cdat$z) * 0.7,
          label = paste0("Area = ", formatC(cdat$p_val, format = "f", digits = 4))
        )

        p <- p +
          geom_area(
            data = right_tail,
            aes(x = x, y = y),
            inherit.aes = FALSE,
            fill = "#A90533",
            alpha = 0.35
          ) +
          geom_vline(
            xintercept = cdat$z,
            linetype = "dashed",
            linewidth = 1,
            color = "#A90533"
          ) +
          geom_label(
            data = right_label,
            aes(x = x, y = y, label = label),
            inherit.aes = FALSE,
            fill = "white",
            color = "#A90533",
            label.size = 0.25,
            size = 4
          )

      } else if (cdat$alt == "less") {
        left_tail <- df %>% filter(x <= cdat$z)

        left_label <- tibble(
          x = cdat$z - 0.9,
          y = dnorm(cdat$z) * 0.7,
          label = paste0("Area = ", formatC(cdat$p_val, format = "f", digits = 4))
        )

        p <- p +
          geom_area(
            data = left_tail,
            aes(x = x, y = y),
            inherit.aes = FALSE,
            fill = "#A90533",
            alpha = 0.35
          ) +
          geom_vline(
            xintercept = cdat$z,
            linetype = "dashed",
            linewidth = 1,
            color = "#A90533"
          ) +
          geom_label(
            data = left_label,
            aes(x = x, y = y, label = label),
            inherit.aes = FALSE,
            fill = "white",
            color = "#A90533",
            label.size = 0.25,
            size = 4
          )
      }

    p
  })

}

shinyApp(ui, server)