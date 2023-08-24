library(shiny)
library(shinydashboard)
library(tidyverse)
library(optistock)

mort_funs <- c(
  "Constant", "Exponential",
  # "Decreasing",
  "Linear", "Bottleneck"
)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Welcome", tabName = "welcome_tab"),
      menuItem("Recruitment", tabName = "recruit_tab"),
      menuItem("Growth", tabName = "growth_tab"),
      menuItem("Mortality", tabName = "mort_tab"),
      menuItem("Hatchery Costs", tabName = "cost_tab"),
      menuItem("Cost-per-fish", tabName = "output_tab")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "welcome_tab",
        h1("Welcome to Optistock"),
        h3("A model for determining most cost-effective stocking time"),
        h3(paste(
          "Play around with the parameters to the left",
          "to see how optimum stocking time changes."
        )),
        br(),
        h4(paste(
          "This model uses assumptions about growth, mortality",
          "and hatchery rearing costs to determine the time",
          "until recruitment into a fishery,"
        )),
        h4(paste(
          "the number needed",
          "to be stocked to achieve a desired number at recruitment,",
          "and the cost-per-fish across stocking times."
        ))
      ),
      tabItem(
        tabName = "recruit_tab",
        fluidRow(
          box(
            width = 4,
            title = "Number of recruits desired",
            textInput("recruits", "#", value = 10000)
          ),
          box(
            width = 4,
            title = "Length at recruitment",
            sliderInput("mll", "Length (in.)", 1, 50, 15, step = 0.5)
          )
        )
      ), # end of recruit_tab
      tabItem(
        withMathJax(),
        tabName = "growth_tab",
        fluidRow(
          box(
            width = 3,
            title = "$$L_\\infty$$",
            numericInput("linf_input", "", 30, min = 0, max = 1e6),
            sliderInput("linf", "", 1, 100, 30, step = 0.01)
          ),
          box(
            width = 3,
            title = "$$k$$",
            numericInput("k_input", "", value = 0.15, min = 0.01, max = 1),
            sliderInput("k", "", min = 0.01, max = 1, value = 0.15, step = 0.01)
          ),
          box(
            width = 3,
            title = "$$t_0$$",
            numericInput("t0_input", "", value = -0.2, min = -5, max = 0.5),
            sliderInput("t0", "", min = -5, max = 0.5, value = -0.2, step = 0.01)
          )
        ),
        fluidRow(
          box(
            width = 6,
            plotOutput("growth_plot")
          ),
          box(
            width = 3,
            title = "Time to recruitment:",
            h4(textOutput("time_to_mll")),
            br(),
            h4(
              paste(
                "Note: the \\(k\\) and \\(t_0\\) parameters",
                "are based on annual growth, but other",
                "calculations involving time are daily.",
                "These parameters will be divided (\\(k\\)) or",
                "multiplied (\\(t_0\\)) by 365 to convert to daily units.",
                "The dashed line represents the time to recruitment."
              ))
          )
        )
      ), # end of growth_tab
      tabItem(
        withMathJax(),
        tabName = "mort_tab",
        box(
          width = 4,
          selectInput(
            "mort_fun",
            label = "Mortality function",
            choices = mort_funs,
            selected = "Exponential"
          ),
          uiOutput("mort_fun_params")
        ),
        box(
          width = 8,
          plotOutput("mort_curve")
        )
      ), # end of mort_tab
      tabItem(
        tabName = "cost_tab",
        box(
          width = 4,
          selectInput(
            "cost_fun_type",
            label = "Cost Function",
            choices = c("Daily", "Total"),
            selected = "Total"
          ),
          uiOutput("cost_fun_params")
        ),
        box(
          plotOutput("cost_curve_plot")
        )
      ), # end of cost_tab
      tabItem(
        tabName = "output_tab",
        column(
          width = 6,
          box(
            width = 12,
            h4(textOutput("cpf_curve_desc")),
            plotOutput("cost_per_fish_curve")

          )
        ),
        column(
          width = 6,
          box(
            width = 12,
            h4(textOutput("n_to_stock_desc")),
            plotOutput("n_to_stock_curve")
          )
        )
      ) # end of output tab
    )
  )
)

server <- function(input, output, session) {

  rec_time <- reactive({
    inv_vb(input$mll, input$linf, input$k / 365, input$t0 * 365)

  })

  # update growth parameters based on slider or numeric input
  observe({
    updateNumericInput(session, "linf_input", value = input$linf)
  }) |> bindEvent(input$linf)

  observe({
    updateSliderInput(session, "linf", value = input$linf_input)
  }) |> bindEvent(input$linf_input)

  observe({
    updateNumericInput(session, "k_input", value = input$k)
  }) |> bindEvent(input$k)

  observe({
    updateSliderInput(session, "k", value = input$k_input)
  }) |> bindEvent(input$k_input)

  observe({
    updateNumericInput(session, "t0_input", value = input$t0)
  }) |> bindEvent(input$t0)

  observe({
    updateSliderInput(session, "t0", value = input$t0_input)
  }) |> bindEvent(input$t0_input)


  output$growth_plot <- renderPlot({
    x_limit <- rec_time() * 2
    ggplot(data = data.frame(x = 1:x_limit / 365), aes(x)) +
      stat_function(
        fun = vbgf,
        args = list(
          linf = input$linf,
          k = input$k,
          t0 = input$t0
        ),
        linewidth = 1.2
      )  +
      geom_segment(
        aes(
          x = rec_time() / 365, y = 0,
          xend = rec_time() / 365, yend = input$mll
        ), linetype = "dashed"
      ) +
      geom_segment(
        aes(
          x = 0, y = input$mll,
          xend = rec_time() / 365, yend = input$mll
        ), linetype = "dashed"
      ) +
      # geom_vline(xintercept = rec_time() / 365, linetype = "dashed") +
      xlab("Age (years)") + ylab("Length (in.)") +
      theme_bw() +
      theme(
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 14)
      ) +
      scale_x_continuous(breaks = scales::breaks_pretty())
  })

  output$time_to_mll <- renderText({
    # rec_time <- inv_vb(input$mll, input$linf, input$k / 365, input$t0 * 365)
    paste(format(round(rec_time()), big.mark = ","), "days")
  })


  output$mort_fun_params <- renderUI({
    rec_time <- round(
      inv_vb(input$mll, input$linf, input$k / 365, input$t0 * 365)
    )
    if (input$mort_fun == "Constant") {
      tagList(
        withMathJax(),
        sliderInput(
          "const_mort_params",
          label = "$$M$$",
          min = 0.01, max = 3, value = 0.2, step = 0.01
        )
      )
    } else if (input$mort_fun == "Exponential") {
      out <- list()
      out[[1]] <- withMathJax()
      out[[2]] <- sliderInput("exp_m_init",
                              label = "$$M_{init}$$",
                              min = 0.01, max = 3, value = 1)
      out[[3]] <- sliderInput("exp_m_inf",
                              label = "$$M_\\infty$$",
                              min = 0.01, max = 3, value = 0.1)
      out[[4]] <- sliderInput("exp_alpha",
                              label = "$$\\alpha$$",
                              min = 0.01, max = 10, value = 2)
      out[[5]] <- sliderInput("exp_t_scale",
                              label = "$$t_{scale}$$",
                              min = 0, max = round(rec_time / 365, 2),
                              value = rec_time * 0.3 / 365,
                              step = 0.1)
      do.call(tagList, out)
    } else if (input$mort_fun == "Decreasing") {
      out <- list()
      out[[1]] <- withMathJax()
      out[[2]] <- sliderInput("dec_m_init",
                              label = "$$M_{init}$$",
                              0.01, 1, 0.8)
      out[[3]] <- sliderInput("dec_m_inf",
                              label = "$$M_\\infty$$",
                              0.01, 1, 0.1)
      out[[4]] <- sliderInput("dec_alpha",
                              label = "$$\\alpha$$",
                              0.00001, 1, 0.99)
      do.call(tagList, out)
    } else if (input$mort_fun == "Linear") {
      out <- list()
      out[[1]] <- sliderInput("lin_m_slope",
                              label = "Slope",
                              -1, 0, -0.1)
      out[[2]] <- sliderInput("lin_m_intercept",
                              label = "Intercept",
                              0.01, 3, 1)
      do.call(tagList, out)
    } else if (input$mort_fun == "Bottleneck") {
      out <- list()
      out[[1]] <- withMathJax()
      out[[2]] <- sliderInput("bn_m_init",
                              label = "$$M_{init}$$",
                              0.01, 1, 0.1)
      out[[3]] <- sliderInput("bn_m_max",
                              label = "$$M_{max}$$",
                              0.01, 2, 0.5)
      out[[4]] <- sliderInput("bn_m_inf",
                              label = "$$M_{\\infty}$$",
                              0.01, 1, 0.1)
      out[[5]] <- sliderInput("bn_t_scale",
                              label = "$$t_{scale}$$",
                              min = 1,
                              max = round(rec_time / 365, 2),
                              value = rec_time * 0.2 / 365)
      out[[6]] <- sliderInput("bn_alpha",
                              label = "$$\\alpha$$",
                              1, 100, 40)
      do.call(tagList, out)
    }
  })

  mort_fun <- reactive({
    mort_fun <- switch(
      input$mort_fun,
      "Constant" = constant_mort,
      "Exponential" = exp_mort,
      "Decreasing" = decreasing_mort,
      "Linear" = linear_mort,
      "Bottleneck" = half_gaussian_mort
    )
  })

  annual_mort_args_list <- reactive({
    switch(
      input$mort_fun,
      "Constant" = list(m = input$const_mort_params),
      "Exponential" = list(
        m_init = input$exp_m_init,
        m_inf = input$exp_m_inf,
        alpha = input$exp_alpha,
        t_scale = input$exp_t_scale
      ),
      "Decreasing" = list(
        m_init = input$dec_m_init,
        m_inf = input$dec_m_inf,
        alpha = input$dec_alpha
      ),
      "Linear" = list(
        alpha = input$lin_m_slope,
        m_init = input$lin_m_intercept
      ),
      "Bottleneck" = list(
        m_init = input$bn_m_init,
        m_max = input$bn_m_max,
        m_inf = input$bn_m_inf,
        t_scale = input$bn_t_scale,
        alpha = input$bn_alpha
      )
    )
  })

  mort_args_list <- reactive({
    switch(
      input$mort_fun,
      "Constant" = list(m = input$const_mort_params / 365),
      "Exponential" = list(
        m_init = input$exp_m_init / 365,
        m_inf = input$exp_m_inf / 365,
        alpha = input$exp_alpha / 365,
        t_scale = input$exp_t_scale * 365
      ),
      "Decreasing" = list(
        m_init = input$dec_m_init / 365,
        m_inf = input$dec_m_inf / 365,
        alpha = input$dec_alpha
      ),
      "Linear" = list(
        alpha = input$lin_m_slope * 365 * 365,
        m_init = input$lin_m_intercept / 365
      ),
      "Bottleneck" = list(
        m_init = input$bn_m_init / 365,
        m_max = input$bn_m_max / 365,
        m_inf = input$bn_m_inf / 365,
        t_scale = input$bn_t_scale * 365,
        alpha = input$bn_alpha
      )
    )
  })

  rec_time <- reactive({
    round(
      inv_vb(input$mll, input$linf, input$k / 365, input$t0 * 365)
    )
  })

  output$mort_curve <- renderPlot({
    mort_args <- annual_mort_args_list()
    g <-
      ggplot(data = data.frame(x = 1:rec_time() / 365), aes(x)) +
      stat_function(fun = mort_fun(), args = mort_args) +
      theme_bw() +
      xlab("Age (years)") + ylab("Natural Mortality") +
      theme(
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 14)
      ) +
      scale_x_continuous(breaks = scales::breaks_pretty()) +
      ylim(c(0, NA))
    g
  })

  output$cost_fun_params <- renderUI({
    if (input$cost_fun_type == "Daily") {
      out <- list()
      out[[1]] <- sliderInput(
        "init_cost",
        "Initial Instantaneous Costs ($ per fish)",
        min = 0.001, max = 20, value = 5, step = 0.01
      )
      out[[2]] <- sliderInput(
        "time_slope",
        "Daily Increase",
        0.0001, 0.1, 0.01
      )
      out[[3]] <- sliderInput(
        "time_exp",
        "Daily Exponential Increase",
        0, 10, 1, step = 0.01
      )
      out[[4]] <- sliderInput(
        "rec_slope",
        "Cost per additional recruit ($ per fish)",
        min = 0.01, max = 10, value = 1, step = 0.01
      )
      out[[5]] <- sliderInput(
        "rec_exp",
        "Exponential Increase per recruit",
        min = 0, max = 10, value = 1, step = 0.01
      )
      do.call(tagList, out)
    } else if (input$cost_fun_type == "Total") {
      out <- list()
      out[[1]] <- sliderInput(
        "total_cost_time_slope",
        "Time Slope",
        min = 0, max = 1, value = 0.01, step = 0.01
      )
      out[[2]] <- sliderInput(
        "total_cost_time_exp",
        "Time Exponent",
        min = 0, max = 10, value = 1, step = 0.1
      )
      out[[3]] <- sliderInput(
        "total_cost_init_cost",
        "Initial Cost ($ per fish)",
        min = 0, max = 20, value = 1, step = 0.1
      )
      out[[4]] <- sliderInput(
        "total_cost_rec_exp",
        "Recruitment Exponent",
        min = 0, max = 10, value = 1, step = 0.1
      )
      do.call(tagList, out)
    }

  })


  output$cost_curve_plot <- renderPlot({
    if (input$cost_fun_type == "Daily") {
      args_list <- list(
        daily_cost = input$init_cost,
        time_slope = input$time_slope,
        time_exp = input$time_exp,
        rec_slope = input$rec_slope,
        rec_exp = input$rec_exp
      )
    } else if (input$cost_fun_type == "Total") {
      args_list <- list(
        time_slope = input$total_cost_time_slope,
        time_exp = input$total_cost_time_exp,
        init_cost = input$total_cost_init_cost,
        rec_exp = input$total_cost_rec_exp
      )
    }

    cost_fun <- ifelse(
      input$cost_fun_type == "Daily",
      daily_total_cost,
      total_cost
    )


    # color values for legend
    legend_colors <- c(
      "1 fish" = "blue",
      "2 fish" = "red",
      "3 fish" = "purple"
    )
    g <-
      ggplot(data = data.frame(x = 1:rec_time()), aes(x)) +
      stat_function(fun = cost_fun, args = c(recruits = 1, args_list),
                    aes(color = "1 fish"), size = 1.2) +
      stat_function(fun = cost_fun, args = c(recruits = 2, args_list),
                    aes(color = "2 fish"), size = 1.2) +
      stat_function(fun = cost_fun, args = c(recruits = 3, args_list),
                    aes(color = "3 fish"), size = 1.2) +
      xlab("Days") + ylab("Cost-per-fish ($)") +
      ylim(c(0, NA)) +
      theme_bw() +
      theme(
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 16)
      ) +
      scale_color_manual(
        values = legend_colors,
        breaks = c("1 fish", "2 fish", "3 fish"),
        name = ""
      )
    g
  })

  output$cpf_curve_desc <- renderText({
    sprintf(
      paste(
        "This curve shows the cost-per-fish on any given",
        "day that will result in %s fish on day %s"
      ),
      format(as.numeric(input$recruits), big.mark = ","),
      format(rec_time(), big.mark = ",")
    )
  })

  output$cost_per_fish_curve <- renderPlot({
    if (input$mort_fun == "Constant" && is.null(mort_args_list()[[1]])) {
      mort_fun_args = list(m = 0.0005)
    }
    if (input$cost_fun_type == "Daily") {
      args_list <- list(
        time_at_rec = rec_time(),
        n_recruits_desired = as.numeric(input$recruits),
        cost_fun = total_daily_cost,
        cost_fun_args = list(
          input$init_cost,
          input$time_slope,
          input$time_exp,
          input$rec_slope,
          input$rec_exp
        ),
        mort_fun = mort_fun(),
        mort_fun_args = mort_args_list()
      )
    } else if (input$cost_fun_type == "Total") {
      args_list <- list(
        time_at_rec = rec_time(),
        n_recruits_desired = as.numeric(input$recruits),
        cost_fun = total_cost,
        cost_fun_args = list(
          time_slope = input$total_cost_time_slope,
          time_exp = input$total_cost_time_exp,
          init_cost = input$total_cost_init_cost,
          rec_exp = input$total_cost_rec_exp
        ),
        mort_fun = mort_fun(),
        mort_fun_args = mort_args_list()
      )
    }

    out <-
      ggplot(data = data.frame(x = 1:rec_time()), aes(x)) +
      stat_function(fun = cost_per_fish, args = args_list, size = 1.2) +
      theme_bw() +
      theme(
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 14)
      ) +
      xlab("Days") + ylab("Cost-per-fish ($)")
    out
  })

  output$n_to_stock_desc <- renderText({
    sprintf(
      paste(
        "This curve shows the number of fish that must be stocked",
        "to result in %s fish on day %s"
      ),
      format(as.numeric(input$recruits), big.mark = ","),
      format(rec_time(), big.mark = ",")
    )
  })

  output$n_to_stock_curve <- renderPlot({
    args_list <- list(
      time_at_rec = rec_time(),
      n_recruits_desired = as.numeric(input$recruits),
      mort_fun = mort_fun(),
      mort_fun_args = mort_args_list()
    )
    g <-
      ggplot(data = data.frame(x = 1:rec_time()), aes(x)) +
      stat_function(fun = n_to_stock, args = args_list, linewidth = 1.2) +
      theme_bw() +
      theme(
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 14)
      ) +
      xlab("Days") + ylab("Number of Fish Stocked")
    g
  })


  # update slider bars of some of the mort functions when they make no sense
  observeEvent(input$exp_m_inf, {
    if (input$exp_m_inf > input$exp_m_init) {
      updateSliderInput(session,
                        "exp_m_init", value = input$exp_m_inf,
                        min = 0.0001, max = 0.01)
    }
  })

  observeEvent(input$exp_m_init, {
    if (input$exp_m_init < input$exp_m_inf) {
      updateSliderInput(session,
                        "exp_m_inf", value = input$exp_m_init,
                        min = 0.0001, max = 0.01)
    }
  })

  observeEvent(input$inv_m_inf, {
    if (input$inv_m_inf > input$inv_m_init) {
      updateSliderInput(session,
                        "inv_m_init", value = input$inv_m_inf,
                        min = 0.0001, max = 0.01)
    }
  })

  observeEvent(input$inv_m_init, {
    if (input$inv_m_init < input$inv_m_inf) {
      updateSliderInput(session,
                        "inv_m_inf", value = input$inv_m_init,
                        min = 0.0001, max = 0.01)
    }
  })

}

# Run the application
shinyApp(ui = ui, server = server)
