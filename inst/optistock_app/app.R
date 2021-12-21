library(shiny)
library(shinydashboard)
library(tidyverse)
library(optistock)

mort_funs <- c("Constant", "Exponential", "Decreasing", "Linear", "Bottleneck")

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
                        title = "Number of recruits desired",
                        textInput("recruits", "#", value = 200)
                    ),
                    box(
                        title = "Length at recruitment",
                        sliderInput("mll", "Length (in.)", 1, 50, 15)
                    )
                )
            ), # end of recruit_tab
            tabItem(
                withMathJax(),
                tabName = "growth_tab",
                fluidRow(
                    box(
                        width = 4,
                        title = "$$L_\\infty$$",
                        sliderInput("linf", "", 1, 100, 30)
                    ),
                    box(
                        width = 4,
                        title = "$$k$$",
                        sliderInput("k", "", 0.01, 0.5, 0.15)
                    ),
                    box(
                        width = 4,
                        title = "$$t_0$$",
                        sliderInput("t0", "", -3, 0.5, -0.2)
                    )
                ),
                fluidRow(
                    box(
                        width = 8,
                        plotOutput("growth_plot")
                    ),
                    box(
                        width = 4,
                        title = "Time to recruitment:",
                        textOutput("time_to_mll"),
                        br(),
                        helpText(
                            paste(
                                "Note: the k and t_0 parameters",
                                "are based on annual growth, but other",
                                "calculations involving time are daily.",
                                "These parameters will be divided (k) or",
                                "multiplied (t_0) by 365 to convert to daily."
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
                    sliderInput("init_cost",
                                "Initial Instantaneous Costs ($ per fish)",
                                0.001, 0.1, 0.01),
                    sliderInput("time_slope",
                                "Daily Increase",
                                0.0001, 0.1, 0.01),
                    sliderInput("time_exp",
                                "Daily Exponential Increase",
                                0, 10, 1, step = 0.01),
                    sliderInput("rec_slope",
                                "Cost per additional recruit ($ per fish)",
                                0.0001, 0.1, 0.01),
                    sliderInput("rec_exp",
                                "Exponential Increase per recruit",
                                0, 10, 1, step = 0.01)
                ),
                box(
                    plotOutput("daily_cost_curve")
                )
            ), # end of cost_tab
            tabItem(
                tabName = "output_tab",
                box(
                    plotOutput("cost_per_fish_curve")
                ),
                box(
                    width = 5,
                    textOutput("cpf_curve_desc")
                )
            ) # end of output tab
        )
    )
)

server <- function(input, output, session) {

    rec_time <- reactive({
        inv_vb(input$mll, input$linf, input$k / 365, input$t0)

    })

    output$growth_plot <- renderPlot({
        ggplot(data = data.frame(x = 1:rec_time()), aes(x)) +
            stat_function(fun = vbgf,
                          args = list(linf = input$linf,
                                      k = input$k / 365,
                                      t0 = input$t0))  +
            xlab("Age (days)") + ylab("Length (in.)") +
            theme_bw() +
            theme(axis.title = element_text(size = 15))
    })

    output$time_to_mll <- renderText({
        rec_time <- inv_vb(input$mll, input$linf, input$k / 365, input$t0)
        paste(format(round(rec_time), big.mark = ","), "days")
    })


    output$mort_fun_params <- renderUI({
        rec_time <- round(
            inv_vb(input$mll, input$linf, input$k / 365, input$t0 * 365)
        )
        if (input$mort_fun == "Constant") {
            tagList(
                withMathJax(),
                sliderInput("const_mort_params",
                            label = "$$M$$",
                            0.00001, 0.01, 0.0005)
            )
        } else if (input$mort_fun == "Exponential") {
            out <- list()
            out[[1]] <- withMathJax()
            out[[2]] <- sliderInput("exp_m_init",
                                    label = "$$M_{init}$$",
                                    0.0001, 0.1, 0.002)
            out[[3]] <- sliderInput("exp_m_inf",
                                    label = "$$M_\\infty$$",
                                    0.0001, 0.1, 0.001)
            out[[4]] <- sliderInput("exp_alpha",
                                    label = "$$\\alpha$$",
                                    0.0001, 0.01, value = 0.001)
            out[[5]] <- sliderInput("exp_t_scale",
                                    label = "$$t_{scale}$$",
                                    0, rec_time, value = rec_time * 0.2)
            do.call(tagList, out)
        } else if (input$mort_fun == "Decreasing") {
            out <- list()
            out[[1]] <- withMathJax()
            out[[2]] <- sliderInput("dec_m_init",
                                    label = "$$M_{init}$$",
                                    0.0001, 1, 0.8)
            out[[3]] <- sliderInput("dec_m_inf",
                                    label = "$$M_\\infty$$",
                                    0.0001, 0.01, 0.003)
            out[[4]] <- sliderInput("dec_alpha",
                                    label = "$$\\alpha$$",
                                    0.00001, 1, 0.99)
            do.call(tagList, out)
        } else if (input$mort_fun == "Linear") {
            out <- list()
            out[[1]] <- sliderInput("lin_m_slope",
                                    label = "Slope",
                                    -0.1, -0.0000001, -0.00001)
            out[[2]] <- sliderInput("lin_m_intercept",
                                    label = "Intercept",
                                    0.0001, 0.01, 0.001)
            do.call(tagList, out)
        } else if (input$mort_fun == "Bottleneck") {
            out <- list()
            out[[1]] <- withMathJax()
            out[[2]] <- sliderInput("bn_m_init",
                                    label = "$$M_{init}$$",
                                    0.001, 1, 0.001)
            out[[3]] <- sliderInput("bn_m_max",
                                    label = "$$M_{max}$$",
                                    0.01, 2, 0.2)
            out[[4]] <- sliderInput("bn_m_inf",
                                    label = "$$M_{\\infty}$$",
                                    0.0001, 0.01, 0.001)
            out[[5]] <- sliderInput("bn_t_scale",
                                    label = "$$t_{scale}$$",
                                    1, rec_time, rec_time * 0.2)
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
            "Linear" = lin_mort,
            "Bottleneck" = half_gaussian_mort
        )
    })

    mort_args_list <- reactive({
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
                slope = input$lin_m_slope,
                intercept = input$lin_m_intercept
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

    rec_time <- reactive({
        round(
            inv_vb(input$mll, input$linf, input$k / 365, input$t0)
        )
    })

    output$mort_curve <- renderPlot({
        mort_args <- mort_args_list()
        g <-
            ggplot(data = data.frame(x = 1:rec_time()), aes(x)) +
            stat_function(fun = mort_fun(), args = mort_args) +
            theme_bw() +
            xlab("Days") + ylab("Natural Mortality") +
            theme(axis.title = element_text(size = 15)) +
            ylim(c(0, NA))
        g
    })

    output$daily_cost_curve <- renderPlot({
        args_list <- list(
            init_cost = input$init_cost,
            time_slope = input$time_slope,
            time_exp = input$time_exp,
            rec_slope = input$rec_slope,
            rec_exp = input$rec_exp
        )
        # color values for legend
        legend_colors <- c(
            "1 fish" = "blue",
            "1000 fish" = "red",
            "10,000 fish" = "purple"
        )
        g <-
            ggplot(data = data.frame(x = 1:rec_time()), aes(x)) +
            stat_function(fun = cost_fun, args = c(recruits = 1, args_list),
                          aes(color = "1 fish"), size = 1.2) +
            stat_function(fun = cost_fun, args = c(recruits = 1000, args_list),
                          aes(color = "1000 fish"), size = 1.2) +
            stat_function(fun = cost_fun, args = c(recruits = 10000, args_list),
                          aes(color = "10,000 fish"), size = 1.2) +
            xlab("Days") + ylab("Cost-per-fish ($)") +
            ylim(c(0, NA)) +
            theme_bw() +
            theme(axis.title = element_text(size = 15)) +
            scale_color_manual(values = legend_colors, name = "")
        g
    })

    output$cost_per_fish_curve <- renderPlot({
        if (input$mort_fun == "Constant" && is.null(mort_args_list()[[1]])) {
            mort_fun_args = list(m = 0.0005)
        }
        args_list <- list(
            time_at_rec = rec_time(),
            n_recruits_desired = as.numeric(input$recruits),
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
        out <-
            ggplot(data = data.frame(x = 1:rec_time()), aes(x)) +
            stat_function(fun = cost_per_fish, args = args_list, size = 1.2) +
            theme_bw() +
            theme(axis.title = element_text(size = 15)) +
            xlab("Days") + ylab("Cost-per-fish ($)")
        out
    })

    output$cpf_curve_desc <- renderText({
        paste(
            "This CPF curve represents the cost-per-fish on any given stocking",
            "day that will result in ", input$recruits, "fish on day",
            format(rec_time(), big.mark = ",")
        )
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
