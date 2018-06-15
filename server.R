

library(shiny)
library(ggplot2)
library(gridExtra)


shinyServer(function(input, output) {

  # react when user clicks 'New Sample'
  data_sample <- eventReactive(c(input$sample_data), {
    y <- rnorm(input$n, input$mu_data, input$sd_data)
    
    mu <- mean(y)
    se <- sd(y) / sqrt(length(y))
    
    mu_prior <- input$mu_prior
    sd_prior <- input$sd_prior
    
    dat_min <- min(y) - 0.2 * sd(y)
    dat_max <- max(y) + 0.2 * sd(y)
    
    xmin <- min(dat_min, qnorm(0.0001, mu, se), qnorm(0.001, mu_prior, sd_prior))
    xmax <- max(dat_max, qnorm(0.9999, mu, se), qnorm(0.999, mu_prior, sd_prior))
    
    return(list(y = y, xmin = xmin, xmax = xmax))
  }, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  
  # react when data_sample() changes (when user clicks 'New Sample')
  true_mean <- eventReactive(data_sample(), {
    return(input$mu_data)
  })
  
  # react when user checks 'Lock x-axis'
  axis_lock <- eventReactive(input$axis_lock, {
    z <- data_sample()
    low <- z$xmin
    upp <- z$xmax
    return(list(low = low, upp = upp))
  }, ignoreNULL = FALSE, ignoreInit = FALSE)
  
  # draw plot
  output$distPlot <- renderPlot({
    
    # get reactive data
    data_sample <- data_sample()
    true_mean <- true_mean()
    axis_lock <- axis_lock()
    
    # sample data
    y <- data_sample$y
    
    # update x-axis limits, depending on whether 'Lock x-axis' is checked
    if (input$axis_lock == FALSE) {
      lim_low <- NULL
      lim_upp <- NULL
      xmin <- data_sample$xmin
      xmax <- data_sample$xmax
    } else {
      lim_low <- axis_lock$low
      lim_upp <- axis_lock$upp
      xmin <- axis_lock$low
      xmax <- axis_lock$upp
    }
    
    # sample mean and standard error
    mu_data <- mean(y)
    se_data <- sd(y) / sqrt(length(y))
    
    # prior mean and standard deviation (user-set)
    mu_prior <- input$mu_prior
    sd_prior <- input$sd_prior
    
    # sequence of x-values for generating probability distributions
    x <- seq(xmin, xmax, length.out = 400)
    
    # prior and likelihood
    pri <- dnorm(x, mu_prior, sd_prior)
    lik <- dnorm(x, mu_data, se_data)
    
    # posterior distribution (product of 2 Gaussian pdfs is also Gaussian)
    # https://www.johndcook.com/blog/2012/10/29/product-of-normal-pdfs/
    mu_ppr <- (mu_data * se_data^(-2) + mu_prior * sd_prior^(-2)) /
              (se_data^(-2) + sd_prior^(-2))
    sd_ppr <- sqrt((se_data^2 * sd_prior^2) / (se_data^2 + sd_prior^2))
    
    # posterior probability density
    ppr <- dnorm(x, mu_ppr, sd_ppr)
    
    # frequentist confidence intervals
    ci_low <- qnorm(0.025, mu_data, se_data)
    ci_upp <- qnorm(0.975, mu_data, se_data)
    
    # bayesian credible intervals
    cr_low <- qnorm(0.025, mu_ppr, sd_ppr)
    cr_upp <- qnorm(0.975, mu_ppr, sd_ppr)
    
    # arrange dfs for prior, likelihood, and posterior
    df_lik <- data.frame(x, type = 'lik', val = lik)
    df_pri <- data.frame(x, type = 'pri', val = pri)
    df_ppr <- data.frame(x, type = 'ppr', val = ppr)
    
    # combine distributions into single df
    df <- rbind.data.frame(df_lik, df_pri, df_ppr)
    df$type <- factor(df$type, levels = c('pri', 'lik', 'ppr'),
                      labels = c('Prior', 'Likelihood', 'Posterior'))
    
    # dfs for confidence intervals, sample data, and population mean
    fc <- 0.12  # factor for positioning CIs, sample data, and pop mean
    
    df_ci <- data.frame(x = mu_data, xmin = ci_low, xmax = ci_upp, y = -1*fc)
    df_cr <- data.frame(x = mu_ppr, xmin = cr_low, xmax = cr_upp, y = -2*fc)
    df_sd <- data.frame(x = y, y = -3*fc)
    df_pm <- data.frame(x = true_mean, y = -4*fc)
    
    # plot settings
    size_labs <- 5.5
    ci_size <- 1.6
    
    # plot
    p1 <- ggplot(df) +
      geom_vline(xintercept = 0, linetype = 2, alpha = 0.7) +
      geom_point(data = df_sd, aes(x, y), size = 4.2, stroke = 0, alpha = 0.4,
                 color = '#e7298a') +
      geom_point(data = df_sd, aes(x, y), shape = 1, stroke = 1.2, size = 4.2,
                 alpha = 0.9, color = '#e7298a') +
      geom_errorbarh(data = df_ci, aes(x = x, xmin = xmin, xmax = xmax, y),
                     height = 0, size = ci_size, color = '#d95f02') +
      geom_errorbarh(data = df_cr, aes(x = x, xmin = xmin, xmax = xmax, y),
                     height = 0, size = ci_size, color = '#7570b3') +
      geom_point(data = df_pm, aes(x, y), size = 2.5, stroke = 1.2, shape = 3) +
      geom_line(aes(x, val, color = type), lwd = 1.5) +
      geom_ribbon(aes(x, ymin = 0, ymax = val, fill = type),
                  lwd = 0, alpha = 0.5) +
      annotate('text', x = Inf, y = -1*fc, label = '  Frequentist 95% CI',
               hjust = 0, size = size_labs, color = '#d95f02', fontface = 2) +
      annotate('text', x = Inf, y = -2*fc, label = '  Bayesian 95% CI',
               hjust = 0, size = size_labs, color = '#7570b3', fontface = 2) +
      annotate('text', x = Inf, y = -3*fc, label = '  Sample data',
               hjust = 0, size = size_labs, color = '#e7298a', fontface = 2) +
      annotate('text', x = Inf, y = -4*fc, label = "  (True population mean)",
               hjust = 0, size = size_labs) +
      coord_cartesian(xlim = c(lim_low, lim_upp), ylim = c(-4*fc, 1.8)) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_color_manual(values = c('#1b9e77', '#d95f02', '#7570b3')) +
      scale_fill_manual(values = c('#1b9e77', '#d95f02', '#7570b3')) +
      guides(col = guide_legend(keyheight = 0.9, keywidth = 0.9, default.unit = "cm")) +
      xlab('y') + ylab(NULL) +
      theme(panel.grid = element_blank(),
            text = element_text(size = 18),
            axis.title = element_text(size = 18),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.background = element_rect(fill = 'grey94'),
            axis.title.x = element_text(margin = margin(.3, 0, 0, 0, unit = 'cm')),
            legend.title = element_blank(),
            legend.justification = c(0, 1),
            legend.box.spacing = unit(1.2, 'pt'),
            legend.text = element_text(size = 16, face = 'bold'),
            legend.background = element_blank(),
            plot.margin = unit(c(5.5, 65.5, 5.5, 5.5), 'pt'))
    
    # allow plotting outside main plot region
    g1 <- ggplotGrob(p1)
    g1$layout$clip[g1$layout$name == "panel"] <- "off"
    
    # output plot
    grid.arrange(g1)
  })
})
