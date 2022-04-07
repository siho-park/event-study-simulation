rm(list = ls())
library(tidyverse)
library(modelr)
library(haven)
library(did)
library(fixest)

# Path and seed-----------------------------------------------------------------

  set.seed(123)
  path = file.path(Sys.getenv("DROPBOX"), "Econometrics exercise/event-study-simulation/figures")

# Function for simulation-------------------------------------------------------

  # Define function
  simulate <- function(shock, rep = 1000){

    # Create a data frame for holding the results
    results = tibble()
    
    # Iterate over 1000 times
    for (i in 1:rep){
      
      # Generate data df
      data <- tibble(
        id = rep(letters[1:20], each = 12),
        date = rep(1:12, 20),
        eventtime = c(-6:5, rep(0, 12*19))
      )
      
      # State-specific shock
      state_level <- tibble(
        id = letters[1:20],
        state_shock = runif(20, min = 50, max = 100)
      )
      
      # Period-specific shock
      period_level <- tibble(
        date = 1:12,
        period_shock = runif(12, min = -5, max = 5)
      )
      

      # Merge the shocks into data df
      data <- data %>%
        left_join(state_level, by = "id") %>%
        left_join(period_level, by = "date")
      
      # Merge tax shock
      data["tax_shock"] = c(shock, rep(0, 12*19))
      
      # Create iid error term
      data["error"] <- rnorm(240, 0, 1)
      
      # Create outcome variable
      data <- data %>%
        mutate(outcome = state_shock + period_shock + tax_shock + error)
      
      # Recast eventtime variable as factor
      ref = -6
      ref_shift = ref + 7
      data <- within(data, eventtime <- relevel(as.factor(eventtime), ref = ref_shift))
      
      # Run OLS and store results
      ols <- feols(outcome ~ eventtime | id + date,
                   vcov = ~id,
                   data = data)
      
      new_results <- tibble(
        estimator = rep("ols", 11),
        period = -5:5,
        point = ols$coefficients,
        se = ols$se,
        lb = confint(summary(ols))[[1]],
        ub = confint(summary(ols))[[2]],
        round = NA
      )
      
      new_results <- new_results %>%
        add_row(
          estimator = "ols",
          period = -6,
          point = 0,
          se = 0,
          lb = 0,
          ub = 0,
          round = NA
        ) %>%
        arrange(period)
      
      results <- results %>%
        rbind(new_results)
      
      
      # Run CS and store results
      data <- data %>%
        mutate(first_treat = ifelse(id == "a", 7, 0))
      
      data <- data %>%
        mutate(id_code = as.factor(id) %>% as.numeric())
      
      cs <- att_gt(yname = "outcome",
                   gname = "first_treat",
                   idname = "id_code",
                   tname = "date",
                   xformla = ~1,
                   data = data,
                   control_group = "notyettreated",
                   est_method = "reg")
      
      event <- aggte(cs, type = "dynamic")
      
      
      critical = event$crit.val.egt[[1]]
      
      new_results <- tibble(
        estimator = rep("cs", 11),
        period = event$egt,
        point = event$att.egt,
        se = event$se.egt,
        round = NA
      ) %>%
        mutate(
          lb = point - critical * se,
          ub = point + critical * se
        )
      
      new_results <- new_results %>%
        add_row(
          estimator = "cs",
          period = -6,
          point = 0,
          se = 0,
          lb = 0,
          ub = 0,
          round = NA
        ) %>%
        arrange(period)
      
      results <- results %>%
        rbind(new_results)
      
      
      # Run CS1 and store results
      data <- data %>%
        mutate(first_treat = ifelse(id == "a", 6, 0))
      
      data <- data %>%
        mutate(id_code = as.factor(id) %>% as.numeric())
      
      cs <- att_gt(yname = "outcome",
                   gname = "first_treat",
                   idname = "id_code",
                   tname = "date",
                   xformla = ~1,
                   data = data,
                   control_group = "notyettreated",
                   est_method = "reg")
      
      event <- aggte(cs, type = "dynamic")
      
      
      critical = event$crit.val.egt[[1]]
      
      new_results <- tibble(
        estimator = rep("cs1", 11),
        period = -5:5,
        point = event$att.egt,
        se = event$se.egt,
        round = NA
      ) %>%
        mutate(
          lb = point - critical * se,
          ub = point + critical * se
        )
      
      new_results <- new_results %>%
        add_row(
          estimator = "cs1",
          period = -6,
          point = 0,
          se = 0,
          lb = 0,
          ub = 0,
          round = NA
        ) %>%
        arrange(period)
      
      results <- results %>%
        rbind(new_results)
      
      
      # Run CS2 and store results
      data <- data %>%
        mutate(first_treat = ifelse(id == "a", 5, 0))
      
      data <- data %>%
        mutate(id_code = as.factor(id) %>% as.numeric())
      
      cs <- att_gt(yname = "outcome",
                   gname = "first_treat",
                   idname = "id_code",
                   tname = "date",
                   xformla = ~1,
                   data = data,
                   control_group = "notyettreated",
                   est_method = "reg")
      
      event <- aggte(cs, type = "dynamic")
      
      
      critical = event$crit.val.egt[[1]]
      
      new_results <- tibble(
        estimator = rep("cs2", 11),
        period = -5:5,
        point = event$att.egt,
        se = event$se.egt,
        round = NA
      ) %>%
        mutate(
          lb = point - critical * se,
          ub = point + critical * se
        )
      
      new_results <- new_results %>%
        add_row(
          estimator = "cs2",
          period = -6,
          point = 0,
          se = 0,
          lb = 0,
          ub = 0,
          round = NA
        ) %>%
        arrange(period)
      
      results <- results %>%
        rbind(new_results)
      

      # Update round variable
      if (i == 1){
        results["round"] <- 1
      } else {
        results <- results %>%
          mutate(round = ifelse(is.na(round), i, round))
      }
    }
    
    return(results)
    
    
  }
  
  
# Simulation--------------------------------------------------------------------
  
  # shock: (5, -5)
  # Simulate the data
  shock <- c(rep(0,5), 5, -5, rep(0,5))  
  results <- simulate(shock, rep = 1000)
  
  

  # Create variable for error
  results <- results %>%
    left_join(
      tibble(
        period = -6:5,
        shock = shock
      ),
      by = "period"
    )
  
  results <- results %>%
    mutate(diff_ub = shock - ub,
           diff_lb = shock - lb,
           diff_prod = diff_ub * diff_lb,
           error = ifelse(diff_prod <= 0, 0, 1))
  
  # Average point estimates
  average <- results %>%
    group_by(estimator, period) %>%
    summarize(point = mean(point),
              error = mean(error))
  
  
  results %>%
    filter(estimator == "ols") %>%
    ggplot() + 
    geom_line(aes(period, point, group = round), alpha = 0.04) +
    geom_line(aes(period, point), data = average %>% filter(estimator == "ols"), color = "red", size = 1) +
    geom_vline(xintercept = -1, color = "red") +
    geom_vline(xintercept = 0, color = "red") +
    scale_x_continuous(breaks = -6:5, minor_breaks = 0) +
    scale_y_continuous(breaks = seq(-30, 30, by = 5), minor_breaks = 0) +
    ggtitle("OLS event study plot") +
    xlab("Period") +
    ylab("Coefficients") +
    theme_bw()
    
  ggsave("event_ols.png", path = path, width = 4, height = 2.5)
  
  results %>%
    filter(estimator == "cs") %>%
    ggplot() + 
    geom_line(aes(period, point, group = round), alpha = 0.04) +
    geom_line(aes(period, point), data = average %>% filter(estimator == "cs"), color = "blue", size = 1) +
    geom_vline(xintercept = -1, color = "blue") +
    geom_vline(xintercept = 0, color = "blue") +
    scale_x_continuous(breaks = -6:5, minor_breaks = 0) +
    scale_y_continuous(breaks = seq(-30, 30, by = 5), minor_breaks = 0) +
    ggtitle("CS with first treatment date = 0") +
    xlab("Period") +
    ylab("Coefficients") +
    theme_bw()
 
  ggsave("event_cs.png", path = path, width = 4, height = 2.5)
  
  results %>%
    filter(estimator == "cs1") %>%
    ggplot() + 
    geom_line(aes(period, point, group = round), alpha = 0.04) +
    geom_line(aes(period, point), data = average %>% filter(estimator == "cs1"), color = "green", size = 1) +
    geom_vline(xintercept = -1, color = "green") +
    geom_vline(xintercept = 0, color = "green") +
    scale_x_continuous(breaks = -6:5, minor_breaks = 0) +
    scale_y_continuous(breaks = seq(-30, 30, by = 5), minor_breaks = 0) +
    ggtitle("CS with first treatment date = -1") +
    xlab("Period") +
    ylab("Coefficients") +
    theme_bw()

  ggsave("event_cs1.png", path = path, width = 4, height = 2.5)
  
  results %>%
    filter(estimator == "cs2") %>%
    ggplot() + 
    geom_line(aes(period, point, group = round), alpha = 0.04) +
    geom_line(aes(period, point), data = average %>% filter(estimator == "cs2"), color = "purple", size = 1) +
    geom_vline(xintercept = -1, color = "purple") +
    geom_vline(xintercept = 0, color = "purple") +
    scale_x_continuous(breaks = -6:5, minor_breaks = 0) +
    scale_y_continuous(breaks = seq(-30, 30, by = 5), minor_breaks = 0) +
    ggtitle("CS with first treatment date = -2") +
    xlab("Period") +
    ylab("Coefficients") +
    theme_bw()

  ggsave("event_cs2.png", path = path, width = 4, height = 2.5)
  
  average %>%
    ggplot() +
    geom_line(aes(period, error, color = estimator), size = 1) +
    geom_vline(xintercept = -1, color = "red")+
    geom_vline(xintercept = 0, color = "red") +
    theme_bw() +
    scale_x_continuous(breaks = -6:5, minor_breaks = 0) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.2), minor_breaks = seq(0, 1, by = 0.1)) +
    scale_color_manual(values = c("blue", "green", "purple", "orange")) +
    theme(legend.position = 'bottom') +
    ggtitle("Comparison of error rate")
    
  ggsave("event_error.png", path = path, width = 4, height = 2.5)
  
  
  
  
  
  
  