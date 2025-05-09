## Scale variables from 0-1

zero.one <- function(x) {
  min.x <- min(x, na.rm = TRUE)
  max.x <- max(x - min.x, na.rm = TRUE)
  return((x - min.x) / max.x)
}

### Dependencies
required_packages <- c("dplyr", "ggplot2", "nnet", "lavaan", "cowplot", "readstata13", "brms", "tidyverse", "modelr", "tidybayes", "simplecolors", "reshape2", "tidybayes")

# Install and load packages if they are not already installed
for (package in required_packages) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# General GG theme
ggtheme <- theme(
  plot.title = element_text(colour = "black", size = 10),
  axis.text.x = element_text(size = 10, colour = "#535353", face = "bold"),
  axis.text.y = element_text(size = 10, colour = "#535353", face = "bold"),
  axis.title = element_text(size = 10, colour = "#535353", face = "bold"),
  axis.title.y = element_text(size = 10, colour = "black", face = "bold", vjust = 1.5),
  axis.ticks.x = element_line(size = 0.5, colour = "#535353"), # Changed x-axis ticks
  axis.ticks.y = element_blank(),
  strip.text.x = element_text(size = 10),
  panel.grid.major = element_line(colour = "#D0D0D0", size = 0.25),
  panel.background = element_rect(fill = "white"),
  legend.text = element_text(size = 10),
  legend.title = element_text(size = 10),
)


logit_linear_models <- function(data = ANES_2020, dependent.variable = "trans.military",
                                independent.variables = c("female", "age", "college", "income", "authoritarianism"),
                                race = "white",
                                model = "logit_model",
                                chains = 3, cores = 8,
                                seed = 1234, iter = 1000) {
  if (!require(brms)) {
    install.packages("brms")
    library(brms)
  }

  model_type <- ifelse(model == "logit_model", "bernoulli", "gaussian")

  # Misc Functions
  firstL <- function(x) {
    sapply(x, function(y) {
      paste(toupper(substring(y, 1, 1)), substring(y, 2, nchar(y)), sep = "")
    })
  }

  print(paste("This returns three models, one for each group. The DV is", dependent.variable, "with", iter, "posterior draws. It takes a bit"))
  # The data
  group <- race
  data_t <- filter(data, race == group) %>% mutate(authoritarianism_2 = authoritarianism^2)
  # The Model
  formula <- paste(dependent.variable, paste(c(independent.variables, "authoritarianism_2"), collapse = " + "), sep = " ~ ")
  suppressWarnings({
    fit <- brm(formula,
      family = model_type,
      data = data_t,
      chains = chains,
      cores = cores,
      seed = seed,
      iter = iter,
      refresh = 0
    )
    print(fit$data %>% dim())
    # The predictions
    predData <- data %>%
      tibble() %>%
      subset(select = c(independent.variables, dependent.variable)) %>%
      na.omit() %>%
      data_grid(
        female = mean(female), age = mean(age),
        college = mean(college), income = mean(income),
        authoritarianism = seq_range(authoritarianism, n = 5)
      ) %>%
      mutate(authoritarianism_2 = authoritarianism^2) %>%
      add_epred_draws(fit)
    # The plot
    plot <- predData %>% ggplot(aes(x = authoritarianism, y = .epred, color = "black")) +
      stat_summary(fun = median, geom = "point", size = 3, color = "black", alpha = 0.8) +
      ggtheme +
      scale_x_continuous(breaks = seq(0, 1, by = 0.5)) +
      geom_point(size = 1.5, alpha = 0.05, color = "black") +
      ggtitle(firstL(race)) +
      theme(legend.key = element_blank())
    return(plot)
  })
}



predPlot <- function(RACE = c("white", "black", "latino"), plot.title = "trans.military", dependent.variable = "trans.military",
                     data = ANES_2020, y.limits = c(0, 0.5), plot.y = "logit", ...) {
  for (race in RACE) {
    assign(paste0("race_", race), logit_linear_models(ANES_2020, dependent.variable = dependent.variable, race = race, ...))
  }
  # Construct the cowplot

  y.label <- ifelse(plot.y == "logit", paste0("Pr(", plot.title, ")"), plot.title)

  row.grid <- plot_grid(
    race_white + xlab("") + theme(legend.position = "none") + ylab(y.label) + ylim(y.limits),
    race_black + theme(legend.position = "none") + ylab("") + xlab("Authoritarianism") + ylim(y.limits),
    race_latino + theme(legend.position = "none") + xlab("") + ylab("") + ylim(y.limits),
    rel_widths = c(1, 1, 1),
    ncol = 3,
    align = "v",
    axis = "b"
  )

  title <- ggdraw() +
    draw_label(
      plot.title,
      fontface = "bold",
      x = 0,
      hjust = 0,
      size = 20,
    ) +
    theme(
      # add margin on the left of the drawing canvas,
      # so title is aligned with left edge of first plot
      plot.margin = margin(0, 0, 0, 7)
    )

  plot <- plot_grid(title, row.grid, ncol = 1, rel_heights = c(0.1, 1))

  return(plot)
}

multinomial_model <- function(data = ANES_full, dependent.variable = "pid3",
                              independent.variables = c("female", "age", "college", "income", "authoritarianism"),
                              race = "white",
                              year.period = "2008 and after",
                              chains = 3, cores = 8,
                              seed = 1234, iter = 1000) {
  if (!require(brms)) {
    install.packages("brms")
    library(brms)
  }

  # Misc Functions
  firstL <- function(x) {
    sapply(x, function(y) {
      paste(toupper(substring(y, 1, 1)), substring(y, 2, nchar(y)), sep = "")
    })
  }
}
