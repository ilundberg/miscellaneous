
# Time use on Christmas and Christmas Eve

setwd("/Users/iandl/Dropbox/christmas_time_use")

# Load packages
library(ipumsr)
library(tidyverse)
library(foreach)
library(doParallel)
library(doRNG)

# Set the seed
set.seed(08544)

# Set number of bootstrap samples for confidence intervals
bs.samps <- 10000

# Make a computing cluster to draw bootstrap samples in parallel
cl <- makeCluster(4)
registerDoParallel(cl)

ddi <- read_ipums_ddi("atus_00011.xml")
data <- read_ipums_micro(ddi)

# Effect of Christmas
christmas_data <- data %>%
  rename_all(.funs = tolower) %>%
  mutate(md = date - trunc(date / 10000)*10000,
         # Code all dates in Dec. and Jan. as 2018-2019
         # (a coding device to pool across years)
         md_date = as.Date(case_when(md <= 131 ~ paste0("2019-01-",md - 100),
                                     md >= 1201 ~ paste0("2018-12-",md - 1200)))) %>%
  # Restrict to tight window of dates
  filter(!is.na(md_date) & 
           md_date >= as.Date("2018-12-10") & 
           md_date <= as.Date("2019-1-10")) %>%
  # Identify Christmas
  mutate(christmas = md_date == "2018-12-25") %>%
  mutate(sex = case_when(sex == 1 ~ "Men",
                         sex == 2 ~ "Women"))

get_plot <- function(outcomeName, label, filename) {
  # Rename outcome variable to "outcome"
  renamed <- christmas_data %>%
    rename_at(vars(outcomeName), function(x) "outcome")
  # Bootstrap a CI for Christmas
  bs <- foreach(i = 1:bs.samps, .combine = "rbind", .packages = "tidyverse") %dorng% {
    renamed %>%
      filter(christmas) %>%
      group_by(sex) %>%
      sample_frac(1, replace = T, weight = wt06) %>%
      summarize(estimate = mean(outcome))
  }
  ci <- bs %>%
    group_by(sex) %>%
    summarize(ci.min = quantile(estimate, .025),
              ci.max = quantile(estimate, .975))
  renamed %>%
    group_by(md_date, christmas, sex) %>%
    summarize(outcome = weighted.mean(outcome,
                                      w = wt06)) %>%
    ggplot() +
    geom_point(aes(x = md_date, y = outcome,
                   color = christmas)) +
    # Add confidence interval on Christmas
    geom_errorbar(data = ci,
                  aes(x = as.Date("2018-12-25"),
                      ymin = ci.min, ymax = ci.max),
                  color = "red") +
    scale_color_manual(values = c("seagreen4","red")) +
    theme(legend.position = "none") +
    facet_wrap(~sex) +
    ylab("Minutes (total in day)") +
    xlab("Date\n\nSource: American Time Use Survey. Estimates weighted.\nSampling frame is civilian non-institutional\npopulation ages 15+ in occupied U.S. households.") +
    scale_x_date(breaks = as.Date(c("2018-12-10","2018-12-25","2019-1-10")),
                 date_labels = "%b %e") +
    ggtitle(bquote("Time spent"~bold(.(label)))) +
    theme_bw() +
    theme(strip.background = element_rect(fill = "seagreen4"),
          strip.text = element_text(color = "white", face = "bold"),
          legend.position = "none",
          panel.grid.minor = element_blank(),
          panel.spacing = unit(1.5, "lines"),
          plot.title = element_text(hjust = .5),
          plot.margin = unit(c(5.5,11,5.5,5.5),"pt")) +
    ggsave(filename, height = 3.5, width = 5)
}

get_plot(outcomeName = "food_drink_preparation",
         label = "preparing food and drink",
         filename = "cooking.pdf")
get_plot(outcomeName = "act_food",
         label = "eating and drinking",
         filename = "eating.pdf")
