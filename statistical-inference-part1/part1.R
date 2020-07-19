## ----self_running, eval=FALSE, include = FALSE--------------------------------
## knitr::purl("./part1.Rmd")
## source("./part1.R")


## ----loading_packages, echo = F, eval = T-------------------------------------
## loading all packages
pacman::p_load(knitr,
               dplyr,
               ggplot2,
               ggthemr)
ggthemr::ggthemr("fresh")

set.seed(1702) # for greater stability while exploring where no one (except my peers) has gone before


## ----sample, fig.align="center", echo = F, eval = T---------------------------
sample_num <- 1000
sample <- rexp(sample_num, rate = 0.2)
sample %>% data.frame(num = .) %>%
    ggplot(aes(x = num)) +
    geom_histogram(bins = 30, col = ggthemr::swatch()[8])
means <- cumsum(sample)/(1:sample_num)


## ----lln_plot, fig.align="center", fig.cap="The simulated exponential distribution should be asymptotic at 5, as $\\lambda$ = 1/5", echo = F, eval = T, warning = F, message = F----
## plotting to see if exp distri follows law of large numbers
means <- cumsum(sample)/(1:sample_num)
asymp_point <- round(mean(means), digits=1)
means %>%
    data.frame(x=1:sim_num, y = .) %>%
    ggplot(aes(x=x, y=y)) +
    geom_point() +
    geom_hline(yintercept = mean(means)) +
    xlab("N_i") + ylab("Average")
#    knitr::kable_styling(caption="The simulated exponential distribution should be asymptotic at 5, as our \lambda = 1/5")


## ----large_scale_exploration_largescale, fig.align="center", echo = F, eval = T, warning = F, message = F, error = F----
sample_averaging_num  <- 40
sim_num <- 1000
mns = NULL
for (i in 1:sim_num) mns = c(mns, mean(rexp(sample_averaging_num, rate = 0.2)))                # to generate exp dist
mns_SE <- (1/0.2)/sqrt(length(mns)) # the Standard Error
mns %>%  data.frame(num = .)  %>%
ggplot(aes(x = num)) +
    geom_histogram(col = ggthemr::swatch()[8], bins = 30) +
    geom_vline(aes(xintercept = mean(mns), color = "mean")) +
    geom_vline(aes(xintercept = 5, color = "EV")) +
    geom_vline(aes(xintercept = 5+mns_SE, color = "SE")) +
    geom_vline(aes(xintercept = 5-mns_SE, color = "SE")) +
    scale_color_manual(name = "statistics", values = c(mean = ggthemr::swatch()[4], EV = ggthemr::swatch()[5], SE = ggthemr::swatch()[6]))
    theme(legend.position = "right")


## ----ref.label=knitr::all_labels(), echo=TRUE, eval=FALSE---------------------
## NA

