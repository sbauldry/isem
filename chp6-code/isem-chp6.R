### Purpose: Prepare Chapter 6 examples
### Author:  S Bauldry
### Date:    Mar 9, 2025

setwd("~/desktop")
library(tidyverse)
library(lavaan)
library(psych)
library(ggpubr)

d1 <- read_csv("isem-nsduh-data.csv")


### Figure 6.1 - distribution of days drinking
f61 <- ggplot(data = d1, mapping = aes(x = drk)) +
  geom_bar(aes(y = after_stat(prop), group = 1)) +
  scale_x_continuous(name = "number of days consumed alcohol in past month") +
  scale_y_continuous(name = "proportion", limits = c(0, 0.5)) +
  theme_light() +
  theme(
    axis.text  = element_text(size = 18),
    axis.title = element_text(size = 20)
  )
f61
ggsave("isem-f61.pdf", f61)

prop.table( table(d1$drk) )


### Figure 6.2 - illustration of underlying continuum and thresholds
x    <- seq(-7, 7, length.out = 1000)
y    <- dnorm(x, 0, 2)
d2   <- tibble(x, y)
tau  <- c(-3.5, -1.5, 2.2, 4)
ytau <- dnorm(tau, 0, 2)
d3   <- tibble(x = tau, y = 0, yend = ytau) 

f62 <- ggplot(d2, aes(x = x, y = y)) +
  geom_line() +
  geom_segment(data = d3, aes(x = x, y = y, xend = x, yend = yend), linetype = "dashed") +
  annotate("text", x = -4.2, y = 0.006, label = "none\nof the time") +
  annotate("text", x = -2.4, y = 0.006, label = "a little\nof the time") +
  annotate("text", x =  0.5, y = 0.006, label = "some\nof the time") +
  annotate("text", x =  3.0, y = 0.006, label = "most\nof the time") +
  annotate("text", x =  4.6, y = 0.006, label = "all\nof the time") +
  scale_x_continuous(breaks = c(-6, -3.5, -1.5, 2.2, 4, 6), labels = c("never", expression(tau[1]), expression(tau[2]), expression(tau[3]), expression(tau[4]), "always")) +
  labs(x = "how often nervous in past 30 days", y = "density") +
  theme_light() +
  theme(
    axis.text  = element_text(size = 18),
    axis.title = element_text(size = 20)
  )
f62
ggsave("isem-f62.pdf", f62)


### Table 6.1 - polychoric and pearson correlations
d3 <- d1 |> 
  select(k6nrv, k6rst, k6dep, k6hop, k6eff, k6wth) |>
  drop_na()
polychoric(d3) 
cor(d3)


### Calculating predicted probabilities
tau <- c(-0.62, 0.18, 1.01, 1.68)
pnorm( -tau )


### Figure 6.4 - predicted responses for nervous and depressed
prfig <- function(t1, t2, t3, t4, l1) {
  x <- seq(-3, 3, length.out = 500)
  p1 <- 1 - pnorm( -1*(t1) + l1*x ) 
  p2 <- pnorm( -1*(t1) + l1*x ) - pnorm( -1*(t2) + l1*x )
  p3 <- pnorm( -1*(t2) + l1*x )  - pnorm( -1*(t3) + l1*x )
  p4 <- pnorm( -1*(t3) + l1*x )  - pnorm( -1*(t4) + l1*x )
  p5 <- pnorm( -1*(t4) + l1*x )
  r = factor( rep( c("N", "L", "S", "M", "A"), each = length(x) ) )
  d <- tibble( x = rep(x, 5), p = c(p1, p2, p3, p4, p5), r = r)
  
  f <- ggplot(d, aes(x = x, y = p, group = r)) +
    geom_line() +
    labs(x = "latent psychological distress", y = "pred prob") +
    theme_light() +
    theme(
      axis.text  = element_text(size = 18),
      axis.title = element_text(size = 20)
    )
  return(f)
}

f64a <- prfig(-0.62, 0.18, 1.01, 1.68, 1) +
  annotate("text", x = -1.5, y = 0.75, label = "N", size = 5) +
  annotate("text", x = -1.5, y = 0.20, label = "L", size = 5) +
  annotate("text", x =  0.5, y = 0.35, label = "S", size = 5) +
  annotate("text", x =  2.0, y = 0.25, label = "M", size = 5) +
  annotate("text", x =  2.0, y = 0.55, label = "A", size = 5) +
  ggtitle("Panel A: Nervous")

f64b <- prfig(0.17, 0.78, 1.42, 1.95, 1.15) +
  annotate("text", x = -1.5, y = 0.93, label = "N", size = 5) +
  annotate("text", x = -1.5, y = 0.07, label = "L", size = 5) +
  annotate("text", x =  0.5, y = 0.27, label = "S", size = 5) +
  annotate("text", x =  2.0, y = 0.22, label = "M", size = 5) +
  annotate("text", x =  2.0, y = 0.55, label = "A", size = 5) +
  ggtitle("Panel B: Depressed")
f64b

f64 <- ggarrange(f64a, f64b)
ggsave("isem-f64.pdf", f64)


### Table 6.4 - marginal effects
me <- function(t1, t2, t3, t4, l1, a, b) {
  x <- c(a, b)
  p1 <- 1 - pnorm( -1*(t1) + l1*x ) 
  p2 <- pnorm( -1*(t1) + l1*x ) - pnorm( -1*(t2) + l1*x )
  p3 <- pnorm( -1*(t2) + l1*x )  - pnorm( -1*(t3) + l1*x )
  p4 <- pnorm( -1*(t3) + l1*x )  - pnorm( -1*(t4) + l1*x )
  p5 <- pnorm( -1*(t4) + l1*x )
  r = factor( rep( c("N", "L", "S", "M", "A"), each = length(x) ) )
  d <- tibble( pid = rep( c("p1", "p2"), 5), p = c(p1, p2, p3, p4, p5), r = r) |>
    pivot_wider(names_from = pid, values_from = p, id_cols = r) |>
    mutate(me = p2 - p1)
  me <- t(d[, 4])
  return(me)
}

menrv <- me(-0.62, 0.18, 1.01, 1.68, 1.00, -0.4, 0.4)
merst <- me(-0.25, 0.38, 1.09, 1.70, 1.00, -0.4, 0.4)
medep <- me( 0.17, 0.78, 1.42, 1.95, 1.15, -0.4, 0.4)
mehop <- me( 0.16, 0.77, 1.38, 1.90, 1.14, -0.4, 0.4)
meeff <- me(-0.24, 0.36, 0.94, 1.47, 1.03, -0.4, 0.4)
mewth <- me( 0.06, 0.66, 1.24, 1.73, 1.15, -0.4, 0.4)

me <- rbind(menrv, merst, medep, mehop, meeff, mewth)
round(me, 2)
