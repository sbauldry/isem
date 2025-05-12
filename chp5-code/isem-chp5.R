### Purpose: Prepare Chapter 5 examples
### Author:  S Bauldry
### Date:    Feb 13, 2025

setwd("~/desktop")
library(tidyverse)
library(lavaan)
library(ggpubr)


### General simulation parameters
set.seed(579111317)
M <- 500
N <- 1000


### Simulation 1: confounders of the X -> M and M -> Y relationships
sim1 <- data.frame( sim = 1:M, ie = numeric(M), pm = numeric(M) )
for(i in 1:M) {
  # data generation
  u1 <- rnorm(N, 0, 1)
  u2 <- rnorm(N, 0, 1)
  x  <- u1 + rnorm(N, 0, 1)
  m1 <- x + u1 + u2 + rnorm(N, 0, 1)
  y  <- m1 + x + u2 + rnorm(N, 0, 1)
  d  <- data.frame( cbind( x, m1, y) )
  
  # define and fit mediation model, store estimate of IE
  mod <- '
    m1 ~  b1*x
    y  ~  b2*m1 + b3*x
    ie := b1*b2
    pm := (b1*b2)/(b1*b2 + b3)
  '
  fit <- sem(mod, data = d)
  ests <- parameterEstimates(fit)
  sim1$ie[i] <- ests[ests$label == "ie", "est"]
  sim1$pm[i] <- ests[ests$label == "pm", "est"]
}

# plot results
simfig <- function(d, eie, tie, epm, tpm, ielb, ieub, pmlb, pmub) {
  fa <- ggplot(d, aes(x = ie)) +
    geom_density(fill = "gray", alpha = 0.7, color = NA) +
    geom_vline(xintercept = eie) +
    geom_vline(xintercept = tie, linetype = "dashed") +
    scale_x_continuous(name = "indirect effect", limits = c(ielb, ieub), breaks = seq(ielb, ieub, 0.5)) +
    scale_y_continuous(name = "density") +
    annotate("text", x = tie, y = max(density(d$ie)$y)*0.8, label = "true value", hjust = -0.1, size = 6) +
    annotate("text", x = eie, y = max(density(d$ie)$y)*0.6, label = "mean value", hjust = 1.1, size = 6) +
    theme_light() +
    theme(
      axis.text  = element_text(size = 16),
      axis.title = element_text(size = 18)
    )
  
  fb <- ggplot(d, aes(x = pm)) +
    geom_density(fill = "gray", alpha = 0.7, color = NA) +
    geom_vline(xintercept = epm) +
    geom_vline(xintercept = tpm, linetype = "dashed") +
    scale_x_continuous(name = "proportion mediated", limits = c(pmlb, pmub)) +
    scale_y_continuous(name = "density") +
    annotate("text", x = tpm, y = max(density(d$pm)$y)*0.8, label = "true value", hjust = -0.1, size = 6) +
    annotate("text", x = epm, y = max(density(d$pm)$y)*0.6, label = "mean value", hjust = 1.1, size = 6) +
    theme_light() +
    theme(
      axis.text  = element_text(size = 16),
      axis.title = element_text(size = 18)
    )
  
  f <- ggarrange(fa, fb)
  return(f)
}

f54 <- simfig(sim1, mean(sim1$ie), 1, mean(sim1$pm), 0.5, 1.0, 2.3, 0.5, 1.0)
ggsave("isem-f54.pdf", f54)


### Simulation 2: confounder of the M -> Y relationship
sim2 <- data.frame( sim = 1:M, ie = numeric(M), pm = numeric(M) )
for(i in 1:M) {
  # data generation
  u  <- rnorm(N, 0, 1)
  x  <- rbinom(N, 1, 0.5)
  m1 <- x + u + rnorm(N, 0, 1)
  y  <- m1 + x + u + rnorm(N, 0, 1)
  d  <- data.frame( cbind( x, m1, y) )
  
  # define and fit mediation model, store estimate of IE
  mod <- '
    m1 ~  b1*x
    y  ~  b2*m1 + b3*x
    ie := b1*b2
    pm := (b1*b2)/(b1*b2 + b3)
  '
  fit <- sem(mod, data = d)
  ests <- parameterEstimates(fit)
  sim2$ie[i] <- ests[ests$label == "ie", "est"]
  sim2$pm[i] <- ests[ests$label == "pm", "est"]
}

f55 <- simfig(sim2, mean(sim2$ie), 1, mean(sim2$pm), 0.5, 1.0, 2.3, 0.5, 1.0)
ggsave("isem-f55.pdf", f55)


### Simulation 3: multiple related mediators
sim3 <- data.frame( sim = 1:M, ie = numeric(M), pm = numeric(M) )
for(i in 1:M) {
  # data generation
  u  <- rnorm(N, 0, 1)
  x  <- rnorm(N, 0, 1)
  m1 <- x + u + rnorm(N, 0, 1)
  m2 <- x + u + rnorm(N, 0, 1)
  y  <- m1 + m2 + x + rnorm(N, 0, 1)
  d  <- data.frame( cbind( x, m1, m2, y) )
  
  # define and fit mediation model, store estimate of IE
  mod <- '
    m1 ~  b1*x
    y  ~  b2*m1 + b3*x
    ie := b1*b2
    pm := (b1*b2)/(b1*b2 + b3)
  '
  fit <- sem(mod, data = d)
  ests <- parameterEstimates(fit)
  sim3$ie[i] <- ests[ests$label == "ie", "est"]
  sim3$pm[i] <- ests[ests$label == "pm", "est"]
}
summary(sim3)

f56 <- simfig(sim3, mean(sim3$ie), 1, mean(sim3$pm), 0.33, 1.0, 2.3, 0.25, 0.6)
ggsave("isem-f56.pdf", f56)


### Simulation 4: causal structure among mediators
sim4 <- data.frame( sim = 1:M, ie1 = numeric(M), pm1 = numeric(M), ie2 = numeric(M), pm2 = numeric(M) )
for(i in 1:M) {
  # data generation
  x  <- rnorm(N, 0, 1)
  m1 <- x + rnorm(N, 0, 1)
  m2 <- x + m1 + rnorm(N, 0, 1)
  y  <- m1 + m2 + x + rnorm(N, 0, 1)
  d  <- data.frame( cbind( x, m1, m2, y) )
  
  # define and fit mediation model, store estimate of IE
  mod <- '
    m1  ~  b1*x
    m2  ~  b4*x
    y   ~  b2*m1 + b5*m2 + b3*x
    ie1 := b1*b2
    ie2 := b4*b5
    pm1 := (b1*b2)/(b1*b2 + b4*b5 + b3)
    pm2 := (b4*b5)/(b1*b2 + b4*b5 + b3)
  '
  fit <- sem(mod, data = d)
  ests <- parameterEstimates(fit)
  sim4$ie1[i] <- ests[ests$label == "ie1", "est"]
  sim4$pm1[i] <- ests[ests$label == "pm1", "est"]
  sim4$ie2[i] <- ests[ests$label == "ie2", "est"]
  sim4$pm2[i] <- ests[ests$label == "pm2", "est"]
}
summary(sim4)

simfig2 <- function(d, eie1, tie1, epm1, tpm1, eie2, tie2, epm2, tpm2) {
  fa <- ggplot(d, aes(x = ie1)) +
    geom_density(fill = "gray", alpha = 0.7, color = NA) +
    geom_vline(xintercept = eie1) +
    geom_vline(xintercept = tie1, linetype = "dashed") +
    scale_x_continuous(name = "indirect effect", limits = c(0.7, 2.3)) +
    scale_y_continuous(name = "density") +
    annotate("text", x = tie1, y = max(density(d$ie1)$y)*0.8, label = "true value", hjust = 1.1, size = 4) +
    annotate("text", x = eie1, y = max(density(d$ie1)$y)*0.6, label = "mean value", hjust = -0.1, size = 4) +
    theme_light() +
    theme(
      axis.text  = element_text(size = 16),
      axis.title = element_text(size = 18)
    )
  
  fb <- ggplot(d, aes(x = pm1)) +
    geom_density(fill = "gray", alpha = 0.7, color = NA) +
    geom_vline(xintercept = epm1) +
    geom_vline(xintercept = tpm1, linetype = "dashed") +
    scale_x_continuous(name = "proportion mediated", limits = c(0.2, 0.6)) +
    scale_y_continuous(name = "density") +
    annotate("text", x = tpm1, y = max(density(d$pm1)$y)*0.8, label = "true value", hjust = 1.1, size = 4) +
    annotate("text", x = epm1, y = max(density(d$pm1)$y)*0.6, label = "mean value", hjust = -0.1, size = 4) +
    theme_light() +
    theme(
      axis.text  = element_text(size = 16),
      axis.title = element_text(size = 18)
    )
  
  fc <- ggplot(d, aes(x = ie2)) +
    geom_density(fill = "gray", alpha = 0.7, color = NA) +
    geom_vline(xintercept = eie2) +
    geom_vline(xintercept = tie2, linetype = "dashed") +
    scale_x_continuous(name = "indirect effect", limits = c(0.7, 2.3)) +
    scale_y_continuous(name = "density") +
    annotate("text", x = tie2, y = max(density(d$ie2)$y)*0.8, label = "true value", hjust = -0.1, size = 4) +
    annotate("text", x = eie2, y = max(density(d$ie2)$y)*0.6, label = "mean value", hjust = 1.1, size = 4) +
    theme_light() +
    theme(
      axis.text  = element_text(size = 16),
      axis.title = element_text(size = 18)
    )
  
  fd <- ggplot(d, aes(x = pm2)) +
    geom_density(fill = "gray", alpha = 0.7, color = NA) +
    geom_vline(xintercept = epm2) +
    geom_vline(xintercept = tpm2, linetype = "dashed") +
    scale_x_continuous(name = "proportion mediated", limits = c(0.2, 0.6)) +
    scale_y_continuous(name = "density") +
    annotate("text", x = tpm2, y = max(density(d$pm2)$y)*0.8, label = "true value", hjust = -0.1, size = 4) +
    annotate("text", x = epm2, y = max(density(d$pm2)$y)*0.6, label = "mean value", hjust = 1.1, size = 4) +
    theme_light() +
    theme(
      axis.text  = element_text(size = 16),
      axis.title = element_text(size = 18)
    )
  
  f <- ggarrange(fa, fb, fc, fd)
  return(f)
}

f57 <- simfig2(sim4, mean(sim4$ie1), 2, mean(sim4$pm1), 0.5, mean(sim4$ie2), 1, mean(sim4$pm2), 0.25)
ggsave("isem-f57.pdf", f57)



### Estimates and CIs for empirical example
d1 <- read_csv("isem-nsduh-data.csv")

m1 <- '
  PD =~ k6nrv + k6rst + k6dep + k6hop + k6eff + k6wth
  PD  ~ a*mar + fem + blk + hsp
  drk ~ b*PD + mar + fem + blk + hsp
  ie := a*b
'
fit1 <- sem(m1, data = d1)
summary(fit1, ci = T)

m2 <- lm(mar ~ fem + blk + hsp, data = d1)
summary(m2)

### Figure 5.8 -- sensitivity analysis
gamma  <- seq(1, 3, length.out = 100)
delta  <- seq(0.01, 0.10, length.out = 100)
grid   <- expand.grid(gamma = gamma, delta = delta)
grid$B <- grid$gamma*grid$delta

f58 <- ggplot(grid, aes(x = gamma, y = delta, z = B)) +
  geom_contour_filled(breaks = seq(0, 0.3, by = 0.03), alpha = 0.8) +
  geom_contour(breaks = c(0.08), color = "darkred", linetype = "dashed", linewidth = 1) +
  geom_contour(breaks = c(0.17), color = "darkblue", linetype = "dashed", linewidth = 1) +
  theme_light() +
  theme(
    axis.title = element_text(size = 16),     
    axis.text = element_text(size = 18),      
    legend.title = element_text(size = 18),   
    legend.text = element_text(size = 16)     
  )
f58
ggsave("isem-f58.pdf", f58, width = 10, height = 9)

