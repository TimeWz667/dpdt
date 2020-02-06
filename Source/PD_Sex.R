rm(list=ls())
library(odin)

load(file="Output/SimDemo.rdata")


## Setting ----
country <- "Malawi"
pop <- sim_sex[[country]]
year0 <- 2005
year1 <- 2050
i_year <- year0:year1 - 1999


## Prepare input ----
pars <- list(
  tt = year0:year1,
  br_f = pop$BirR_F[i_year],
  mr_f = pop$MigR_F[i_year],
  dr_f = pop$DeaR_F[i_year],
  F0 = pop$PopN_F[i_year[1]],
  br_m = pop$BirR_M[i_year],
  mr_m = pop$MigR_M[i_year],
  dr_m = pop$DeaR_M[i_year],
  M0 = pop$PopN_M[i_year[1]]
)


## Construct model ----
f <- system.file("example/PD_Sex.R", package = "dpdt")
model_sex <- odin::odin(f)

cm_sex <- model_sex(user=pars)


## Simulate ----
times <- seq(year0, year1, 0.1)
ys <- cm_sex$run(times)
ys <- ys[times == round(times), ]


## Compare with data
library(ggplot2)
library(gridExtra)


sim <- data.frame(ys)[c("t", "N")]
dat <- data.frame(t = year0:year1, N = pop$PopN_F[i_year] + pop$PopN_M[i_year])
red <- (sim - dat) / dat * 100
red$t <- dat$t


g_all <- ggplot(data = sim, aes(x = t, y = N / 1e6)) +
  geom_line(aes(colour = "Simulation")) + 
  geom_point(data=dat, aes(colour = "Data")) + 
  scale_x_continuous("Year") +
  scale_y_continuous("Total Population (millions)") +
  scale_color_discrete("Dynamics") + 
  expand_limits(y = c(0, NA)) +
  labs(title="Population dynamics, all") +
  theme_bw() + 
  theme(legend.position = c(1, 0), 
        legend.justification = c(1, 0), 
        axis.text.x = element_text(angle = 90, hjust = 1))


g_red <- ggplot(data = red, aes(x = t, y = N)) +
  geom_point() + 
  scale_x_continuous("Year") +
  scale_y_continuous("Percentage of real data (%)", limits = c(-.1, .1)) +
  labs(title="Residuals") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


sim <- rbind(
  data.frame(t = ys[, "t"], N = ys[, "NF"], Sex="Female"),
  data.frame(t = ys[, "t"], N = ys[, "NM"], Sex="Male")
)

dat <- rbind(
  data.frame(t = year0:year1, N = pop$PopN_F[i_year], Sex="Female"),
  data.frame(t = year0:year1, N = pop$PopN_M[i_year], Sex="Male")
)

red <- data.frame(
  t = sim$t,
  N = (sim$N - dat$N) / dat$N * 100,
  Sex = sim$Sex
) 

g_sex <- ggplot(dat, aes(x = t, y = N / 1e6)) +
  geom_point(aes(colour = "Data")) + 
  geom_line(data = sim, aes(colour = "Simulation")) + 
  scale_x_continuous("Year") +
  scale_y_continuous("Total Population (millions)") +
  scale_color_discrete("Dynamics") + 
  expand_limits(y = c(0, NA)) +
  facet_wrap(.~Sex) + 
  labs(title="Population dynamics, sex") +
  theme_bw() + 
  theme(legend.position = c(1, 0), 
        legend.justification = c(1, 0), 
        axis.text.x = element_text(angle = 90, hjust = 1))

g_red_sex <- ggplot(data = red, aes(x = t, y = N)) +
  geom_point() + 
  scale_x_continuous("Year") +
  scale_y_continuous("Percentage of real data (%)", limits = c(-.1, .1)) +
  facet_wrap(.~Sex) + 
  labs(title="Residuals") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


grid.arrange(g_all, g_sex, g_red, g_red_sex, 
             layout_matrix = cbind(c(1, 1, 3), c(2, 2, 4)))



