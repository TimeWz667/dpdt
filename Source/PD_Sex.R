rm(list=ls())
library(dpdt)
library(odin)


## Prepare input ----
demo <- fetch_demography(folder = "ByCountry/United Kingdom")
sim <- as_sim_sex(demo, 2000, 2050)
pars <- as_pars_sex(sim, pkg = "odin")


## Construct model ----
f <- system.file("example/PD_Sex.R", package = "dpdt")
model <- odin::odin(f)
compiled <- model(user=pars)


## Simulate ----
times <- seq(2000, 2050, 0.1)
ys <- compiled$run(times)
ys <- ys[times == round(times), ]
print(ys)

## Compare with data
library(ggplot2)
library(gridExtra)


res <- data.frame(ys)[c("t", "N")]
dat <- data.frame(t = sim$Year, N = sim$PopN_F + sim$PopN_M)
red <- (res$N - dat$N) / dat$N * 100
red <- data.frame(t = sim$Year, Red = red) 


g_all <- ggplot(data = res, aes(x = t, y = N / 1e6)) +
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


g_red <- ggplot(data = red, aes(x = t, y = Red)) +
  geom_point() + 
  scale_x_continuous("Year") +
  scale_y_continuous("Percentage of real data (%)", limits = c(-.1, .1)) +
  labs(title="Residuals") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


res <- rbind(
  data.frame(t = ys[, "t"], N = ys[, "NF"], Sex="Female"),
  data.frame(t = ys[, "t"], N = ys[, "NM"], Sex="Male")
)

dat <- rbind(
  data.frame(t = sim$Year, N = sim$PopN_F, Sex="Female"),
  data.frame(t = sim$Year, N = sim$PopN_M, Sex="Male")
)

red <- data.frame(
  t = res$t,
  Red = (res$N - dat$N) / dat$N * 100,
  Sex = dat$Sex
) 

g_sex <- ggplot(dat, aes(x = t, y = N / 1e6)) +
  geom_point(aes(colour = "Data")) + 
  geom_line(data = res, aes(colour = "Simulation")) + 
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

g_red_sex <- ggplot(data = red, aes(x = t, y = Red)) +
  geom_point() + 
  scale_x_continuous("Year") +
  scale_y_continuous("Percentage of real data (%)", limits = c(-.1, .1)) +
  facet_wrap(.~Sex) + 
  labs(title="Residuals") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


grid.arrange(g_all, g_sex, g_red, g_red_sex, 
             layout_matrix = cbind(c(1, 1, 3), c(2, 2, 4)))



