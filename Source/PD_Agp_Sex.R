rm(list=ls())
library(odin)
library(ggplot2)
library(gridExtra)


load(file="Output/SimDemo.rdata")

## Setting ----
country <- "Malawi"
pop <- sim_agp_sex[[country]]
year0 <- 2005
year1 <- 2050
i_year <- year0:year1 - 1999


## Prepare input ----
pars <- list(
  tt = year0:year1,
  br_f = pop$BirR_F[i_year],
  mr_f = pop$MigR_F[i_year, ],
  dr_f = pop$DeaR_F[i_year, ],
  F0 = pop$PopN_F[i_year[1], ],
  br_m = pop$BirR_M[i_year],
  mr_m = pop$MigR_M[i_year, ],
  dr_m = pop$DeaR_M[i_year, ],
  M0 = pop$PopN_M[i_year[1], ]
)


## Construct model ----
model_agp_sex <- odin::odin({
  deriv(AF[1]) <- NF * br_f_t + AF[i] * (mr_f_t[i] - dr_f_t[i] - 1/5)
  deriv(AF[2:(N_agp-1)]) <- AF[i] * (mr_f_t[i] - dr_f_t[i] - 1/5) + AF[i-1] * 1/5
  deriv(AF[N_agp]) <- AF[i] * (mr_f_t[i] - dr_f_t[i]) + AF[i-1] * 1/5
  initial(AF[1:N_agp]) <- F0[i] 
  dim(AF) <- N_agp
    
  deriv(AM[1]) <- NM * br_m_t + AM[i] * (mr_m_t[i] - dr_m_t[i] - 1/5)
  deriv(AM[2:(N_agp-1)]) <- AM[i] * (mr_m_t[i] - dr_m_t[i] - 1/5) + AM[i-1] * 1/5
  deriv(AM[N_agp]) <- AM[i] * (mr_m_t[i] - dr_m_t[i]) + AM[i-1] * 1/5
  initial(AM[1:N_agp]) <- M0[i] 
  dim(AM) <- N_agp
  
  output(NF) <- NF
  output(NM) <- NM
  output(N) <- NF + NM
  
  N_agp <- 16
  NF <- sum(AF)
  NM <- sum(AM)
  
  F0[] <- user()
  dim(F0) <- N_agp
  
  M0[] <- user()
  dim(M0) <- N_agp
  
  br_f_t <- interpolate(tt, br_f, "constant")
  mr_f_t[] <- interpolate(tt, mr_f, "constant")
  dim(mr_f_t) <- N_agp
  dr_f_t[] <- interpolate(tt, dr_f, "constant")
  dim(dr_f_t) <- N_agp
  
  br_m_t <- interpolate(tt, br_m, "constant")
  mr_m_t[] <- interpolate(tt, mr_m, "constant")
  dim(mr_m_t) <- N_agp
  dr_m_t[] <- interpolate(tt, dr_m, "constant")
  dim(dr_m_t) <- N_agp
  
  tt[] <- user()
  dim(tt) <- user()
  br_f[] <- user()
  dim(br_f) <- user()
  mr_f[, ] <- user()
  dim(mr_f) <- user()
  dr_f[, ] <- user()
  dim(dr_f) <- user()
  
  br_m[] <- user()
  dim(br_m) <- user()
  mr_m[, ] <- user()
  dim(mr_m) <- user()
  dr_m[, ] <- user()
  dim(dr_m) <- user()
})

cm_agp_sex <- model_agp_sex(user=pars)


## Simulate ----
times <- seq(year0, year1, .1)
ys <- cm_agp_sex$run(times)
ys <- ys[times == round(times), ]


## Compare with data
sim <- data.frame(t = ys[, "t"], N = ys[, "N"])
dat <- data.frame(t = year0:year1, N = rowSums(pop$PopN_F[i_year, ] + pop$PopN_M[i_year, ]))
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
  scale_y_continuous("Percentage of real data (%)", limits = c(-3, 3)) +
  labs(title="Residuals") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


sim <- rbind(
  data.frame(t = ys[, "t"], N = ys[, "NF"], Sex="Female"),
  data.frame(t = ys[, "t"], N = ys[, "NM"], Sex="Male")
)

dat <- rbind(
  data.frame(t = year0:year1, N = rowSums(pop$PopN_F[i_year, ]), Sex="Female"),
  data.frame(t = year0:year1, N = rowSums(pop$PopN_M[i_year, ]), Sex="Male")
)

red <- data.frame(
  t = sim$t,
  N = (sim$N - dat$N) / dat$N * 100,
  Sex = sim$Sex
) 

g_sex <- ggplot(data = sim, aes(x = t, y = N / 1e6)) +
  geom_line(aes(colour = "Simulation")) + 
  geom_point(data=dat, aes(colour = "Data")) + 
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
  scale_y_continuous("Percentage of real data (%)", limits = c(-10, 10)) +
  facet_wrap(.~Sex) + 
  labs(title="Residuals") +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


grid.arrange(g_all, g_sex, g_red, g_red_sex, 
             layout_matrix = cbind(c(1, 1, 3), c(2, 2, 4)))
