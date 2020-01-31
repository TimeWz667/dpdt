rm(list=ls())
library(odin)
library(ggplot2)
library(gridExtra)

load(file="Output/SimDemo.rdata")

## Setting ----
country <- "Malawi"
pop <- sim_agp[[country]]
year0 <- 2005
year1 <- 2035
i_year <- year0:year1 - 1999


## Prepare input ----
pars <- list(
  tt = year0:year1,
  br = pop$BirR_T[i_year],
  mr = pop$MigR_T[i_year, ],
  dr = pop$DeaR_T[i_year, ],
  A0 = pop$PopN_T[i_year[1], ]
)


## Construct model ----
model_agp <- odin::odin({
  deriv(A[1]) <- N * br_t + A[i] * (mr_t[i] - dr_t[i] - 1/5)
  deriv(A[2:N_agp]) <-  A[i] * (mr_t[i] - dr_t[i] - 1/5) + A[i-1] * 1/5
  # deriv(A[N_agp]) <-        A[i] * (mr_t[i] - dr_t[i]) +       A[i-1] * 1/5
  
  initial(A[]) <- A0[i] 
  dim(A) <- N_agp
  
  output(N) <- N
  
  N_agp <- 16
  N <- sum(A)
  
  A0[] <- user()
  dim(A0) <- N_agp
  
  br_t <- interpolate(tt, br, "constant")
  mr_t[] <- interpolate(tt, mr, "constant")
  dim(mr_t) <- N_agp
  dr_t[] <- interpolate(tt, dr, "constant")
  dim(dr_t) <- N_agp
  
  tt[] <- user()
  dim(tt) <- user()
  br[] <- user()
  dim(br) <- user()
  mr[, ] <- user()
  dim(mr) <- user()
  dr[, ] <- user()
  dim(dr) <- user()
})

cm_agp <- model_agp(user=pars)


## Simulate ----
times <- seq(year0, year1, 0.01)
ys <- cm_agp$run(times)
ys <- ys[times == round(times), ]


## Compare with data
sim <- data.frame(t = ys[, "t"], N = ys[, "N"])
dat <- data.frame(t = year0:year1, N = rowSums(pop$PopN_T[i_year, 1:16]))
red <- (log(sim) - log(dat)) * 100
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


grid.arrange(g_all, g_red, layout_matrix = cbind(c(1, 1, 2)))

