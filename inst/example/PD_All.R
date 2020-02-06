deriv(N) <- N * (br_t + mr_t - dr_t)
initial(N) <- N0

N0 <- user()

br_t <- interpolate(tt, br, "constant")
mr_t <- interpolate(tt, mr, "constant")
dr_t <- interpolate(tt, dr, "constant")


tt[] <- user()
dim(tt) <- user()
br[] <- user()
dim(br) <- user()
mr[] <- user()
dim(mr) <- user()
dr[] <- user()
dim(dr) <- user()