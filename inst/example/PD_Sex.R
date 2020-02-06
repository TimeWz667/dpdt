deriv(NF) <- NF * (br_f_t + mr_f_t - dr_f_t)
initial(NF) <- F0

deriv(NM) <- NM * (br_m_t + mr_m_t - dr_m_t)
initial(NM) <- M0

output(N) <- NF + NM

F0 <- user()
M0 <- user()

br_f_t <- interpolate(tt, br_f, "constant")
mr_f_t <- interpolate(tt, mr_f, "constant")
dr_f_t <- interpolate(tt, dr_f, "constant")

br_m_t <- interpolate(tt, br_m, "constant")
mr_m_t <- interpolate(tt, mr_m, "constant")
dr_m_t <- interpolate(tt, dr_m, "constant")


tt[] <- user()
dim(tt) <- user()

br_f[] <- user()
dim(br_f) <- user()
mr_f[] <- user()
dim(mr_f) <- user()
dr_f[] <- user()
dim(dr_f) <- user()

br_m[] <- user()
dim(br_m) <- user()
mr_m[] <- user()
dim(mr_m) <- user()
dr_m[] <- user()
dim(dr_m) <- user()