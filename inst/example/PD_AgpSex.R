deriv(AF[1]) <- NF * br_f_t + AF[1] * (mr_f_t[1] - dr_f_t[1] - ar[i])
deriv(AF[2:N_agp]) <- AF[i] * (mr_f_t[i] - dr_f_t[i] - ar[i]) + AF[i - 1] * ar[i-1]
initial(AF[1:N_agp]) <- F0[i] 
dim(AF) <- N_agp

deriv(AM[1]) <- NM * br_m_t + AM[1] * (mr_m_t[1] - dr_m_t[1] - ar[i])
deriv(AM[2:N_agp]) <- AM[i] * (mr_m_t[i] - dr_m_t[i] - ar[i]) + AM[i - 1] * ar[i-1]
initial(AM[1:N_agp]) <- M0[i] 
dim(AM) <- N_agp

output(NF) <- NF
output(NM) <- NM
output(N) <- NF + NM

N_agp <- user()
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
ar[] <- user()
dim(ar) <- user()

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