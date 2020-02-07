deriv(A[1]) <- N * br_t + A[i] * (mr_t[i] - dr_t[i] - ar[i])
deriv(A[2:N_agp]) <-  A[i] * (mr_t[i] - dr_t[i] - ar[i]) + A[i-1] * ar[i-1]

initial(A[]) <- A0[i] 
dim(A) <- N_agp

output(N) <- N

N_agp <- user()
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
ar[] <- user()
dim(ar) <- user()
mr[, ] <- user()
dim(mr) <- user()
dr[, ] <- user()
dim(dr) <- user()