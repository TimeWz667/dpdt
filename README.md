# dpdt
A tool for generating simulation-friendly demography inputs based on the data alike the World Population Perspects releases.

## Installation

In R, you can install from github
```r
devtools::install_github("TimeWz667/dpdt", upgrade = FALSE)
```

Load the package before use it
```r
library(dpdt)
```

## Usage

### Fetch/load data
As the defaults, the package fetches the data from [TimeWz667/pop4modelling](https://github.com/TimeWz667/pop4modelling.git) repo.
However, you can add your data in the same format. 


#### Fetch data from GitHub
```r
demo <- fetch_demography(folder = "ByCountry/United Kingdom")
```

#### Load data from a local folder

TBA


### Reform the orgin data to a specific grouping
We provide four types of grouping for the data.

#### Model for an overall population
```r
sim <- as_sim_all(demo, year0 = 2000, year1 = 2020)
head(sim)
```


#### Model for a population with binary sex strata
```r
sim <- as_sim_sex(demo, year0 = 2000, year1 = 2020, sex_ratio = 107)
head(sim)
```

#### Model for a population with age grouping
```r
agp = c(rep(1, 15), rep(2, 50), rep(3, 36)) # Define age groups
agl = c("Child", "Adult", "Old") # Label age groups

sim <- as_sim_age(dat, year0 = 2000, year1 = 2020, agp = agp, agl = agl)
head(sim)
```

#### Model for a population with age grouping and binary sex strata
```r

agp = c(rep(1:16, each=5), rep(16, 21)) # Define age groups
agl = c(paste(seq(0, 70, 5), seq(4, 74, 5), sep = "-"), "75+") # Label age groups

sim <- as_sim_age_sex(dat, year0 = 2000, year1 = 2020, agp = agp, agl = agl, sex_ratio = 107)
head(sim)
```

### Bring the reformatted data to simulation models

#### Use `deSolve`
```r
## Prepare input ----
demo <- fetch_demography(folder = "ByCountry/United Kingdom")
sim <- as_sim_sex(demo, 2000, 2050)
pars <- as_pars_sex(sim, pkg = "deSolve")

## Construct model ----
TBA
## Simulate ----
TBA
```

#### Use `odin`
```r
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
```



## Background
The population dynamics data are usually delivered by discrete-time scales (e.g. year/quarter). Taking the data directly into system dynamic models, either continuous-time Markov chain models or ODE-based compartmental models, might against a common assumption as they numerically permit the more than once transition during a time step. That is, following the rates of birth, death, and migration of a population might not render the results matching the population sizes in the respective data. 

For example, for modelling a birth-ageing-death process with 100 age-specific compartments, if we initialise a community with the aged zero only, the model will not inject every people the next age after one year simulation time. The property is not an error but a numerical phenomenon, which commonly exists in many system dynamic models. The property can stabilise the model during simulations. 

This repo provides a transformation from [the United Nations population data](https://www.un.org/en/development/desa/population/publications/database/index.asp) to demographic parameters for system dynamic models. 


### Assumptions: 

- We use the data from the UN population datasets with the medium variant. 
- We target to provide the demographic parameters which can simulate the population size as the data. 
- The birth and death rates are consistent with real data.
- The migration rates are the residuals filling the gaps between the birth-ageing-death process and the population size. 
- The following simulation model should be able to externalise the population dynamics. For example, the deaths due to disease will not significantly affect the population sizes. 



## Methods
We considered four processes, birth, ageing, death, and migration, to capture the population dynamics

See functions in (/R/migration.R) for the construction of  population dynamics in matrix forms


### Birth, **B**
We sourced the birth rates from the age-specific fertality rates given the number of females in single year. We calculated the birth number per year by the dot products of the fertality rates and female populations.

We assumpted the 1.07 (**sex_ratio = 107**) males per females at birth by the global estimation. 

#### In simulation models, 
We suggested to use the crude birth numbers as the inflow to the zero age compartment.

In amatrix form, set up the birth numbers over the population size at the first row of the matrix for all compartments.
```r
# n_agp: number of age group
B <- matrix(0, n_agp, n_agp)
B[1, ] <- rate_birth
print(B)
```

### Ageing, **A**
We assumed a homogenous ageing rate as 1/(age span) per year in the simulation. 

#### In simulation models,
In a matrix form, the ageing rates locates at: 
```r
# n_agp: number of age group
A <- diag(rate_ageing)
A[2:n_agp, 1:(n_agp - 1)] <- A[2:n_agp, 1:(n_agp - 1)] + diag(ageing[1:(n_agp - 1)])
print(A)
```

For the final age group, if you can use as_sim_age(..., ageing_to_dead = False) to silence the ageing of them

### Death, **D**


#### In simulation models,
In a matrix form, the death rates with negative signs locate at: 
```r
# n_agp: number of age group
D <- diag(- rate_death)
print(D)
```

### Migration, **M**

The **B**, **A**, **D**, and **M** can move the populatin in the current year (**P0**) to the next (**P1**). That is, 

**P1 = expm(B+A+D+M) P0**, where **expm** is the matrix exponential. 

We solved **M** with `optim` function for minimising the euclidean distance between **P1** in simulation and in WPP projection.

#### In simulation models,
In transition matrix, the migration rates with negative signs locate at: 

```r
# n_agp: number of age group
M <- diag(rate_migration)
print(M)
```

To note that, migration is the difference between immigration and emigration, so it comes with a positive sign in the matrix.


## Academic contact

Chu-Chang Ku,

Health Economic and Decision Science, University of Sheffield, UK

Email: C.Ku@sheffield.ac.uk


## License

MIT
