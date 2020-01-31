# dp_dt
A model of population dynamics based on the United Nations population dynamic data.

## Usage

For using the data, you can either

1. git clone the project 
2. download the project

and in R
```
load(file="Output/SimDemo.rdata")
```


In Output/SimDemo.rdata, we provide four groups of simulation-friendly dataset

Overall population dynamics
```
sim_all
```

Sex-specific population dynamics/ five-years age groups (0-4, 5-9, ..., 75+) / sex + age groups
```
sim_sex
sim_agp
sim_agp_sex
```

To specify a country,
```
names(sim_agp) # to see the list of available countries

pars <- sim_agp['United Kindom']
```


See /Example for the usage in `deSolve` and `odin`


## Introduction
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
We considered four processes, birht, ageing, death, and migration, to capture the population dynamics

See functions in (Source/xxx # todo ) for the construction of  population dynamics in matrix forms


### Birth, **B**
We sourced the birth rates from the age-specific fertality rates given the number of females in single year. We calculated the birth number per year by the dot products of the fertality rates and female numbers. Then, we divided it by the total population and fetch the **Crude birth rates** (BirR_F, BirR_M, and BirR_T for females, males, and total population). 

We assumpted the 1.07 males per females at birth for all settings as the global estimation. 

#### In simulation models, 
We suggested to use the crude birth rates times the total population as the inflow to the zero age compartment.

In matrix form, you can borrow the deaths to generate the newborns. 


### Ageing, **A**
In five-year age group datasets, we assumed a homogenous ageing rate as 1/5 per year in the simulation. 

#### In simulation models,
In transition matrix, the ageing rates locates at: **A**[age + 1, age] <- 1/5 

### Death, **D**


#### In simulation models,
In transition matrix, the death rates locates at: **D**[absorbing state, age groups] <- death rates 


### Migration, **M**

The **B**, **A**, **D**, and **M** can move the populatin in the current year (**P0**) to the next (**P1**). That is, 

**P1 = expm(B+A+D+M) P0**, where **expm** is the matrix exponential. 

We solved **M** with `optim` function for minimising the euclidean distance between **P0** and **P1**.

#### In simulation models,
In transition matrix, the death rates locates at: **M**[absorbing state, age groups] <- - migration rates

To note that, migration is the difference between immigration and emigration, so it comes with a negative sign in the matrix.


## Academic contact

Chu-Chang Ku,
Health Economic and Decision Science, University of Sheffield, UK
Email: C.Ku@sheffield.ac.uk

