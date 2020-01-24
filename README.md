# dp_dt
A model of population dynamics based on the United Nations population dynamic data.

### Usage

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



See /Example for the usage in **deSolve** and **odin**


### Introduction
The population dynamics data are usually delivered by discrete-time scales (e.g. year/quarter). Taking the data directly into system dynamic models, either continuous-time Markov chain models or ODE-based compartmental models, might against a common assumption as they numerically permit the more than once transition during a time step. That is, following the rates of birth, death, and migration of a population might not render the results matching the population sizes in the respective data. 

For example, for modelling a birth-ageing-death process with 100 age-specific compartments, if we initialise a community with the aged zero only, the model will not inject every people the next age after one year simulation time. The property is not an error but a numerical phenomenon, which commonly exists in many system dynamic models. The property can stabilise the model during simulations. 


This repo provides a transformation from [the United Nations population data](https://www.un.org/en/development/desa/population/publications/database/index.asp) to demographic parameters for system dynamic models. 


#### Assumptions: 

- We use the data from the UN population datasets with the medium variant. 
- We target to provide the demographic parameters which can simulate the population size as the data. 
- The birth and death rates are consistent with real data.
- The migration rates are the residuals filling the gaps between the birth-ageing-death process and the population size. 
- The following simulation model should be able to externalise the population dynamics. For example, the deaths due to disease will not significantly affect the population sizes. 



### Methods


### Academic contact

Chu-Chang Ku,
Health Economic and Decision Science, University of Sheffield, UK
Email: C.Ku@sheffield.ac.uk







