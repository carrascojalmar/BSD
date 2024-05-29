rm(list = ls())

# Simulation Study

library(gamlss)
library(ggplot2)
library(dplyr)
library(tidyr)

#   

source("Functions.R")
source("MonteCarlo.R")
source("Graphs.R")

# General values

R <- 5
nSample <- c(50, 100, 150, 200, 1000)

# Scenario 1
theta1.1 <- c(0.5, 0.5, 2, 2, 1)
table1.1 <- simula(R, nSample, theta1.1)

theta1.2 <- c(0.5, 0.5, 5, 5, 1)
table1.2 <- simula(R, nSample, theta1.2)

theta1.3 <- c(0.9, 0.9, sqrt(11), sqrt(11), 1)
table1.3 <- simula(R, nSample, theta1.3)

graph.Sim(table1.1,table1.2,table1.3, scenario=1)


# Scenario 2
theta2.1 <- c(0.5, 0.5, 2, 2, -1)
table2.1 <- simula(R, nSample, theta2.1)

theta2.2 <- c(0.5, 0.5, 5, 5, -1)
table2.2 <- simula(R, nSample, theta2.2)
 
theta2.3 <- c(0.9, 0.9, sqrt(11), sqrt(11), -1)
table2.3 <- simula(R, nSample, theta2.3)

graph.Sim(table2.1,table2.2,table2.3,scenario=2)
 
# Scenario 3

theta3.1 <- c(0.5, 0.5, 2, 2, 0)
table3.1 <- simula(R, nSample, theta3.1)
 
theta3.2 <- c(0.5, 0.5, 5, 5, 0)
table3.2 <- simula(R, nSample, theta3.2)
 
theta3.3 <- c(0.9, 0.9, sqrt(11), sqrt(11), 0)
table3.3 <- simula(R, nSample, theta3.3)

graph.Sim(table3.1,table3.2,table3.3,scenario=3)
