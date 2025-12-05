#-------------------------------------------------------------------
#-------------------------
# Application to real data: 

# Data from the Regional Labor Court of the 5th Region (Bahia, Brazil).
## This data frame contains the following columns:
### VARA: Regional Labor Court
### TC: Congestion Rate
### IC: Conciliation Index

#---------------------
# Load the functions
source("functions.R")

#--------------
library(gamlss)
library(readxl)
#--------------

#---------------------------------------------------------
# Section 5.1 Global trends in mental health disorder data
#---------------------------------------------------------

Disorder.df <- read_excel("dados_dsb.xlsx")
head(Disorder.df)

y1 <- Disorder.df$Alcohol_use_disorders
y2 <- Disorder.df$Depression
y <- data.frame(Alcohol=y1,Depression=y2)

mle.Simplex <- estimBSB(y, dist = "Simplex", copula = "FGM")
summaryBSBC(mle.Simplex)

mle.Beta <- estimBSB(y, dist = "Beta", copula = "FGM")
summaryBSBC(mle.Beta)

param <- mle.Simplex$parameters

plotBSD(y$Alcohol, y$Depression, param, dist = "Simplex",
        copula_type = "FGM",
        plot_type = "surface3d",
        title = NULL, n_grid = 150)

plotBSD(y$Alcohol, y$Depression, param, dist = "Simplex",
        copula_type = "FGM",
        plot_type = "contour",
        title = NULL, n_grid = 150)

eBsimplex(param, method = "struve")
eBsimplex(param, method = "bessel")  

#------------------------------
# Section 5.2 Jurimetric data
#------------------------------

Jurimetric.df <- read_excel("trt5.xlsx") #  Load the data.
head(Jurimetric.df,5)

y1 <- Jurimetric.df$TC
y2 <- Jurimetric.df$IC
y <- data.frame(y1,y2)
head(y)

mle.Jurimetric <- estimBSB(y, dist = "Simplex", copula = "FGM")
summaryBSBC(est)

param <- mle.Jurimetric$parameters

plotBSD(y1, y2, param, dist = "Simplex",
        copula_type = "FGM",
        plot_type = "surface3d",
        title = NULL, n_grid = 150)

plotBSD(y1, y2, param, dist = "Simplex",
        copula_type = "FGM",
        plot_type = "contour",
        title = NULL, n_grid = 150)

eBsimplex(param, method = "struve")
eBsimplex(param, method = "bessel")  
#
#------------------------------------------------------------------
