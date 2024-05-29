# 
library(readxl)
library(gamlss)
library(dplyr)
library(tidyr)
library(ggplot2)

#

source("Functions-BSD.R")

# 

df = read_excel("data/Data-BSD.xlsx")
colnames(df) = c("ID", "VARA", "Y1", "Y2")
head(df)

# 

v.iniciais = c(mean(df$Y1), mean(df$Y2), sd(df$Y1), sd(df$Y2), 0.5)
op = optim(par = v.iniciais, fn = lvBsimplex, dados = df[, 3:4], hessian = TRUE,
           method = "L-BFGS-B", lower=c(0,0,0,0,-1),upper=c(1,1,Inf,Inf,1))

op

# 

Estimates = op$par
ep = sqrt(diag(solve(op$hessian)))

IC_LI = Estimates-1.96*ep
IC_LS = Estimates+1.96*ep

tab = data.frame(Estimates=round(Estimates,3),
                     StandardError = round(ep,3),
                     IC = paste0("(",round(IC_LI,3),";",round(IC_LS,3),")"))
tab

#

x = seq(0.001, 0.999, length.out = 50)
y = seq(0.001, 0.999, length.out = 50)
theta = Estimativas
fxy = function(x, y) dBsimplex(theta, dados = cbind(x, y))
z = outer(x, y, fxy)

persp(x, y, z, theta = 35, phi = 30, expand = 1, ticktype = 'detailed', 
      xlab = "Y1", ylab = "Y2", zlab = "")

contour(x, y, z, nlevels = 25, labcex = 0.75,
        xlim = c(0.25, 0.80), ylim = c(0.05, 0.70), col = "#5d5a56", 
        xlab = "Y1", ylab = "Y2")

dados <- df[, 3:4]
points(dados$Y1, dados$Y2, pch = 20)
points(dados[c(10,11,33,84),], col = "red", pch = 1, cex = 1.2)
text(dados[c(10,11),], c("10","11"), adj = c(-0.1, 1.3), cex = 1.5)
text(dados[c(33,84),], c("33","84"), adj = c(1.4, 0), cex = 1.5)

# 

g = boxplot(df$Y1, df$Y2, names = c("Y1", "Y2"))
outliers  = sort(identity(g)$out)
outliers = data.frame(Y2 = outliers)
outliers = left_join(outliers, df, by = "Y2")
outliers = outliers %>% select("ID", "VARA", "Y2")

df_plot = pivot_longer(df, cols = c(3:4), names_to = "Y2", values_to = "Y1")

outliers1 = outliers[c(1, 2, 4, 5),]
outliers2 = outliers[3,]

ggplot(df_plot, aes(x = Y2, y = Y1, fill = factor(Y2))) +
  geom_boxplot() +
  theme_bw() + xlab(c("")) + ylab("") +
  theme(legend.position = "none", 
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 25)) +
  annotate("text", x = 2, y = outliers1$Y2, label = outliers1$ID, hjust = -0.5, cex = 5) +
  annotate("text", x = 2, y = outliers2$Y2, label = outliers2$ID, hjust = -0.5, vjust = -0.5, cex = 5)

