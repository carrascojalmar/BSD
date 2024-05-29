graph.Sim <- function(table1,table2,table3, scenario){
  for (i in 1:3) {
    medidas <- list("Bias", "RMSE", "Coverage")
    medidas_1 <- list(table1$Vies, table1$REQM, table1$Cov)
    medidas_2 <- list(table2$Vies, table2$REQM, table2$Cov)
    medidas_3 <- list(table3$Vies, table3$REQM, table3$Cov)
    
    tabela_a <- as.data.frame(medidas_1[i])
    tabela_a$V6 <- c(50, 100, 150, 200, 1000)
    tabela_a$V7 <- c(rep("1", 5))
    tabela_b <- as.data.frame(medidas_2[i])
    tabela_b$V6 <- c(50, 100, 150, 200, 1000)
    tabela_b$V7 <- c(rep("2", 5))
    tabela_c <- as.data.frame(medidas_3[i])
    tabela_c$V6 <- c(50, 100, 150, 200, 1000)
    tabela_c$V7 <- c(rep("3", 5))
    tabela <- bind_rows(tabela_a, tabela_b, tabela_c)
    colnames(tabela) <- c("mu1", "mu2", "sigma1", "sigma2", "lambda", "n", "estudo")
    tabela <- tabela %>% select("estudo", "n", "mu1", "mu2", "sigma1", "sigma2", "lambda")
    tabela <- pivot_longer(tabela, cols=3:7, names_to="theta", values_to="estimativas")
    
    tabela_1 <- tabela %>% filter(theta=="mu1" | theta=="mu2")
    tabela_2 <- tabela %>% filter(theta=="sigma1" | theta=="sigma2")
    tabela_3 <- tabela %>% filter(theta=="lambda")
    
    tabelas <- list(tabela_1, tabela_2, tabela_3)
    par <- list("mu", "sigma", "lambda")
    
    if (i != 3) {
      for (j in tabelas) {
        g1 <- ggplot(j, aes(n, estimativas, color=estudo)) +
          geom_line(linewidth=1.5, aes(linetype=theta)) + 
          geom_point(aes(shape=estudo), size=15) +
          scale_linetype_manual(values=c(1,2)) +
          geom_hline(yintercept=0, linetype="dashed", color="black", size=1.5) +
          labs(x="", y=medidas[i]) + 
          theme_bw() +
          theme(legend.position="none", 
                axis.text=element_text(size=25),
                axis.title=element_text(size=25)) +
          scale_x_continuous(breaks=c(50, 100, 150, 200, 1000), trans="log10") +
          scale_y_continuous(labels=scales::number_format(scale=1, accuracy=0.001))
        print(g1)
        try(if (all(summary(j)==summary(tabela_2))==F & all(summary(j)==summary(tabela_3))==F) {
          l=1
        })
        try(if (all(summary(j)==summary(tabela_1))==F & all(summary(j)==summary(tabela_3))==F) {
          l=2
        })
        try(if (all(summary(j)==summary(tabela_1))==F & all(summary(j)==summary(tabela_2))==F) {
          l=3
        })
        ggsave(paste0(scenario,"_",medidas[i],"_",par[l],".pdf"), plot=g1, width=8.27, height=8.27, units="in")
      }
    }
    if (i==3) {
      for (j in tabelas) {
        g1 <- ggplot(j, aes(n, estimativas, color=estudo)) +
          geom_line(linewidth=1.5, aes(linetype=theta)) + 
          geom_point(aes(shape=estudo), size=15) +
          scale_linetype_manual(values=c(1,2)) +
          geom_hline(yintercept=95, linetype="dashed", color="black", size=1.5) +
          labs(x="", y=medidas[i]) + 
          theme_bw() +
          theme(legend.position="none", 
                axis.text=element_text(size=25),
                axis.title=element_text(size=25)) +
          scale_x_continuous(breaks=c(50, 100, 150, 200, 1000), trans="log10") +
          scale_y_continuous(labels=scales::number_format(scale=1, accuracy=0.1))
        print(g1)
        try(if (all(summary(j)==summary(tabela_2))==F & all(summary(j)==summary(tabela_3))==F) {
          l=1
        })
        try(if (all(summary(j)==summary(tabela_1))==F & all(summary(j)==summary(tabela_3))==F) {
          l=2
        })
        try(if (all(summary(j)==summary(tabela_1))==F & all(summary(j)==summary(tabela_2))==F) {
          l=3
        })
        ggsave(paste0(scenario,"_",medidas[i],"_",par[l],".pdf"), plot=g1, width=8.27, height=8.27, units="in")
      }
    }
  }
  
}
