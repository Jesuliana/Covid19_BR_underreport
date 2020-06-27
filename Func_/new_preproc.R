library(readxl)
library(dplyr)
library(reshape)
library(boot)
#source("timeseries.R")
source('https://raw.githubusercontent.com/balthapaixao/Covid19_BR_underreport/master/Func_/timeseries.R')
library(ggplot2)
library(zoo)
#source("ETL_SRAG.R")
source("https://raw.githubusercontent.com/balthapaixao/Covid19_BR_underreport/master/Func_/ETL_SRAG.R")
source('https://raw.githubusercontent.com/balthapaixao/Covid19_BR_underreport/master/Func_/evtdet.R')

compute_ma <- function(data) {
  data_ema <- data %>% select(ano, semana)
  for (i in 3:ncol(data)) {
    sw <- data.frame(ts.sw(data[,i], 52*4))
    sw <- sw[,sprintf("t%d",  52*((4:1)-1))]
    ema <- apply(sw, 1, ts.emean)
    data_ema[, colnames(data)[i]] = ema
  }
  return(data_ema)
}
linreg <- function(data) {
  #browser()
  data <- as.data.frame(data)
  colnames(data) <- "x"
  data$t <- 1:nrow(data)
  
  #Adjusting a linear regression to the whole window
  lm(x~t, data)
}
pre_proc_data <- function(data = X, tipo = 'graph', datelim = '2020-05-02'){
  datelim <- as.POSIXct(datelim)
  a_day <- 60*60*24
  data_corte <- trunc(datelim,"days")+ a_day
  
  X <- X[which(X$Tipo =="Estado" & X$sexo =="Total" & X$escala =="casos"), ]
  colnames(X)[8] <- "ano"
  colnames(X)[9] <- "semana"
  colnames(X)[12] <- "total"
  X$semana <- paste("", formatC(X$semana, width = 2, flag="0"))
  X$datadia <- as.POSIXct(as.Date(paste(X$ano, X$semana, 7, sep = '-'), "%Y-%U-%u"))
  X <- X[X$datadia < data_corte, ]
  
  X_casos <- X[which(X$dado =="srag"), ]
  X_obitos <- X[which(X$dado =="obito"), ]
  
  if(tipo == 'graph'){
    filtro <- c('datadia', "Unidade da Federação", 'total')
  }
  else{
    filtro <- c('ano', 'semana', "Unidade da Federação",
                'total', 'SARS-CoV-2')
  }
  
  dt_casos <- X_casos[filtro]
  dt_obitos <- X_obitos[filtro]
  
  if(tipo == 'graph'){
    lista_serie_casos <- pre_proc_merge(dt_casos, tipo)
    lista_serie_obitos <- pre_proc_merge(dt_obitos, tipo)
    
    serie_total = list("cases" = lista_serie_casos, "deaths" = lista_serie_obitos)
  }
  else{
    lista_serie_casos <- pre_proc_merge(dt_casos, tipo)
    lista_serie_obitos <- pre_proc_merge(dt_obitos, tipo)
    
    serie_total = list("cases" = lista_serie_casos, "deaths" = lista_serie_obitos)
  }
  return (serie_total)
}
pre_proc_merge <- function(data, tipo){
  #ufs <- read_excel("~/Aux_arqs/ufs.xls")
  ufs <- read_delim("https://raw.githubusercontent.com/balthapaixao/Covid19_BR_underreport/master/Aux_arqs/ufs.csv", delim = ';', col_types = cols())
  ufs["X1"] <- NULL
  ufs$Sigla <- as.factor(ufs$Sigla)
  if(tipo == 'graph'){
    XU <- merge(x=data, y=ufs, by.x="Unidade da Federação", by.y="Estado") %>% 
      select(sigla=Sigla, datadia, value=total)
    serie_sem_covid <- cast(XU, datadia ~ sigla)
    serie <- list("no_covid" = serie_sem_covid)
  }
  else{ ##else é subnotif
    XU <- merge(x=data, y=ufs, by.x="Unidade da Federação", by.y="Estado") %>%
      select(sigla=Sigla, ano, semana, value=total)
    serie_sem_covid <- cast(XU, ano+semana ~ sigla, mean)
    
 
    XU_covid <- merge(x=data, y=ufs, by.x="Unidade da Federação", by.y="Estado") %>%
      select(sigla=Sigla, ano, semana, value='SARS-CoV-2')
    serie_covid <- cast(XU_covid, ano+semana ~ sigla, mean)
    serie_covid <- serie_covid %>% filter(ano == 2020)
    
    serie <- list('no_covid' = serie_sem_covid, 'covid' = serie_covid)
  }
  return(serie)
} 
pre_proc_ms <- function(datelim = '2020-05-02'){
  infogripe_data = datelim
  #serie_ms <- read_excel('~/Aux_arqs/HIST_PAINEL_COVIDBR_31mai2020.xlsx') #dado do ms
  serie_ms <- read_csv("https://raw.githubusercontent.com/balthapaixao/Covid19_BR_underreport/master/Aux_arqs/HIST_PAINEL_COVIDBR_31mai2020.csv", col_types = cols())
  serie_ms["X1"] <- NULL
  serie_ms$casosAcumulado <- as.numeric(serie_ms$casosAcumulado)
  serie_ms$obitosAcumulado <- as.numeric(serie_ms$obitosAcumulado)
  
  serie <- serie_ms[which(serie_ms$regiao != 'Brasil' & serie_ms$data == infogripe_data), ]
  
  serie_total_casos <- serie[c('estado', 'casosAcumulado')]
  serie_total_obitos <- serie[c('estado', 'obitosAcumulado')]
  
  serie_total_casos <- aggregate(serie_total_casos$casosAcumulado, 
                                 by=list(Category=serie_total_casos$estado), FUN=max)
  serie_total_obitos <- aggregate(serie_total_obitos$obitosAcumulado , 
                                  by=list(Category=serie_total_obitos$estado), FUN=max)
  serie_total_casos <- cast(serie_total_casos, ~Category)
  serie_total_obitos <- cast(serie_total_obitos, ~Category)
  estados <- names(serie_total_casos[2:28])
  serie_total_casos <- serie_total_casos[estados]
  serie_total_obitos <- serie_total_obitos[estados]
  
  serie_ms_total <- list("hm_acc_cases" = t(serie_total_casos), "hm_acc_deaths" = t(serie_total_obitos))
  return(serie_ms_total)
}
get_anomaly_srag <- function(serie, estado){
  filtro <- c('datadia', estado)
  serie_plot <- serie[filtro]
  #CP_V3 (linear regression)
  events_v3 <- evtdet.changepoints_v3(serie_plot, mdl=linreg, m=4)
  #Adaptative normalization
  w <-  vector()
  x <- ts.an.outliers.boxplot(serie_plot[[estado]], 104, alpha=3)
  serie_plot$anom <- x
  z <- subset(serie_plot, serie_plot['anom']==TRUE)
  
  #Bindin data to show anomalies(A_N) and change points(CP_V3) together
  if (nrow(z) != 0){
    z$serie <- estado
    z$type <- "anomaly"
    z <- z[c('datadia', 'serie', 'type')]
    colnames(z)[1] <- "time"
    an_v3 <- rbind(z, events_v3[events_v3$type=="change point", ] )
  }
  else {
    an_v3 <- events_v3[events_v3$type=="change point", ]
  }
  return(an_v3)
}
plot_subnotif <- function(acc_tbl, state){
  p <- ggplot(acc_tbl, aes_string(x = index(acc_tbl), y = state))+
    geom_point()+ geom_line() +theme_minimal() + ggtitle(paste("Accummulated underreport - ", state))+ 
    xlab("Epidemiological week") + ylab("Accummulated underreport")
  print(p)
}
fig <- function(width, heigth){
     options(repr.plot.width = width, repr.plot.height = heigth)
}

fig(13,6)
plot_srag <- function(serie, state){
  
  an_v3 <- get_anomaly_srag(serie, state)
  
  filtro <- c('datadia', state)
  max_casos <- max(serie[[state]])
  lims <- as.POSIXct(strptime(c("2009-02-01","2020-06-01"), format = "%Y-%m-%d")) 
  
  if ( max_casos < 50){
    p <- evtplot(serie[filtro], an_v3, mark.cp=TRUE, ylim = c(0, 50)) +
      scale_x_datetime(date_breaks = "6 months",
                       minor_breaks = "1 month",
                       date_labels = "%m-%Y",
                       limits = lims) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            aspect.ratio = 2/5)
  }
  else{
    p <- evtplot(serie[filtro], an_v3, mark.cp=TRUE, ylim = c(0, max_casos + 100)) +
      scale_x_datetime(date_breaks = "6 months",
                       minor_breaks = "1 month", 
                       date_labels = "%m-%Y", 
                       limits = lims) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            aspect.ratio = 2/5)
  }
  print(p)
}
calc_error <- function(serie){#serie tipo subnotif #FECHADO
  print("------- GENERATING THE ERRORS OF THE MODEL TABLE -------")

  serie_ma_16_19 <- compute_ma(serie) %>% filter(ano == 2019)
  serie_2020 <- serie %>% filter(ano == 2020)
  #error_2020 <- serie_2020[1:8, 3:29] - serie_ma_16_19[1:8, 3:29]
  
  serie_ma_15_18 <- compute_ma(serie) %>% filter((ano >= 2015 & ano <= 2018))
  serie_16_19 <- serie %>% filter((ano >= 2016 & ano <= 2019))
  error_baseline <- serie_16_19[, 3:29] - serie_ma_15_18[, 3:29]
  mean_noise_baseline <- apply(error_baseline, 2, mean)
  
  up <- sapply(error_baseline, FUN = upper_boundaries)
  low <- sapply(error_baseline, FUN = lower_boundaries)
  
  interval <- paste('[' , paste(round(low, 3), round(up, 3), sep = ', '), ']' ,sep = "") 
  
  data <- data.frame("epsilon_baseline" = round(mean_noise_baseline, 3), 
                     "interval" = interval) #tabela de intervalos de erros
  return(data)
}


calc_underreport <- function(serie, serie_covid, hmdata){
  print("------- GENERATING THE CUMULATIVE UNDERREPORTED TABLE -------")

  serie_sema_16_19 <- compute_ma(serie) %>% filter(ano == 2019) #SEMA to test the expected error in 2020 first 8 weeks 
  serie_2020 <- serie %>% filter(ano == 2020) 

  serie_sema_15_18 <- compute_ma(serie) %>% filter((ano >= 2015 & ano <= 2018)) #SEMA to calculate the noise of the method
  serie_16_19 <- serie %>% filter((ano >= 2016 & ano <= 2019)) 
  error_baseline <- serie_16_19[, 3:29] - serie_sema_15_18[, 3:29] #testing the noise
  mean_noise_baseline <- apply(error_baseline, 2, mean) #mean of the noise of the method

  mean_df <- data.frame(lapply(mean_noise_baseline, function(x) t(data.frame(x))))
  mean_df <- mean_df[rep(seq_len(nrow(mean_df)), each = 8), ]

  novelty <- serie_2020[11:nrow(serie_2020), 3:29] - serie_sema_16_19[11:nrow(serie_2020), 3:29] #removing the noise from 2020

  wilcoxtest_novelty <- mapply(wilcox.test, novelty, error_baseline, alternative = 'greater') # verifica se houve novidade
  p_values_novelty <- wilcoxtest_novelty[seq(3, length(wilcoxtest_novelty), 7)]

  uppers <- sapply(error_baseline, FUN = upper_boundaries)
  uppers <- data.frame(lapply(uppers, function(x) t(data.frame(x))))
  uppers <- uppers[rep(seq_len(nrow(uppers)), each = 8), ]
  lowers <- sapply(error_baseline, FUN = lower_boundaries)
  lowers <- data.frame(lapply(lowers, function(x) t(data.frame(x))))
  lowers <- lowers[rep(seq_len(nrow(lowers)), each = 8), ]
  #TIRO O ERRO POR SEMANA
  novelty_upper <- novelty - lowers #boundaries 
  novelty_lower <- novelty - uppers #boundaries
  novelty <- novelty - mean_df

  wilcoxtest_ur <- mapply(wilcox.test, novelty, serie_covid[11:nrow(serie_2020),3:29],
                          alternative = 'greater', paired = TRUE) # verifica se houve novidade, teste pareado 
  p_values_ur <- wilcoxtest_ur[seq(3, length(wilcoxtest_ur), 7)]

  cum_covid <- t(tail(cumsum(serie_covid[11:nrow(serie_2020), 3:29]), 1))[,1]
  ur_inf <- novelty_lower # - serie_covid[11:nrow(serie_2020), 3:29]#underreport limits and predicted number
  ur_middle <- novelty # - serie_covid[11:nrow(serie_2020), 3:29] 
  ur_sup <- novelty_upper # - serie_covid[11:nrow(serie_2020), 3:29]

  df_p_values <- data.frame("epsilon_2020" = mean_noise_baseline,
                            "random_noise_test" = (unlist(p_values_novelty) < 0.05),
                            "reported_values_test" = (unlist(p_values_ur) < 0.05)) #teste de p_values
  df_p_values$epsilon_2020 <- NULL
  print("------- P-VALUES TEST TABLE -------")
  print(df_p_values)

  ur_inf_tbl <- t(tail(cumsum(ur_inf), 1))
  ur_middle_tbl <- t(tail(cumsum(ur_middle), 1))
  ur_sup_tbl <- t(tail(cumsum(ur_sup), 1))

  acc_table <- data.frame('inferior' = round(ur_inf_tbl, 0),
                          'middle' = round(ur_middle_tbl, 0),
                          'superior' = round(ur_sup_tbl, 0),
                          'covid_cum' = round(cum_covid, 0)) #create the accumulated table

  colnames(acc_table)[1] <- "inferior"
  colnames(acc_table)[2] <- "predicted"
  colnames(acc_table)[3] <- "superior"
  colnames(acc_table)[4] <- "cum_covid"

  table_to_show <- acc_table
  table_to_show[table_to_show < 0] <- 0
  table_to_show <-table_to_show[c('predicted', "cum_covid")]

  txs <- calc_ur_rate(acc_table)
  txs$rate = paste(txs[['Rate observed']],txs[['Confidence interval']],
                   sep = ' +/- ')


  UR_table <- table_to_show
  UR_table$rate <- txs$rate
  UR_table$Cum._Covid_HM <- hmdata

  colnames(UR_table)[1] <- "Cum._Novelty_SARI"
  colnames(UR_table)[2] <- "Cum._Covid_SARI"
  colnames(UR_table)[3] <- "Rate"
                              
  for (i in seq(1:27)){
    if (df_p_values$random_noise_test[i] == FALSE){
      UR_table$Rate[i] <- "*"
    }else{
      if(df_p_values$reported_values_test[i] == FALSE){
       UR_table$Rate[i] <- "**"
    }
  }
}

  return(UR_table)
}

calc_ur_rate <- function(acc_table){ ## acc table is generated by calc_underreport()
    
  cov <- t(acc_table[4])
  inf <- t(acc_table[1]) - cov
  mid <- t(acc_table[2]) - cov
  sup <- t(acc_table[3]) - cov        
  inf[inf < 0] <- 0
  mid[mid < 0] <- 0
  sup[sup < 0] <- 0
   
  ur_inf <- (inf / cov)
  ur_mid <- (mid / cov)
  ur_sup <- (sup / cov)
   
  sup_minus_mid <- ur_sup - ur_mid
  mid_minus_inf <- ur_mid - ur_inf 
  ci <- pmax(sup_minus_mid, mid_minus_inf) #get the max value between the two differences generated
  
  rate_table <- data.frame('Baseline' = t(round(ur_mid, 3)),
                           'intervalo de confianca' = t(round(ci, 3)))
  
  colnames(rate_table)[1] <- "Rate observed"
  colnames(rate_table)[2] <- "Confidence interval"
  
  return(rate_table)
}

lower_boundaries <- function(x, replic=1000) {
  set.seed(10)
  # function to obtain the mean
  Bmean <- function(data, i) {
    d <- data[i] # allows boot to select sample
    return(mean(d))
  }
  results <- boot(data=x, statistic=Bmean, R=replic)  
  result <- boot.ci(results, type=c("bca"))
  return(lower=result$bca[4])
}
upper_boundaries <- function(x, replic=1000) {
  set.seed(10)
  # function to obtain the mean
  Bmean <- function(data, i) {
    d <- data[i] # allows boot to select sample
    return(mean(d))
  }
  results <- boot(data=x, statistic=Bmean, R=replic)  
  result <- boot.ci(results, type=c("bca"))
  return(upper = (result$bca[5]))
}
