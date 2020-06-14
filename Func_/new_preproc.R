library(readxl)
library(dplyr)
library(reshape)
#source("timeseries.R")
source('https://raw.githubusercontent.com/balthapaixao/Covid19_BR_underreport/master/Func_/timeseries.R')
library(ggplot2)
library(zoo)
#source("ETL_SRAG.R")
source("https://raw.githubusercontent.com/balthapaixao/Covid19_BR_underreport/master/Func_/ETL_SRAG.R")

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
pre_proc_data <- function(data = X, tipo = 'graficos'){
  data_corte <- as.POSIXct('2020-05-03')
  
  X <- X[which(X$Tipo =="Estado" & X$sexo =="Total" & X$escala =="casos"), ]
  colnames(X)[8] <- "ano"
  colnames(X)[9] <- "semana"
  colnames(X)[12] <- "total"
  X$semana <- paste("", formatC(X$semana, width = 2, flag="0"))
  X$datadia <- as.POSIXct(as.Date(paste(X$ano, X$semana, 7, sep = '-'), "%Y-%U-%u"))
  X <- X[X$datadia < data_corte, ]
  
  X_casos <- X[which(X$dado =="srag"), ]
  X_obitos <- X[which(X$dado =="obito"), ]
  
  if(tipo == 'graficos'){
    filtro <- c('datadia', "Unidade da Federação", 'total')
  }
  else{
    filtro <- c('ano', 'semana', "Unidade da Federação",
                'total', 'SARS-CoV-2')
  }
  
  dt_casos <- X_casos[filtro]
  dt_obitos <- X_obitos[filtro]
  
  if(tipo == 'graficos'){
    lista_serie_casos <- pre_proc_merge(dt_casos, tipo)
    lista_serie_obitos <- pre_proc_merge(dt_obitos, tipo)
    
    #serie_total = list("casos" = lista_serie_casos, "obitos" = lista_serie_obitos)
    serie_total = list("cases" = lista_serie_casos, "deaths" = lista_serie_obitos)
    #return (lista_serie_casos, lista_serie_obitos)
  }
  else{
    lista_serie_casos <- pre_proc_merge(dt_casos, tipo)
    lista_serie_obitos <- pre_proc_merge(dt_obitos, tipo)
    #serie <- list(serie_sem_covid, serie_covid)
    
    #serie_total = list("casos" = lista_serie_casos, "obitos" = lista_serie_obitos)
    serie_total = list("cases" = lista_serie_casos, "deaths" = lista_serie_obitos)
    
    #return (lista_serie_casos, lista_serie_obitos)
    #serie_casos, serie_casos_covid <- pre_proc_merge(dt_casos, tipo)
    #serie_obitos, serie_obitos_covid <- pre_proc_merge(dt_obitos, tipo)
    #return (serie_casos, serie_casos_covid, serie_obitos, serie_obitos_covid)
  }
  return (serie_total)
}
pre_proc_merge <- function(data, tipo){
  #ufs <- read_excel("~/Aux_arqs/ufs.xls")
  ufs <- read_csv2("https://raw.githubusercontent.com/balthapaixao/Covid19_BR_underreport/master/Aux_arqs/ufs.csv")
  ufs["X1"] <- NULL
  ufs$Sigla <- as.factor(ufs$Sigla)
  if(tipo == 'graficos'){
    XU <- merge(x=data, y=ufs, by.x="Unidade da Federação", by.y="Estado") %>% 
      select(sigla=Sigla, datadia, value=total)
    serie_sem_covid <- cast(XU, datadia ~ sigla)
    serie <- list("no_covid" = serie_sem_covid)
    #return(serie)
    
  }else{ ##else é subnotif
    XU <- merge(x=data, y=ufs, by.x="Unidade da Federação", by.y="Estado") %>%
      select(sigla=Sigla, ano, semana, value=total)
    serie_sem_covid <- cast(XU, ano+semana ~ sigla, mean)
    
    
    XU_covid <- merge(x=data, y=ufs, by.x="Unidade da Federação", by.y="Estado") %>%
      select(sigla=Sigla, ano, semana, value='SARS-CoV-2')
    serie_covid <- cast(XU_covid, ano+semana ~ sigla, mean)
    serie_covid <- serie_covid %>% filter(ano == 2020)
    
    serie <- list('no_covid' = serie_sem_covid, 'covid' = serie_covid)
    #return(serie, serie_covid)
  }
  return(serie)
}
pre_proc_ms <- function(){
  infogripe_data = '2020-05-02'
  #serie_ms <- read_excel('~/Aux_arqs/HIST_PAINEL_COVIDBR_31mai2020.xlsx') #dado do ms
  serie_ms <- read_csv2("https://raw.githubusercontent.com/balthapaixao/Covid19_BR_underreport/master/Aux_arqs/HIST_PAINEL_COVIDBR_31mai2020.csv")
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
  
  serie_ms_total <- list("hm_acc_cases" = serie_total_casos, "hm_acc_obitos" = serie_total_obitos)
  
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
  serie_plott$anom <- x
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
    xlab("Epidemilogical week") + ylab("Accummulated underreport")
  print(p)
}
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
calc_erro <- function(serie){#serie tipo subnotif #FECHADO
  serie_ma_16_19 <- compute_ma(serie) %>% filter(ano == 2019)
  serie_2020 <- serie %>% filter(ano == 2020)
  
  erro_2020 <- serie_2020[1:8, 3:29] - serie_ma_16_19[1:8, 3:29]
  #sd_noise_2020 <- apply(erro, 2, sd)
  mean_noise_2020 <- apply(erro_2020, 2, mean)
  
  serie_ma_15_18 <- compute_ma(serie) %>% filter((ano >= 2015 & ano <= 2018))
  serie_16_19 <- serie %>% filter((ano >= 2016 & ano <= 2019))
  
  erro_baseline <- serie_16_19[, 3:29] - serie_ma_15_18[, 3:29]
  sd_noise_baseline <- apply(erro_baseline, 2, sd)
  mean_noise_baseline <- apply(erro_baseline, 2, mean)
  
  
  #erro_geral <- abs(mean_noise_2020 - mean_noise_baseline)
  #erro_geral_inf <- erro_geral - sd_noise_baseline
  #erro_geral_sup <- erro_geral + sd_noise_baseline
  #status <- erro_geral <= sd_noise_baseline
  
  #data <- data.frame("Erro_inf" = erro_geral_inf,
  #                 "Erro" = erro_geral,
  #               "Erro_sup" = erro_geral_sup,
  #             "Status" = status)
  data <- data.frame("epsilon_2020" = mean_noise_2020,
                     "epsilon_baseline" = mean_noise_baseline,
                     "desvio_padrao" = sd_noise_baseline)
  return(data)
}


calc_underreport <- function(serie, serie_covid){
  serie_sema_16_19 <- compute_ma(serie) %>% filter(ano == 2019) #SEMA to test the expected error in 2020 first 8 weeks 
  serie_2020 <- serie %>% filter(ano == 2020) 
  error_2020 <- serie_2020[1:8, 3:29] - serie_sema_16_19[1:8, 3:29] #removing the expected error of the first weeks 
  #mean_noise_2020 <- apply(error_2020, 2, mean)
  
  serie_sema_15_18 <- compute_ma(serie) %>% filter((ano >= 2015 & ano <= 2018)) #SEMA to calculate the noise of the method
  serie_16_19 <- serie %>% filter((ano >= 2016 & ano <= 2019)) 
  error_baseline <- serie_16_19[, 3:29] - serie_sema_15_18[, 3:29] #testing the noise
  sd_noise_baseline <- apply(error_baseline, 2, sd) #standard deviation of the noise of the method
  mean_noise_baseline <- apply(error_baseline, 2, mean) #mean of the noise of the method
  
  mean_df <- data.frame(lapply(mean_noise_baseline, function(x) t(data.frame(x))))
  mean_df <- mean_df[rep(seq_len(nrow(mean_df)), each = 8), ]
  sd_df <- data.frame(lapply(sd_noise_baseline, function(x) t(data.frame(x))))
  sd_df <- sd_df[rep(seq_len(nrow(sd_df)), each = 8), ]
  
  novelty <- serie_2020[11:nrow(serie_2020), 3:29] - serie_sema_16_19[11:nrow(serie_2020), 3:29] #removing the noise from 2020
  
  novelty <- novelty - mean_df
  novelty_sd_p <- novelty + sd_df
  novelty_sd_m <- novelty - sd_df
  
  ur_inf <- novelty_sd_m - serie_covid[11:nrow(serie_2020), 3:29]#underreport limits and predicted number
  ur_middle <- novelty - serie_covid[11:nrow(serie_2020), 3:29] 
  ur_sup <- novelty_sd_p - serie_covid[11:nrow(serie_2020), 3:29]
  
  ur_inf[ur_inf < 0] <- 0
  ur_middle[ur_middle < 0] <- 0
  ur_sup[ur_sup < 0] <- 0
  
  ur_inf_tbl <- t(tail(cumsum(ur_inf), 1))
  ur_middle_tbl <- t(tail(cumsum(ur_middle), 1))
  ur_sup_tbl <- t(tail(cumsum(ur_sup), 1))
  
  acc_table <- data.frame('inferior' = ur_inf_tbl,
                          'middle' = ur_middle_tbl,
                          'superior' = ur_sup_tbl) #create the accumulated table
  
  colnames(acc_table)[1] <- "inferior"
  colnames(acc_table)[2] <- "predicted"
  colnames(acc_table)[3] <- "superior"
  
  return(acc_table)
}

calc_ur_rate <- function(acc_table, hm_data){
  
  ur_inf <- ((t(acc_table[1])) / hm_data) + 1
  ur_mid <- ((t(acc_table[2])) / hm_data) + 1
  ur_sup <- ((t(acc_table[3])) / hm_data) + 1
  
  sup_minus_mid <- ur_sup - ur_mid
  mid_minus_inf <- ur_mid - ur_inf 
  ci <- pmax(sup_minus_mid, mid_minus_inf) #get the max value between the two differences generated
  
  rate_table <- data.frame('Baseline' = t(ur_mid),
                           'intervalo de confianca' = t(ci))
  
  colnames(rate_table)[1] <- "Predicted rate"
  colnames(rate_table)[2] <- "Confidence interval"
  
  return(rate_table)
}




