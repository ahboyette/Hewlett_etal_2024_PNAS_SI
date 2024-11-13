

library(readr)
library(rethinking)
library(dplyr)
library(ggplot2)


data <- read_csv("~/Documents/GitHub/Hewlett_etal_2024_PNAS_SI/data_PNAS_fig1.csv")

m_temp <- data[complete.cases(data),]

m_temp <- m_temp[,c("learn_freq", "id", "nbr_id", "age", "mode","nbr_freq", "totalmin")]
m_test <- as.list(m_temp)
m_test$mode <- as.integer(as.factor(m_test$mode))
m_test$id <- as.integer(as.factor(m_temp$id))
m_test$nbr_id <- as.integer(as.factor(m_temp$nbr_id))
m_test$totalmin <-standardize(log(m_test$totalmin))


age_range <- range(m_test$age)
age_range <- age_range[2]-age_range[1]+1
Dmat <- matrix(NA, nrow = age_range, ncol = age_range)
for( i in 1:age_range){
  for(j in 1:age_range){
    Dmat[i, j] <- abs(i-j)
  }
}

m_test$age_cat <- as.integer(as.factor(m_test$age))
m_test$N <- length(m_test$mode)
m_test$N_id <- length(unique(m_test$id))
m_test$N_pair_id <- length(unique(m_test$nbr_id))
m_test$M <- length(unique(m_test$mode))
m_test$L <- age_range
m_test$Dmat <- Dmat



##################################
########### FULL BINOMODEL #######

m_temp <- m_temp[,c("learn_freq", "id", "nbr_id", "age", "mode","nbr_freq", "totalmin")]
m_test <- as.list(m_temp)
m_test$mode <- as.integer(as.factor(m_test$mode))
m_test$id <- as.integer(as.factor(m_temp$id))
m_test$nbr_id <- as.integer(as.factor(m_temp$nbr_id))
m_test$totalmin <-standardize(log(m_test$totalmin))


age_range<-range(m_test$age)
age_range <- age_range[2]-age_range[1]+1
Dmat <- matrix(NA, nrow = age_range, ncol = age_range)
for( i in 1:age_range){
  for(j in 1:age_range){
    Dmat[i, j] <- abs(i-j)
  }
}

m_test$age_cat <- as.integer(as.factor(m_test$age))
m_test$N <- length(m_test$mode)
m_test$N_id <- length(unique(m_test$id))
m_test$N_pair_id <- length(unique(m_test$nbr_id))
m_test$M <- length(unique(m_test$mode))
m_test$L <- age_range
m_test$Dmat <- Dmat



m1 <- stan("~/Documents/GitHub/learning_encounters/code/stan_models/binomial_model.stan", data = m_test, chains = 4, cores = 2)
precis(m1)
post <- extract.samples(m1)


sim_theta<-function(mode, age_cat, obs){
  with(post, 
       rbinom(1000,  obs, inv_logit(at +
                                      bt_mode[,mode]*sigma_btm  +
                                      rowMeans(bt_id)*bt_id_sigma +
                                      #bt_totalmin*-1 +
                                      rowMeans(bt_nbr_id) * bt_nbr_id_sigma + 
                                      (K[, mode, age_cat])
       )
       )
  )
}

par(mfrow = c(2,3))
for(i in 1:m_test$M){
  seq<-seq(1:13)
  output<-sapply(seq, function(x) sim_theta(i, x, 7))
  plot(colMeans(output) ~ seq, type = "l", xlab = "age-3", ylab = "learning prob", main = i, xlim = c(0, 13))
  #rethinking::shade(apply(output, 2, PI, .89), seq)
}



########################
######JUST THE RAW PROB
sim_theta2<-function(mode, age_cat, obs){
  with(post,  inv_logit(at +
                          bt_mode[,mode]*sigma_btm  +
                          rowMeans(bt_id)*bt_id_sigma +
                          #bt_totalmin*-1 +
                          rowMeans(bt_nbr_id) * bt_nbr_id_sigma + 
                          (K[, mode, age_cat])
  )
  )
}




sim_theta3<-function(mode, age_group, obs){
  with(post,  inv_logit(at +
                          bt_mode[,mode]*sigma_btm  +
                          rowMeans(bt_id)*bt_id_sigma +
                          #bt_totalmin*-1 +
                          rowMeans(bt_nbr_id) * bt_nbr_id_sigma + 
                          rowMeans(K[, mode, age_group])
  )
  )
}
seq<-list(c(1:9), c(10:13) )



titles <- c("Vertical", "Remote Generation", "Horizontal Intrafamilial", "Horizontal Extrafamilial", "Oblique Intrafamilial", "Oblique Extrafamilial")

cont <- c()
label <- c()
for(i in 1:m_test$M){
  output<-sapply(seq, function(x) sim_theta3(i, x, 7))
  cont <- c(cont, output[,2]-output[,1])
  label <- c(label, rep((titles[i]), length(output[,1])))
}


df_plot <- data.frame(cont, label)
df_plot %>% mutate(label=factor(label, levels=c("Oblique Extrafamilial", "Oblique Intrafamilial", "Horizontal Extrafamilial", "Horizontal Intrafamilial", "Remote Generation", "Vertical"))) %>% 
  ggplot(aes(x = cont, y = label)) +
  geom_violin(draw_quantiles = c(.11, .89), trim = T) +
  geom_vline(aes(xintercept = 0)) +
  scale_x_continuous(breaks = round(seq(min(cont), max(cont), by = 0.05),1)) +
  xlab("Contrast in relative learning probability (older - younger)") +
  ylab("")


