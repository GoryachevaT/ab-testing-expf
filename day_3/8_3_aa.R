# Представим, что валидируем правильно работающую сплитовалку -------
# Объявим данные где FPR сойдется
load("aa.RData")

set.seed(1)
n_idx <- 100000
aa <- 
  data.frame(
    values = rnorm(n_idx),
    split = 0:1
  )

# параметры для симуляции
iters <- 1000
n_s <- 1000
res <- list()
res$good$p_value <- rep(NA,iters)

# на каждой итерации мы будем брать подвыборки из А/А
for(i in 1:iters){
  smpl_0 <- sample_n(aa[aa$split == 0,],n_s,replace=F)
  smpl_1 <- sample_n(aa[aa$split == 1,],n_s,replace=F)
  
  res$good$p_value[i] <- wilcox.test(smpl_0$values, smpl_1$values)$p.value
  
  res$good$index[i] <- i
  message(i)
} 

# Считаем FPR
res$good$fpr$`0.005` <- sum(res$good$p_value<=c(0.005))/length(res$good$p_value)
res$good$fpr$`0.01` <- sum(res$good$p_value<=c(0.01))/length(res$good$p_value)
res$good$fpr$`0.05` <- sum(res$good$p_value<=c(0.05))/length(res$good$p_value)

res$good

# Представим, что валидируем плохую сплитовалку  -------
# Объявим данные для нарушенного FPR, где прилично отличаются средние
set.seed(1)
aa_bad <- 
  data.frame(
    values = c(
      rnorm(n_idx/2, mean = 1, sd = 2),
      rnorm(n_idx/2, mean = 1.2, sd = 2)
    ),
    split = c(
      rep(0,n_idx/2),
      rep(1,n_idx/2)
    )
  )

# параметры для симуляции
res$bad$p_value <- rep(NA,iters)

# на каждой итерации мы будем брать подвыборки из А/А
for(i in 1:iters){
  smpl_0 <- sample_n(aa_bad[aa_bad$split == 0,],n_s,replace=F)
  smpl_1 <- sample_n(aa_bad[aa_bad$split == 1,],n_s,replace=F)
  
  res$bad$p_value[i] <- wilcox.test(smpl_0$values, smpl_1$values)$p.value
  
  res$bad$index[i] <- i
  mesage(i)
} 

# Считаем FPR
res$bad$fpr$`0.005` <- sum(res$bad$p_value<=c(0.005))/length(res$bad$p_value)
res$bad$fpr$`0.01` <- sum(res$bad$p_value<=c(0.01))/length(res$bad$p_value)
res$bad$fpr$`0.05` <- sum(res$bad$p_value<=c(0.05))/length(res$bad$p_value)

res$bad

 save.image("aa.RData")
 