#Dominant Height function

dom_height <- function(stand.df){

  hd1 <- stand.df$hd1
  age1<- stand.df$age1
  age2<- stand.df$age2

  b1 <-0.01605
  b2 <- 0.61208
  b3 <- 4.43722

  X0 <- 0.5*( (log(hd1) + b2*log(1-exp(-b1*age1))) + sqrt((log(hd1) + b2*log(1-exp(-b1*age1)))^2 -4*b3*log(1-exp(-b1*age1))))

  hd2 <- hd1* ( (1-exp(-b1*age2)) / (1-exp(-b1*age1)) )^( (b2 + b3)/X0 )

  return(hd2)
}

#Survival Function

survival <- function(stand.df){

  tph1 <- stand.df$tph1
  age1 <- stand.df$age1
  age2 <- stand.df$age2
  si <- stand.df$si
  bpha <- stand.df$bpha
  bphb <- stand.df$bphb


  b1 <- -1.0085
  b2 <- 0.03675
  b3 <- 3.76228
  b4 <- 2.55410
  b5 <- -1.0097

   thin_quot <- ifelse(is.na(bpha),1,bpha/bphb)
   tph2 <- (tph1^b1 + b2*thin_quot * (si/1000)^b3 * (age2^b4 - age1^b4))^(1/b5)
   return(tph2)
}


basal_area <- function(stand.df){
  bph1 <- stand.df$bph1
  hd1<- stand.df$hd1
  hd2<- stand.df$hd2
  hd_thin<- stand.df$hd_thin
  tph1<- stand.df$tph1
  tph2<- stand.df$tph2
  bpha<- stand.df$bpha
  bphb<- stand.df$bphb
  thin_quot <- ifelse(is.na(bpha),1,bpha/bphb)
  b1 <- 4.77696
  b2 <- 0.30957
  b3 <- -0.1479

    bph2 <- bph1^(hd1/hd2)*exp(b1*(tph2/tph1)^b2*thin_quot^(b3*(hd_thin/hd2))*(1-(hd1/hd2)))
    return(bph2)
}

#Quadratic mean diameter

qmd <- function(stand.df){

  tph <- stand.df$tph2
  bph <- stand.df$bph2

  qmd <- sqrt(bph/(tph*0.0000785))
  return(qmd)
}

#sph_after

tree_reduction <- function(stand.df){

  bpha <- stand.df$bpha
  bphb <- stand.df$bphb

  b0 <- -1.93269
  b1 <- 1.92954

  thin_quot <- bpha/bphb

  Sab <- exp(b0 + b1*thin_quot)

  return(Sab)
}

volume <- function(stand.df){

  hd2 <- stand.df$hd2
  bph2 <- stand.df$bph2
  age2 <- stand.df$age2
  b0 <- 0.24961
  b1 <- 1.15036
  b2 <- 1.01153
  b3 <- 2.3204

  VOL2 <- b0*hd2^b2 * bph2^b1 * exp(b3/age2)
  return(VOL2)
}


perc_waste_volume <- function(stand.df){

  qmd2 <- stand.df$qmd2
  a <- 0.9743915
  b <- 0.7129442
  c <- 0.0022819
  pw <- c + a * b^qmd2

  return(pw)
}

perc_saw_volume <- function(stand.df){
  hd2 <- stand.df$hd2
  qmd2 <- stand.df$qmd2

  b <- 0.1288
  b1 <- 0.005437
  c <- 19.56
  psnt <- 0.95 *(1-exp(-(b+b1*hd2)*qmd2))^c

  return(psnt)
}


perc_saw_volume_thin <- function(stand.df){
  qmd2 <- stand.df$qmd2
  hd2<- stand.df$hd2
  qmd_after<- stand.df$qmda
  saw_perc_after<- stand.df$saw_perc_after

  b <- -0.1113639
  b1 <- 0.0182163

  pst <- 0.94 *(1-exp(-(b + b1*hd2)*(qmd2-qmd_after) + log(1-saw_perc_after/0.94)))

  return(pst)
}

