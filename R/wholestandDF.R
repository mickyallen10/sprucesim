# Function for converting wholestand simulation to data.frame
# micky G Allen II
#December 23, 2019
############################################################
wholestandDF <- function(fl){

 # browser()

  plot.id <- data.frame(plot.id = rep(fl$plot.id,each=length(fl$age)))

  age <- fl$age %>%
    tidyr::pivot_longer(everything(),names_to="period",values_to = "age") %>%
    dplyr::bind_cols(plot.id,.)

  tph <- fl$tph %>%
    tidyr::pivot_longer(everything(),names_to="period",values_to = "tph")%>%
    dplyr::bind_cols(plot.id,.)

  tpha <- fl$tpha %>%
    tidyr::pivot_longer(everything(),names_to="period",values_to = "tpha")%>%
    dplyr::bind_cols(plot.id,.)

  tphb <- fl$tphb %>%
    tidyr::pivot_longer(everything(),names_to="period",values_to = "tphb")%>%
    dplyr::bind_cols(plot.id,.)

  bph <- fl$bph %>%
    tidyr::pivot_longer(everything(),names_to="period",values_to = "bph")%>%
    dplyr::bind_cols(plot.id,.)

  bpha <- fl$bpha %>%
    tidyr::pivot_longer(everything(),names_to="period",values_to = "bpha")%>%
    dplyr::bind_cols(plot.id,.)

  bphb <- fl$bphb %>%
    tidyr::pivot_longer(everything(),names_to="period",values_to = "bphb")%>%
    dplyr::bind_cols(plot.id,.)

  bphr <- fl$bphr %>%
    tidyr::pivot_longer(everything(),names_to="period",values_to = "bphr")%>%
    dplyr::bind_cols(plot.id,.)

  qmd <- fl$qmd %>%
    tidyr::pivot_longer(everything(),names_to="period",values_to = "qmd")%>%
    dplyr::bind_cols(plot.id,.)

  qmda <- fl$qmda %>%
    tidyr::pivot_longer(everything(),names_to="period",values_to = "qmda")%>%
    bind_cols(plot.id,.)

  qmdb <- fl$qmdb %>%
    tidyr::pivot_longer(everything(),names_to="period",values_to = "qmdb")%>%
    dplyr::bind_cols(plot.id,.)

  hd <- fl$hd %>%
    tidyr::pivot_longer(everything(),names_to="period",values_to = "hd")%>%
    dplyr::bind_cols(plot.id,.)

  hd_thin <- fl$hd_thin %>%
    tidyr::pivot_longer(everything(),names_to="period",values_to = "hd_thin")%>%
    dplyr::bind_cols(plot.id,.)

  tage <- fl$tage %>%
    tidyr::pivot_longer(everything(),names_to="period",values_to = "tage")%>%
    dplyr::bind_cols(plot.id,.)

  si <- fl$si %>%
    tidyr::pivot_longer(everything(),names_to="period",values_to = "si")%>%
    dplyr::bind_cols(plot.id,.)

  thinid <- fl$thinid %>%
    pivot_longer(everything(),names_to="period",values_to = "thinid")%>%
    dplyr::bind_cols(plot.id,.)

  vol <- fl$vol %>%
    tidyr::pivot_longer(everything(),names_to="period",values_to = "vol")%>%
    dplyr::bind_cols(plot.id,.)

  vola <- fl$vola %>%
    tidyr::pivot_longer(everything(),names_to="period",values_to = "vola")%>%
    dplyr::bind_cols(plot.id,.)

  volb <- fl$volb %>%
    tidyr::pivot_longer(everything(),names_to="period",values_to = "volb")%>%
    dplyr::bind_cols(plot.id,.)

  volr <- fl$volr %>%
    tidyr::pivot_longer(everything(),names_to="period",values_to = "volr")%>%
    dplyr::bind_cols(plot.id,.)

  fl.df <- list(age,tph,tpha,tphb,bph,bpha,bphb,bphr,qmd,qmda,qmdb,hd,hd_thin,vol,vola,volb,volr,tage,si,thinid) %>% purrr::reduce(left_join,by=c("plot.id","period"))

  return(fl.df)
}
