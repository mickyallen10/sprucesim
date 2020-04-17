# Stand-level Growth and yield similator
# Micky G Allen II
# December 23rd 2019
####################################
sprucesim <- function(stand.df,
                       functions,
                       n.periods=5,
                       period.length=5,
                       print.comments=F,
                       domsi = F,

                       thin=F,
                       num.thins = NULL,

                       thin.bph = T,
                       thinbyperc=F,

                       min.bph.thin1 = NULL,
                       min.bph.thin2 = NULL,

                       min.hd.thin1 = NULL,
                       min.hd.thin2 = NULL,

                       bph.after.thin1 = NULL,
                       bph.after.thin2 = NULL,

                       bph.tq1 = NULL,
                       bph.tq2 = NULL,

                       ...){

  ################################################################################
  #Notes
  ################################################################################

  #For thinning set "thin=T" else "thin=F" gives no thinning;
  #For the number of thins set "num.thins= 1 or 2" ... currently only supporting two thinnings;

  #For thinning when bph reaches a threshold set "thin.bph=T"
  ##else "thin.bph=F" Thins when dominant height(hd) reaches a threshold;

  ##################################################################################
  #For thinning by basal "thin.bph=T" area set the threshold basal area as:
  ## first thin "min.thin.bph1 = [value]"
  ## second thin "min.thin.bph2 = [value]"
  ## where [value] = the absolute basal area threshold that,when surpassed, thinning occurs

  #For thinning by hd set the threshold height as
  ## first thin "min.thin.hd1 = [value]"
  ## second thin "min.thin.hd2 = [value]"
  ## where [value] is the absolute hd thrshold when surpassed thinning occurs

  #################################################################################
  #For thinning by percentage basal area removed, set "thinbyperc = T"
  #Else thinning is performed reducing to a specified residual basal area;


  #If "thinbyperc = T" set the percentage removal as:
  ## first thin "bph.tq1 = [value]"
  ## second thin "bph.tq2 = [value]"
  ### where bph.tq is ratio of basal area after and before = 1 - thin percentage;

  #If "thinbyperc = F" set the residual basal area as:
  ## first thin "bph.after.thin1 = [value]"
  ## second thin "bph.after.thin2 = [value]"
  ### where [value] is the absolute basal area after thin

  ##################################################################################
  # END NOTES
  ##################################################################################

  others <- list(...)


  #Build the stand data objects
  foo <- function(mvec, n.periods) {
    mdt <- matrix(0, nrow = length(mvec), ncol = (n.periods +1))
    mdt[,1] <- mvec
    colnames(mdt) <- paste("t", 0:n.periods, sep = "")
    return(as.data.frame(mdt))
  }

  #browser()

  suppressWarnings(

  fl <- list(
    plot.id  = stand.df$plot.id,
    age = foo(as.integer(stand.df$age),n.periods = n.periods),
    tph = foo(as.integer(stand.df$tph),n.periods = n.periods),
    tpha = foo(as.integer(stand.df$tpha),n.periods = n.periods),
    tphb = foo(as.integer(stand.df$tphb),n.periods = n.periods),
    bph = foo(as.integer(stand.df$bph),n.periods = n.periods),
    bpha = foo(as.integer(stand.df$bpha),n.periods = n.periods),
    bphb = foo(as.integer(stand.df$bphb),n.periods = n.periods),
    bphr = foo(as.integer(stand.df$bphr),n.periods = n.periods),
    qmd = foo(as.integer(stand.df$qmd),n.periods = n.periods),
    qmda = foo(as.integer(stand.df$qmda),n.periods = n.periods),
    qmdb = foo(as.integer(stand.df$qmdb),n.periods = n.periods),
    hd = foo(as.integer(stand.df$hd),n.periods = n.periods),
    tage = foo(as.integer(stand.df$tage),n.periods = n.periods),
    hd_thin = foo(as.integer(stand.df$hd_thin),n.periods = n.periods),
    si = foo(as.integer(stand.df$si),n.periods = n.periods),
    thinid = foo(as.integer(stand.df$thinid),n.periods = n.periods),
    vol = foo(as.integer(stand.df$vol),n.periods = n.periods),
    vola = foo(as.integer(stand.df$vola),n.periods = n.periods),
    volb = foo(as.integer(stand.df$volb),n.periods = n.periods),
    volr = foo(as.integer(stand.df$volr),n.periods = n.periods)
  )
  )


  #loop over all periods
  for(i.period in (1:n.periods)){

    #print(paste0("period: ",i.period))


    #Grow plots
    #Set internal data.frames
    z1 <- data.frame(
      age1 = fl$age[,i.period],
      age2 = fl$age[,i.period] + period.length,
      tph1 = fl$tph[,i.period],
      tpha = fl$tpha[,i.period],
      tphb = fl$tphb[,i.period],
      bph1 = fl$bph[,i.period],
      bpha = fl$bpha[,i.period],
      bphb = fl$bphb[,i.period],
      bphr = fl$bphr[,i.period],
      qmd1 = fl$qmd[,i.period],
      qmda = fl$qmda[,i.period],
      qmdb = fl$qmdb[,i.period],
      hd1 = fl$hd[,i.period],
      tage = fl$tage[,i.period],
      hd_thin = fl$hd_thin[,i.period],
      si = fl$si[,i.period],
      thinid = fl$thinid[,i.period],
      vol1 = fl$vol[,i.period],
      vola = fl$vola[,i.period],
      volb = fl$volb[,i.period],
      volr = fl$volr[,i.period]
    )

    #browser()
    #Dominant height projection
    if(domsi == F){
    z1$hd2 <- functions$fn.domht(z1)
    }else{
    z1$hd2 <- functions$fn.domhtsi(z1)
    }
    #Survival projection
    z1$tph2 <- functions$fn.survival(z1)

    #Basal area projection
    z1$bph2 <- functions$fn.basalarea(z1)
    #qmd calculation
    z1$qmd2 <- functions$fn.qmd(z1)

    #Volume calculation
    z1$vol2 <- functions$fn.vol(z1)


  #Add new values to list
    fl$age[,i.period + 1] <- z1$age2
    fl$tph[,i.period + 1] <- z1$tph2
    fl$tpha[,i.period + 1] <- z1$tpha
    fl$tphb[,i.period + 1] <- z1$tphb
    fl$bph[,i.period + 1] <-  z1$bph2
    fl$bpha[,i.period + 1] <-  fl$bpha[,i.period]
    fl$bphb[,i.period + 1] <-  fl$bphb[,i.period]
    fl$bphr[,i.period + 1] <-  fl$bphr[,i.period]
    fl$qmd[,i.period + 1] <-  z1$qmd2
    fl$qmda[,i.period + 1] <-  z1$qmda
    fl$qmdb[,i.period + 1] <-  z1$qmdb
    fl$hd[,i.period + 1] <-  z1$hd2
    fl$tage[,i.period + 1] <- fl$tage[,i.period]
    fl$hd_thin[,i.period + 1] <- fl$hd_thin[,i.period]
    fl$si[,i.period + 1] <-  fl$si[,i.period]
    fl$thinid[,i.period + 1] <-  fl$thinid[,i.period]

    fl$vol[,i.period + 1] <-  z1$vol2
    fl$vola[,i.period + 1] <-  fl$vola[,i.period]
    fl$volb[,i.period + 1] <-  fl$volb[,i.period]
    fl$volr[,i.period + 1] <-  fl$volr[,i.period]


################################################################################
## perform the thinning
    if(thin == T){


    ##############################
    # Thinning using basal area as thinning trigger
    if(thin.bph == T){

    ###################
    #first thin

    z1$thinid2 <- with(z1,ifelse(thinid == 0 & bph2 > min.bph.thin1,1,0))

    if(thinbyperc == TRUE){
    #For thinning by percentage basal area removal
    z1$qmdb <- with(z1,ifelse(thinid2 == 1,functions$fn.qmd(z1),qmdb))
    #Remove basal area
    z1$bphb <- with(z1,ifelse(thinid2 == 1,bph2,bphb))
    z1$bpha <- with(z1,ifelse(thinid2 == 1,bph.tq1*bphb,bpha))
    z1$bphr <- with(z1,ifelse(thinid2 == 1, bphb-bpha,bphr))
    z1$bph2 <- with(z1,ifelse(thinid2 == 1, bpha, bph2))
    }else{
      #For thinning by prespecified residual basal area

      z1$bphb <- with(z1,ifelse(thinid2 == 1,bph2,bphb))
      z1$bpha <- with(z1,ifelse(thinid2 == 1,bph.after.thin1,bpha))
      z1$bphr <- with(z1,ifelse(thinid2 == 1, bphb-bpha,bphr))
      z1$bph2 <- with(z1,ifelse(thinid2 == 1, bpha, bph2))
    }

    z1$tphb <- with(z1,ifelse(thinid2 == 1,tph2,tphb))
    #Remove trees per hectare
    z1$tphab <- with(z1,ifelse(thinid2 == 1,functions$fn.treerem(z1),0))
    z1$tpha <- with(z1, ifelse(thinid2 == 1,tph2*tphab,tpha))
    z1$tph2 <- with(z1, ifelse(thinid2 == 1,tpha,tph2 ))
    #Change tage
    z1$tage <- with(z1,ifelse(thinid2 == 1, age2, tage))

    z1$volb <- with(z1,ifelse(thinid2 == 1,vol2,volb))
    z1$vola <- with(z1,ifelse(thinid2 == 1,functions$fn.vol(z1),vola))
    z1$volr <- with(z1,ifelse(thinid2 == 1, volb-vola,volr))
    z1$vol2 <- with(z1,ifelse(thinid2 == 1, vola, vol2))

    #change thinid
    z1$thinid <- with(z1, ifelse(thinid2 == 1, 1, thinid))

    #change hd_thin
    z1$hd_thin <- with(z1, ifelse(thinid2 == 1,hd2,hd_thin))

    #qmd calculation
    z1$qmda <- with(z1,ifelse(thinid2 == 1,functions$fn.qmd(z1),qmda))
    z1$qmd2 <- functions$fn.qmd(z1)


    #update values
    fl$tph[,i.period + 1] <- z1$tph2
    fl$tpha[,i.period + 1] <- z1$tpha
    fl$tphb[,i.period + 1] <- z1$tphb
    fl$bph[,i.period + 1] <-  z1$bph2
    fl$bpha[,i.period + 1] <-  z1$bpha
    fl$bphb[,i.period + 1] <-  z1$bphb
    fl$bphr[,i.period + 1] <-  z1$bphr
    fl$qmd[,i.period + 1] <-  z1$qmd2
    fl$qmda[,i.period + 1] <-  z1$qmda
    fl$qmdb[,i.period + 1] <-  z1$qmdb
    fl$tage[,i.period + 1] <- z1$tage
    fl$hd_thin[,i.period + 1] <- z1$hd_thin
    fl$thinid[,i.period + 1] <-  z1$thinid

    fl$vol[,i.period + 1] <-  z1$vol2
    fl$vola[,i.period + 1] <-  z1$vola
    fl$volb[,i.period + 1] <-  z1$volb
    fl$volr[,i.period + 1] <-  z1$volr

    #END first THin
    ####################

    ###################
    #second thin
    #browser()
    if(num.thins == 2){


    z1$thinid3 <- with(z1,ifelse(thinid == 1 & tage < age2 & bph2 > min.bph.thin2,1,0))

    if(thinbyperc == T){

      #For thinning by percentage basal area removal
      z1$qmdb <- with(z1,ifelse(thinid3 == 1,functions$fn.qmd(z1),qmdb))
      #Remove basal area
      z1$bphb <- with(z1,ifelse(thinid3 == 1,bph2,bphb))
      z1$bpha <- with(z1,ifelse(thinid3 == 1,bph.tq2*bphb,bpha))
      z1$bphr <- with(z1,ifelse(thinid3 == 1, bphb-bpha,bphr))
      z1$bph2 <- with(z1,ifelse(thinid3 == 1, bpha, bph2))
    }else{
      #For thinning by prespecified residual basal area
      z1$bphb <- with(z1,ifelse(thinid3 == 1,bph2,bphb))
      z1$bpha <- with(z1,ifelse(thinid3 == 1,bph.after.thin2,bpha))
      z1$bphr <- with(z1,ifelse(thinid3 == 1, bphb-bpha,bphr))
      z1$bph2 <- with(z1,ifelse(thinid3 == 1, bpha, bph2))
    }

    z1$tphb <- with(z1,ifelse(thinid3 == 1,tph2,tphb))
    #Remove trees per hectare
    z1$tphab <- with(z1,ifelse(thinid3 == 1,functions$fn.treerem(z1),0))
    z1$tpha <- with(z1, ifelse(thinid3 == 1,tph2*tphab,tpha))
    z1$tph2 <- with(z1, ifelse(thinid3 == 1,tpha,tph2 ))
    #Change tage
    z1$tage <- with(z1,ifelse(thinid3 == 1, age2, tage))

    z1$volb <- with(z1,ifelse(thinid3 == 1,vol2,volb))
    z1$vola <- with(z1,ifelse(thinid3 == 1,functions$fn.vol(z1),vola))
    z1$volr <- with(z1,ifelse(thinid3 == 1, volb-vola,volr))
    z1$vol2 <- with(z1,ifelse(thinid3 == 1, vola, vol2))

    #change thinid
    z1$thinid <- with(z1, ifelse(thinid3 == 1, 2, thinid))

    #change hd_thin
    z1$hd_thin <- with(z1, ifelse(thinid3 == 1,hd2,hd_thin))

    #qmd calculation
    z1$qmda <- with(z1,ifelse(thinid3 == 1,functions$fn.qmd(z1),qmda))
    z1$qmd2 <- functions$fn.qmd(z1)


    #update values
    fl$tph[,i.period + 1] <- z1$tph2
    fl$tpha[,i.period + 1] <- z1$tpha
    fl$tphb[,i.period + 1] <- z1$tphb
    fl$bph[,i.period + 1] <-  z1$bph2
    fl$bpha[,i.period + 1] <-  z1$bpha
    fl$bphb[,i.period + 1] <-  z1$bphb
    fl$bphr[,i.period + 1] <-  z1$bphr
    fl$qmd[,i.period + 1] <-  z1$qmd2
    fl$qmda[,i.period + 1] <-  z1$qmda
    fl$qmdb[,i.period + 1] <-  z1$qmdb
    fl$tage[,i.period + 1] <- z1$tage
    fl$hd_thin[,i.period + 1] <- z1$hd_thin
    fl$thinid[,i.period + 1] <-  z1$thinid

    fl$vol[,i.period + 1] <-  z1$vol2
    fl$vola[,i.period + 1] <-  z1$vola
    fl$volb[,i.period + 1] <-  z1$volb
    fl$volr[,i.period + 1] <-  z1$volr
    } #END second Thin
    ####################

    } else{
    #For thinning by dominant height as the thinning trigger
      ###################
      #first thin
      #browser()

      z1$thinid2 <- with(z1,ifelse(thinid == 0  & hd2 > min.hd.thin1,1,0))

      if(thinbyperc == T){
        #For thinning by percentage basal area removal
        z1$qmdb <- with(z1,ifelse(thinid2 == 1,functions$fn.qmd(z1),qmdb))
        #Remove basal area
        z1$bphb <- with(z1,ifelse(thinid2 == 1,bph2,bphb))
        z1$bpha <- with(z1,ifelse(thinid2 == 1,bph.tq1*bphb,bpha))
        z1$bphr <- with(z1,ifelse(thinid2 == 1, bphb-bpha,bphr))
        z1$bph2 <- with(z1,ifelse(thinid2 == 1, bpha, bph2))
      }else{
        #For thinning by prespecified residual basal area
        z1$bphb <- with(z1,ifelse(thinid2 == 1,bph2,bphb))
        z1$bpha <- with(z1,ifelse(thinid2 == 1,bph.after.thin1,bpha))
        z1$bphr <- with(z1,ifelse(thinid2 == 1, bphb-bpha,bphr))
        z1$bph2 <- with(z1,ifelse(thinid2 == 1, bpha, bph2))
      }

      z1$tphb <- with(z1,ifelse(thinid2 == 1,tph2,tphb))
      #Remove trees per hectare
      z1$tphab <- with(z1,ifelse(thinid2 == 1,functions$fn.treerem(z1),0))
      z1$tpha <- with(z1, ifelse(thinid2 == 1,tph2*tphab,tpha))
      z1$tph2 <- with(z1, ifelse(thinid2 == 1,tpha,tph2 ))
      #Change tage
      z1$tage <- with(z1,ifelse(thinid2 == 1, age2, tage))

      z1$volb <- with(z1,ifelse(thinid2 == 1,vol2,volb))
      z1$vola <- with(z1,ifelse(thinid2 == 1,functions$fn.vol(z1),vola))
      z1$volr <- with(z1,ifelse(thinid2 == 1, volb-vola,volr))
      z1$vol2 <- with(z1,ifelse(thinid2 == 1, vola, vol2))

      #change thinid
      z1$thinid <- with(z1, ifelse(thinid2 == 1, 1, thinid))

      #change hd_thin
      z1$hd_thin <- with(z1, ifelse(thinid2 == 1,hd2,hd_thin))

      #qmd calculation
      z1$qmda <- with(z1,ifelse(thinid2 == 1,functions$fn.qmd(z1),qmda))
      z1$qmd2 <- functions$fn.qmd(z1)


      #update values
      fl$tph[,i.period + 1] <- z1$tph2
      fl$tpha[,i.period + 1] <- z1$tpha
      fl$tphb[,i.period + 1] <- z1$tphb
      fl$bph[,i.period + 1] <-  z1$bph2
      fl$bpha[,i.period + 1] <-  z1$bpha
      fl$bphb[,i.period + 1] <-  z1$bphb
      fl$bphr[,i.period + 1] <-  z1$bphr
      fl$qmd[,i.period + 1] <-  z1$qmd2
      fl$qmda[,i.period + 1] <-  z1$qmda
      fl$qmdb[,i.period + 1] <-  z1$qmdb
      fl$tage[,i.period + 1] <- z1$tage
      fl$hd_thin[,i.period + 1] <- z1$hd_thin
      fl$thinid[,i.period + 1] <-  z1$thinid

      fl$vol[,i.period + 1] <-  z1$vol2
      fl$vola[,i.period + 1] <-  z1$vola
      fl$volb[,i.period + 1] <-  z1$volb
      fl$volr[,i.period + 1] <-  z1$volr

      #END first THin
      ####################

      ###################
      #second thin
      #browser()
      if(num.thins == 2){

        z1$thinid3 <- with(z1,ifelse(thinid == 1 & tage < age2 & hd2 > min.hd.thin2,1,0))

        if(thinbyperc == T){
          #For thinning by percentage basal area removal
          z1$qmdb <- with(z1,ifelse(thinid3 == 1,functions$fn.qmd(z1),qmdb))
          #Remove basal area
          z1$bphb <- with(z1,ifelse(thinid3 == 1,bph2,bphb))
          z1$bpha <- with(z1,ifelse(thinid3 == 1,bph.tq2*bphb,bpha))
          z1$bphr <- with(z1,ifelse(thinid3 == 1, bphb-bpha,bphr))
          z1$bph2 <- with(z1,ifelse(thinid3 == 1, bpha, bph2))
        }else{
          #For thinning by prespecified residual basal area
          z1$bphb <- with(z1,ifelse(thinid3 == 1,bph2,bphb))
          z1$bpha <- with(z1,ifelse(thinid3 == 1,bph.after.thin1,bpha))
          z1$bphr <- with(z1,ifelse(thinid3 == 1, bphb-bpha,bphr))
          z1$bph2 <- with(z1,ifelse(thinid3 == 1, bpha, bph2))
        }

        z1$tphb <- with(z1,ifelse(thinid3 == 1,tph2,tphb))
        #Remove trees per hectare
        z1$tphab <- with(z1,ifelse(thinid3 == 1,functions$fn.treerem(z1),0))
        z1$tpha <- with(z1, ifelse(thinid3 == 1,tph2*tphab,tpha))
        z1$tph2 <- with(z1, ifelse(thinid3 == 1,tpha,tph2 ))
        #Change tage
        z1$tage <- with(z1,ifelse(thinid3 == 1, age2, tage))

        z1$volb <- with(z1,ifelse(thinid3 == 1,vol2,volb))
        z1$vola <- with(z1,ifelse(thinid3 == 1,functions$fn.vol(z1),vola))
        z1$volr <- with(z1,ifelse(thinid3 == 1, volb-vola,volr))
        z1$vol2 <- with(z1,ifelse(thinid3 == 1, vola, vol2))

        #change thinid
        z1$thinid <- with(z1, ifelse(thinid3 == 1, 2, thinid))

        #change hd_thin
        z1$hd_thin <- with(z1, ifelse(thinid3 == 1,hd2,hd_thin))

        #qmd calculation
        z1$qmda <- with(z1,ifelse(thinid3 == 1,functions$fn.qmd(z1),qmda))
        z1$qmd2 <- functions$fn.qmd(z1)


        #update values
        fl$tph[,i.period + 1] <- z1$tph2
        fl$tpha[,i.period + 1] <- z1$tpha
        fl$tphb[,i.period + 1] <- z1$tphb
        fl$bph[,i.period + 1] <-  z1$bph2
        fl$bpha[,i.period + 1] <-  z1$bpha
        fl$bphb[,i.period + 1] <-  z1$bphb
        fl$bphr[,i.period + 1] <-  z1$bphr
        fl$qmd[,i.period + 1] <-  z1$qmd2
        fl$qmda[,i.period + 1] <-  z1$qmda
        fl$qmdb[,i.period + 1] <-  z1$qmdb
        fl$tage[,i.period + 1] <- z1$tage
        fl$hd_thin[,i.period + 1] <- z1$hd_thin
        fl$thinid[,i.period + 1] <-  z1$thinid

        fl$vol[,i.period + 1] <-  z1$vol2
        fl$vola[,i.period + 1] <-  z1$vola
        fl$volb[,i.period + 1] <-  z1$volb
        fl$volr[,i.period + 1] <-  z1$volr
      }
    } #END if(Thin.bph = T) THinning by bph as trigger

    } #End if(Thin == T)

  } #END loop through periods

  return(fl)
} #ENF function


