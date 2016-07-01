as.numeric.factor <-
  function(x) {as.numeric(levels(x))[x]}
formatData <-
  function(df, kcases = F){
    
    
    #study	authors	year	ref_number	sampl_method	n_baseline	overall	prop_male	sex_subgroups	mean_age_base	pa_domain_subgroup	age_range	menopausal_status	menopausal_def	smoking_subgroup	
    #smoking_def	mean_followup	sd_followup	tot_personyrs	outcome	outcome_def	pa_tool	pa_unit	pa_cat_def	pa_cat1	pa_cat2	pa_cat3	pa_cat4	pa_cat5	MET.hr/wk_cat1	MET.hr/wk_cat2	MET.hr/wk_cat3	
    # MET.hr/wk_cat4	MET.hr/wk_cat5	MET.hr/wk_cat6	MET.hr/wk_cat7	MET.hr/wk_cat8	MET.hr/wk_cat9	MET.hr/wk_cat10	MMET.hr/wk_cat1	MMET.hr/wk_cat2	MMET.hr/wk_cat3	MMET.hr/wk_cat4	MMET.hr/wk_cat5	
    #MMET.hr/wk_cat6	MMET.hr/wk_cat7	MMET.hr/wk_cat8	MMET.hr/wk_cat9	MMET.hr/wk_cat10	tot_cases	cases1	cases2	cases3	cases4	cases5	cases6	cases7	cases8	cases9	cases10	personyrs1	personyrs2	personyrs3	personyrs4	personyrs5	personyrs6	personyrs7	personyrs8	personyrs9	personyrs10	totalpersons1	totalpersons2	totalpersons3	totalpersons4	totalpersons5	totalpersons6	totalpersons7	totalpersons8	totalpersons9	totalpersons10	effect_measure	adjustments	effect1	lci_effect1	uci_effect1	effect2	lci_effect2	uci_effect2	effect3	lci_effect3	uci_effect3	effect4	lci_effect4	uci_effect4	effect5	lci_effect5	uci_effect5	effect6	lci_effect6	uci_effect6	effect7	lci_effect7	uci_effect7	effect8	lci_effect8	uci_effect8	effect9	lci_effect9	uci_effect9	effect10	lci_effect10	uci_effect10	adjustments1	effect1_adj1	lci_effect1_adj1	uci_effect1_adj1	effect2_adj1	lci_effect2_adj1	uci_effect2_adj1	effect3_adj1	lci_effect3_adj1	uci_effect3_adj1	effect4_adj1	lci_effect4_adj1	uci_effect4_adj1	effect5_adj1	lci_effect5_adj1	uci_effect5_adj1	effect6_adj1	lci_effect6_adj1	uci_effect6_adj1	effect7_adj1	lci_effect7_adj1	uci_effect7_adj1	effect8_adj1	lci_effect8_adj1	uci_effect8_adj1	effect9_adj1	lci_effect9_adj1	uci_effect9_adj1	effect10_adj1	lci_effect10_adj1	uci_effect10_adj1	METhday	obs	EM						
    
    df3 <- NULL
    for (i in 1:nrow(df)){
      
      ref_number <- rep(df[i,"ref_number"], 10)
      study <- rep(df[i,"study"], 10)
      authors <- rep(df[i,"authors"], 10)
      em <- rep(df[i, "effect_measure"], 10)
      
      sex_subgroups <- rep(df[i,"sex_subgroups"], 10)
      overall <- rep(df[i,"overall"], 10)
      outcome <- rep(df[i,"outcome"], 10)
      follow_up <- rep(df[i,"mean_followup"], 10)
      tp <- (t(df[i,(grep("^totalper", names(df), value = T))]))
      tp <- gsub(",","",tp)
      
      py <- t(df[i,(grep("^person", names(df), value = T))])
      py <- gsub(",","",py)
      # !is.na(trimws(df[i, "adjustments1"])) || 
      if (trimws(df[i, "adjustments1"]) != "" ) {
        # Temporarily reading MMETs instead of alternate MMETs
        dose <- (t(df[i,(grep("^MMET.hr", names(df), value = T))]))
        dose <- gsub(",","",dose)
        
        rr <- t(df[i,(grep("^effect[0-9]{,2}_adj", names(df), value = T))])
        rr <- gsub(",","",rr)
        
        cases <- t(df[i,(grep("^cases", names(df), value = T))])
        cases <- gsub(",","",cases)
        
        lci <- t(df[i,(grep("^lci_effect[0-9]{,2}_adj", names(df), value = T))])
        uci <- t(df[i,(grep("^uci_effect[0-9]{,2}_adj", names(df), value = T))])
        
      }else{
        #cat("Without adjustmetns: ", df[i,"study"], "\n")
        
        dose <- (t(df[i,(grep("^MMET.hr", names(df), value = T))]))
        dose <- gsub(",","",dose)
        
        rr <- t(df[i,(grep("^effect[0-9]{,2}$", names(df), value = T))])
        rr <- gsub(",","",rr)
        
        cases <- t(df[i,(grep("^cases", names(df), value = T))])
        cases <- gsub(",","",cases)
        
        lci <- t(df[i,(grep("^lci_effect[0-9]{,2}$", names(df), value = T))])
        uci <- t(df[i,(grep("^uci_effect[0-9]{,2}$", names(df), value = T))])
      }
      
      df2 <- as.data.frame(qpcR:::cbind.na(ref_number, study, authors, outcome, em, follow_up, sex_subgroups, overall, tp, py, dose, rr, cases, lci, uci))
      colnames(df2) <- c("ref_number", "study", "authors", "outcome" , "effect_measure", "follow_up", "sex_subgroups", "overall", "totalpersons", "personyears", "dose", "rr", "cases", "lci", "uci")
      row.names(df2) <- NULL
      
      df2[,1] <- as.numeric.factor(df2[,1])
      
      for (j in 6:ncol(df2)){
        
        df2[,j] <- as.numeric.factor(df2[,j])
      }
      
      #df2 <- subset(df2, (!is.na(totalpersons) | !is.na(personyears)))
      
      df2$logrr <- log(df2$rr)
      df2$se <- with(df2, (log(uci)-log(lci))/(2*qnorm(.975)))
      if (i == 1){
        df3 <- df2
      }else{
        df3 <- rbind(df3, df2)
      }
    }
    if (kcases)
      df3 <- subset(df3, !is.na(cases))

    # convert effect_measure into a character column
    df3$effect_measure <- as.character(df3$effect_measure)
    df3$effect_measure <- tolower(df3$effect_measure)
    # The values for casecontrol,incidence-rate, and cumulative incidence data are cc, ir, and ci.
    lookup <- data.frame(code = c("or", "rr", "hr"), val = c("ir", "ir" ,"ci"))
    df3$type <- lookup$val[match(df3$effect_measure, lookup$code)]
    df3$type <- as.character(df3$type)
    
    df3[df3$effect_measure == "rr" & (is.na(df3$totalpersons) | df3$totalpersons == 0) ,]$totalpersons <-
      df3[df3$effect_measure == "rr" & (is.na(df3$totalpersons) |  df3$totalpersons == 0) ,]$personyears /
      df3[df3$effect_measure == "rr" & (is.na(df3$totalpersons) |  df3$totalpersons == 0) ,]$follow_up
    
    
    df3[df3$effect_measure == "hr" & (is.na(df3$personyears) | df3$personyears == 0) ,]$personyears <-
      df3[df3$effect_measure == "hr" & (is.na(df3$personyears) |  df3$personyears == 0) ,]$totalpersons *
      df3[df3$effect_measure == "hr" & (is.na(df3$personyears) |  df3$personyears == 0) ,]$follow_up
    
    df3[df3$effect_measure == "or" & (is.na(df3$totalpersons) |  df3$totalpersons == 0) ,]$totalpersons <-
      df3[df3$effect_measure == "or" & (is.na(df3$totalpersons) |  df3$totalpersons == 0) ,]$personyears /
      df3[df3$effect_measure == "or" & (is.na(df3$totalpersons) |  df3$totalpersons == 0) ,]$follow_up
    
    #df3$n <- with(df3,ifelse(is.na(personyears),totalpersons,personyears))

    ## Convert all lci, uci and se to zero when logrr is zero
    
    #df3[df3$logrr == 0,]$se <- df3[df3$logrr == 0,]$lci <- df3[df3$logrr == 0,]$uci <- 0
    
    df3
  }

getDataSorted <-
  function(df){
    
    
    #study	authors	year	ref_number	sampl_method	n_baseline	overall	prop_male	sex_subgroups	mean_age_base	pa_domain_subgroup	age_range	menopausal_status	menopausal_def	smoking_subgroup	
    #smoking_def	mean_followup	sd_followup	tot_personyrs	outcome	outcome_def	pa_tool	pa_unit	pa_cat_def	pa_cat1	pa_cat2	pa_cat3	pa_cat4	pa_cat5	MET.hr/wk_cat1	MET.hr/wk_cat2	MET.hr/wk_cat3	
    # MET.hr/wk_cat4	MET.hr/wk_cat5	MET.hr/wk_cat6	MET.hr/wk_cat7	MET.hr/wk_cat8	MET.hr/wk_cat9	MET.hr/wk_cat10	MMET.hr/wk_cat1	MMET.hr/wk_cat2	MMET.hr/wk_cat3	MMET.hr/wk_cat4	MMET.hr/wk_cat5	
    #MMET.hr/wk_cat6	MMET.hr/wk_cat7	MMET.hr/wk_cat8	MMET.hr/wk_cat9	MMET.hr/wk_cat10	tot_cases	cases1	cases2	cases3	cases4	cases5	cases6	cases7	cases8	cases9	cases10	personyrs1	personyrs2	personyrs3	personyrs4	personyrs5	personyrs6	personyrs7	personyrs8	personyrs9	personyrs10	totalpersons1	totalpersons2	totalpersons3	totalpersons4	totalpersons5	totalpersons6	totalpersons7	totalpersons8	totalpersons9	totalpersons10	effect_measure	adjustments	effect1	lci_effect1	uci_effect1	effect2	lci_effect2	uci_effect2	effect3	lci_effect3	uci_effect3	effect4	lci_effect4	uci_effect4	effect5	lci_effect5	uci_effect5	effect6	lci_effect6	uci_effect6	effect7	lci_effect7	uci_effect7	effect8	lci_effect8	uci_effect8	effect9	lci_effect9	uci_effect9	effect10	lci_effect10	uci_effect10	adjustments1	effect1_adj1	lci_effect1_adj1	uci_effect1_adj1	effect2_adj1	lci_effect2_adj1	uci_effect2_adj1	effect3_adj1	lci_effect3_adj1	uci_effect3_adj1	effect4_adj1	lci_effect4_adj1	uci_effect4_adj1	effect5_adj1	lci_effect5_adj1	uci_effect5_adj1	effect6_adj1	lci_effect6_adj1	uci_effect6_adj1	effect7_adj1	lci_effect7_adj1	uci_effect7_adj1	effect8_adj1	lci_effect8_adj1	uci_effect8_adj1	effect9_adj1	lci_effect9_adj1	uci_effect9_adj1	effect10_adj1	lci_effect10_adj1	uci_effect10_adj1	METhday	obs	EM						
    
    cnames <- c("study", "outcome")
    df$id <- as.factor(df$study)
    df1 <- df[,(grep("id|outcome|^totalper|^person|^MMET.hr|^effect|^cases|^lci_e|^uci_e", names(df), value = T))]
    df3 <- NULL
    for (i in 1:nrow(df1)){
      
      study <- rep(df1[i,"id"], 10)
      outcome <- rep(df1[i,"outcome"], 10)
      tp <- (t(df1[i,(grep("^totalper", names(df1), value = T))]))
      py <- t(df1[i,(grep("^person", names(df1), value = T))])
      dose <- (t(df1[i,(grep("^MMET.hr", names(df1), value = T))]))
      rr <- t(df1[i,(grep("^effect[0-9]{,2}$", names(df1), value = T))])
      cases <- t(df1[i,(grep("^cases", names(df1), value = T))])
      lci <- t(df1[i,(grep("^lci_effect[0-9]{,2}$", names(df1), value = T))])
      uci <- t(df1[i,(grep("^uci_effect[0-9]{,2}$", names(df1), value = T))])
      
      df2 <- as.data.frame(qpcR:::cbind.na(outcome, study, tp, py, dose, rr, cases, lci, uci))
      colnames(df2) <- c("outcome", "study", "totalpersons", "personyears", "dose", "rr", "cases", "lci", "uci")
      row.names(df2) <- NULL
      
      for (j in 2:ncol(df2)){
        
        df2[,j] <- as.numeric.factor(df2[,j])
      }
      
      df2 <- subset(df2, (!is.na(totalpersons) | !is.na(personyears)))
      
      df2$logrr <- log(df2$rr)
      df2$se <- with(df2, (log(uci)-log(lci))/(2*qnorm(.975)))
      if (i == 1){
        df3 <- df2
      }else{
        df3 <- rbind(df3, df2)
      }
    }
    df3
  }
getDiseaseSpecificData <-
  function(df, outcome1, paexposure, overall1, gender = NA){
    if (is.na(gender))
      subset(df, outcome == outcome1 &
               pa_domain_subgroup == paexposure &
               overall == overall1)
    else
      subset(df, outcome == outcome1 &
               pa_domain_subgroup == paexposure &
               sex_subgroups == gender)
    
  }
metaAnalysis <-
  function (pa, center1 = T, intercept1 = F, ptitle = NA, covMethed = F,  returnval = F) 
  {
    library(dosresmeta)
    library(rms)
    
    k <- quantile(pa$dose, c(.1, .5, .9))
    spl <- NULL
    if (covMethed){
      spl <- dosresmeta(logrr ~ rcs(dose, k), cases = cases, n = ifelse(effect_measure == "hr", personyears, totalpersons),
                        type = type, se = se, id = ref_number, 
                        center = center1, 
                        intercept = intercept1,
                        covariance = "h",
                        data = pa)
    }
    else{
      spl <- dosresmeta(logrr ~ rcs(dose, k), cases = cases, n = ifelse(effect_measure == "hr", personyears, totalpersons), 
                        type = type, se = se, id = ref_number, 
                        center = center1, 
                        intercept = intercept1,
                        data = pa)
    }
    
    newdata <- data.frame(dose = seq(min(pa$dose), max(pa$dose), length.out = 100))
    pred_spl <- predict(spl, newdata, expo = T)
    #pred_spl <- predict(spl, newdata, expo = T, xref = 0)
    #windows()
    with(pred_spl,
         matplot(newdata$dose, cbind(pred, ci.lb, ci.ub), type = "l", bty = "n",
                 xlab = "Dose", ylab = "Relative Risk", las = 1, 
                 col = "black", lty = "solid", log = "y", main = ptitle)
    )
    
    if (returnval)
      return(list(newdata$dose,cbind(pred_spl$pred, pred_spl$ci.lb, pred_spl$ci.ub)))
    
  }

plotMetaAnalysis <-
  function(data, outcome, ptitle, paexposure1, overall1, sex = NA){
    
    if (is.na(sex))
      dt <- getDiseaseSpecificData(data, outcome, paexposure = paexposure1, overall1 = overall1)
    
    else
      dt <- getDiseaseSpecificData(data, outcome, paexposure = paexposure1, gender = sex)
    
    fdata <- formatData(dt, kcases = T)
    # Remove all rows with missing RR and Dose
    fdata <- subset(fdata, !is.na(rr) & !is.na(dose))
    metaAnalysis(fdata, ptitle = ptitle)
  }
