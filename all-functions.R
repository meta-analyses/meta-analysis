as.numeric.factor <-
  function(x) {as.numeric(levels(x))[x]}
formatData <-
  function(df, kcases = F, infertotalpersons = F){
    
    
    #study	authors	year	ref_number	sampl_method	n_baseline	overall	prop_male	sex_subgroups	mean_age_base	pa_domain_subgroup	age_range	menopausal_status	menopausal_def	smoking_subgroup	
    #smoking_def	mean_followup	sd_followup	tot_personyrs	outcome	outcome_def	pa_tool	pa_unit	pa_cat_def	pa_cat1	pa_cat2	pa_cat3	pa_cat4	pa_cat5	MET.hr/wk_cat1	MET.hr/wk_cat2	MET.hr/wk_cat3	
    # MET.hr/wk_cat4	MET.hr/wk_cat5	MET.hr/wk_cat6	MET.hr/wk_cat7	MET.hr/wk_cat8	MET.hr/wk_cat9	MET.hr/wk_cat10	MMET.hr/wk_cat1	MMET.hr/wk_cat2	MMET.hr/wk_cat3	MMET.hr/wk_cat4	MMET.hr/wk_cat5	
    #MMET.hr/wk_cat6	MMET.hr/wk_cat7	MMET.hr/wk_cat8	MMET.hr/wk_cat9	MMET.hr/wk_cat10	tot_cases	cases1	cases2	cases3	cases4	cases5	cases6	cases7	cases8	cases9	cases10	personyrs1	personyrs2	personyrs3	personyrs4	personyrs5	personyrs6	personyrs7	personyrs8	personyrs9	personyrs10	totalpersons1	totalpersons2	totalpersons3	totalpersons4	totalpersons5	totalpersons6	totalpersons7	totalpersons8	totalpersons9	totalpersons10	effect_measure	adjustments	effect1	lci_effect1	uci_effect1	effect2	lci_effect2	uci_effect2	effect3	lci_effect3	uci_effect3	effect4	lci_effect4	uci_effect4	effect5	lci_effect5	uci_effect5	effect6	lci_effect6	uci_effect6	effect7	lci_effect7	uci_effect7	effect8	lci_effect8	uci_effect8	effect9	lci_effect9	uci_effect9	effect10	lci_effect10	uci_effect10	adjustments1	effect1_adj1	lci_effect1_adj1	uci_effect1_adj1	effect2_adj1	lci_effect2_adj1	uci_effect2_adj1	effect3_adj1	lci_effect3_adj1	uci_effect3_adj1	effect4_adj1	lci_effect4_adj1	uci_effect4_adj1	effect5_adj1	lci_effect5_adj1	uci_effect5_adj1	effect6_adj1	lci_effect6_adj1	uci_effect6_adj1	effect7_adj1	lci_effect7_adj1	uci_effect7_adj1	effect8_adj1	lci_effect8_adj1	uci_effect8_adj1	effect9_adj1	lci_effect9_adj1	uci_effect9_adj1	effect10_adj1	lci_effect10_adj1	uci_effect10_adj1	METhday	obs	EM						
    # df <- acmdata

    df3 <- NULL
    for (i in 1:nrow(df)){
      i <- 1
      ref_number <- rep(df[i,"ref_number"], 10)
      study <- rep(df[i,"study"], 10)
      authors <- rep(df[i,"authors"], 10)
      year <- rep(df[i,"year"], 10)
      location <- rep(df[i,"location"], 10)
      study_design <- rep(df[i,"study_design"], 10)
      pop_source <- rep(df[i,"pop_source"], 10)
      sampl_method <- rep(df[i,"sampl_method"], 10)
      
      n_baseline <- rep(df[i,"n_baseline"], 10)
      prop_male <- rep(df[i,"prop_male"], 10)
      
      mean_age_base <- rep(df[i,"mean_age_base"], 10)
      sd_age_base <- rep(df[i,"sd_age_base"], 10)
      lci_age_base <- rep(df[i,"lci_age_base"], 10)
      uci_age_base <- rep(df[i,"uci_age_base"], 10)
      pa_domain_subgroup <- rep(df[i,"pa_domain_subgroup"], 10)
      sd_followup <- rep(df[i,"sd_followup"], 10)
      tot_personyrs <- rep(df[i,"tot_personyrs"], 10)
      outcome_type <- rep(df[i,"outcome_type"], 10)
      outcome_def <- rep(df[i,"outcome_def"], 10)
      outcome_tool_baseline <- rep(df[i,"outcome_tool_baseline"], 10)
      pa_tool <- rep(df[i,"pa_tool"], 10)
      pa_unit <- rep(df[i,"pa_unit"], 10)
      pa_cat_def <- rep(df[i,"pa_cat_def"], 10)
      other_pa_domains <- rep(df[i,"other_pa_domains"], 10)
      adjustments <- rep(df[i,"adjustments"], 10)
      adjustments1 <- rep(df[i,"adjustments1"], 10)
      adjustments2 <- rep(df[i,"adjustments2"], 10)
      
      pa_cat <- (t(df[i,(grep("^pa_cat[0-9]{,2}", names(df), value = T))]))
      pa_cat <- gsub(",", "", pa_cat)
      
      personyrs <- (t(df[i,(grep("^personyrs[0-9]{,2}", names(df), value = T))]))
      personyrs <- gsub(",", "", personyrs)
      
      totalpersons <- (t(df[i,(grep("^totalpersons[0-9]{,2}", names(df), value = T))]))
      totalpersons <- gsub(",", "", totalpersons)
      
      rr1 <- t(df[i,(grep("^effect[0-9]{,2}_adj1", names(df), value = T))])
      rr1 <- gsub(",", "", rr1)
      
      rr2 <- t(df[i,(grep("^effect[0-9]{,2}_adj2", names(df), value = T))])
      rr2 <- gsub(",", "", rr2)
      
      lci1 <- t(df[i,(grep("^lci_effect[0-9]{,2}_adj1", names(df), value = T))])
      lci1 <- gsub(",","",lci1)
      
      uci1 <- t(df[i,(grep("^uci_effect[0-9]{,2}_adj1", names(df), value = T))])
      uci1 <- gsub(",","",uci1)
      
      lci2 <- t(df[i,(grep("^lci_effect[0-9]{,2}_adj2", names(df), value = T))])
      lci2 <- gsub(",","",lci2)
      
      uci2 <- t(df[i,(grep("^uci_effect[0-9]{,2}_adj2", names(df), value = T))])
      uci2 <- gsub(",","",uci2)
      
      obs <- rep(df[i,"obs"], 10)
      
      o_ref <- ""
      
      notes_exposure <- ""
      
      ## "n_baseline",	"overall",	"prop_male",	"sex_subgroups",
      # "mean_age_base",	"sd_age_base",	"lci_age_base",	"uci_age_base",	"pa_domain_subgroup",	"mean_followup",	"sd_followup",	"tot_personyrs",	"outcome", "outcome_type"	,"outcome_def",	"outcome_tool_baseline",
      # "pa_tool", "pa_unit", "pa_cat_def",	"other_pa_domains", "tot_cases", "effect_measure", "adjustments", "adjustments1",	"adjustments2", "pa_cat",	"cases",	"personyrs",	"totalpersons",	"effect",	
      # "lci_effect", "uci_effect",	"effect_adj1",	"lci_effect_adj1",	"uci_effect1_adj1" ,	"effect_adj2",	"lci_effect_adj2",	"uci_effect_adj2",	"obs",	"Other References required for dose derivation/harmonisation",	
      # "Harmonised exposure (MMET-hrs/wk)", "Notes on exposure harmonisation"
      # 
      ##
      em <- rep(df[i, "effect_measure"], 10)
      type <- rep(df[i, "type"], 10)
      
      sex_subgroups <- rep(df[i,"sex_subgroups"], 10)
      overall <- rep(df[i,"overall"], 10)
      outcome <- rep(df[i,"outcome"], 10)
      follow_up <- rep(df[i,"mean_followup"], 10)
      tp <- (t(df[i,(grep("^totalper", names(df), value = T))]))
      tp <- gsub(",","",tp)
      tot_tp <- sum(as.integer(tp), na.rm = T)
      
      
      py <- t(df[i,(grep("^person", names(df), value = T))])
      py <- gsub(",","",py)
      tot_py <- sum(as.integer(py), na.rm = T)
      
      # !is.na(trimws(df[i, "adjustments1"])) || 
      
      cases <- t(df[i,(grep("^cases", names(df), value = T))])
      cases <- gsub(",","",cases)
      tot_cases <- as.integer(df[i,"tot_cases"])
      if (length(tot_cases) == 0 || is.na(tot_cases) || tot_cases == 0)
        tot_cases <- sum(as.integer(cases), na.rm = T)
      
      if (tot_py == 0 && tot_py == 0){
        n_b <- as.integer(df[i,"n_baseline"])
        tp <- round( (as.integer(cases) / tot_cases) * as.integer(n_b))
      }
      
      tot_given_cases <- as.integer(df[i,"tot_cases"])
      
      if (sum(as.integer(cases), na.rm = T) == 0){
        cases <- (tp * tot_given_cases) / as.integer(n_b)
      }
      
      dose <- (t(df[i,(grep("^Alt_MMET.hr", names(df), value = T))]))
      dose <- gsub(",","",dose)
      tot_dose <- sum(as.numeric(dose), na.rm = T)
      
      if (tot_dose == 0){
        # cat("Study: ", df[i,"ref_number"] , "\n")
        dose <- (t(df[i,(grep("^MMET.hr", names(df), value = T))]))
        dose <- gsub(",","",dose)
      }
      
      
      rr <- t(df[i,(grep("^effect[0-9]{,2}_adj", names(df), value = T))])
      rr <- gsub(",","",rr)
      tot_rr <- sum(as.numeric(rr), na.rm = T)
      
      if (tot_rr == 0){
        rr <- t(df[i,(grep("^effect[0-9]{,2}$", names(df), value = T))])
        rr <- gsub(",","",rr)
      }
      
      lci <- t(df[i,(grep("^lci_effect[0-9]{,2}_adj", names(df), value = T))])
      lci <- gsub(",","",lci)
      tot_lci <- sum(as.numeric(lci), na.rm = T)
      
      if (tot_lci == 0){
        lci <- t(df[i,(grep("^lci_effect[0-9]{,2}$", names(df), value = T))])
        lci <- gsub(",","",lci)  
      }
      
      uci <- t(df[i,(grep("^uci_effect[0-9]{,2}_adj", names(df), value = T))])
      uci <- gsub(",","",uci)
      tot_uci <- sum(as.numeric(uci), na.rm = T)
      
      if (tot_uci == 0){
        uci <- t(df[i,(grep("^uci_effect[0-9]{,2}$", names(df), value = T))])
        uci <- gsub(",","",uci)
      }
      
      # Auto-increasing ID
      id <- rep(i, 10)
      
      
      ## Total columns ##
      # Checked	PubmedID	DOI Weblink	study	authors	year	location	study_design	pop_source	sampl_method	n_baseline	overall	prop_male	sex_subgroups	mean_age_base	sd_age_base	lci_age_base	uci_age_base	pa_domain_subgroup	mean_followup	sd_followup	tot_personyrs	outcome	outcome_type	outcome_def	outcome_tool_baseline	pa_tool	pa_unit	pa_cat_def	other_pa_domains	tot_cases	effect_measure	adjustments	adjustments1	adjustments2	pa_cat1	cases1	personyrs1	totalpersons1	effect1	lci_effect1	uci_effect1	effect1_adj1	lci_effect1_adj1	uci_effect1_adj1	effect1_adj2	lci_effect1_adj2	uci_effect1_adj2	obs	Other References required for dose derivation/harmonisation	MMET.hr/wk_cat1	Notes on exposure harmonisation

      
      #df2 <- as.data.frame(qpcR:::cbind.na(id, ref_number, study, authors, outcome, em, type, follow_up, sex_subgroups, overall, tp, py, dose, rr, cases, lci, uci))
      #colnames(df2) <- c("id", "ref_number", "study", "authors", "outcome" , "effect_measure", "type",  "follow_up", "sex_subgroups", "overall", "totalpersons", "personyears", "dose", "rr", "cases", "lci", "uci")
      
      df2 <- as.data.frame(qpcR:::cbind.na(ref_number, "", "",	"", "", study, authors, year, location, study_design, pop_source, sampl_method,	n_baseline, overall, prop_male, sex_subgroups, 
                                           mean_age_base, sd_age_base, lci_age_base, uci_age_base, pa_domain_subgroup, follow_up, sd_followup, tot_personyrs, outcome, outcome_type, outcome_def, outcome_tool_baseline,
                                           pa_tool, pa_unit, pa_cat_def, other_pa_domains, tot_cases, em, adjustments, adjustments1, adjustments2, pa_cat, cases, personyrs, totalpersons, rr,
                                           lci, uci, rr1, lci1, uci1, rr2, lci2, uci2, obs, o_ref,
                                           dose, notes_exposure))
                                           
                                           
                                           #outcome, em, type, follow_up, sex_subgroups, overall, tp, py, dose, rr, cases, lci, uci))
      
      colnames(df2) <- c("ref_number", "Checked", "PubmedID",	"DOI", "Weblink", "study","authors",	"year",	"location",	"study_design",	"pop_source",	"sampl_method",	"n_baseline",	"overall",	"prop_male",	"sex_subgroups",
                         "mean_age_base",	"sd_age_base",	"lci_age_base",	"uci_age_base",	"pa_domain_subgroup",	"mean_followup",	"sd_followup",	"tot_personyrs",	"outcome", "outcome_type"	,"outcome_def",	"outcome_tool_baseline",
                         "pa_tool", "pa_unit", "pa_cat_def",	"other_pa_domains", "tot_cases", "effect_measure", "adjustments", "adjustments1",	"adjustments2", "pa_cat",	"cases",	"personyrs",	"totalpersons",	"effect",	
                         "lci_effect", "uci_effect",	"effect_adj1",	"lci_effect_adj1",	"uci_effect1_adj1" ,	"effect_adj2",	"lci_effect_adj2",	"uci_effect_adj2",	"obs",	"Other References required for dose derivation/harmonisation",	
                         "Harmonised exposure (MMET-hrs/wk)", "Notes on exposure harmonisation")
                         

      row.names(df2) <- NULL
      
      
      
      for (j in 1:ncol(df2)){
        if (!(j %in% c(2,4,5,6,7,9:12,21,25:32, 34:38, 51, 52, 54))){
          df2[,j][is.na(df2[,j])] <- ""
          df2[,j] <- as.numeric.factor(df2[,j])
        }
      }
      
      # df2$effect <- as.numeric.factor(df2$effect)
      # df2$uci_effect <- as.numeric.factor(df2$uci_effect)
      # df2$lci_effect <- as.numeric.factor(df2$lci_effect)
      # 
      # df2[,1] <- as.numeric.factor(df2[,1])
      # df2[,2] <- as.numeric.factor(df2[,2])
      # 
      # for (j in 8:ncol(df2)){
      #   
      #   df2[,j] <- as.numeric.factor(df2[,j])
      # }
      
      #df2 <- subset(df2, (!is.na(totalpersons) | !is.na(personyears)))
      
      #df2$logrr <- log(df2$effect)
      #df2$se <- with(df2, (log(uci_effect)-log(lci_effect))/(2*qnorm(.975)))
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
    #df3$type <- lookup$val[match(df3$effect_measure, lookup$code)]
    #df3$type <- as.character(df3$type)
    
    # df3[df3$effect_measure == "rr" & (is.na(df3$totalpersons) | df3$totalpersons == 0) ,]$totalpersons <-
    #   round(df3[df3$effect_measure == "rr" & (is.na(df3$totalpersons) |  df3$totalpersons == 0) ,]$personyears /
    #   df3[df3$effect_measure == "rr" & (is.na(df3$totalpersons) |  df3$totalpersons == 0) ,]$mean_followup)
    # 
    # 
    # df3[df3$effect_measure == "hr" & (is.na(df3$personyears) | df3$personyears == 0) ,]$personyears <-
    #   df3[df3$effect_measure == "hr" & (is.na(df3$personyears) |  df3$personyears == 0) ,]$totalpersons *
    #   df3[df3$effect_measure == "hr" & (is.na(df3$personyears) |  df3$personyears == 0) ,]$mean_followup
    # 
    # df3[df3$effect_measure == "or" & (is.na(df3$totalpersons) |  df3$totalpersons == 0) ,]$totalpersons <-
    #   round(df3[df3$effect_measure == "or" & (is.na(df3$totalpersons) |  df3$totalpersons == 0) ,]$personyears /
    #   df3[df3$effect_measure == "or" & (is.na(df3$totalpersons) |  df3$totalpersons == 0) ,]$mean_followup)
    
    
    if (infertotalpersons){
      df3[(is.na(df3$totalpersons) |  df3$totalpersons == 0) ,]$totalpersons <-
        round(df3[(is.na(df3$totalpersons) |  df3$totalpersons == 0) ,]$personyears /
        df3[(is.na(df3$totalpersons) |  df3$totalpersons == 0) ,]$mean_followup)
    }
      
    
    #df3$n <- with(df3,ifelse(is.na(personyears),totalpersons,personyears))
    
    ## Convert all lci, uci and se to zero when logrr is zero
    
    #df3[df3$logrr == 0,]$se <- df3[df3$logrr == 0,]$lci <- df3[df3$logrr == 0,]$uci <- 0
    
    df3
  }


getMissingVariables <- 
  function(df, infertotalpersons = F, kcases = F){
    # df <- acmfdata
    # infertotalpersons = T
    # kcases = T
    index <- 1
    rdf <- NULL
    urn <- NULL
    for(i in unique(df$ref_number)){
      urn <- subset(df, ref_number == i)
        for(j in unique(urn$ref_number)){
          uout <- subset(urn, ref_number == j)
          uout$id <- index
          
          
          
          uout[uout$effect_measure == "rr" & (is.na(uout$totalpersons) | uout$totalpersons == 0) ,]$totalpersons <-
            round(uout[uout$effect_measure == "rr" & (is.na(uout$totalpersons) |  uout$totalpersons == 0) ,]$personyrs /
            uout[uout$effect_measure == "rr" & (is.na(uout$totalpersons) |  uout$totalpersons == 0) ,]$mean_followup)


          uout[uout$effect_measure == "hr" & (is.na(uout$personyrs) | uout$personyrs == 0) ,]$personyrs <-
            uout[uout$effect_measure == "hr" & (is.na(uout$personyrs) |  uout$personyrs == 0) ,]$totalpersons *
            uout[uout$effect_measure == "hr" & (is.na(uout$personyrs) |  uout$personyrs == 0) ,]$mean_followup

          uout[uout$effect_measure == "or" & (is.na(uout$totalpersons) |  uout$totalpersons == 0) ,]$totalpersons <-
            round(uout[uout$effect_measure == "or" & (is.na(uout$totalpersons) |  uout$totalpersons == 0) ,]$personyrs /
            uout[uout$effect_measure == "or" & (is.na(uout$totalpersons) |  uout$totalpersons == 0) ,]$mean_followup)
          
          
          if (infertotalpersons){
            uout[(is.na(uout$totalpersons) |  uout$totalpersons == 0) ,]$totalpersons <-
              round(uout[(is.na(uout$totalpersons) |  uout$totalpersons == 0) ,]$personyrs /
                      uout[(is.na(uout$totalpersons) |  uout$totalpersons == 0) ,]$mean_followup)
          }
          
          if (kcases)
            uout <- subset(uout, !is.na(cases))
          
          
          uout$logrr <- log(uout$effect)
          uout$se <- with(uout, (log(uci_effect)-log(lci_effect))/(2*qnorm(.975)))
          
          
          
          uout$n <- with(uout,ifelse(is.na(personyrs),totalpersons,personyrs))
          
          #cat(i, j, index, "\n")
          
          ## Convert all lci, uci and se to zero when logrr is zero
          #if (nrow(uout[!is.na(uout$logrr) & uout$logrr == 0,]) > 0)
          #  uout[!is.na(uout$logrr) & uout$logrr == 0,]$se <- uout[!is.na(uout$logrr) & uout$logrr == 0,]$lci <- uout[!is.na(uout$logrr) & uout$logrr == 0,]$uci <- 0
          
          
          #cat(i, j, index, "\n")
          
          
          if (index == 1){
            rdf <- uout
          }else{
            rdf <- rbind(rdf, uout)
          }
          
          
          index <- index + 1
          
        }
    }
    
    rdf
    
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
  function(df, outcome1, paexposure, overall1, gender = NA, out_type = "all"){
    return_df <- NULL
    
    if (is.na(gender)){
      
      with_overall <- subset(df, outcome == outcome1 &
                               pa_domain_subgroup == paexposure &
                               overall == overall1)
      
      without_overall <- subset(df, outcome == outcome1 &
                                  pa_domain_subgroup == paexposure &
                                  overall != overall1)
      
      td1 <- unique(with_overall$ref_number)
      td2 <- unique(without_overall$ref_number)
      td3 <- td2[!td2 %in% td1]
      
      if (length(td3) > 0){
        temp <- subset(df, outcome == outcome1 &
                         pa_domain_subgroup == paexposure &
                         (ref_number %in% td3))
        return_df <- rbind(with_overall, temp)
      }else{
        return_df <- with_overall
      }
      
    }else
      return_df <- subset(df, outcome == outcome1 &
               pa_domain_subgroup == paexposure &
               sex_subgroups == gender) #  & overall == 0)
    
    if (out_type != "all")
      subset(return_df, outcome_type == out_type)
    else
      return_df
  }

metaAnalysis <-
  function (pa, center1 = T, intercept1 = F, ptitle = NA, covMethed = F,  returnval = F, minQuantile = 0.1, maxQuantile = 0.9, lout = 100, lby = NULL) 
  {
    if (!is.null(pa) && nrow(pa) > 0){
      library(dosresmeta)
      library(rms)
      
      k <- quantile(pa$dose, c(minQuantile, (minQuantile + maxQuantile) / 2, maxQuantile))
      spl <- NULL
      if (covMethed){
        spl <- dosresmeta(logrr ~ rcs(dose, k), cases = cases, n = ifelse(effect_measure == "hr", personyrs, totalpersons),
                          type = type, se = se, id = id, 
                          center = center1, 
                          intercept = intercept1,
                          covariance = "h",
                          data = pa)#,
        #method = "fixed")
      }
      else{
        spl <- dosresmeta(logrr ~ rcs(dose, k), cases = cases, n = ifelse(effect_measure == "hr", personyrs, totalpersons), 
                          type = type, se = se, id = id,  
                          center = center1, 
                          intercept = intercept1,
                          data = pa)#,
        #method = "fixed")
      }
      newdata <- NULL
      if (is.null(lby))
        newdata <- data.frame(dose = seq(min(pa$dose), max(pa$dose), length.out = lout))
      else{
        newdata <- data.frame(dose = seq(min(pa$dose), max(pa$dose), by = lby))
      }
      pred_spl <- predict(spl, newdata, expo = T)
      #pred_spl <- predict(spl, newdata, expo = T, xref = 0)
      #windows()
      # Comment out capitalization of start of words
      #ptitle <- trimws(stringi::stri_trans_totitle(ptitle))
      #png(filename=paste0("data/", trimws(ptitle), ".png"))
      #write.csv(pa, file = paste0("data/", trimws(ptitle), ".csv"), row.names = F)
      if (!returnval)
        with(pred_spl,
             matplot(newdata$dose, cbind(pred, ci.lb, ci.ub), type = "l", bty = "n",
                     xlab = "Dose", ylab = "Relative Risk", las = 1, 
                     col = "black", lty = "solid", log = "y", main = paste(simpleCap(ptitle), ' \n Number of samples: ', 
                                                                           length(unique(pa$id)), 
                                                                           ' \n Number of population: ' , sum(pa$totalpersons)))
        )
      #dev.off()
      
      if (returnval)
        return(list(newdata$dose,cbind(pred_spl$pred, pred_spl$ci.lb, pred_spl$ci.ub)))
    }
    
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



getPIF <- function(acmfdata, plot_data){
  
  m <- matrix(nrow = 1, ncol = 8)
  
  removeNA <- F
  
  # Update RR from the lookup table
  for (k in 1:nrow(acmfdata)){
    val <- subset(plot_data, round(dose, 1) <= (acmfdata$dose[k] + 0.05) & round(dose, 1) >= (acmfdata$dose[k] - 0.05))
    if (nrow(val) > 0){
      acmfdata$rr[k] <- val$RR[1]
      if (removeNA){
        if (!is.na(acmfdata$lci[k]))
          acmfdata$lci[k] <- val$lb[1]
        if (!is.na(acmfdata$lci[k]))
          acmfdata$uci[k] <- val$ub[1]
      }else{
        acmfdata$lci[k] <- val$lb[1]
        acmfdata$uci[k] <- val$ub[1]
      }
    }
  }
  
  sum_tp <- sum(acmfdata$totalpersons * acmfdata$rr, na.rm = T) 
  
  acmfdata_ls <- acmfdata
  
  #Replace lower dose with 8.75
  acmfdata_ls[acmfdata_ls$dose < 8.75,]$dose <- 8.75
  
  local_var <- acmfdata_ls
  
  val <- subset(plot_data, round(dose, 1) <= (8.75 + 0.05) & round(dose, 1) >= (8.75 - 0.05))
  
  if (nrow(val) > 0)
    acmfdata_ls[acmfdata_ls$dose == 8.75,]$rr <- val$RR[1]
  
  sum_ls_tp <- sum(acmfdata$totalpersons * acmfdata_ls$rr, na.rm = T)
  
  pert_ls <- ((sum_tp - sum_ls_tp) / sum_tp) * 100
  
  acmfdata_ls <- local_var
  
  if (nrow(val) > 0){
    
    if (removeNA){
      acmfdata_ls[acmfdata_ls$dose == 8.75 & !is.na(acmfdata_ls$uci),]$uci <- val$ub[1]
    }else{
      acmfdata_ls[acmfdata_ls$dose == 8.75,]$uci <- val$ub[1]
    }
    
  }
  
  sum_ls_lower_tp <- sum(acmfdata$totalpersons * acmfdata_ls$uci, na.rm = T)
  
  sum_tp <- sum(acmfdata$totalpersons * acmfdata$uci, na.rm = T)
  
  pert_ls_lower <- ((sum_tp - sum_ls_lower_tp) / sum_tp) * 100
  
  acmfdata_ls <- local_var
  
  if (nrow(val) > 0){
    if (removeNA){
      acmfdata_ls[acmfdata_ls$dose == 8.75 & !is.na(acmfdata_ls$uci),]$lci <- val$lb[1]
    }else{
      acmfdata_ls[acmfdata_ls$dose == 8.75,]$lci <- val$lb[1]
    }
  }
  
  sum(acmfdata_ls$lci, na.rm = T)
  
  sum(acmfdata$lci, na.rm = T)
  
  sum_ls_upper_tp <- sum(acmfdata$totalpersons * acmfdata_ls$lci, na.rm = T)
  
  sum_tp <- sum(acmfdata$totalpersons * acmfdata$lci, na.rm = T)
  
  pert_ls_upper <- ((sum_tp - sum_ls_upper_tp) / sum_tp) * 100
  
  
  
  m[1,1] <- stringi::stri_trans_totitle(uoutcome$outcome[i])
  
  m[1,2] <- stringi::stri_trans_totitle(outcome_type[j])
  
  m[1,3] <- round(pert_ls, 2)
  
  m[1,4] <- round(pert_ls_lower, 2)
  
  m[1,5] <- round(pert_ls_upper, 2)
  
  lower_guideline_value <- paste0(round(pert_ls, 2) , "% (95% CI: ", round(pert_ls_lower, 2), " - ",  round(pert_ls_upper, 2), ")" )
  
  acmfdata_hs <- acmfdata
  
  #Replace higher dose with 17.5
  acmfdata_hs[acmfdata_hs$dose < 17.5,]$dose <- 17.5
  
  local_var <- acmfdata_hs
  
  val <- subset(plot_data, round(dose, 1) <= (17.5 + 0.05) & round(dose, 1) >= (17.5 - 0.05))
  if (nrow(val) == 0)
    val <- subset(plot_data, round(dose, 1) <= (17.5 + 0.1) & round(dose, 1) >= (17.5 - 0.1))
  
  if (nrow(val) > 0){
    acmfdata_hs[acmfdata_hs$dose == 17.5,]$rr <- val$RR[1]
  }
  
  sum_hs_tp <- sum(acmfdata$totalpersons * acmfdata_hs$rr, na.rm = T)
  
  sum_tp <- sum(acmfdata$totalpersons * acmfdata$rr, na.rm = T) 
  
  pert_hs <- ((sum_tp - sum_hs_tp) / sum_tp) * 100
  
  acmfdata_hs <- local_var
  
  if (nrow(val) > 0){
    if (removeNA){
      acmfdata_hs[acmfdata_hs$dose == 17.5 & !is.na(acmfdata_ls$uci),]$uci <- val$ub[1]
    }else{
      acmfdata_hs[acmfdata_hs$dose == 17.5,]$uci <- val$ub[1]
    }
  }
  
  sum_hs_lower_tp <- sum(acmfdata$totalpersons * acmfdata_hs$uci, na.rm = T)
  
  sum_tp <- sum(acmfdata$totalpersons * acmfdata$uci, na.rm = T)
  
  pert_hs_lower <- ((sum_tp - sum_hs_lower_tp) / sum_tp) * 100
  
  acmfdata_hs <- local_var
  
  if (nrow(val) > 0){
    if (removeNA){
      acmfdata_hs[acmfdata_hs$dose == 17.5 & !is.na(acmfdata_ls$uci),]$lci <- val$lb[1]
    }else{
      acmfdata_hs[acmfdata_hs$dose == 17.5,]$lci <- val$lb[1]
    }
  }
  
  sum_hs_upper_tp <- sum(acmfdata$totalpersons * acmfdata_hs$lci, na.rm = T)
  
  sum_tp <- sum(acmfdata$totalpersons * acmfdata$lci, na.rm = T)
  
  pert_hs_upper <- ((sum_tp - sum_hs_upper_tp) / sum_tp) * 100
  
  upper_guideline_value <<- paste0(round(pert_hs, 2) , "% (95% CI: ", round(pert_hs_lower, 2), " - ",  round(pert_hs_upper, 2), ")" )
  
  m[1,6] <- round(pert_hs, 2)
  
  m[1,7] <- round(pert_hs_lower, 2)
  
  m[1,8] <- round(pert_hs_upper, 2)
  
  m
  
}


simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}