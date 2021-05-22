######### CREATE combined data frame from 2011-2014 NHANES DATA (G and H series) ############

GH_data <- function(d_G, d_H){
  
  d_G$series <- 'G'
  d_H$series <- 'H'
  
  # df_types <- 
  #   tibble(
  #     G_names = names(d_G),
  #     G_type = map_chr(d_G, function(x) class(x)[1]),
  #     H_names = names(d_H),
  #     H_type = map_chr(d_H, function(x) class(x)[1]),
  #     Equal_names = G_names == H_names,
  #     Equal_types = G_type == H_type
  #   )
  
  d_GH <- bind_rows(d_G, d_H)
  
  # Create 4 year weights # https://wwwn.cdc.gov/nchs/nhanes/tutorials/module3.aspx
  # We still use the same var name for the 2-year weights to remain
  # compatible with our "design_build" function "all_designs"
  
  d_GH$WTMEC2YR <- d_GH$WTMEC2YR/2 
  
  # Create numeric versions of dichotomous vars
  
  d_GH$sex2 <- ifelse(d_GH$sex == 'female', 0, 1)
  d_GH$depressed2 <- as.numeric(d_GH$depressed)
  d_GH$special_equipment2 <- as.numeric(d_GH$special_equipment)
  d_GH$whitebloodcell2 <- ifelse(d_GH$whitebloodcell < 12, 0, 1)
  d_GH$partnered2 <- as.numeric(d_GH$partnered)
  d_GH$perceived_abnormal_weight2 <- as.numeric(d_GH$perceived_abnormal_weight)
  
  # create combinded depressed status
  d_GH$depressed_combined <- d_GH$depressed | d_GH$depressed_selfreport
  
  # save(d_GH, file='d_GH.rda')
  return(d_GH)
}





# Create designs ----------------------------------------------------------

# adults = d$age>=18 & d$age<=60
# 
# d.design.G = 
#   svydesign(
#     id = ~SDMVPSU , 
#     strata = ~SDMVSTRA ,
#     nest = TRUE ,
#     weights = ~WTMEC2YR, #~WTINT2YR ,
#     data = d_G
#   )
# 
# d.design.adults.G <-
#   subset(
#     d.design.G ,
#     adults
#   )
# 
# 
# d.design.H = 
#   svydesign(
#     id = ~SDMVPSU , 
#     strata = ~SDMVSTRA ,
#     nest = TRUE ,
#     weights = ~WTMEC2YR, #~WTINT2YR ,
#     data = d_H
#   )
# 
# d.design.adults.H <-
#   subset(
#     d.design.H ,
#     adults
#   )
# 
#   
#   
#  d.design.GH = 
#     svydesign(
#       id = ~SDMVPSU , 
#       strata = ~SDMVSTRA ,
#       nest = TRUE ,
#       weights = ~MEC4YR, #~WTMEC2YR, #~WTINT2YR,
#       data = d_GH
#     )
#   
#   
#   d.design.adults.GH <-
#     subset(
#       d.design.GH ,
#       adults
#     )
#   
#   # d.design.male <-
#   #   subset(
#   #     d.design ,
#   #     sex == 'male'
#   #   )
#   # 
#   # d.design.female <-
#   #   subset(
#   #     d.design ,
#   #     sex == 'female'
#   #   )
#   # 
#   # 
#   # d.design.healthy_adults <-
#   #   subset(
#   #     d.design ,
#   #     age >= 18 & disability_score == 0
#   #   )
#   
#   # Center and Standardize --------------------------------------------------
#   
#   
#   # Center and standardize variables for models with interactions
#   # Divide by 2SD, per Gelman 2008
#   
#   mean_age = svymean(~age, d.design.adults)[[1]]
#   sd_age = sqrt(svyvar(~age, d.design.adults)[[1]])
#   d.design.adults = update(d.design.adults, age_centered = (age-mean_age)/(2*sd_age))
#   low_age = signif(mean_age - sd_age, 2) # -1 SD
#   high_age = signif(mean_age + sd_age, 2) # -1 SD
#   
#   mean_weight = svymean(~weight, d.design.adults, na.rm=T)[[1]]
#   sd_weight = sqrt(svyvar(~weight, d.design.adults, na.rm=T)[[1]])
#   d.design.adults = update(d.design.adults, weight_centered = (weight-mean_weight)/(2*sd_weight))
#   
#   mean_height = svymean(~height, d.design.adults, na.rm=T)[[1]]
#   sd_height = sqrt(svyvar(~height, d.design.adults, na.rm=T)[[1]])
#   d.design.adults = update(d.design.adults, height_centered = (height-mean_height)/(2*sd_height))
#   
#   mean_bmi = svymean(~bmi, d.design.adults, na.rm=T)[[1]]
#   sd_bmi = sqrt(svyvar(~bmi, d.design.adults, na.rm=T)[[1]])
#   d.design.adults = update(d.design.adults, bmi_centered = (bmi-mean_bmi)/(2*sd_bmi))
#   
#   mean_strength = svymean(~strength, d.design.adults, na.rm=T)[[1]]
#   sd_strength = sqrt(svyvar(~strength, d.design.adults, na.rm=T)[[1]])
#   d.design.adults = update(d.design.adults, strength_centered = (strength-mean_strength)/(2*sd_strength))
#   
#   mean_testosterone = svymean(~testosterone, d.design.adults, na.rm=T)[[1]]
#   sd_testosterone = sqrt(svyvar(~testosterone, d.design.adults, na.rm=T)[[1]])
#   d.design.adults = update(d.design.adults, testosterone_centered = (testosterone-mean_testosterone)/(2*sd_testosterone))
#   
#   # mean_T4free = svymean(~T4free, d.design.adults, na.rm=T)[[1]]
#   # sd_T4free = sqrt(svyvar(~T4free, d.design.adults, na.rm=T)[[1]])
#   # d.design.adults = update(d.design.adults, T4free_centered = (T4free-mean_T4free)/(2*sd_T4free))
#   
#   # mean_TSH = svymean(~TSH, d.design.adults, na.rm=T)[[1]]
#   # sd_TSH = sqrt(svyvar(~TSH, d.design.adults, na.rm=T)[[1]])
#   # d.design.adults = update(d.design.adults, TSH_centered = (TSH-mean_T4free)/(2*sd_TSH))
#   
#   mean_income = svymean(~income, d.design.adults, na.rm=T)[[1]]
#   sd_income = sqrt(svyvar(~income, d.design.adults, na.rm=T)[[1]])
#   d.design.adults = update(d.design.adults, income_centered = (income-mean_income)/(2*sd_income))
#   
#   mean_edu = svymean(~edu, d.design.adults, na.rm=T)[[1]]
#   sd_edu = sqrt(svyvar(~edu, d.design.adults, na.rm=T)[[1]])
#   d.design.adults = update(d.design.adults, edu_centered = (edu-mean_edu)/(2*sd_edu))
#   
#   # mean_days_poor_health = svymean(~days_poor_health, d.design.adults, na.rm=T)[[1]]
#   # sd_days_poor_health = sqrt(svyvar(~days_poor_health, d.design.adults, na.rm=T)[[1]])
#   # d.design.adults = update(d.design.adults, days_poor_health_centered = (days_poor_health-mean_days_poor_health)/(2*sd_days_poor_health))
#   
#   mean_whitebloodcell = svymean(~whitebloodcell, d.design.adults, na.rm=T)[[1]]
#   sd_whitebloodcell = sqrt(svyvar(~whitebloodcell, d.design.adults, na.rm=T)[[1]])
#   d.design.adults = update(d.design.adults, whitebloodcell_centered = (whitebloodcell-mean_whitebloodcell)/(2*sd_whitebloodcell))
#   
#   mean_redbloodcell = svymean(~redbloodcell, d.design.adults, na.rm=T)[[1]]
#   sd_redbloodcell = sqrt(svyvar(~redbloodcell, d.design.adults, na.rm=T)[[1]])
#   d.design.adults = update(d.design.adults, redbloodcell_centered = (redbloodcell-mean_redbloodcell)/(2*sd_redbloodcell))
#   
#   mean_hemoglobin = svymean(~hemoglobin, d.design.adults, na.rm=T)[[1]]
#   sd_hemoglobin = sqrt(svyvar(~hemoglobin, d.design.adults, na.rm=T)[[1]])
#   d.design.adults = update(d.design.adults, hemoglobin_centered = (hemoglobin-mean_hemoglobin)/(2*sd_hemoglobin))
#   
#   mean_disability_score = svymean(~disability_score, d.design.adults, na.rm=T)[[1]]
#   sd_disability_score = sqrt(svyvar(~disability_score, d.design.adults, na.rm=T)[[1]])
#   d.design.adults = update(d.design.adults, disability_score_centered = (disability_score-mean_disability_score)/(2*sd_disability_score))
#   
#   # Sex specific
#   
#   d.design.adult.male <-
#     subset(
#       d.design ,
#       adults & sex == 'male'
#     )
#   
#   d.design.adult.female <-
#     subset(
#       d.design ,
#       adults & sex == 'female'
#     )
#   
# 
# 
# 
