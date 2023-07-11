library(survey)

# Center and Standardize --------------------------------------------------

standardize_vars <- function(design){

  # Center and standardize variables for models with interactions
  # Divide by 2SD, per Gelman 2008

  mean_age = svymean(~age, design)[[1]]
  sd_age = sqrt(svyvar(~age, design)[[1]])
  design = update(design, age_centered = (age-mean_age)/(2*sd_age))
  low_age = signif(mean_age - sd_age, 2) # -1 SD
  high_age = signif(mean_age + sd_age, 2) # -1 SD

  mean_weight = svymean(~weight, design, na.rm=T)[[1]]
  sd_weight = sqrt(svyvar(~weight, design, na.rm=T)[[1]])
  design = update(design, weight_centered = (weight-mean_weight)/(2*sd_weight))

  mean_height = svymean(~height, design, na.rm=T)[[1]]
  sd_height = sqrt(svyvar(~height, design, na.rm=T)[[1]])
  design = update(design, height_centered = (height-mean_height)/(2*sd_height))

  mean_bmi = svymean(~bmi, design, na.rm=T)[[1]]
  sd_bmi = sqrt(svyvar(~bmi, design, na.rm=T)[[1]])
  design = update(design, bmi_centered = (bmi-mean_bmi)/(2*sd_bmi))

  mean_strength = svymean(~strength, design, na.rm=T)[[1]]
  sd_strength = sqrt(svyvar(~strength, design, na.rm=T)[[1]])
  design = update(design, strength_centered = (strength-mean_strength)/(2*sd_strength))

  mean_testosterone = svymean(~testosterone, design, na.rm=T)[[1]]
  sd_testosterone = sqrt(svyvar(~testosterone, design, na.rm=T)[[1]])
  design = update(design, testosterone_centered = (testosterone-mean_testosterone)/(2*sd_testosterone))

  mean_test_sex <- svyby(~testosterone, by =~sex, design = design, FUN = svymean, na.rm = T)
  var_test_sex <- svyby(~testosterone, by =~sex, design = design, FUN = svyvar, na.rm = T)
  design = update(
    design,
    testosterone_sex_centered = ifelse(
      sex == 'female',
      (testosterone - mean_test_sex$testosterone[2])/(2*sqrt(var_test_sex$testosterone[2])),
      (testosterone - mean_test_sex$testosterone[1])/(2*sqrt(var_test_sex$testosterone[1]))
    ))

  mean_income = svymean(~income, design, na.rm=T)[[1]]
  sd_income = sqrt(svyvar(~income, design, na.rm=T)[[1]])
  design = update(design, income_centered = (income-mean_income)/(2*sd_income))

  mean_edu = svymean(~edu, design, na.rm=T)[[1]]
  sd_edu = sqrt(svyvar(~edu, design, na.rm=T)[[1]])
  design = update(design, edu_centered = (edu-mean_edu)/(2*sd_edu))

  mean_whitebloodcell = svymean(~whitebloodcell, design, na.rm=T)[[1]]
  sd_whitebloodcell = sqrt(svyvar(~whitebloodcell, design, na.rm=T)[[1]])
  design = update(design, whitebloodcell_centered = (whitebloodcell-mean_whitebloodcell)/(2*sd_whitebloodcell))

  mean_redbloodcell = svymean(~redbloodcell, design, na.rm=T)[[1]]
  sd_redbloodcell = sqrt(svyvar(~redbloodcell, design, na.rm=T)[[1]])
  design = update(design, redbloodcell_centered = (redbloodcell-mean_redbloodcell)/(2*sd_redbloodcell))

  mean_hemoglobin = svymean(~hemoglobin, design, na.rm=T)[[1]]
  sd_hemoglobin = sqrt(svyvar(~hemoglobin, design, na.rm=T)[[1]])
  design = update(design, hemoglobin_centered = (hemoglobin-mean_hemoglobin)/(2*sd_hemoglobin))

  mean_disability_score = svymean(~disability_score, design, na.rm=T)[[1]]
  sd_disability_score = sqrt(svyvar(~disability_score, design, na.rm=T)[[1]])
  design = update(design, disability_score_centered = (disability_score-mean_disability_score)/(2*sd_disability_score))

  mean_chronic_score <- svymean(~chronic_disease_score, design, na.rm=T)[[1]]
  sd_chronic_score <- sqrt(svyvar(~chronic_disease_score, design, na.rm=T))[[1]]
  design <- update(design, chronic_disease_score_centered = (chronic_disease_score - mean_chronic_score)/(2*sd_chronic_score))

  mean_physical_count <- svymean(~physical_disease_count, design, na.rm=T)[[1]]
  sd_physical_count <- sqrt(svyvar(~physical_disease_count, design, na.rm=T))[[1]]
  design <- update(design, physical_disease_count_centered = (physical_disease_count - mean_physical_count)/(2*sd_physical_count))

  return(design)

}

all_designs <- function(d){

  adults = d$age>=18 & d$age<=60

  d.design <-
    svydesign(
      id = ~SDMVPSU ,
      strata = ~SDMVSTRA ,
      nest = TRUE ,
      weights = ~WTMEC2YR, #~WTINT2YR ,
      data = d
    )

  d.design.adults <-
    subset(
      d.design,
      adults
    )

  d.design.adults.maximal <-
    subset(
      d.design,
      adults & !questionable_effort
    )

  d.design.healthy_adults <-
    subset(
      d.design,
      age >= 18 & disability_score == 0
    )

  # Sex specific

  d.design.male <-
    subset(
      d.design,
      sex == 'male'
    )

  d.design.female <-
    subset(
      d.design,
      sex == 'female'
    )

  d.design.adult.male <-
    subset(
      d.design.adults,
      sex == 'male'
    )

  d.design.adult.female <-
    subset(
      d.design.adults,
      sex == 'female'
    )

 # adults_diet = d_diet$age>=18 & d_diet$age<=60

  d_diet <- d[!is.na(d$WTDR2D), ]
  adults_diet = d_diet$age>=18 & d_diet$age<=60

  d.design.dietary <-
    svydesign(
      id = ~SDMVPSU ,
      strata = ~SDMVSTRA ,
      nest = TRUE ,
      weights = ~WTDR2D , #~WTMEC2YR, #~WTINT2YR ,
      data = d_diet
    )

  d.design.dietary.adults <-
    subset(
      d.design.dietary,
      adults_diet
    )

  d.design.adults <- standardize_vars(d.design.adults)
  d.design.adults.maximal <- standardize_vars(d.design.adults.maximal)
  d.design.dietary.adults <- standardize_vars(d.design.dietary.adults)

  return(list(
    'd.design' = d.design,
    'd.design.female' = d.design.female,
    'd.design.male' = d.design.male,
    'd.design.adults' = d.design.adults,
    'd.design.adults.maximal' = d.design.adults.maximal,
    'd.design.adult.female' = d.design.adult.female,
    'd.design.adult.male' = d.design.adult.male,
    'd.design.healthy_adults' = d.design.healthy_adults,
    'd.design.dietary.adults' = d.design.dietary.adults
  ))

}
