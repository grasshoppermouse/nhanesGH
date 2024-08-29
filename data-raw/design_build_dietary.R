library(survey)

# Center and Standardize --------------------------------------------------

svy_standardize_var <- function(design, var, newvar){
  mean_var <- svymean(make.formula(var), design, na.rm = T)[[1]]
  sd_var <- sqrt(svyvar(make.formula(var), design, na.rm = T)[[1]])
  design$variables[[newvar]] <- (design$variables[[var]] - mean_var)/(2*sd_var)
  return(design)
}

svy_standardize_sex <- function(design, var, newvar){
  mean_var_sex <- svyby(make.formula(var), by =~sex, design = design, FUN = svymean, na.rm = T)
  mean_var_sex <- mean_var_sex[[var]]
  sd_var_sex <- svyby(make.formula(var), by =~sex, design = design, FUN = svyvar, na.rm = T)
  sd_var_sex <- sqrt(sd_var_sex[[var]])
  design$variables[[newvar]] <- ifelse(
    design$variables$sex == 'female',
    (design$variables[[var]] - mean_var_sex[2])/(2*sd_var_sex[2]),
    (design$variables[[var]] - mean_var_sex[1])/(2*sd_var_sex[1])
  )
  return(design)
}

standardize_vars <- function(design){

  design <- svy_standardize_var(design, 'age', 'age_centered')
  design <- svy_standardize_var(design, 'weight', 'weight_centered')
  design <- svy_standardize_var(design, 'height', 'height_centered')
  design <- svy_standardize_var(design, 'bmi', 'bmi_centered')
  design <- svy_standardize_var(design, 'strength', 'strength_centered')
  design <- svy_standardize_sex(design, 'strength', 'strength_sex_centered')

  design <- svy_standardize_var(design, 'ArmLeanexclBMC', 'arm_lean_centered')
  design <- svy_standardize_var(design, 'LegLeanexclBMC', 'leg_lean_centered')
  design <- svy_standardize_var(design, 'TrunkLeanexclBMC', 'trunk_lean_centered')
  design <- svy_standardize_var(design, 'TotalLeanexclBMC', 'total_lean_centered')
  design <- svy_standardize_sex(design, 'ArmLeanexclBMC', 'arm_lean_sex_centered')
  design <- svy_standardize_sex(design, 'LegLeanexclBMC', 'leg_lean_sex_centered')
  design <- svy_standardize_sex(design, 'TrunkLeanexclBMC', 'trunk_lean_sex_centered')
  design <- svy_standardize_sex(design, 'TotalLeanexclBMC', 'total_lean_sex_centered')

  design <- svy_standardize_var(design, 'ArmLeaninclBMC', 'arm_leanbmc_centered')
  design <- svy_standardize_var(design, 'LegLeaninclBMC', 'leg_leanbmc_centered')
  design <- svy_standardize_var(design, 'TrunkLeaninclBMC', 'trunk_leanbmc_centered')
  design <- svy_standardize_var(design, 'TotalLeaninclBMC', 'total_leanbmc_centered')
  design <- svy_standardize_sex(design, 'ArmLeaninclBMC', 'arm_leanbmc_sex_centered')
  design <- svy_standardize_sex(design, 'LegLeaninclBMC', 'leg_leanbmc_sex_centered')
  design <- svy_standardize_sex(design, 'TrunkLeaninclBMC', 'trunk_leanbmc_sex_centered')
  design <- svy_standardize_sex(design, 'TotalLeaninclBMC', 'total_leanbmc_sex_centered')

  design <- svy_standardize_var(design, 'UpperLeanexclBMC', 'upper_lean_centered')
  design <- svy_standardize_sex(design, 'UpperLeanexclBMC', 'upper_lean_sex_centered')

  design <- svy_standardize_var(design, 'UpperLeaninclBMC', 'upper_leanbmc_centered')
  design <- svy_standardize_sex(design, 'UpperLeaninclBMC', 'upper_leanbmc_sex_centered')

  design <- svy_standardize_var(design, 'TotalLeanFat', 'total_leanfat_centered')
  design <- svy_standardize_var(design, 'TotalPercentFat', 'total_percentfat_centered')

  design <- svy_standardize_var(design, 'testosterone', 'testosterone_centered')
  design <- svy_standardize_sex(design, 'testosterone', 'testosterone_sex_centered')
  design <- svy_standardize_var(design, 'income', 'income_centered')
  design <- svy_standardize_var(design, 'edu', 'edu_centered')
  design <- svy_standardize_var(design, 'whitebloodcell', 'whitebloodcell_centered')
  design <- svy_standardize_var(design, 'redbloodcell', 'redbloodcell_centered')
  design <- svy_standardize_var(design, 'hemoglobin', 'hemoglobin_centered')
  design <- svy_standardize_var(design, 'disability_score', 'disability_score_centered')
  design <- svy_standardize_var(design, 'chronic_disease_score', 'chronic_disease_score_centered')
  design <- svy_standardize_var(design, 'physical_disease_count', 'physical_disease_count_centered')
  design <- svy_standardize_var(design, 'total_work_MET', 'total_work_MET_centered')
  design <- svy_standardize_var(design, 'total_rec_MET', 'total_rec_MET_centered')
  design <- svy_standardize_var(design, 'wob_MET', 'wob_MET_centered')
  design <- svy_standardize_var(design, 'tot_MET', 'tot_MET_centered')
  design <- svy_standardize_var(design, 'foodinsecurity_adult', 'foodinsecurity_adult_centered')
  design <- svy_standardize_var(design, 'avgcalories', 'avgcalories_centered')

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

  d.design.adult.heterosexual <-
    subset(
      d.design.adults,
      sexualorientation == 1
    )

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
  d.design.adult.female <- standardize_vars(d.design.adult.female)
  d.design.adult.male <- standardize_vars(d.design.adult.male)

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
