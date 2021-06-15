
######### CREATE data frame from 2011-2012 data-raw/NHANES data (G series) ############

### In H series but not in G series ### #fixed 04.30; extraneous and mismatched vars deleted, total vars = 105
#
# [1] "LBDTSTLC" "LBXEST"   "LBDESTLC" "LBXSHBG"  "LBDSHGLC" "AGQ030"
# [7] "MCQ151"   "MCQ160O"  "MCQ203"   "MCQ206"   "DIQ175X"

G_data <- function(){
# Demographics ------------------------------------------------------------

dem = read.xport('data-raw/NHANES data/DEMO_G.XPT')

# Marital status
dem$DMDMARTL[dem$DMDMARTL == 77 | dem$DMDMARTL == 9] <- NA

# PHQ-9 -------------------------------------------------------------------


# DPQ010 - Have little interest in doing things
# DPQ020 - Feeling down, depressed, or hopeless
# DPQ030 - Trouble sleeping or sleeping too much
# DPQ040 - Feeling tired or having little energy
# DPQ050 - Poor appetite or overeating
# DPQ060 - Feeling bad about yourself
# DPQ070 - Trouble concentrating on things
# DPQ080 - Moving or speaking slowly or too fast
# DPQ090 - Thought you would be better off dead


phq = read.xport('data-raw/NHANES data/DPQ_G.XPT')

phq[phq==7 | phq==9] = NA
phq$depression = rowSums(phq[,2:10])
phq$depression_scaled = phq$depression/27 # scale to [0, 1]; max depression score is 27
phq$depressed = phq$depression>=10
phq$depressed_factor = factor(phq$depressed)
phq$suicidal = phq$DPQ090 > 0




# Current Health ----------------------------------------------------------


# HSQ_G data
# Current health status during last 30 days
#
# SEQN - Respondent sequence number
# HSD010 - General health condition
# HSQ470 - no. of days physical health was not good
# HSQ480 - no. of days mental health was not good
# HSQ490 - inactive days due to phys./mental hlth
# HSQ493 - Pain make it hard for usual activities
# HSQ496 - How many days feel anxious
# HSQ500 - SP have head cold or chest cold
# HSQ510 - SP have stomach or intestinal illness?
# HSQ520 - SP have flu, pneumonia, ear infection?
# HSQ571 - SP donated blood in past 12 months?
# HSQ580 - How long ago was last blood donation?
# HSQ590 - Blood ever tested for HIV virus?
# HSAQUEX - Source of Health Status Data

hsq = read.xport('data-raw/NHANES data/HSQ_G.XPT')
hsq[hsq==7 | hsq==9 | hsq==77 | hsq==99] = NA

hsq$general_health = ordered(hsq$HSD010)
hsq$days_poor_health = hsq$HSQ470



# Physical Functioning ----------------------------------------------------


# PFQ_G data
# Physical Functioning
# These data have checkpoints based on age and/or disability. Be careful!
#
# SEQN - Respondent sequence number

### Children or adolescents ###
# PFQ020 - Crawl, walk, run, play limitations
# PFQ030 - Long term impairment/health problem
# PFQ035A - CHECK ITEM
# PFQ041 - Receive Special Ed or Early Intervention
#
### Adults ###
# PFQ049 - Limitations keeping you from working
# PFQ051 - Limited in amount of work you can do
# PFQ054 - Need special equipment to walk
# PFQ057 - Experience confusion/memory problems
# PFQ058 - CHECK ITEM
# PFQ059 - Physical, mental, emotional limitations
#
# This CHECK ITEM IS IMPORTANT!
# PFQ059A - CHECK ITEM
# PFQ061A - Managing money difficulty
# PFQ061B - Walking for a quarter mile difficulty
# PFQ061C - Walking up ten steps difficulty
# PFQ061D - Stooping, crouching, kneeling difficulty
# PFQ061E - Lifting or carrying difficulty
# PFQ061F - House chore difficulty
# PFQ061G - Preparing meals difficulty
# PFQ061H - Walking between rooms on same floor
# PFQ061I - Standingup from armless chair difficulty
# PFQ061J - Getting in and out of bed difficulty
# PFQ061K - Using fork, knife, drinking from cup
# PFQ061L - Dressing yourself difficulty
# PFQ061M - Standing for long periods difficulty
# PFQ061N - Sitting for long periods difficulty
# PFQ061O - Reaching up over head difficulty
# PFQ061P - Grasp/holding small objects difficulty
# PFQ061Q - Going out to movies, events difficulty
# PFQ061R - Attending social event difficulty
# PFQ061S - Leisure activity at home difficulty
# PFQ061T - Push or pull large objects difficulty
# PFQ066A - CHECK ITEM
# PFQ063A - Health problems causing difficulty
# PFQ063B - Health problems causing difficulty
# PFQ063C - Health problems causing difficulty
# PFQ063D - Health problems causing difficulty
# PFQ063E - Health problems causing difficulty
# PFQ090 - Require special healthcare equipment

# BOX 1E CHECK ITEM PFQ.059A: IF SP AGE IS <=59
# AND 'NO' (CODE 2) ENTERED IN PFQ.049, PFQ.057 AND PFQ.059, GO TO PFQ.090.
# OTHERWISE, CONTINUE.

pfq = read.xport('data-raw/NHANES data/PFQ_G.XPT')

# Physical (not emotional) disability for adults age 20 or higher

# Rated from 1 (no difficulty) to 4 (unable to do).
# There is also a 5 (do not do this activity), which
# probably should also be included.
# Items B and C not displayed if participant answered
# "yes" to "Need special equipment to walk"

disability_items = c(
  "PFQ061B", # Walking for a quarter mile difficulty
  "PFQ061C", # Walking up ten steps difficulty
  "PFQ061D", # Stooping, crouching, kneeling difficulty
  "PFQ061E", # Lifting or carrying difficulty
  "PFQ061H", # Walking between rooms on same floor
  "PFQ061I", # Standingup from armless chair difficulty
  "PFQ061J", # Getting in and out of bed difficulty
  "PFQ061K", # Using fork, knife, drinking from cup
  "PFQ061L", # Dressing yourself difficulty
  "PFQ061M", # Standing for long periods difficulty
  "PFQ061N", # Sitting for long periods difficulty
  "PFQ061O", # Reaching up over head difficulty
  "PFQ061P", # Grasp/holding small objects difficulty
  "PFQ061T"  # Push or pull large objects difficulty
)

pfq <- left_join(pfq, dem[c('SEQN', 'RIDAGEYR')])

# Health problems
# PFQ063A - Health problems causing difficulty
# PFQ063B - Health problems causing difficulty
# PFQ063C - Health problems causing difficulty
# PFQ063D - Health problems causing difficulty
# PFQ063E - Health problems causing difficulty

health_coding <- function(v){
  sum(!v %in% c(14, 28, 77, 99, NA), na.rm=T)
}

depression_coding <- function(v){
  14 %in% v
}

pfq <-
  pfq %>%
  # select(PFQ063A:PFQ063E) %>%
  rowwise() %>%
  mutate(
    physical_disease_count = health_coding(c_across(PFQ063A:PFQ063E)),
    depressed_selfreport = depression_coding(c_across(PFQ063A:PFQ063E))
  ) %>%
  ungroup

### This disability score is more comprehensive but is only defined for ages >= 20 ###

pfq20 <- pfq[pfq$RIDAGEYR>=20 & !is.na(pfq$PFQ054),] # Missing data for age < 20; also, one person is NA on all pfq vars

# BOX 1D. CHECK ITEM PFQ.058:
# IF 'YES' (CODE 1) IN PFQ.049, PFQ.051, PFQ.054, OR PFQ.057,
# GO TO PFQ.061. OTHERWISE, CONTINUE.

disability_check1 <- pfq20$PFQ049==1 | pfq20$PFQ051==1 | pfq20$PFQ054==1 | pfq20$PFQ057==1
disability_check2 <- pfq20$RIDAGEYR<=59 & pfq20$PFQ049==2 & pfq20$PFQ057==2 & pfq20$PFQ059==2

disability_check <- disability_check1 | !disability_check2

pfq20$disability_check <- disability_check

# Max could be 4 or 5 because 5
# means "do not do this activity", which
# could indicate impairment in that activity

max_severity <- 5

disability <-
  (pfq20$PFQ054==1) |
  ((pfq20$PFQ054==2) & (pfq20$PFQ061B>=2 & pfq20$PFQ061B<=max_severity)) |
  ((pfq20$PFQ054==2) & (pfq20$PFQ061C>=2 & pfq20$PFQ061C<=max_severity)) |
  (pfq20$PFQ061D>=2 & pfq20$PFQ061D<=max_severity) |
  (pfq20$PFQ061E>=2 & pfq20$PFQ061E<=max_severity) |
  (pfq20$PFQ061H>=2 & pfq20$PFQ061H<=max_severity) |
  (pfq20$PFQ061I>=2 & pfq20$PFQ061I<=max_severity) |
  (pfq20$PFQ061J>=2 & pfq20$PFQ061J<=max_severity) |
  (pfq20$PFQ061K>=2 & pfq20$PFQ061K<=max_severity) |
  (pfq20$PFQ061L>=2 & pfq20$PFQ061L<=max_severity) |
  (pfq20$PFQ061M>=2 & pfq20$PFQ061M<=max_severity) |
  (pfq20$PFQ061N>=2 & pfq20$PFQ061N<=max_severity) |
  (pfq20$PFQ061O>=2 & pfq20$PFQ061O<=max_severity) |
  (pfq20$PFQ061P>=2 & pfq20$PFQ061P<=max_severity) |
  (pfq20$PFQ061T>=2 & pfq20$PFQ061T<=max_severity)


pfq20$disability <- FALSE
pfq20$disability[disability_check] <- disability[disability_check]

disability_score <-
  (pfq20$PFQ054==1)*2 + # Equivalent to "yes" for both 61B and 61C
  ((pfq20$PFQ054==2) & (pfq20$PFQ061B>=2 & pfq20$PFQ061B<=max_severity)) +
  ((pfq20$PFQ054==2) & (pfq20$PFQ061C>=2 & pfq20$PFQ061C<=max_severity)) +
  (pfq20$PFQ061D>=2 & pfq20$PFQ061D<=max_severity) +
  (pfq20$PFQ061E>=2 & pfq20$PFQ061E<=max_severity) +
  (pfq20$PFQ061H>=2 & pfq20$PFQ061H<=max_severity) +
  (pfq20$PFQ061I>=2 & pfq20$PFQ061I<=max_severity) +
  (pfq20$PFQ061J>=2 & pfq20$PFQ061J<=max_severity) +
  (pfq20$PFQ061K>=2 & pfq20$PFQ061K<=max_severity) +
  (pfq20$PFQ061L>=2 & pfq20$PFQ061L<=max_severity) +
  (pfq20$PFQ061M>=2 & pfq20$PFQ061M<=max_severity) +
  (pfq20$PFQ061N>=2 & pfq20$PFQ061N<=max_severity) +
  (pfq20$PFQ061O>=2 & pfq20$PFQ061O<=max_severity) +
  (pfq20$PFQ061P>=2 & pfq20$PFQ061P<=max_severity) +
  (pfq20$PFQ061T>=2 & pfq20$PFQ061T<=max_severity)

pfq20$disability_score <- 0 # Absence of this line explains different results in commit 19a7b54
pfq20$disability_score[disability_check] <- disability_score[disability_check]

### Walk run only ###

locomotion_disability <-
  (pfq20$PFQ054==1) |
  ((pfq20$PFQ054==2) & (pfq20$PFQ061B>=2 & pfq20$PFQ061B<=4)) |
  ((pfq20$PFQ054==2) & (pfq20$PFQ061C>=2 & pfq20$PFQ061C<=4)) |
  (pfq20$PFQ061H>=2 & pfq20$PFQ061H<=4)

pfq20$locomotion_disability <- FALSE
pfq20$locomotion_disability[disability_check] <- locomotion_disability[disability_check]

# Now add in equivalent for 18-20 year olds

### This disability variable is defined for 18 <= age < 20, but is only for walk, run ###

pfq18 <- pfq[pfq$RIDAGEYR>=18 & pfq$RIDAGEYR<20,]

# Add NA columns to match pfq20
pfq18$disability_check <- NA
pfq18$disability <- NA
pfq18$disability_score <- NA

pfq18$locomotion_disability <- 0
pfq18$locomotion_disability[!is.na(pfq18$PFQ020)] <- pfq18$PFQ020[!is.na(pfq18$PFQ020)] == 1 # walk, run limitations

# Merge two data frames
pfq <- rbind(pfq18, pfq20)


# Body Measurements -------------------------------------------------------


# BMX_G Body measurements

# SEQN - Respondent sequence number
# BMDSTATS - Body Measures Component Status Code
# BMXWT - Weight (kg)
# BMIWT - Weight Comment
# BMXRECUM - Recumbent Length (cm)
# BMIRECUM - Recumbent Length Comment
# BMXHEAD - Head Circumference (cm)
# BMIHEAD - Head Circumference Comment
# BMXHT - Standing Height (cm)
# BMIHT - Standing Height Comment
# BMXBMI - Body Mass Index (kg/m**2)
# BMDBMIC - BMI Category - Children/Adolescents
# BMXLEG - Upper Leg Length (cm)
# BMILEG - Upper Leg Length Comment
# BMXARML - Upper Arm Length (cm)
# BMIARML - Upper Arm Length Comment
# BMXARMC - Arm Circumference (cm)
# BMIARMC - Arm Circumference Comment
# BMXWAIST - Waist Circumference (cm)
# BMIWAIST - Waist Circumference Comment
# BMXSAD1 - Sagittal Abdominal Diameter 1st (cm)
# BMXSAD2 - Sagittal Abdominal Diameter 2nd (cm)
# BMXSAD3 - Sagittal Abdominal Diameter 3rd (cm)
# BMXSAD4 - Sagittal Abdominal Diameter 4th (cm)
# BMDSADCM - Sagittal Abdominal Diameter Comment

bmx = read.xport('data-raw/NHANES data/BMX_G.XPT')


# Grip Strength -----------------------------------------------------------


# MGX Grip strength data

# SEQN - Respondent sequence number
# MGDEXSTS - Grip test status
# MGD050 - Ever had surgery on hands or wrists
# MGD060 - Which hand or wrist had surgery
# MGQ070 - Recent pain/aching/stiffness-right hand
# MGQ080 - Cause of recent pain in right hand
# MGQ090 - Pain in right hand gotten worse recently
# MGQ100 - Recent pain/aching/stiffness-left hand
# MGQ110 - Cause of recent pain in left hand
# MGQ120 - Pain in left hand gotten worse recently
# MGD130 - Dominant hand
# MGQ90DG - 90 degree angle with index finger
# MGDSEAT - Testing position
# MGAPHAND - Hand assigned for practice trial
# MGATHAND - Begin the test with this hand.
# MGXH1T1 - Grip strength (kg), hand 1, test 1
# MGXH1T1E - Grip strength, hand 1, test 1 effort
# MGXH2T1 - Grip strength (kg), hand 2, test 1
# MGXH2T1E - Grip strength, hand 2, test 1 effort
# MGXH1T2 - Grip strength (kg), hand 1, test 2
# MGXH1T2E - Grip strength, hand 1, test 2 effort
# MGXH2T2 - Grip strength (kg), hand 2, test 2
# MGXH2T2E - Grip strength, hand 2, test 2 effort
# MGXH1T3 - Grip strength (kg), hand 1, test 3
# MGXH1T3E - Grip strength, hand 1, test 3 effort
# MGXH2T3 - Grip strength (kg), hand 2, test 3
# MGXH2T3E - Grip strength, hand 2, test 3 effort
# MGDCGSZ - Combined grip strength (kg)



msx = read.xport('data-raw/NHANES data/MGX_G.XPT')

# Fasting -----------------------------------------------------------------

# Fasting data, which as the time period of the venous draw (for testosterone)

# PHDSESN - Session in which SP was examined
#
# 0	morning
# 1	afternoon
# 2	evening

fastqx = read.xport('data-raw/NHANES data/FASTQX_G.XPT')



# Testosterone ------------------------------------------------------------


# Serum testosterone (ng/dL): LBXTST
# Relevant to upper body strength and
# reported suppressive effect on depression

tst = read.xport('data-raw/NHANES data/TST_G.XPT')


# Thyroid -----------------------------------------------------------------



# Freely circulating thyroxine and thyroid stimulating hormone - LBXT4F (ng/dL) and LBDTSH1S (mIU/L)
# Confounder; may both decrease strength and increase depression (1/3 sample only!!!)

thyrod = read.xport('data-raw/NHANES data/THYROD_G.XPT')


# Weight ------------------------------------------------------------------



# Weight history
# Relevant to reported assocation of body image and depression
#
# WHQ030 How do you consider your weight
# 1	Overweight
# 2	Underweight
# 3	About the right weight?
# 7	Refused
# 9	Don't know
# .	Missing
#
# WHQ040 - Like to weigh more, less or same

# 1	More
# 2	Less
# 3	Stay about the same?
# 7	Refused	3
# 9	Don't know
# .	Missing

whq = read.xport('data-raw/NHANES data/WHQ_G.XPT')
whq[whq==7 | whq==9 | whq==77 | whq==99] = NA

# Still need to merge tst and whq


# Complete blood ----------------------------------------------------------


# Complete blood count

# SEQN - Respondent sequence number
# LBXWBCSI - White blood cell count (1000 cells/uL)
# LBXLYPCT - Lymphocyte percent (%)
# LBXMOPCT - Monocyte percent (%)
# LBXNEPCT - Segmented neutrophils percent (%)
# LBXEOPCT - Eosinophils percent (%)
# LBXBAPCT - Basophils percent (%)
# LBDLYMNO - Lymphocyte number (1000 cells/uL)
# LBDMONO - Monocyte number (1000 cells/uL)
# LBDNENO - Segmented neutrophils num (1000 cell/uL)
# LBDEONO - Eosinophils number (1000 cells/uL)
# LBDBANO - Basophils number (1000 cells/uL)
# LBXRBCSI - Red blood cell count (million cells/uL)
# LBXHGB - Hemoglobin (g/dL)
# LBXHCT - Hematocrit (%)
# LBXMCVSI - Mean cell volume (fL)
# LBXMCHSI - Mean cell hemoglobin (pg)
# LBXMC - MCHC (g/dL)
# LBXRDW - Red cell distribution width (%)
# LBXPLTSI - Platelet count (1000 cells/uL)
# LBXMPSI - Mean platelet volume (fL)

cbc = read.xport('data-raw/NHANES data/CBC_G.XPT')


## add chronic disease data for exploratory analysis re confounds (eg Jokela et al., 2019)


# Medical conditions ------------------------------------------------------


# medical conditions

# SEQN - Respondent sequence number
# MCQ010 - Ever been told you have asthma
# MCQ025 - Age when first had asthma
# MCQ035 - Still have asthma
# MCQ040 - Had asthma attack in past year
# MCQ050 - Emergency care visit for asthma/past yr
# MCQ051 - Dr. prescribed medication for asthma?
# MCQ053 - Taking treatment for anemia/past 3 mos
# MCQ070 - Ever been told you have psoriasis?
# MCQ075 - Degree of Psoriasis
# MCQ080 - Doctor ever said you were overweight
# MCQ082 - Ever been told you have celiac disease?
# MCQ084 - Difficulties in thinking or remembering
# MCQ086 - Are you on a gluten-free diet?
# MCQ092 - Ever receive blood transfusion
# MCD093 - Year receive blood transfusion
# MCQ140 - Trouble seeing even with glass/contacts
# MCQ149 - Menstrual periods started yet?
# MCQ160a - Doctor ever said you had arthritis
# MCQ180a - Age when told you had arthritis
# MCQ195 - Which type of arthritis was it?
# MCQ160n - Doctor ever told you that you had gout?
# MCQ180n - Age when told you had gout.
# MCQ160b - Ever told had congestive heart failure
# MCQ180b - Age when told you had heart failure
# MCQ160c - Ever told you had coronary heart disease
# MCQ180c - Age when told had coronary heart disease
# MCQ160d - Ever told you had angina/angina pectoris
# MCQ180d - Age when told you had angina pectoris
# MCQ160e - Ever told you had heart attack
# MCQ180e - Age when told you had heart attack
# MCQ160f - Ever told you had a stroke
# MCQ180f - Age when told you had a stroke
# MCQ160g - Ever told you had emphysema
# MCQ180g - Age when told you had emphysema
# MCQ160m - Ever told you had thyroid problem
# MCQ170m - Do you still have thyroid problem
# MCQ180m - Age when told you had thyroid problem
# MCQ160k - Ever told you had chronic bronchitis
# MCQ170k - Do you still have chronic bronchitis
# MCQ180k - Age when told you had chronic bronchitis
# MCQ160l - Ever told you had any liver condition
# MCQ170l - Do you still have a liver condition
# MCQ180l - Age when told you had a liver condition
# MCQ220 - Ever told you had cancer or malignancy
# MCQ230a - What kind of cancer
# MCQ230b - What kind of cancer
# MCQ230c - What kind of cancer
# MCQ230d - What kind of cancer
# MCQ240a - Age when bladder cancer first diagnosed
# MCQ240aa - Age testicular cancer first diagnosed
# MCQ240b - Age when blood cancer first diagnosed
# MCQ240bb - Age when thyroid cancer first diagnosed
# MCQ240c - Age when bone cancer first diagnosed
# MCQ240cc - Age when uterine cancer first diagnosed
# MCQ240d - Age when brain cancer first diagnosed
# MCQ240dd - Age other type of cancer first diagnosed
# MCQ240dk - How old when cancer first diagnosed?
# MCQ240e - Age when breast cancer first diagnosed
# MCQ240f - Age when cervical cancer first diagnosed
# MCQ240g - Age when colon cancer first diagnosed
# MCQ240h - Age esophageal cancer first diagnosed
# MCQ240i - Age gallbladder cancer first diagnosed
# MCQ240j - Age when kidney cancer first diagnosed
# MCQ240k - Age larynx/windpipe cancer diagnosed
# MCQ240l - Age when leukemia first diagnosed
# MCQ240m - Age when liver cancer first diagnosed
# MCQ240n - Age when lung cancer first diagnosed
# MCQ240o - Age lymphoma/Hodgkins' diagnosed
# MCQ240p - Age when melanoma first diagnosed
# MCQ240q - Age mouth\tongue\lip cancer diagnosed
# MCQ240r - Age nervous system cancer diagnosed
# MCQ240s - Age when ovarian cancer first diagnosed
# MCQ240t - Age pancreatic cancer first diagnosed
# MCQ240u - Age prostate cancer first diagnosed
# MCQ240v - Age when rectal cancer first diagnosed
# MCQ240w - Age non-melanoma skin cancer diagnosed
# MCQ240x - Age unknown skin cancer first diagnosed
# MCQ240y - Age soft tissue cancer first diagnosed
# MCQ240z - Age stomach cancer first diagnosed
# MCQ300a - Close relative had heart attack?
# MCQ300b - Close relative had asthma?
# MCQ300c - Close relative had diabetes?
# MCQ365a - Doctor told you to lose weight
# MCQ365b - Doctor told you to exercise
# MCQ365c - Doctor told you to reduce salt in diet
# MCQ365d - Doctor told you to reduce fat/calories
# MCQ370a - Are you now controlling or losing weight
# MCQ370b - Are you now increasing exercise
# MCQ370c - Are you now reducing salt in diet
# MCQ370d - Are you now reducing fat in diet
# MCQ380 - Past 7 days, had trouble remembering

mcq = read.xport('data-raw/NHANES data/MCQ_G.XPT')
mcq[mcq==7 | mcq==9 | mcq==77 | mcq==99] = NA


mcq$asthma <- ifelse(mcq$MCQ010 == 1, TRUE, FALSE) # - Ever been told you have asthma
mcq$heart_disease <- ifelse(mcq$MCQ160C == 1, TRUE, FALSE) # - Ever told you had coronary heart disease
mcq$stroke <- ifelse(mcq$MCQ160F ==1, TRUE, FALSE)  # - Ever told you had a stroke
mcq$bronchitis <-  ifelse(mcq$MCQ160K == 1, TRUE, FALSE) # - Ever told you had chronic bronchitis
mcq$cancer <- ifelse(mcq$MCQ220== 1, TRUE, FALSE) # - Ever told you had cancer or malignancy
mcq$emphysema <- ifelse(mcq$MCQ160G == 1, TRUE, FALSE) #- Ever told you had emphysema
mcq$arthritis <- ifelse(mcq$MCQ160A == 1, TRUE, FALSE) # - Doctor ever said you had arthritis



# Diabetes ----------------------------------------------------------------


## diabetes

# SEQN - Respondent sequence number
# DIQ010 - Doctor told you have diabetes
# DID040 - Age when first told you had diabetes
# DIQ159 - CHECK ITEM
# DIQ160 - Ever told you have prediabetes
# DIQ170 - Ever told have health risk for diabetes
# DIQ172 - Feel could be at risk for diabetes
# DIQ175A - Family history
# DIQ175B - Overweight
# DIQ175C - Age
# DIQ175D - Poor diet
# DIQ175E - Race
# DIQ175F - Had a baby weighed over 9 lbs. at birth
# DIQ175G - Lack of physical activity
# DIQ175H - High blood pressure
# DIQ175I - High blood sugar
# DIQ175J - High cholesterol
# DIQ175K - Hypoglycemic
# DIQ175L - Extreme hunger
# DIQ175M - Tingling/numbness in hands or feet
# DIQ175N - Blurred vision
# DIQ175O - Increased fatigue
# DIQ175P - Anyone could be at risk
# DIQ175Q - Doctor warning
# DIQ175R - Other, specify
# DIQ175S - Gestational diabetes
# DIQ175T - Frequent urination
# DIQ175U - Thirst
# DIQ175V - Craving for sweet/eating a lot of sugar
# DIQ175W - Medication
# DIQ180 - Had blood tested past three years
# DIQ050 - Taking insulin now
# DID060 - How long taking insulin
# DIQ060U - Unit of measure (month/year)
# DIQ065 - CHECK ITEM
# DIQ070 - Take diabetic pills to lower blood sugar
# DIQ229 - CHECK ITEM
# DIQ230 - How long ago saw a diabetes specialist
# DIQ240 - Is there one Dr you see for diabetes
# DID250 - Past year how many times seen doctor
# DID260 - How often check blood for glucose/sugar
# DIQ260U - Unit of measure (day/week/month/year)
# DIQ275 - Past year Dr checked for A1C
# DIQ280 - What was your last A1C level
# DIQ291 - What does Dr say A1C should be
# DIQ295 - CHECK ITEM
# DIQ300S - What was your recent SBP
# DIQ300D - What was your recent DBP
# DID310S - What does Dr say SBP should be
# DID310D - What does Dr say DBP should be
# DID320 - What was most recent LDL number
# DID330 - What does Dr say LDL should be
# DID341 - Past year times Dr check feet for sores
# DID350 - How often do you check your feet
# DIQ350U - Unit of measure (day/week/month/year)
# DIQ360 - Last time had pupils dilated for exam
# DIQ080 - Diabetes affected eyes/had retinopathy

diq = read.xport('data-raw/NHANES data/DIQ_G.XPT')
diq[diq==7 | diq==9 | diq==77 | diq==99] = NA

# DIQ010 - Doctor told you have diabetes
diq$diabetes <- ifelse(diq$DIQ010 == 1, TRUE, FALSE) ## 3 = borderline, currently coding as false


# Food security -----------------------------------------------------------

# FSDHH - Household food security category
# FSDAD - Adult food security category

fsq <- read.xport('data-raw/NHANES data/FSQ_G.XPT')


# Health care -------------------------------------------------------------

# HIQ011 - Are you covered by some health care plan

hiq <- read.xport('data-raw/NHANES data/HIQ_G.XPT')


# Physical activity -------------------------------------------------------

# PAQ605 - Does your work involve vigorous activity (large increase in breathing/heart rate)
# PAQ620 - Does your work involve moderate activity (small increase in breathing/heart rate)
# PAQ650 - Vigorous recreational activities
# PAQ665 - Moderate recreational activities

paq <- read.xport('data-raw/NHANES data/PAQ_G.XPT')


# Finances ----------------------------------------------------------------

# INQ244 - Does your family have at least 5000 in savings yes or no
# IND247 - For people who had under 5000 in savings, give savings in ranges

inq <- read.xport('data-raw/NHANES data/INQ_G.XPT')


# Sexual Behavior ---------------------------------------------------------

# In general, if a person wasn't asked a question, the response to the question is shown as “missing.”
# It is up to the analysts to recode the response to zero as they see fit. For example, if a respondent
# reported that they had zero lifetime vaginal sex partners, they wouldn't be asked the number of vaginal
# sex partners in the past 12 months. Since the respondent does not receive the question about partners
# in the past 12 months, the data will be coded as missing for this variable. Analysts may want to recode
# this to “0.”

# For questions that ascertain total number of partners for all types of sex
# (i.e., SXD171, SXD510, SXD101 and SXD450), if the respondent had reported never having any type of sex,
# the responses were coded as “0.”


sxq <- read.xport('data-raw/NHANES data/SXQ_G.XPT')
sxq[sxq==77777 | sxq==99999] = NA

# SXD021 - Ever had vaginal, anal, or oral sex
sxq$SXD021[sxq$SXD021==7 | sxq$SXD021==9] = NA
# SXQ800 - Ever had vaginal sex with a woman
sxq$SXQ800[sxq$SXQ800==7 | sxq$SXQ800==9] = NA
# SXQ803 - Ever performed oral sex on a woman
sxq$SXQ803[sxq$SXQ803==7 | sxq$SXQ803==9] = NA
# SXQ806 - Ever had anal sex with a woman
sxq$SXQ806[sxq$SXQ806==7 | sxq$SXQ806==9] = NA
# SXQ809 - Ever had any sex with a man: anal, oral
sxq$SXQ809[sxq$SXQ809==7 | sxq$SXQ809==9] = NA
# SXD862 - CHECK ITEM
# SXQ700 - Ever had vaginal sex with a man
sxq$SXQ700[sxq$SXQ700==7 | sxq$SXQ700==9] = NA
# SXQ703 - Ever performed oral sex on a man
sxq$SXQ703[sxq$SXQ703==7 | sxq$SXQ703==9] = NA
# SXQ706 - Ever had anal sex with a man
sxq$SXQ706[sxq$SXQ706==7 | sxq$SXQ706==9] = NA
# SXQ709 - Ever had any kind of sex with a woman
sxq$SXQ709[sxq$SXQ709==7 | sxq$SXQ709==9] = NA
# SXQ762 - CHECK ITEM

# SXD031 - How old when first had sex
sxq$SXD031[sxq$SXD031==77 | sxq$SXD031==99] = NA
# SXD801 - CHECK ITEM

# SXD171 - # female sex partners/lifetime
# SXD815 - CHECK ITEM
# SXD510 - # female sex partners/year
# SXQ821 - CHECK ITEM
# SXQ824 - # female vaginal sex partners/lifetime
# SXQ827 - # female vaginal sex partners/year
# SXQ830 - CHECK ITEM
# SXD633 - Age first performed oral sex on a woman
sxq$SXD633[sxq$SXD633==77 | sxq$SXD633==99] = NA

# SXQ636 - # female performed oral sex/lifetime
# SXQ639 - # female performed oral sex/year
# SXQ868 - CHECK ITEM
# SXD642 - Performed oral sex new female (days)
# SXQ833 - CHECK ITEM
# SXQ410 - # men anal/oral sex partners/lifetime
# SXQ875 - CHECK ITEM
# SXQ550 - # men anal/oral sex partners/year
# SXQ836 - # men anal sex partners/lifetime
# SXQ839 - CHECK ITEM
# SXQ841 - # men anal sex partners/year
# SXQ853 - Ever performed oral sex on a man
sxq$SXQ853[sxq$SXQ853==7 | sxq$SXQ853==9] = NA

# SXD847 - CHECK ITEM
# SXD621 - How old when first had oral sex
sxq$SXD621[sxq$SXD621==77 | sxq$SXD621==99] = NA

# SXQ624 - # male oral sex partners/lifetime
# SXQ850 - CHECK ITEM
# SXQ627 - # male oral sex partners/year
# SXQ765 - CHECK ITEM
# SXQ865 - CHECK ITEM
# SXD630 - Last performed oral sex new male (days)
# SXD844 - CHECK ITEM
# SXD744 - CHECK ITEM
# SXQ845 - CHECK ITEM
# SXQ645 - Use protection when performing oral sex
sxq$SXQ645[sxq$SXQ645==7 | sxq$SXQ645==9] = NA
# SXD871 - CHECK ITEM
# SXQ648 - Had sex with new partner/year
sxq$SXQ648[sxq$SXQ648==7 | sxq$SXQ648==9] = NA
# SXQ859 - CHECK ITEM
# SXQ610 - # times had vaginal or anal sex/year
sxq$SXQ610[sxq$SXQ610==77 | sxq$SXQ610==99] = NA

# SXD245 - CHECK ITEM
# SXQ251 - # times had sex without condom/year
sxq$SXQ251[sxq$SXQ251==7 | sxq$SXQ251==9] = NA

# SXD856 - CHECK ITEM
# SXQ590 - # sex partners five years older/year
# SXQ600 - # sex partners five years younger/year
# SXD001 - CHECK ITEM
# SXD002 - CHECK ITEM
# SXD101 - # male sex partners/lifetime
# SXD715 - CHECK ITEM
# SXD450 - # male sex partners/year
# SXQ721 - CHECK ITEM
# SXQ724 - # male vaginal sex partners/lifetime
# SXQ727 - # male vaginal sex partners/year
# SXQ730 - CHECK ITEM
# SXD733 - CHECK ITEM
# SXQ130 - # female sex partners/lifetime
# SXQ490 - # female sex partners/year
# SXQ741 - Ever performed oral sex on a woman
sxq$SXQ741[sxq$SXQ741==7 | sxq$SXQ741==9] = NA

# SXQ768 - CHECK ITEM
# SXQ747 - CHECK ITEM
# SXD771 - CHECK ITEM
# SXQ759 - CHECK ITEM
# SXD750 - CHECK ITEM
# SXQ753 - Ever told by doctor, you had HPV
sxq$SXQ753[sxq$SXQ753==7 | sxq$SXQ753==9] = NA
# SXQ260 - Doctor ever told you had genital herpes
sxq$SXQ260[sxq$SXQ260==7 | sxq$SXQ260==9] = NA
# SXQ265 - Doctor ever told you had genital warts
sxq$SXQ265[sxq$SXQ265==7 | sxq$SXQ265==9] = NA

## SXQ267 - Age when told you had genital warts
#sxq$SXQ803[sxq$SXQ803==7 | sxq$SXQ803==9] = NA

# SXQ270 - Doctor ever told you had gonorrhea
sxq$SXQ270[sxq$SXQ270==7 | sxq$SXQ270==9] = NA
# SXQ272 - Doctor ever told you had chlamydia
sxq$SXQ272[sxq$SXQ272==7 | sxq$SXQ272==9] = NA
# SXQ280 - Are you circumcised or uncircumcised
sxq$SXQ280[sxq$SXQ280==7 | sxq$SXQ280==9] = NA

# SXQ292 - Describe sexual orientation (male)
sxq$SXQ292[sxq$SXQ292==7 | sxq$SXQ292==9] = NA
# SXQ756 - CHECK ITEM
# SXQ294 - Describe sexual orientation (female)
sxq$SXQ294[sxq$SXQ294==7 | sxq$SXQ294==9] = NA

# Ever had vaginal, anal, or oral sex
sxq$ever_sex <- sxq$SXD021

# How old were you when you had sex for the first time?
sxq$age_first_sex <- sxq$SXD031

# In your lifetime, with how many [men/women] have you had any kind of sex?
#If never had sex, coded as zero
sxq$sex_partners <- ifelse(is.na(sxq$SXD171), sxq$SXD101, sxq$SXD171)

# In your lifetime, with how many [men/women] have you had vaginal sex? (ages 18-59)
sxq$vaginal_sex_partners <- ifelse(is.na(sxq$SXQ824), sxq$SXQ724, sxq$SXQ824)

#In the past 12 months, with how many [men/women] have you had any kind of sex? (ages 18-59)
#if never had sex, coded as zero
sxq$sex_partners_year <- ifelse(is.na(sxq$SXD510), sxq$SXD450, sxq$SXD510)

# In the past 12 months, with how many [men/women] have you had vaginal sex? (ages 18-59)
sxq$vaginal_sex_partners_year <- ifelse(is.na(sxq$SXQ827), sxq$SXQ727, sxq$SXQ827)

#If never had sex, set number of vaginal sex partners to zero
sxq$vaginal_sex_partners <- ifelse(sxq$ever_sex==2, 0, sxq$vaginal_sex_partners)

#If never had sex, set number of vaginal sex partners in past year to zero
sxq$vaginal_sex_partners_year <- ifelse(sxq$ever_sex==2, 0, sxq$vaginal_sex_partners_year)

#Do you think of yourself as...
# 1	Heterosexual or straight
# 2	Homosexual or gay
# 3	Bisexual (attracted to men and women)	25	1752
# 4	Something else
# 5	Not sure
sxq$sexual_orientation <- ifelse(is.na(sxq$SXQ292), sxq$SXQ294, sxq$SXQ292)
sxq$heterosexual <- ifelse(sxq$sexual_orientation==1, TRUE, FALSE)

# In the past 12 months, did you have any kind of sex with a person that you never had sex with before?
sxq$new_partner <- sxq$SXQ648
sxq$new_partner <- ifelse(sxq$ever_sex==2, 2, sxq$new_partner)

# Of the persons you had any kind of sex with in the past 12 months, how many were five or more years older than you?
sxq$older_partners<- sxq$SXQ590
sxq$older_partners <- ifelse(sxq$ever_sex==2, 0, sxq$older_partners)

# Of the persons you had any kind of sex with in the past 12 months, how many were five or more years younger than you?
sxq$younger_partners <- sxq$SXQ600
sxq$younger_partners <- ifelse(sxq$ever_sex==2, 0, sxq$younger_partners)

# Analysts should be aware that the 2011-2012 sexual behavior data has some inconsistencies in terms of
# number of partners reported for each type of sex. For example, about 7% of males and 4% of females
# reported a greater number of vaginal sex partners in the past 12 months compared with number of “total”
# sex partners in the past 12 months.


# Create dataframe --------------------------------------------------------


# Demographic data

# RIAGENDR - Gender. 1: Male, 2: Female

d <- dem %>%
  dplyr::select(
    SEQN,
    sex = RIAGENDR,
    age = RIDAGEYR,
    SDMVPSU,
    SDMVSTRA,
    WTINT2YR,
    WTMEC2YR,
    edu = DMDEDUC2,
    edu_child = DMDEDUC3,
    household_size = DMDHHSIZ,
    maritalstatus = DMDMARTL,
    pregnant = RIDEXPRG,
    income = INDFMPIR,
    income2 = INDFMIN2,
    race = RIDRETH3) %>%
  mutate(
    sex = factor(sex, labels=c('male', 'female')),
    top_income = (income==5)*1,
    income2 = case_when(
      income2 <= 10 ~ income2,
      income2 == 12 ~ 9, # median = 8, mean = 9.6
      income2 == 13 ~ 3,
      income2 == 14 ~ 14,
      income2 == 15 ~ 15,
      T ~ NA_real_
    ),
    edu_child = case_when(
      edu_child <= 12 ~ 2,
      edu_child == 13 | edu_child == 14 ~ 3,
      edu_child == 15 ~ 4,
      edu_child == 66 ~ 1
    ),
    edu = ifelse(!is.na(edu_child), edu_child, edu),
    partnered = maritalstatus == 1 | maritalstatus == 6,
    race = factor(race, labels = c('MexicanAmerican', 'OtherHispanic', 'NonHispanicWhite', 'NonHispanicBlack',
                                   'NonHispanicAsian', 'OtherRace'))
    ) %>%
  left_join(bmx[,c('SEQN', 'BMXWT', 'BMXHT', 'BMXBMI', 'BMXLEG', 'BMXARML')]) %>%
  rename(weight=BMXWT, height=BMXHT, bmi=BMXBMI, leglength=BMXLEG, armlength=BMXARML) %>%
  mutate(BMI_category = cut(bmi, breaks=c(0, 30, 85), right=F)) %>%
  left_join(msx) %>%
  rename(strength=MGDCGSZ) %>%
  left_join(phq) %>%
 # left_join(hsq) %>%
  left_join(fastqx[,c('SEQN', 'PHDSESN')]) %>%
  rename(session=PHDSESN) %>%
  left_join(tst[,c('SEQN', 'LBXTST')]) %>%
  rename(testosterone=LBXTST) %>%
  left_join(whq[,c('SEQN', 'WHQ030', 'WHQ040')]) %>%
  rename(perceived_weight=WHQ030, desired_weight=WHQ040) %>%
  mutate(perceived_abnormal_weight = (perceived_weight != 3)) %>%
  mutate(perceived_weight=factor(perceived_weight, levels=c(3,1,2)), desired_weight=factor(desired_weight, levels=c(3,1,2))) %>%
  # left_join(thyrod[,c("SEQN","LBXT4F","LBDTSH1S")]) %>%
  #left_join(thyrod[,c("SEQN", "LBXT4F","LBDTSH1S")]) %>%
  #rename(T4free=LBXT4F, TSH=LBDTSH1S) %>%
  left_join(cbc[c('SEQN', 'LBXWBCSI', 'LBXRBCSI', 'LBXHGB', 'LBXHCT')]) %>%
  rename(whitebloodcell = LBXWBCSI, redbloodcell = LBXRBCSI, hemoglobin = LBXHGB, hematocrit = LBXHCT) %>%
  left_join(pfq[c('SEQN', 'PFQ054', "PFQ061B", "PFQ061C", "PFQ061D", "PFQ061E","PFQ061H","PFQ061I","PFQ061J","PFQ061K","PFQ061L",
  "PFQ061M","PFQ061N","PFQ061O","PFQ061P", "PFQ061T", 'disability', 'disability_score', 'locomotion_disability', 'physical_disease_count', 'depressed_selfreport')]) %>%
  rename(special_equipment=PFQ054) %>%
  left_join(mcq[,c('SEQN', 'asthma', 'heart_disease', 'stroke', 'bronchitis', 'cancer', 'emphysema', 'arthritis')]) %>%
  left_join(diq[,c('SEQN', 'diabetes')]) %>%
  left_join(fsq[c('SEQN', 'FSDHH', 'FSDAD')]) %>%
  rename(
    foodinsecurity_household = FSDHH,
    foodinsecurity_adult = FSDAD
  ) %>%
  left_join(hiq[c('SEQN', 'HIQ011')]) %>%
  rename(healthcare = HIQ011) %>%
  mutate(
    healthcare = ifelse(healthcare >= 7, NA, healthcare),
    healthcare = ifelse(healthcare == 1, 1, 0)
  ) %>%
  left_join(paq[c('SEQN', 'PAQ605', 'PAQ620', 'PAQ650', 'PAQ665')]) %>%
  rename(
    vigorous_work = PAQ605,
    moderate_work = PAQ620,
    vigorous_rec = PAQ650,
    moderate_rec = PAQ665
  ) %>%
  mutate(
    vigorous_work = ifelse(vigorous_work >= 7, NA, vigorous_work),
    vigorous_work = ifelse(vigorous_work == 1, 1, 0),
    moderate_work = ifelse(moderate_work >= 7, NA, moderate_work),
    moderate_work = ifelse(moderate_work == 1, 1, 0),
    vigorous_rec = ifelse(vigorous_rec >= 7, NA, vigorous_rec),
    vigorous_rec = ifelse(vigorous_rec == 1, 1, 0),
    moderate_rec = ifelse(moderate_rec >= 7, NA, moderate_rec),
    moderate_rec = ifelse(moderate_rec == 1, 1, 0)
  ) %>%
  left_join(inq[c('SEQN', 'INQ244')]) %>%
  rename(
    savings5000 = INQ244
  ) %>%
  mutate(
    savings5000 = ifelse(savings5000 >= 7, NA, savings5000),
    savings5000 = ifelse(savings5000 == 1, 1, 0)
  ) %>%
  left_join(sxq)

d$chronic_disease_score <-
  (d$heart_disease == 1) +
  (d$stroke == 1) +
  ((d$asthma == 1) | (d$bronchitis == 1) | (d$emphysema == 1)) +
  (d$diabetes == 1) +
  (d$cancer == 1) +
  (d$arthritis == 1)

d$chronic_disease_count <- ifelse(d$chronic_disease_score >=3, 3, d$chronic_disease_score)
d$chronic_disease_count2 <- ordered(d$chronic_disease_count)

#d$poor_health <- d$general_health == 5

d$special_equipment = d$special_equipment == 1
d$non_locomotion_disability <- d$disability & (d$locomotion_disability != 1)
d$disability <- factor(d$disability)

d$interest <- ordered(d$DPQ010)
d$hopeless <- ordered(d$DPQ020)
d$sleep <- ordered(d$DPQ030)
d$energy <- ordered(d$DPQ040)
d$appetite <- ordered(d$DPQ050)
d$feelbad <- ordered(d$DPQ060)
d$concentrate <- ordered(d$DPQ070)
d$psychomotor <- ordered(d$DPQ080)
d$suicidalideation <- ordered(d$DPQ090)

d$interest1 <- d$DPQ010
d$hopeless1 <- d$DPQ020
d$sleep1 <- d$DPQ030
d$energy1 <- d$DPQ040
d$appetite1 <- d$DPQ050
d$feelbad1 <- d$DPQ060
d$concentrate1 <- d$DPQ070
d$psychomotor1 <- d$DPQ080
d$suicidalideation1 <- d$DPQ090

# WBC and RBC counts ------------------------------------------------------

# Construct high and low white and red blood cell counts

# Delete two enormous outliers on wbc?
# d$whitebloodcell[d$whitebloodcell>30] <- NA

# WBC count normal range: 4,500 to 10,000 cells/mcL
d$WBC = cut(d$whitebloodcell, breaks=c(0,4.5,10.1,55), right=F, include.lowest = T, labels=c('Low', 'Normal', 'High'))

# Set base level to 'Normal'
d$WBC = factor(d$WBC, levels=c('Normal', 'Low', 'High'))

# RBC count normal range:
#   Male: 4.7 to 6.1 million cells/mcL
d$RBC[d$sex=='male'] = cut(d$redbloodcell[d$sex=='male'], breaks=c(0,4.7,6.2,8), right=F, include.lowest = T, labels=F)

#   Female: 4.2 to 5.4 million cells/mcL
d$RBC[d$sex=='female'] = cut(d$redbloodcell[d$sex=='female'], breaks=c(0,4.2,5.5,8), right=F, include.lowest = T, labels=F)

# Set base level to 'Normal'
d$RBC = factor(d$RBC, levels=c(2, 1, 3), labels=c('Normal', 'Low', 'High'))

# Education score, income (ready above), living alone
d$edu[d$edu==7] <- NA; d$edu[d$edu==9] <- 1
d$living_alone <- (d$household_size==1)*1


# Effort ------------------------------------------------------------------

# Construct an "effort" score
#
# Effort is the technician's assessment if the participant
# made a maximal or questionable effort on that test
#
# Combinded grip strength is the sum of the largest
# reading from each hand. Hence, first need to determine
# which test had the highest value for each hand. Then we
# can check effort on that test on that hand

high_H1 = max.col(d[,c('MGXH1T1', 'MGXH1T2', 'MGXH1T3')])
high_H2 = max.col(d[,c('MGXH2T1', 'MGXH2T2', 'MGXH2T3')])

# # Test that the sum of these = combined grip strength (MGDCGSZ)
# # Can't think of a vector solution
# combined2 = rep(NA, length(high_H1))
# for (i in 1:length(high_H1)){
#     if (!is.na(high_H1[i]) & !is.na(high_H2[i])){
#         combined2[i] = d[,c('MGXH1T1', 'MGXH1T2', 'MGXH1T3')][i,high_H1[i]] + d[,c('MGXH2T1', 'MGXH2T2', 'MGXH2T3')][i,high_H2[i]]
#     }
# }
# summary(d$strength - combined2)
# It does!

# Now construct effort score

effort = rep(NA, length(high_H1))
for (i in 1:length(high_H1)){
  if (!is.na(high_H1[i]) & !is.na(high_H2[i])){
    effort[i] = d[,c('MGXH1T1E', 'MGXH1T2E', 'MGXH1T3E')][i,high_H1[i]] + d[,c('MGXH2T1E', 'MGXH2T2E', 'MGXH2T3E')][i,high_H2[i]]
  }
}

# 2: maximal effort on both hands
# 3: questionable effort on one hand
# 4: questionable effort on both hands

table(effort) # only 35 3's and 21 4's
d$questionable_effort = effort>2

# Define "adult" age range; change lower bound = 18?
# that is the lowest age for which depression
# scores are available, and it is close to 20
# adults = d$age>=18 & d$age<=60
# d.adults = d[adults,]

d_G <- d
# save(d_G, file='d_G.rda')
return(d_G)
}
