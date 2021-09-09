
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
msx = read.xport('data-raw/NHANES data/MGX_G.XPT')

# SEQN - Respondent sequence number
# MGDEXSTS - Grip test status
msx$gripteststatus <- msx$MGDEXSTS

# MGD050 - Ever had surgery on hands or wrists
msx$everhandsurgery <- msx$MGD050

# MGD060 - Which hand or wrist had surgery
msx$whichhandsurgery <- msx$MGD060

# MGQ070 - Recent pain/aching/stiffness-right hand
msx$painrighthand <- msx$MGQ070

# MGQ080 - Cause of recent pain in right hand
msx$causerighthandpain <- msx$MGQ080

# MGQ090 - Pain in right hand gotten worse recently
msx$painrighthandworse <- msx$MGQ090

# MGQ100 - Recent pain/aching/stiffness-left hand
msx$painlefthand <- msx$MGQ100

# MGQ110 - Cause of recent pain in left hand
msx$causelefthandpain <- msx$MGQ110

# MGQ120 - Pain in left hand gotten worse recently
msx$painlefthandworse <- msx$MGQ120

# MGD130 - Dominant hand
msx$dominanthand <- msx$MGD130

# MGQ90DG - 90 degree angle with index finger
msx$indexfingerangle <- msx$MGQ90DG

# MGDSEAT - Testing position
msx$testingposition <- msx$MGDSEAT

# MGAPHAND - Hand assigned for practice trial
msx$practicehand <- msx$MGAPHAND

# MGATHAND - Begin the test with this hand.
msx$beginhand <- msx$MGATHAND

# MGXH1T1 - Grip strength (kg), hand 1, test 1
msx$hand1test1 <- msx$MGXH1T1

# MGXH1T1E - Grip strength, hand 1, test 1 effort
msx$hand1test1effort <- msx$MGXH1T1E

# MGXH2T1 - Grip strength (kg), hand 2, test 1
msx$hand2test1 <- msx$MGXH2T1

# MGXH2T1E - Grip strength, hand 2, test 1 effort
msx$hand2test1effort <- msx$MGXH2T1E

# MGXH1T2 - Grip strength (kg), hand 1, test 2
msx$hand1test2 <- msx$MGXH1T2

# MGXH1T2E - Grip strength, hand 1, test 2 effort
msx$hand1test2effort <- msx$MGXH1T2E

# MGXH2T2 - Grip strength (kg), hand 2, test 2
msx$hand2test2 <- msx$MGXH2T2

# MGXH2T2E - Grip strength, hand 2, test 2 effort
msx$hand2test2effort <- msx$MGXH2T2E

# MGXH1T3 - Grip strength (kg), hand 1, test 3
msx$hand1test3 <- msx$MGXH1T3

# MGXH1T3E - Grip strength, hand 1, test 3 effort
msx$hand1test3effort <- msx$MGXH1T3E

# MGXH2T3 - Grip strength (kg), hand 2, test 3
msx$hand2test3 <- msx$MGXH2T3

# MGXH2T3E - Grip strength, hand 2, test 3 effort
msx$hand2test3effort <- msx$MGXH2T3E

# MGDCGSZ - Combined grip strength (kg)



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


# SXD862 - CHECK ITEM BOX 1A CHECK ITEM SXD.862: IF SXQ.800, SXQ.803, SXQ.806, AND SXQ.809
# NOT EQUAL TO '1', GO TO BOX 8. OTHERWISE, SXD031.Target:Males only 18 YEARS - 69 YEARS
sxq$SXD862 <- sxq$SXQ800 != 1 & sxq$SXQ803 != 1 & sxq$SXQ806 != 1 & sxq$SXQ809 != 1

sxq$SXD862b <- sxq$SXQ800 == 2 & sxq$SXQ803 == 2 & sxq$SXQ806 == 2 & sxq$SXQ809 == 2

sxq[sxq==77777 | sxq==99999] = NA

# SXD021 - Ever had vaginal, anal, or oral sex (Males and Females 18-69)
sxq$SXD021[sxq$SXD021==7 | sxq$SXD021==9] = NA
sxq$ever_sex <- sxq$SXD021

# SXQ800 - Ever had vaginal sex with a woman (Males 18-69)
sxq$SXQ800[sxq$SXQ800==7 | sxq$SXQ800==9] = NA
sxq$men_evervaginalsexwithwoman <- sxq$SXQ800

# SXQ803 - Ever performed oral sex on a woman (Males 18-69)
sxq$SXQ803[sxq$SXQ803==7 | sxq$SXQ803==9] = NA
sxq$men_everoralsexonwoman <- sxq$SXQ803

# SXQ806 - Ever had anal sex with a woman (Males 18-69)
sxq$SXQ806[sxq$SXQ806==7 | sxq$SXQ806==9] = NA
sxq$men_everanalsexwithwoman <- sxq$SXQ806

# SXQ809 - Ever had any sex with a man: anal, oral (Males 18-69)
sxq$SXQ809[sxq$SXQ809==7 | sxq$SXQ809==9] = NA
sxq$men_eversexwithman <- sxq$SXQ809

# SXD862 - CHECK ITEM

# SXQ700 - Ever had vaginal sex with a man (Females 18-69)
sxq$SXQ700[sxq$SXQ700==7 | sxq$SXQ700==9] = NA
sxq$wom_evervaginalsexwithman <- sxq$SXQ700

# SXQ703 - Ever performed oral sex on a man (Females 18-69)
sxq$SXQ703[sxq$SXQ703==7 | sxq$SXQ703==9] = NA
sxq$wom_everoralsexonman <- sxq$SXQ703

# SXQ706 - Ever had anal sex with a man  (Females 18-69)
sxq$SXQ706[sxq$SXQ706==7 | sxq$SXQ706==9] = NA
sxq$wom_everanalsexwithman <- sxq$SXQ706

# SXQ709 - Ever had any kind of sex with a woman  (Females 18-69)
sxq$SXQ709[sxq$SXQ709==7 | sxq$SXQ709==9] = NA
sxq$wom_eversexwithwoman <- sxq$SXQ709

# SXQ762 - CHECK ITEM

# SXD031 - How old when first had sex (Males and Females 18-69)
sxq$SXD031[sxq$SXD031==77 | sxq$SXD031==99] = NA
# How old were you when you had sex for the first time?
sxq$age_first_sex <- sxq$SXD031


# SXD801 - CHECK ITEM

# SXD171 - # female sex partners/lifetime (Males 18-69)
# SXD815 - CHECK ITEM
# SXD510 - # female sex partners/year (Males 18-59)
# SXQ821 - CHECK ITEM
# SXQ824 - # female vaginal sex partners/lifetime (Males 18-59)
# SXQ827 - # female vaginal sex partners/year (Males 18-59)

# SXQ830 - CHECK ITEM
# SXD633 - Age first performed oral sex on a woman (Males and Females 18-59)
sxq$SXD633[sxq$SXD633==77 | sxq$SXD633==99] = NA
sxq$ageoralsexonwoman <- sxq$SXD633

# SXQ636 - # female performed oral sex/lifetime (Males and Females 18-59)
sxq$numfemaleoralsex <- sxq$SXQ636

# SXQ639 - # female performed oral sex/year (Males and Females 18-59)
sxq$numfemaleoralsexyear <- sxq$SXQ639

# SXQ868 - CHECK ITEM
# SXD642 - Performed oral sex new female (days) (Males and Females 18-59)
# How long has it been since the last time you performed oral sex on a new female partner?
# A new sexual partner is someone that you had never had sex with before.
sxq$daysoralsexnewfemalepartner <- sxq$SXD642

# SXQ833 - CHECK ITEM
# SXQ410 - # men anal/oral sex partners/lifetime (Males 18-69)
#In your lifetime, with how many men have you had anal or oral sex?
sxq$men_malesexpartnerslifetime <- sxq$SXQ410

# SXQ875 - CHECK ITEM
# SXQ550 - # men anal/oral sex partners/year (Males 18-59)
sxq$men_malesexpartnersyear <- sxq$SXQ550

# SXQ836 - # men anal sex partners/lifetime (Males 18-69)
sxq$men_maleanalsexpartnerslifetime <- sxq$SXQ836

# SXQ839 - CHECK ITEM
# SXQ841 - # men anal sex partners/year (Males 18-59)
sxq$men_maleanalsexpartnersyear <- sxq$SXQ841

# SXQ853 - Ever performed oral sex on a man (Males 18-69)
sxq$SXQ853[sxq$SXQ853==7 | sxq$SXQ853==9] = NA
sxq$men_everoralsexonman <- sxq$SXQ853

# SXD847 - CHECK ITEM
# SXD621 - How old when first had oral sex (Males and Females 18-69)
sxq$SXD621[sxq$SXD621==77 | sxq$SXD621==99] = NA
sxq$agefirstoralsex <- sxq$SXD621

# SXQ624 - # male oral sex partners/lifetime (Males and Females 18-69)
sxq$nummaleoralsex <- sxq$SXQ624

# SXQ850 - CHECK ITEM
# SXQ627 - # male oral sex partners/year (Males and Females 18-59)
sxq$nummaleoralsexyear <- sxq$SXQ627

# SXQ765 - CHECK ITEM
# SXQ865 - CHECK ITEM
# SXD630 - Last performed oral sex new male (days) (Males and Females 18-59)
sxq$daysoralsexnewmalepartner <- sxq$SXD630

# SXD844 - CHECK ITEM
# SXD744 - CHECK ITEM
# SXQ845 - CHECK ITEM
# SXQ645 - Use protection when performing oral sex (Males and Females 18-59)
sxq$SXQ645[sxq$SXQ645==7 | sxq$SXQ645==9] = NA
sxq$oralsexprotection <- sxq$SXQ645

# SXD871 - CHECK ITEM
# SXQ648 - Had sex with new partner/year (Males and Females 18-59)
sxq$SXQ648[sxq$SXQ648==7 | sxq$SXQ648==9] = NA
# In the past 12 months, did you have any kind of sex with a person that you never had sex with before?
sxq$new_partner <- sxq$SXQ648
sxq$new_partner <- ifelse(sxq$ever_sex==2, 2, sxq$new_partner)

# SXQ859 - CHECK ITEM
# SXQ610 - # times had vaginal or anal sex/year (Males and Females 18-59)
#In the past 12 months, about how many times have you had {vaginal or anal/vaginal/anal} sex?
sxq$SXQ610[sxq$SXQ610==77 | sxq$SXQ610==99] = NA
sxq$numsexpastyear <- sxq$SXQ610

# SXD245 - CHECK ITEM
# SXQ251 - # times had sex without condom/year (Males and Females 18-59)
sxq$SXQ251[sxq$SXQ251==7 | sxq$SXQ251==9] = NA
sxq$numsexnocondompastyear <- sxq$SXQ251

# SXD856 - CHECK ITEM
# SXQ590 - # sex partners five years older/year (Males and Females 18-29)
# Of the persons you had any kind of sex with in the past 12 months, how many were five or more years older than you?
sxq$older_partners<- sxq$SXQ590
sxq$older_partners <- ifelse(sxq$ever_sex==2, 0, sxq$older_partners)

# SXQ600 - # sex partners five years younger/year (Males and Females 18-29)
# Of the persons you had any kind of sex with in the past 12 months, how many were five or more years younger than you?
sxq$younger_partners <- sxq$SXQ600
sxq$younger_partners <- ifelse(sxq$ever_sex==2, 0, sxq$younger_partners)

# SXD001 - CHECK ITEM
# SXD002 - CHECK ITEM
# SXD101 - # male sex partners/lifetime (Females 18-69)
# SXD715 - CHECK ITEM
# SXD450 - # male sex partners/year (Females 18-59)
# SXQ721 - CHECK ITEM
# SXQ724 - # male vaginal sex partners/lifetime (Females 18-59)
# SXQ727 - # male vaginal sex partners/year (Females 18-59)
# SXQ730 - CHECK ITEM
# SXD733 - CHECK ITEM
# SXQ130 - # female sex partners/lifetime (Females 18-59)
sxq$wom_femalesexpartnerslifetime <- sxq$SXQ130

# SXQ490 - # female sex partners/year (Females 18-59)
sxq$wom_femalesexpartnersyear <- sxq$SXQ490

# SXQ741 - Ever performed oral sex on a woman (Females 18-59)
sxq$SXQ741[sxq$SXQ741==7 | sxq$SXQ741==9] = NA
sxq$wom_everoralsexonwoman <- sxq$SXQ741

# SXQ768 - CHECK ITEM
# SXQ747 - CHECK ITEM
# SXD771 - CHECK ITEM
# SXQ759 - CHECK ITEM
# SXD750 - CHECK ITEM
# SXQ753 - Ever told by doctor, you had HPV (Females 18-59)
sxq$SXQ753[sxq$SXQ753==7 | sxq$SXQ753==9] = NA
sxq$wom_hpv <- sxq$SXQ753

# SXQ260 - Doctor ever told you had genital herpes (Males and Females 18-59)
sxq$SXQ260[sxq$SXQ260==7 | sxq$SXQ260==9] = NA
sxq$genitalherpes <- sxq$SXQ260

# SXQ265 - Doctor ever told you had genital warts (Males and Females 18-59)
sxq$SXQ265[sxq$SXQ265==7 | sxq$SXQ265==9] = NA
sxq$genitalwarts <- sxq$SXQ265

## SXQ267 - Age when told you had genital warts
#sxq$SXQ803[sxq$SXQ803==7 | sxq$SXQ803==9] = NA

# SXQ270 - Doctor ever told you had gonorrhea (Males and Females 18-59)
sxq$SXQ270[sxq$SXQ270==7 | sxq$SXQ270==9] = NA
sxq$gonorrhea <- sxq$SXQ270

# SXQ272 - Doctor ever told you had chlamydia (Males and Females 18-59)
sxq$SXQ272[sxq$SXQ272==7 | sxq$SXQ272==9] = NA
sxq$chlamydia <- sxq$SXQ272

# SXQ280 - Are you circumcised or uncircumcised (Males 18-59)
sxq$SXQ280[sxq$SXQ280==7 | sxq$SXQ280==9] = NA
sxq$circumcised <- sxq$SXQ280

# SXQ292 - Describe sexual orientation (male) (Males 18-59)
sxq$SXQ292[sxq$SXQ292==7 | sxq$SXQ292==9] = NA
# SXQ756 - CHECK ITEM
# SXQ294 - Describe sexual orientation (female) (Females 18-59)
sxq$SXQ294[sxq$SXQ294==7 | sxq$SXQ294==9] = NA
#Do you think of yourself as...
# 1	Heterosexual or straight
# 2	Homosexual or gay
# 3	Bisexual (attracted to men and women)	25	1752
# 4	Something else
# 5	Not sure
# sxq$sexual_orientation <- ifelse(is.na(sxq$SXQ292), sxq$SXQ294, sxq$SXQ292)



# In your lifetime, with how many [men/women] have you had any kind of sex?
#If never had sex, coded as zero
# sxq$sex_partners <- ifelse(is.na(sxq$SXD171), sxq$SXD101, sxq$SXD171)

# In your lifetime, with how many [men/women] have you had vaginal sex? (ages 18-59)
# sxq$vaginal_sex_partners <- ifelse(is.na(sxq$SXQ824), sxq$SXQ724, sxq$SXQ824)

#In the past 12 months, with how many [men/women] have you had any kind of sex? (ages 18-59)
#if never had sex, coded as zero
# sxq$sex_partners_year <- ifelse(is.na(sxq$SXD510), sxq$SXD450, sxq$SXD510)

# In the past 12 months, with how many [men/women] have you had vaginal sex? (ages 18-59)
# sxq$vaginal_sex_partners_year <- ifelse(is.na(sxq$SXQ827), sxq$SXQ727, sxq$SXQ827)




# Analysts should be aware that the 2011-2012 sexual behavior data has some inconsistencies in terms of
# number of partners reported for each type of sex. For example, about 7% of males and 4% of females
# reported a greater number of vaginal sex partners in the past 12 months compared with number of “total”
# sex partners in the past 12 months.

sxq2 <-
  sxq %>%
  left_join(dem[c("SEQN", "RIAGENDR", "RIDAGEYR")]) %>%
  mutate(
    evervaginalsex = ifelse(RIAGENDR == 2, wom_evervaginalsexwithman, men_evervaginalsexwithwoman),
    everhetoralsex = ifelse(RIAGENDR == 2, wom_everoralsexonman, men_everoralsexonwoman),
    eversamesexpartner = ifelse(RIAGENDR == 2, wom_eversexwithwoman, men_eversexwithman),
    eversamesexoralsex = ifelse(RIAGENDR == 2, wom_everoralsexonwoman, men_everoralsexonman),
    everhetanalsex = ifelse(RIAGENDR == 2, wom_everanalsexwithman, men_everanalsexwithwoman),
    sex_partners = ifelse(RIAGENDR == 2, SXD101, SXD171),
    sex_partners_year = ifelse(RIAGENDR == 2, SXD450, SXD510),
    vaginal_sex_partners = ifelse(RIAGENDR == 2, SXQ724, SXQ824),
    vaginal_sex_partners_year = ifelse(RIAGENDR == 2, SXQ727, SXQ827),
    numsamesexpastyear = ifelse(RIAGENDR == 2, SXQ490, SXQ550),
    numsamesexpartners = ifelse(RIAGENDR == 2, SXQ130, SXQ410),
    sexualorientation = ifelse(RIAGENDR == 2, SXQ294, SXQ292)
  ) %>%
  dplyr::select(-RIAGENDR)

#If never had sex with same sex partner, set ever had oral sex with same sex partner to no
sxq2$eversamesexoralsex <- ifelse(sxq2$eversamesexpartner==2 & is.na(sxq2$eversamesexoralsex), 2, sxq2$eversamesexoralsex)

#If number of life time sex partners = 0, set number of sex partners in last year to 0 (missing ppl over 59)
sxq2$sex_partners_year <- ifelse(sxq2$sex_partners == 0 & is.na(sxq2$sex_partners_year), 0, sxq2$sex_partners_year)

#If number of sex partners equals 0, set number of vaginal sex partners to zero
sxq2$vaginal_sex_partners <- ifelse(sxq2$sex_partners == 0 & is.na(sxq2$vaginal_sex_partners), 0, sxq2$vaginal_sex_partners)

#If number of sex partners past year equals 0, set number of vaginal sex partners in past year to zero
sxq2$vaginal_sex_partners_year <- ifelse(sxq2$sex_partners_year ==0 & is.na(sxq2$vaginal_sex_partners_year), 0, sxq2$vaginal_sex_partners_year)

#If never had same sex encounter, set number of same sex partners to 0
sxq2$numsamesexpartners <- ifelse(sxq2$eversamesexpartner == 2 & is.na(sxq2$numsamesexpartners), 0, sxq2$numsamesexpartners)

#If never had same sex encounter, set number of same sex partners in past year to 0
sxq2$numsamesexpastyear <- ifelse(sxq2$eversamesexpartner == 2 & is.na(sxq2$numsamesexpastyear), 0, sxq2$numsamesexpastyear)


sxq2$heterosexual <- ifelse(sxq2$sexualorientation==1, TRUE, FALSE)

# Diet --------------------------------------------------------------------
dr1 <- read.xport('data-raw/NHANES data/DR1TOT_G.XPT')


# SEQN - Respondent sequence number
# WTDRD1 - Dietary day one sample weight
# WTDR2D - Dietary two-day sample weight
# DR1DRSTZ - Dietary recall status
# DR1EXMER - Interviewer ID code
# DRABF - Breast-fed infant (either day)
# DRDINT - Number of days of intake
# DR1DBIH - # of days b/w intake and HH interview
#   DR1DAY - Intake day of the week
# DR1LANG - Language respondent used mostly
# DR1MNRSP - Main respondent for this interview
# DR1HELPD - Helped in responding for this interview
# DBQ095Z - Type of table salt used
# DBD100 - How often add salt to food at table
# DRQSPREP - Salt used in preparation?
#   DRQSDIET - On special diet?
#   DRQSDT1 - Weight loss/Low calorie diet
# DRQSDT2 - Low fat/Low cholesterol diet
# DRQSDT3 - Low salt/Low sodium diet
# DRQSDT4 - Sugar free/Low sugar diet
# DRQSDT5 - Low fiber diet
# DRQSDT6 - High fiber diet
# DRQSDT7 - Diabetic diet
# DRQSDT8 - Weight gain/Muscle building diet
# DRQSDT9 - Low carbohydrate diet
# DRQSDT10 - High protein diet
# DRQSDT11 - Gluten-free/Celiac diet
# DRQSDT12 - Renal/Kidney diet
# DRQSDT91 - Other special diet
# DR1TNUMF - Number of foods reported
# DR1TKCAL - Energy (kcal)
# DR1TPROT - Protein (gm)
# DR1TCARB - Carbohydrate (gm)
# DR1TSUGR - Total sugars (gm)
# DR1TFIBE - Dietary fiber (gm)
# DR1TTFAT - Total fat (gm)
# DR1TSFAT - Total saturated fatty acids (gm)
# DR1TMFAT - Total monounsaturated fatty acids (gm)
# DR1TPFAT - Total polyunsaturated fatty acids (gm)
# DR1TCHOL - Cholesterol (mg)
# DR1TATOC - Vitamin E as alpha-tocopherol (mg)
# DR1TATOA - Added alpha-tocopherol (Vitamin E) (mg)
# DR1TRET - Retinol (mcg)
# DR1TVARA - Vitamin A, RAE (mcg)
# DR1TACAR - Alpha-carotene (mcg)
# DR1TBCAR - Beta-carotene (mcg)
# DR1TCRYP - Beta-cryptoxanthin (mcg)
# DR1TLYCO - Lycopene (mcg)
# DR1TLZ - Lutein + zeaxanthin (mcg)
# DR1TVB1 - Thiamin (Vitamin B1) (mg)
# DR1TVB2 - Riboflavin (Vitamin B2) (mg)
# DR1TNIAC - Niacin (mg)
# DR1TVB6 - Vitamin B6 (mg)
# DR1TFOLA - Total folate (mcg)
# DR1TFA - Folic acid (mcg)
# DR1TFF - Food folate (mcg)
# DR1TFDFE - Folate, DFE (mcg)
# DR1TCHL - Total choline (mg)
# DR1TVB12 - Vitamin B12 (mcg)
# DR1TB12A - Added vitamin B12 (mcg)
# DR1TVC - Vitamin C (mg)
# DR1TVD - Vitamin D (D2 + D3) (mcg)
# DR1TVK - Vitamin K (mcg)
# DR1TCALC - Calcium (mg)
# DR1TPHOS - Phosphorus (mg)
# DR1TMAGN - Magnesium (mg)
# DR1TIRON - Iron (mg)
# DR1TZINC - Zinc (mg)
# DR1TCOPP - Copper (mg)
# DR1TSODI - Sodium (mg)
# DR1TPOTA - Potassium (mg)
# DR1TSELE - Selenium (mcg)
# DR1TCAFF - Caffeine (mg)
# DR1TTHEO - Theobromine (mg)
# DR1TALCO - Alcohol (gm)
# DR1TMOIS - Moisture (gm)
# DR1TS040 - SFA 4:0 (Butanoic) (gm)
# DR1TS060 - SFA 6:0 (Hexanoic) (gm)
# DR1TS080 - SFA 8:0 (Octanoic) (gm)
# DR1TS100 - SFA 10:0 (Decanoic) (gm)
# DR1TS120 - SFA 12:0 (Dodecanoic) (gm)
# DR1TS140 - SFA 14:0 (Tetradecanoic) (gm)
# DR1TS160 - SFA 16:0 (Hexadecanoic) (gm)
# DR1TS180 - SFA 18:0 (Octadecanoic) (gm)
# DR1TM161 - MFA 16:1 (Hexadecenoic) (gm)
# DR1TM181 - MFA 18:1 (Octadecenoic) (gm)
# DR1TM201 - MFA 20:1 (Eicosenoic) (gm)
# DR1TM221 - MFA 22:1 (Docosenoic) (gm)
# DR1TP182 - PFA 18:2 (Octadecadienoic) (gm)
# DR1TP183 - PFA 18:3 (Octadecatrienoic) (gm)
# DR1TP184 - PFA 18:4 (Octadecatetraenoic) (gm)
# DR1TP204 - PFA 20:4 (Eicosatetraenoic) (gm)
# DR1TP205 - PFA 20:5 (Eicosapentaenoic) (gm)
# DR1TP225 - PFA 22:5 (Docosapentaenoic) (gm)
# DR1TP226 - PFA 22:6 (Docosahexaenoic) (gm)
# DR1_300 - Compare food consumed yesterday to usual
# DR1_320Z - Total plain water drank yesterday (gm)
# DR1_330Z - Total tap water drank yesterday (gm)
# DR1BWATZ - Total bottled water drank yesterday (gm)
# DR1TWS - Tap water source
# DRD340 - Shellfish eaten during past 30 days
# DRD350A - Clams eaten during past 30 days
# DRD350AQ - # of times clams eaten in past 30 days
#   DRD350B - Crabs eaten during past 30 days
# DRD350BQ - # of times crabs eaten in past 30 days
#   DRD350C - Crayfish eaten during past 30 days
# DRD350CQ - # of times crayfish eaten past 30 days
#   DRD350D - Lobsters eaten during past 30 days
# DRD350DQ - # of times lobsters eaten past 30 days
#   DRD350E - Mussels eaten during past 30 days
# DRD350EQ - # of times mussels eaten in past 30 days
#   DRD350F - Oysters eaten during past 30 days
# DRD350FQ - # of times oysters eaten in past 30 days
#   DRD350G - Scallops eaten during past 30 days
# DRD350GQ - # of times scallops eaten past 30 days
#   DRD350H - Shrimp eaten during past 30 days
# DRD350HQ - # of times shrimp eaten in past 30 days
#   DRD350I - Other shellfish eaten past 30 days
# DRD350IQ - # of times other shellfish eaten
#   DRD350J - Other unknown shellfish eaten past 30 d
# DRD350JQ - # of times other unknown shellfish eaten
#   DRD350K - Refused on shellfish eaten past 30 days
# DRD360 - Fish eaten during past 30 days
# DRD370A - Breaded fish products eaten past 30 days
# DRD370AQ - # of times breaded fish products eaten
#   DRD370B - Tuna eaten during past 30 days
# DRD370BQ - # of times tuna eaten in past 30 days
#   DRD370C - Bass eaten during past 30 days
# DRD370CQ - # of times bass eaten in past 30 days
#   DRD370D - Catfish eaten during past 30 days
# DRD370DQ - # of times catfish eaten in past 30 days
#   DRD370E - Cod eaten during past 30 days
# DRD370EQ - # of times cod eaten in past 30 days
#   DRD370F - Flatfish eaten during past 30 days
# DRD370FQ - # of times flatfish eaten past 30 days
#   DRD370G - Haddock eaten during past 30 days
# DRD370GQ - # of times haddock eaten in past 30 days
#   DRD370H - Mackerel eaten during past 30 days
# DRD370HQ - # of times mackerel eaten past 30 days
#   DRD370I - Perch eaten during past 30 days
# DRD370IQ - # of times perch eaten in past 30 days
#   DRD370J - Pike eaten during past 30 days
# DRD370JQ - # of times pike eaten in past 30 days
#   DRD370K - Pollock eaten during past 30 days
# DRD370KQ - # of times pollock eaten in past 30 days
#   DRD370L - Porgy eaten during past 30 days
# DRD370LQ - # of times porgy eaten in past 30 days
#   DRD370M - Salmon eaten during past 30 days
# DRD370MQ - # of times salmon eaten in past 30 days
#   DRD370N - Sardines eaten during past 30 days
# DRD370NQ - # of times sardines eaten past 30 days
#   DRD370O - Sea bass eaten during past 30 days
# DRD370OQ - # of times sea bass eaten past 30 days
#   DRD370P - Shark eaten during past 30 days
# DRD370PQ - # of times shark eaten in past 30 days
#   DRD370Q - Swordfish eaten during past 30 days
# DRD370QQ - # of times swordfish eaten past 30 days
#   DRD370R - Trout eaten during past 30 days
# DRD370RQ - # of times trout eaten in past 30 days
#   DRD370S - Walleye eaten during past 30 days
# DRD370SQ - # of times walleye eaten in past 30 days
#   DRD370T - Other fish eaten during past 30 days
# DRD370TQ - # of times other fish eaten past 30 days
#   DRD370U - Other unknown fish eaten in past 30 days
# DRD370UQ - # of times other unknown fish eaten
#   DRD370V - Refused on fish eaten past 30 days



# Diet day 2 --------------------------------------------------------------

dr2 <- read.xport('data-raw/NHANES data/DR2TOT_G.XPT')

# SEQN - Respondent sequence number
# WTDRD1 - Dietary day one sample weight
# WTDR2D - Dietary two-day sample weight
# DR2DRSTZ - Dietary recall status
# DR2EXMER - Interviewer ID code
# DRABF - Breast-fed infant (either day)
# DRDINT - Number of days of intake
# DR2DBIH - # of days b/w intake and HH interview
# DR2DAY - Intake day of the week
# DR2LANG - Language respondent used mostly
# DR2MNRSP - Main respondent for this interview
# DR2HELPD - Helped in responding for this interview
# DR2TNUMF - Number of foods reported
# DR2TKCAL - Energy (kcal)
# DR2TPROT - Protein (gm)
# DR2TCARB - Carbohydrate (gm)
# DR2TSUGR - Total sugars (gm)
# DR2TFIBE - Dietary fiber (gm)
# DR2TTFAT - Total fat (gm)
# DR2TSFAT - Total saturated fatty acids (gm)
# DR2TMFAT - Total monounsaturated fatty acids (gm)
# DR2TPFAT - Total polyunsaturated fatty acids (gm)
# DR2TCHOL - Cholesterol (mg)
# DR2TATOC - Vitamin E as alpha-tocopherol (mg)
# DR2TATOA - Added alpha-tocopherol (Vitamin E) (mg)
# DR2TRET - Retinol (mcg)
# DR2TVARA - Vitamin A, RAE (mcg)
# DR2TACAR - Alpha-carotene (mcg)
# DR2TBCAR - Beta-carotene (mcg)
# DR2TCRYP - Beta-cryptoxanthin (mcg)
# DR2TLYCO - Lycopene (mcg)
# DR2TLZ - Lutein + zeaxanthin (mcg)
# DR2TVB1 - Thiamin (Vitamin B1) (mg)
# DR2TVB2 - Riboflavin (Vitamin B2) (mg)
# DR2TNIAC - Niacin (mg)
# DR2TVB6 - Vitamin B6 (mg)
# DR2TFOLA - Total folate (mcg)
# DR2TFA - Folic acid (mcg)
# DR2TFF - Food folate (mcg)
# DR2TFDFE - Folate, DFE (mcg)
# DR2TCHL - Total choline (mg)
# DR2TVB12 - Vitamin B12 (mcg)
# DR2TB12A - Added vitamin B12 (mcg)
# DR2TVC - Vitamin C (mg)
# DR2TVD - Vitamin D (D2 + D3) (mcg)
# DR2TVK - Vitamin K (mcg)
# DR2TCALC - Calcium (mg)
# DR2TPHOS - Phosphorus (mg)
# DR2TMAGN - Magnesium (mg)
# DR2TIRON - Iron (mg)
# DR2TZINC - Zinc (mg)
# DR2TCOPP - Copper (mg)
# DR2TSODI - Sodium (mg)
# DR2TPOTA - Potassium (mg)
# DR2TSELE - Selenium (mcg)
# DR2TCAFF - Caffeine (mg)
# DR2TTHEO - Theobromine (mg)
# DR2TALCO - Alcohol (gm)
# DR2TMOIS - Moisture (gm)
# DR2TS040 - SFA 4:0 (Butanoic) (gm)
# DR2TS060 - SFA 6:0 (Hexanoic) (gm)
# DR2TS080 - SFA 8:0 (Octanoic) (gm)
# DR2TS100 - SFA 10:0 (Decanoic) (gm)
# DR2TS120 - SFA 12:0 (Dodecanoic) (gm)
# DR2TS140 - SFA 14:0 (Tetradecanoic) (gm)
# DR2TS160 - SFA 16:0 (Hexadecanoic) (gm)
# DR2TS180 - SFA 18:0 (Octadecanoic) (gm)
# DR2TM161 - MFA 16:1 (Hexadecenoic) (gm)
# DR2TM181 - MFA 18:1 (Octadecenoic) (gm)
# DR2TM201 - MFA 20:1 (Eicosenoic) (gm)
# DR2TM221 - MFA 22:1 (Docosenoic) (gm)
# DR2TP182 - PFA 18:2 (Octadecadienoic) (gm)
# DR2TP183 - PFA 18:3 (Octadecatrienoic) (gm)
# DR2TP184 - PFA 18:4 (Octadecatetraenoic) (gm)
# DR2TP204 - PFA 20:4 (Eicosatetraenoic) (gm)
# DR2TP205 - PFA 20:5 (Eicosapentaenoic) (gm)
# DR2TP225 - PFA 22:5 (Docosapentaenoic) (gm)
# DR2TP226 - PFA 22:6 (Docosahexaenoic) (gm)
# DR2_300 - Compare food consumed yesterday to usual
# DR2_320Z - Total plain water drank yesterday (gm)
# DR2_330Z - Total tap water drank yesterday (gm)
# DR2BWATZ - Total bottled water drank yesterday (gm)
# DR2TWS - Tap water source

# Most analyses of NHANES data use data collected in the MEC and the variable WTMEC2YR should be used
# for the sample weights. However, for the WWEIA dietary data, different sample weights are
# recommended for analysis. Although attempts are made to schedule MEC exams uniformly throughout
# the week, proportionally more exams occur on weekend days than on weekdays. Because food intake
# can vary by day of the week, use of the MEC weights would disproportionately represent intakes on
# weekends.
#
# A set of weights (WTDRD1) is provided that should be used when an analysis uses the Day 1 dietary
# recall data (either alone or when Day 1 nutrient data are used in conjunction with MEC data). The
# set of weights (WTDRD1) is applicable to the 8,519 participants with Day 1 data. Day 1 weights were
# constructed by taking the MEC sample weights (WTMEC2YR) and further adjusting for (a) the additional
# non-response and (b) the differential allocation by day of the week for the dietary intake data
# collection. These Day 1 weights are more variable than the MEC weights, and the sample size is
# smaller, so estimated standard errors using Day 1 data and Day 1 weights are larger than standard
# errors for similar estimates based on MEC weights.
#
# When analysis is based on both days of dietary intake, only 7,605 sample participants have complete
# data. The NHANES protocol requires an attempt to collect the second day of dietary data at least 3
# days after the first day, but the actual number of days between the two interviews is variable. A
# set of adjusted weights, WTDR2D, is to be used only when an analysis uses both Day 1 and Day 2
# dietary data. This two-day weight was constructed for the 7,605 participants by taking the Day 1
# weights (WTDRD1) and further adjusting for (a) the additional non-response for the second recall
# and (b) for the proportion of weekend-weekday combinations of Day 1 and Day 2 recalls.
#
# Note that all sample weights are person-level weights and each set of dietary weights should sum to
# the same population control total as the MEC weights (WTMEC2YR). In addition, the MEC weights
# (WTMEC2YR) are appropriate for use in the analysis of the fish and shellfish consumption data
# (i.e., variables DRD340, DRD350A-K, DRD350AQ-JQ DRD360, DRD370A-V, and DRD370AQ-UQ) located in
# the Day 1 Total Nutrient Intake File (DR1TOT_G), if no other dietary data are included in the
# analysis. Additional explanation of sample weights and appropriate uses are included in the NHANES
# Analytic Guidelines. Please also refer to the on-line NHANES Tutorial for further details on other
# analytic issues.


# Prescription medications ------------------------------------------------

rx <-
  read.xport('data-raw/NHANES data/RXQ_RX_G.XPT') %>%
  dplyr::select(SEQN, RXDDRUG, RXDDAYS) %>%
  mutate(
    RXDDRUG = ifelse(RXDDRUG == "", "none", RXDDRUG)
  ) %>%
  pivot_wider(names_from = "RXDDRUG", values_from = "RXDDAYS", values_fill = 0, values_fn = mean)

rx_meds <- rx[c("SEQN", "TESTOSTERONE")]

rx_drugs <- read.xport('data-raw/NHANES data/RXQ_DRUG.xpt')

rx2 <-
  read.xport('data-raw/NHANES data/RXQ_RX_G.XPT') %>%
  dplyr::select(SEQN, RXDDRGID, RXDDAYS, RXDCOUNT) %>%
  mutate(
    RXDDRUG = ifelse(RXDDRGID == "", "none", RXDDRGID)
  ) %>%
  left_join(rx_drugs[c('RXDDRGID', 'RXDDCN1A')]) %>%
  mutate(
    RXDDCN1A = ifelse(RXDDRUG == 'none', 'None', RXDDCN1A)
  ) %>%
  group_by(SEQN, RXDDCN1A) %>%
  summarise(
    Taking = 1,
    med_count = mean(RXDCOUNT, na.rm=T)
  ) %>%
  mutate(
    med_count = ifelse(is.nan(med_count), 0, med_count)
  ) %>%
  pivot_wider(names_from = 'RXDDCN1A', values_from = Taking, values_fill = 0, names_repair = 'universal')


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
  left_join(sxq) %>%
  left_join(dr1) %>%
  left_join(dr2) %>%
  left_join(rx_meds) %>%
  left_join(rx2)

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
