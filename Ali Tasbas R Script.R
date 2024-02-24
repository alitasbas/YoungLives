setwd("D:/Tutoring/Homeworks/YoungLives")

df <- read.csv("younglives.csv")

# I will analyze factors that lead to a healthy or non-healthy birth
# Birth weight is my dependent variable (bwght_et1).
# For explanatory variables, I have chosen Antenatal visits(NUMANTE), difficulty of pregnancy(DIFFPREG),
# *fluency of the local language(FLUENCY), Mother Religion(MOTHREL), Perception of child size at birth(BSIZE),
# whether someone in the household has debt with money lenders or not(INFDEBT)*
# bad event occurred during pregnancy or not(BADEVENT)

df <- df[, c("childid", "bwght_et1", "numante_et1", "diffpreg_et1", "fluency_et1", "literany_et1",
             "mothrel_et1", "bplace_et1", "typesite_et1", "bsize_et1", "infdebt_et1", "badevent_et1")]

colnames(df) <- c("childid", "birth_weight", "antenatal_visits", "preg_difficulty", "local_fluency", "literacy",
                  "religion", "birth_place", "site_type", "perceived_bby_size", "money_lenders_debt", "bad_event_during_pregnancy")
  
# Starting with my response variable

summary(df$birth_weight)

# -9999 are Unknown values

indi = which(df$birth_weight != -9999); indi

df <- df[df$birth_weight != -9999, ]

df$z_birth_weight <- scale(df$birth_weight, center = T, scale = T)


# Tidying the explanatory variables

# NUMANTE 88 can imply no visits or the child carer is not the mother. I will also omit 88 to avoid doubtful assumptions
# I want to use the number of antenatal visits as it can be a predictor of child health at birth.
# However, value = 99 corresponds to cases where there were no visits or the respondent was not the mother.
# 0 antenatal visits may suggest a healthy pregnancy, or on the contrary, be an indication of poor care.

df <- df[df$antenatal_visits != 99, ]
df <- df[df$antenatal_visits != 88, ]

# Difficulty of pregnancy
# 99 are missing values

df <- df[df$preg_difficulty != 99, ]

# Let's also convert it to a 2-level ordered factor.

df$preg_difficulty <- factor(df$preg_difficulty, levels = c(2, 1), labels = c("Bad/Poor", "Good/Average"), ordered = T)

# Fluency has missing values coded as 88

table(df$local_fluency)
df <- df[df$local_fluency != 88, ]
df$local_fluency <- factor(df$local_fluency, levels = c(3, 2, 1), labels = c("Basic", "Good", "Fluent"), ordered = T)

# *Literacy: must convert to factor

table(df$literacy)
df$literacy <- factor(df$literacy, levels = c(3, 2, 1), labels = c("Not at all", "With difficulty", "Easily"), ordered = T)

# Caregiver's Religion must convert to factor

table(df$religion)
df$religion <- factor(df$religion, labels = c("Muslim", "Catholic", "Protestant", "Orthodox", "Other"))


# Perceived baby size: 99 represents missing values

table(df$perceived_bby_size)
df <- df[df$perceived_bby_size != 99, ]
df$perceived_bby_size <- factor(df$perceived_bby_size, levels = c(5, 4, 3, 2, 1),
                                labels = c("Very small", "Small", "Average", "Large", "Very Large"), ordered = T)

# Debt to money leaders: 99 represents missing values

table(df$money_lenders_debt)
df <- df[df$money_lenders_debt != 99, ]
df$money_lenders_debt <- factor(df$money_lenders_debt, labels = c("Yes", "No", "Don't have debts"))

# Bad event: convert to factor

table(df$bad_event_during_pregnancy)
df$bad_event_during_pregnancy <- factor(df$bad_event_during_pregnancy, labels = c("Yes", "No"))

# Birth Place: convert to factor

table(df$birth_place)
df$birth_place <- factor(df$birth_place, labels = c("Home", "Hospital", "Other Health Facility"))

# Site type
table(df$site_type)
df$site_type <- factor(df$site_type, labels = c("Urban", "Rural"))

# Constructing our model

# All the variables mixed
model <- lm(birth_weight ~ antenatal_visits + preg_difficulty + local_fluency + literacy + birth_place + site_type +
            religion + perceived_bby_size + money_lenders_debt + bad_event_during_pregnancy, data = df)

summary(model)

# A good fit
model3 <- lm(birth_weight ~ preg_difficulty + literacy + site_type +
               religion + perceived_bby_size + money_lenders_debt, data = df)

summary(model3)
