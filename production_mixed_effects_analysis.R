# Production Study: Linear Mixed-Effects Analysis
# ------------------------------------------------
# Cleaned from the final mixed-effects workflow in Codes.R.
#
# Acoustic measures analysed here:
#   VOT = voice onset time
#   CV  = closure voicing
#
# The final Codes.R workflow contains:
#   - separate Initial/Medial x Bilabial/Velar models
#   - participant random intercepts
#   - stimulus-speaker gender as a fixed effect
#   - Tukey-adjusted pairwise comparisons using emmeans
#   - full Category x Context x Place models
#
# NOTE:
# Closure duration (CD) is present in the source dataset, but a final CD
# mixed-effects block is not present in the final Codes.R file used to build
# this script, so it is not added here.
#
# Input file expected:
#   Production_Data_for_R.xlsx


# Packages -----------------------------------------------------------------

library(readxl)
library(dplyr)
library(stringr)
library(lme4)
library(lmerTest)
library(emmeans)


# Load data ----------------------------------------------------------------

Data <- read_excel("Production_Data_for_R.xlsx")


# Convert acoustic durations from seconds to milliseconds ------------------

Data$CV  <- Data$CV  * 1000
Data$CD  <- Data$CD  * 1000
Data$VOT <- Data$VOT * 1000


# Derive stimulus variables ------------------------------------------------

Data <- Data %>%
  mutate(
    BaseStimuli = str_remove(Stimuli, "-[mf]$"),

    StimGender = case_when(
      str_ends(Stimuli, "-m") ~ "m",
      str_ends(Stimuli, "-f") ~ "f",
      TRUE ~ NA_character_
    ),

    Context = case_when(
      BaseStimuli %in% c(
        "ba", "bha", "pa", "pha",
        "ga", "gha", "ka", "kha"
      ) ~ "Initial",

      BaseStimuli %in% c(
        "aba", "abha", "apa", "apha",
        "aga", "agha", "aka", "akha"
      ) ~ "Medial",

      TRUE ~ NA_character_
    ),

    Place = case_when(
      BaseStimuli %in% c(
        "ba", "bha", "pa", "pha",
        "aba", "abha", "apa", "apha"
      ) ~ "Bilabial",

      BaseStimuli %in% c(
        "ga", "gha", "ka", "kha",
        "aga", "agha", "aka", "akha"
      ) ~ "Velar",

      TRUE ~ NA_character_
    ),

    Category = case_when(
      BaseStimuli %in% c("ba", "aba", "ga", "aga") ~ "Voiced_Unasp",
      BaseStimuli %in% c("bha", "abha", "gha", "agha") ~ "Voiced_Asp",
      BaseStimuli %in% c("pa", "apa", "ka", "aka") ~ "Voiceless_Unasp",
      BaseStimuli %in% c("pha", "apha", "kha", "akha") ~ "Voiceless_Asp",
      TRUE ~ NA_character_
    ),

    Category = factor(
      Category,
      levels = c(
        "Voiced_Unasp",
        "Voiced_Asp",
        "Voiceless_Unasp",
        "Voiceless_Asp"
      )
    ),

    Context = factor(
      Context,
      levels = c("Initial", "Medial")
    ),

    Place = factor(
      Place,
      levels = c("Bilabial", "Velar")
    ),

    StimGender = factor(StimGender),
    Participants = factor(Participants)
  )


# Check derived categories --------------------------------------------------

table(
  Data$Category,
  Data$Context,
  Data$Place
)

sum(is.na(Data$Category))


# ==========================================================================
# VOT MODELS
# ==========================================================================


# Separate VOT models -------------------------------------------------------

initial_bilabial_vot <- lmer(
  VOT ~ BaseStimuli + StimGender + (1 | Participants),
  data = subset(
    Data,
    Context == "Initial" &
      Place == "Bilabial"
  )
)

medial_bilabial_vot <- lmer(
  VOT ~ BaseStimuli + StimGender + (1 | Participants),
  data = subset(
    Data,
    Context == "Medial" &
      Place == "Bilabial"
  )
)

initial_velar_vot <- lmer(
  VOT ~ BaseStimuli + StimGender + (1 | Participants),
  data = subset(
    Data,
    Context == "Initial" &
      Place == "Velar"
  )
)

medial_velar_vot <- lmer(
  VOT ~ BaseStimuli + StimGender + (1 | Participants),
  data = subset(
    Data,
    Context == "Medial" &
      Place == "Velar"
  )
)


# VOT model summaries -------------------------------------------------------

summary(initial_bilabial_vot)
summary(medial_bilabial_vot)
summary(initial_velar_vot)
summary(medial_velar_vot)


# VOT post-hoc comparisons --------------------------------------------------

vot_initial_bilabial_emm <- emmeans(
  initial_bilabial_vot,
  pairwise ~ BaseStimuli
)

vot_medial_bilabial_emm <- emmeans(
  medial_bilabial_vot,
  pairwise ~ BaseStimuli
)

vot_initial_velar_emm <- emmeans(
  initial_velar_vot,
  pairwise ~ BaseStimuli
)

vot_medial_velar_emm <- emmeans(
  medial_velar_vot,
  pairwise ~ BaseStimuli
)

vot_initial_bilabial_emm
vot_medial_bilabial_emm
vot_initial_velar_emm
vot_medial_velar_emm


# Full VOT model ------------------------------------------------------------
# Maximum likelihood was used in the original full-model analysis.

VOT_full <- lmer(
  VOT ~ Category * Context * Place +
    StimGender +
    (1 | Participants),
  data = Data,
  REML = FALSE
)

summary(VOT_full)


# ==========================================================================
# CLOSURE VOICING (CV) MODELS
# ==========================================================================


# Separate closure-voicing models ------------------------------------------

initial_bilabial_cv <- lmer(
  CV ~ BaseStimuli + StimGender + (1 | Participants),
  data = subset(
    Data,
    Context == "Initial" &
      Place == "Bilabial"
  )
)

medial_bilabial_cv <- lmer(
  CV ~ BaseStimuli + StimGender + (1 | Participants),
  data = subset(
    Data,
    Context == "Medial" &
      Place == "Bilabial"
  )
)

initial_velar_cv <- lmer(
  CV ~ BaseStimuli + StimGender + (1 | Participants),
  data = subset(
    Data,
    Context == "Initial" &
      Place == "Velar"
  )
)

medial_velar_cv <- lmer(
  CV ~ BaseStimuli + StimGender + (1 | Participants),
  data = subset(
    Data,
    Context == "Medial" &
      Place == "Velar"
  )
)


# Closure-voicing model summaries ------------------------------------------

summary(initial_bilabial_cv)
summary(medial_bilabial_cv)
summary(initial_velar_cv)
summary(medial_velar_cv)


# Closure-voicing post-hoc comparisons -------------------------------------

cv_initial_bilabial_emm <- emmeans(
  initial_bilabial_cv,
  pairwise ~ BaseStimuli
)

cv_medial_bilabial_emm <- emmeans(
  medial_bilabial_cv,
  pairwise ~ BaseStimuli
)

cv_initial_velar_emm <- emmeans(
  initial_velar_cv,
  pairwise ~ BaseStimuli
)

cv_medial_velar_emm <- emmeans(
  medial_velar_cv,
  pairwise ~ BaseStimuli
)

cv_initial_bilabial_emm
cv_medial_bilabial_emm
cv_initial_velar_emm
cv_medial_velar_emm


# Full closure-voicing model ------------------------------------------------
# Maximum likelihood was used in the original full-model analysis.

CV_full <- lmer(
  CV ~ Category * Context * Place +
    StimGender +
    (1 | Participants),
  data = Data,
  REML = FALSE
)

summary(CV_full)
