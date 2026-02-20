library(jsonlite)
library(progress)
library(tidyverse)
library(easystats)

options(warn = 2) # Stop on warnings

# path <- "C:/Users/maisi/Box/FakeChat/
# path <- "C:/Users/dmm56/Box/Data/FakeChat/"
# path <- "C:/Users/domma/Box/Data/FakeChat/"
# path <- "C:/Users/asf25/Box/FakeChat/"
path <- "C:/Users/emmal/Box/FakeChat/"

# Run loop ----------------------------------------------------------------


files <- list.files(path, full.names = TRUE, pattern = "*.csv")

# Progress bar
progbar <- progress_bar$new(total = length(files))

alldata <- data.frame()
#alldata_task <- data.frame()

for (file in files) {
  progbar$tick()
  rawdata <- read.csv(file)
  
  
  # Initialize participant-level data
  dat <- rawdata[rawdata$screen == "browser_info", ]
  
  if (nrow(dat) == 0) {
    print(paste0("skip (no browser_info): ", gsub(path, "", file)))
    next
  }
  
  # Deal with other datetime formats
  if(dat$date == "2/5/2025") {
    dat$date <- "05/02/2025"
  }
  
  data_ppt <- data.frame(
    Participant = dat$participantID,
    Recruitment = dat$researcher,
    Experiment_StartDate = as.POSIXct(paste(dat$date, dat$time), format = "%d/%m/%Y %H:%M:%S"),
    Experiment_Duration = max(rawdata$time_elapsed) / 1000 / 60,
    Browser_Version = paste(dat$browser, dat$browser_version),
    Mobile = dat$mobile,
    Platform = dat$os,
    Screen_Width = dat$screen_width,
    Screen_Height = dat$screen_height
  )

  data_ppt$AttentionScore <- rawdata[rawdata$screen == "demographics_debrief", "AttentionScore"]
  
  # Demographics
  resp <- jsonlite::fromJSON(rawdata[rawdata$screen == "demographic_questions", ]$response)
  
  data_ppt$Gender <- ifelse(!is.null(resp$Gender), resp$Gender, NA)
  data_ppt$Age <- ifelse(!is.null(resp$Age), resp$Age, NA)
  
  # Education
  
  
  data_ppt$Education <- ifelse(resp$Education == "other", resp$`Education-Comment`, resp$Education)
  data_ppt$Education <- ifelse(data_ppt$Education %in% c("HND (college)"), "High school", data_ppt$Education)
  # Detect "equivalent to a Bachelors" and "HND (college)" and convert to "Bachelor" and "High school"
  data_ppt$Education <- ifelse(stringr::str_detect(data_ppt$Education, "equivalent to a Bachelors"), "Bachelor", data_ppt$Education)
  data_ppt$Education <- ifelse(data_ppt$Education %in% c("3rd year BSc", "Bachelor non-university", "graduate certificate (Certificate IV)"), "Bachelor", data_ppt$Education)
  data_ppt$Education <- ifelse(data_ppt$Education %in% c("NVQ 4", "Professional", "Vocational degree.", "level 3 nvq's",
                                                         "tech college", "College - HND", "Vocational diploma", "Vocational Qualification",
                                                         "Professional qualification"), "High school", data_ppt$Education)
  
  data_ppt$Student <- ifelse(!is.null(resp$Student), resp$Student, NA)
  data_ppt$Country <- ifelse(!is.null(resp$Country), resp$Country, NA)
  
  # Ethnicity
  data_ppt$Ethnicity <- ifelse(!is.null(resp$Ethnicity), resp$Ethnicity, NA)
  data_ppt$Ethnicity <- ifelse(
    !is.na(resp$Ethnicity) && resp$Ethnicity == "other",
    resp$`Ethnicity-Comment`,
    resp$Ethnicity
  )
  data_ppt$Ethnicity <- ifelse(data_ppt$Ethnicity %in% c("Indigenous Native Indian"), "Other", data_ppt$Ethnicity)
  data_ppt$Ethnicity <- ifelse(data_ppt$Ethnicity %in% c("Turkish"), "Middle Eastern/North African", data_ppt$Ethnicity)
  data_ppt$Ethnicity <- ifelse(
    data_ppt$Ethnicity %in% c("Prefer not to say"),
    NA,
    data_ppt$Ethnicity
  )
  
  # Feedback
  feedback <- jsonlite::fromJSON(rawdata[rawdata$screen == "experiment_feedback", "response"])
  data_ppt$Experiment_Enjoyment <- ifelse(is.null(feedback$Feedback_Enjoyment), NA, feedback$Feedback_Enjoyment)
  data_ppt$Experiment_Quality <- ifelse(is.null(feedback$Feedback_Quality), NA, feedback$Feedback_Quality)
  data_ppt$Experiment_Feedback <- ifelse(is.null(feedback$Feedback_Text), NA, feedback$Feedback_Text)
  
  
  # Questionnaires
  mint <- as.data.frame(jsonlite::fromJSON(rawdata[
    rawdata$screen == "questionnaire_mint",
    "response"
  ]))
  names(mint) <- gsub("MINT_ReIA", "MINT_RelA", names(mint))
  data_ppt <- cbind(data_ppt, mint)
  data_ppt$Duration_MINT <- as.numeric(rawdata[
    rawdata$screen == "questionnaire_mint",
    "rt"
  ]) /
    1000 /
    60
  
  bait <- as.data.frame(jsonlite::fromJSON(rawdata[
    rawdata$screen == "questionnaire_bait",
    "response"
  ]))
  data_ppt <- cbind(data_ppt, bait)
  data_ppt$Duration_BAIT <- as.numeric(rawdata[
    rawdata$screen == "questionnaire_bait",
    "rt"
  ]) /
    1000 /
    60
  if (!"BAIT_AI_Use" %in% names(bait)) {
    data_ppt$BAIT_AI_Use <- NA
  }
  
  phq4 <- jsonlite::fromJSON(rawdata[
    rawdata$screen == "questionnaire_phq4",
    "response"
  ])
  phq4$instructions_phq4 <- NULL
  if (is.null(phq4$LifeSatisfaction)) phq4$LifeSatisfaction <- NA
  data_ppt <- cbind(data_ppt, as.data.frame(phq4))
  

  # Multiple Choices
  mental <- jsonlite::fromJSON(rawdata[rawdata$screen == "questionnaire_mentalhealth", "response"])
  v <- mental$Disorders_Psychiatric
  v[grep("GAD", v)] <- "GAD"
  v[grep("Eating", v)] <- "Eating"
  v[grep("PTSD", v)] <- "PTSD"
  v[grep("MDD", v)] <- "MDD"
  v[grep("ADHD", v)] <- "ADHD"
  v[grep("Specific Phobias", v)] <- "Phobia"
  v[grep("Autism", v)] <- "ASD"
  v[grep("Addiction ", v)] <- "Addiction"
  v[grep("Social Anxiety ", v)] <- "Social Phobia"
  v[grep("Borderline", v)] <- "BPD"
  v[grep("Panic ", v)] <- "Panic"
  v[grep("Bipolar ", v)] <- "Bipolar"
  v[grep("Obsessive-Compulsive ", v)] <- "OCD"
  v[grep("none", v)] <- "None"
  data_ppt$Disorders_Psychiatric <- paste0(v, collapse = "; ")
  
  if (!is.null(mental$Disorders_PsychiatricTreatment)) {
    v <- mental$Disorders_PsychiatricTreatment
    v[grep("Antidepressant", v)] <- "Antidepressant"
    v[grep("Antipsychotic ", v)] <- "Antipsychotic"
    v[grep("Anxiolytic", v)] <- "Anxiolytic"
    v[grep("LITHIUM", v)] <- "Mood Stabilizers"
    v[grep("Mindfulness", v)] <- "Mindfulness"
    v[grep("CBT", v)] <- "Psychotherapy"
    v[grep("Lifestyle", v)] <- "Lifestyle"
    v[grep("Alternative ", v)] <- "Alternative"
    data_ppt$Disorders_PsychiatricTreatment <- paste0(v, collapse = "; ")
    if (all(data_ppt$Disorders_PsychiatricTreatment == "none")) {
      data_ppt$Disorders_PsychiatricTreatment <- NA
    }
  } else {
    data_ppt$Disorders_PsychiatricTreatment <- NA
  }
  
  somatic <- jsonlite::fromJSON(rawdata[rawdata$screen == "questionnaire_somatichealth", "response"])
  somatic$Disorders_Somatic_Instructions <- NULL
  somatic <- somatic[grep("-Comment", names(somatic), invert = TRUE)]
  for (s in names(somatic)) {
    somatic[[s]] <- ifelse(somatic[[s]] == "other", paste0("Other ", gsub("Disorders_Somatic_", "", s)), somatic[[s]])
  }
  somatic <- unlist(somatic[sapply(somatic, function(x) all(x != "none"))])
  somatic[grep("IBS", somatic)] <- "IBS"
  somatic[grep("Lactose ", somatic)] <- "Lactose"
  somatic[grep("Gluten Intolerance", somatic)] <- "Gluten"
  somatic[grep("palpitations", somatic)] <- "Cardiac Arrhythmia"
  somatic[grep("GERD", somatic)] <- "Reflux"
  somatic[grep("COPD ", somatic)] <- "COPD"
  somatic[grep("Crohn's ", somatic)] <- "Crohn"
  somatic[grep("Sjogren's ", somatic)] <- "Sjogren"
  data_ppt$Disorders_Somatic <- paste0(somatic, collapse = "; ")
  
  alldata <- bind_rows(alldata, data_ppt)
}

# unique(alldata$Disorders_Psychiatric)
# unique(alldata$Disorders_PsychiatricTreatment)
# unique(alldata$Disorders_Somatic)
# table(alldata$Education)
# table(alldata$Ethnicity)
# table(alldata$Recruitment)

# Attention checks --------------------------------------------------------

checks <- data.frame(
  ID = alldata$Participant,
  Experiment_Duration = alldata$Experiment_Duration,
  MINT = as.numeric(alldata$MINT_AttentionCheck) / 6,
  BAIT = 1 - as.numeric(alldata$BAIT_AttentionCheck) / 6
)

checks$Score <- rowMeans(checks[, c("MINT", "BAIT")], na.rm = TRUE)

checks <- checks[!is.na(checks$ID), ]
checks <- checks[order(checks$Score, decreasing = TRUE), ]

hist(checks$Score)

score <- checks[!checks$ID %in% c("os", "fw", "dm"), "Score"]
hist(score)

mean(score < 0.6, na.rm = TRUE)

# checks[checks$ID=="5b7a360589bc2f0001e60363", ]

# MINT: "I can always accurately answer to the extreme left on this question to show that I am reading it"
# BAIT: "I can show that I am Human and not an AI by answering all the way to the right"


# Make sure there are no duplicates
if (nrow(alldata[duplicated(alldata), ]) > 0) {
  stop("Duplicates detected!")
}

# Anonymize ---------------------------------------------------------------
# Generate IDs
ids <- paste0("S", format(sprintf("%03d", 1:nrow(alldata))))
# Sort Participant according to date and assign new IDs
names(ids) <- alldata$Participant[order(alldata$Experiment_StartDate)]
# Replace IDs
alldata$Participant <- ids[alldata$Participant]
#alldata_task$Participant <- ids[alldata_task$Participant]


# Save --------------------------------------------------------------------
# restore default warnings settings
options(warn = 0)

write.csv(alldata, "../data/rawdata_participants.csv", row.names = FALSE)
#write.csv(alldata_task, "../data/rawdata_task.csv", row.names = FALSE)
