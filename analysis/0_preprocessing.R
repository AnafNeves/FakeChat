library(jsonlite)
library(progress)

# path for data
# path <- "C:/Users/asf25/Box/FakeChat/data/"
# path <- "C:/Users/asf25/Box/FakeChat/tests/"



files <- list.files(path, pattern = "*.csv")

# Progress bar
progbar <- progress_bar$new(total = length(files))

alldata <- data.frame()
swap_links <- data.frame()
all_task <- data.frame()

for (file in files){
 # file <- "Emma_test.csv"
  progbar$tick()
  rawdata <- read.csv(paste0(path, "/", file))
  message(paste("\nProcessing:", file))
  
  dat <- rawdata[rawdata$screen == "browser_info", ]
  
  participantID <- gsub(".csv", "", rev(strsplit(file, "/")[[1]])[1]) # Filename without extension
  
  data_ppt <- data.frame(
    Participant = participantID,
    Recruitment = dat$researcher,
    Experiment_StartDate = as.POSIXct(paste(dat$date, dat$time), format = "%d/%m/%Y %H:%M:%S"),
    Experiment_Duration = rawdata[rawdata$screen == "demographics_debrief", "time_elapsed"] / 1000 / 60,
    Browser_Version = paste(dat$browser, dat$browser_version),
    Mobile = dat$mobile,
    Platform = dat$os,
    Screen_Width = dat$screen_width,
    Screen_Height = dat$screen_height)
  
  # Demographics
  demog <- jsonlite::fromJSON(rawdata[rawdata$screen == "demographic_questions", ]$response)
  
  # Gender
  demog$Gender <- ifelse(demog$Gender == "other", demog$`Gender-Comment`, demog$Gender)
  demog$`Gender-Comment` <- NULL
  
  # Education
  demog$Education <- ifelse(demog$Education == "other", demog$`Education-Comment`, demog$Education)
  demog$`Education-Comment` <- NULL
  
  demog$Student <- ifelse(!is.null(demog$Student), demog$Student, NA)
  demog$Student <-  ifelse(demog$Student == "other", demog$`Student-Comment`, demog$Student)
  demog$`Student-Comment` <- NULL
  
  # Ethnicity
  demog$Ethnicity <- ifelse(!is.null(demog$Ethnicity), demog$Ethnicity, NA)
  demog$Ethnicity <- ifelse(demog$Ethnicity == "other", demog$`Ethnicity-Comment`, demog$Ethnicity)
  demog$`Ethnicity-Comment` <- NULL

  demog <- as.data.frame(demog)
  data_ppt <- cbind(data_ppt, demog)
  
  
  # UESTIONNAIRESS ==================================================================================================
  
  ### --------------------------------------------------Before Task
  
  # DeJong
  
  dejong <- jsonlite::fromJSON(rawdata[rawdata$screen == "questionnaire_dejong", "response"])
  data_ppt <- cbind(data_ppt, as.data.frame(dejong))
  
  # UCLA 
  
  ucla <- jsonlite::fromJSON(rawdata[rawdata$screen == "questionnaire_ucla", "response"])
  data_ppt <- cbind(data_ppt, as.data.frame(ucla))
  
  # PHQ4
  phq4 <- jsonlite::fromJSON(rawdata[rawdata$screen == "questionnaire_phq4", "response"])
  data_ppt <- cbind(data_ppt, as.data.frame(phq4[-6]))
  
  # Mental health 
  
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
  v[grep("OCD", v)] <- "OCD"
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
  
  # Medical and somatic difficulties
  
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
  
  ###-------------------------------- After Task 
  
  # MINT
  
  mint <- jsonlite::fromJSON(rawdata[rawdata$screen == "questionnaire_mint", "response"])
  data_ppt <- cbind(data_ppt, as.data.frame(mint))
  
  # BAIT
  
  bait <- jsonlite::fromJSON(rawdata[rawdata$screen == "questionnaire_bait", "response"])
  data_ppt <- cbind(data_ppt, as.data.frame(bait))
  
  # IRI
  
  iri <- jsonlite::fromJSON(rawdata[rawdata$screen == "questionnaire_iri", "response"])
  data_ppt <- cbind(data_ppt, as.data.frame(iri))
  
  # SEACS
  
  seacs <- jsonlite::fromJSON(rawdata[rawdata$screen == "questionnaire_seacs", "response"])
  data_ppt <- cbind(data_ppt, as.data.frame(seacs))

  
  # RQS  
  rqs <- jsonlite::fromJSON(rawdata[rawdata$screen == "questionnaire_rqs", "response"])
  data_ppt <- cbind(data_ppt, as.data.frame(rqs))
  
  # --------------------------------- Loneliness Follow Up
  
  # Loneliness General
  loneliness_g <- jsonlite::fromJSON(rawdata[rawdata$screen == "loneliness_general", "response"])
  data_ppt <- cbind(data_ppt, as.data.frame(loneliness_g))
  
  # Loneliness Mental 
  loneliness_m <- jsonlite::fromJSON(rawdata[rawdata$screen == "loneliness_mental", "response"])
  data_ppt <- cbind(data_ppt, as.data.frame(loneliness_m))
  
  # Loneliness Romantic 
  loneliness_r <- jsonlite::fromJSON(rawdata[rawdata$screen == "loneliness_romantic", "response"])
  data_ppt <- cbind(data_ppt, as.data.frame(loneliness_r))
  
  # Loneliness People 
  loneliness_p <- jsonlite::fromJSON(rawdata[rawdata$screen == "loneliness_people", "response"])
  # loneliness_p <- loneliness_p[!sapply(loneliness_p, is.null)]

  # Standardize checkbox responses: replace "other" with "Other" and keep others as-is
  if (!is.null(loneliness_p$LonelinessPeople)) {
    loneliness_p$LonelinessPeople <- ifelse(
      loneliness_p$LonelinessPeople == "other",
      "Other",
      loneliness_p$LonelinessPeople
    )
  }

  # rename 
  loneliness_p$LonelinessPeople[grep("AI ", loneliness_p$LonelinessPeople)] <- "AI"
  loneliness_p$LonelinessPeople[grep("Favourite TV show", loneliness_p$LonelinessPeople)] <- "Media"
  loneliness_p$LonelinessPeople[grep("Charities", loneliness_p$LonelinessPeople)] <- "Charities"

  # Collapse multiple selections into a single string (if you want)
  data_ppt$Loneliness_People <- paste0(loneliness_p$LonelinessPeople, collapse = "; ")

  # Create separate TRUE/FALSE columns for each option
  options_vec <- c("Friends", "Family", "AI", "Media", "Charities", "Other")
  for (opt in options_vec) {
    col_name <- paste0("LonelinessPeople_", opt)
    data_ppt[[col_name]] <- opt %in% loneliness_p$LonelinessPeople
  }

  # Extract Likert ratings (keeping NA if they didn't select that option)
  rating_vars <- c(
    "LonelinessFriends",
    "LonelinessFamily",
    "LonelinessAI",
    "LonelinessMedia",
    "LonelinessCharities"
  )

  for (var in rating_vars) {
    data_ppt[[var]] <- ifelse(is.null(loneliness_p[[var]]), NA, loneliness_p[[var]])
  }
  
  # Feedback
  
  feedback <- jsonlite::fromJSON(rawdata[rawdata$screen == "experiment_feedback", "response"])
  
  data_ppt$Feedback_Enjoyment <- ifelse(is.null(feedback$Feedback_Enjoyment), NA, feedback$Feedback_Enjoyment)
  data_ppt$Feedback_Quality <- ifelse(is.null(feedback$Feedback_Quality), NA, feedback$Feedback_Quality)
  data_ppt$Feedback_Text <- ifelse(is.null(feedback$Feedback_Text), NA, feedback$Feedback_Text)
  
  
  # Study Swap links
  #swap_links <- data.frame(link = ifelse(is.null(feedback$Study_Swap), NA, feedback$Study_Swap))
  

  alldata <- rbind(alldata, data_ppt)  
  
  # TASK DATA ====================================================================================
  
  # ---------------------------phase 1
  stims1 <- rawdata[rawdata$screen == "vignette_image1", ]
  
  resp1 <- sapply(
    rawdata[rawdata$screen == "vignette_scales", "response"],
    \(x) {
      x <- as.data.frame(jsonlite::fromJSON(x))
      if (!"AttentionCheck" %in% names(x)) {
        x$AttentionCheck <- NA
      }
      x
    },
    simplify = FALSE,
    USE.NAMES = FALSE
  )
  
  
  attention_check1 <- sapply(
    rawdata[rawdata$screen == "vignette_attentioncheck", "response"],
    \(x) {
      x <- as.data.frame(jsonlite::fromJSON(x))
      x
    },
    simplify = FALSE,
    USE.NAMES = FALSE
  )
  
  resp1 <- do.call(rbind, resp1)
  attention_check1 <- do.call(rbind, attention_check1)
  ratings1 <- rbind(resp1, attention_check1)


  stims1$order <- seq_len(nrow(stims1))
  ratings1$order <- seq_len(nrow(ratings1))

  
  data_task <- merge(
    stims1[, c("participantID",
               "vignette_id",
               "condition",
               "topic",
               "order")],
    ratings1,
    by = "order",
    all.x = TRUE
  )
  
 data_task$participantID <- rep(participantID, each = 6)
  
  # Remove order column
  data_task$order <- NULL
  
  
  # --------------------------phase 2 
  
  stims2 <- rawdata[rawdata$screen == "phase2_vignette", ]
  
  resp2 <- sapply(
    rawdata[rawdata$screen == "phase2_scale", "response"],
    \(x) {
      dat <- jsonlite::fromJSON(x)
      dat$Box <- NULL
      as.data.frame(dat)
    },
    simplify = FALSE,
    USE.NAMES = FALSE
  )
  
  
  ratings2 <- do.call(rbind, resp2)

  
  stims2$order <- seq_len(nrow(stims2))
  ratings2$order <- seq_len(nrow(ratings2))
  
  
  data_task2 <- merge(
    stims2[, c("participantID",
               "vignette_id",
               "order")],
    ratings2,
    by = "order",
    all.x = TRUE
  )
  
  data_task2$participantID <- rep(participantID, each = 6)
  
  # Remove order column
  data_task2$order <- NULL
  data_task2$Box <- NULL
  
  # Merge and clean
  data_task <- merge(data_task, data_task2, by = c("participantID", "vignette_id"), all.x = TRUE)
  
  all_task <- rbind(all_task, data_task)
  all_task <- as.data.frame(all_task)
}


# Reanonimize ============================================================

# # order based on the date of the experiment
# alldata <- alldata[order(alldata$Experiment_StartDate), ]
# # Create correspondence map (mapping original Participant IDs to new ones)
# correspondance <- setNames(paste0("S", sprintf("%03d", seq_along(alldata$Participant))), alldata$Participant)
# # Reanonymize both datasets by updating the 'Participant' column
# alldata$Participant <- correspondance[alldata$Participant]
# alldata$Participant <- correspondance[alldata$Participant]

# Save --------------------------------------------------------------------

write.csv(alldata, "../data/rawdata_participants.csv", row.names = FALSE)
write.csv(all_task, "../data/rawdata_task.csv", row.names = FALSE)


# Study Swap Link
#write.csv(swap_links , "../swap_links.csv", row.names = FALSE)









