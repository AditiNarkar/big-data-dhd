# Person 2: adjusted logistic-regression analysis
# Run from the project root:
#   Rscript scripts/02_person2_model.R
#
# This script uses only base R and reads Person 1's supplied analysis file.

options(stringsAsFactors = FALSE)

input_file <- file.path("outputs", "person1", "analysis_person_level.csv")
output_dir <- file.path("outputs", "person2")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(input_file)) stop("Person 1 handoff file not found: ", input_file)

d <- read.csv(input_file, stringsAsFactors = FALSE, check.names = FALSE)
needed <- c("ACCIDENT_NO", "PERSON_ID", "AGE_GROUP", "ROAD_USER_TYPE_DESC",
            "ACCIDENT_TYPE_DESC", "SPEED_ZONE", "ATMOSPH_COND_DESC",
            "SURFACE_COND_DESC", "vehicle_group", "serious_fatal")
missing_columns <- setdiff(needed, names(d))
if (length(missing_columns) > 0) {
  stop("Missing columns: ", paste(missing_columns, collapse = ", "))
}
if (anyDuplicated(d[c("ACCIDENT_NO", "PERSON_ID")])) {
  stop("Duplicate ACCIDENT_NO + PERSON_ID records found.")
}

d$serious_fatal <- as.integer(d$serious_fatal)
if (!all(d$serious_fatal %in% c(0L, 1L))) {
  stop("serious_fatal must contain only 0 and 1.")
}

# Create stable categories for modelling. Unknown codes are retained as a
# category instead of being treated as actual speed limits or weather values.
d$SPEED_ZONE <- trimws(as.character(d$SPEED_ZONE))
valid_zones <- c("30", "40", "50", "60", "70", "75", "80", "90", "100", "110")
d$speed_zone_model <- ifelse(d$SPEED_ZONE %in% valid_zones,
                             d$SPEED_ZONE, "Unknown/other")
d$weather_model <- ifelse(
  d$ATMOSPH_COND_DESC %in% c("Clear", "Raining", "Fog", "Strong winds"),
  d$ATMOSPH_COND_DESC,
  ifelse(d$ATMOSPH_COND_DESC == "Not known", "Unknown", "Other weather")
)
d$surface_model <- ifelse(
  d$SURFACE_COND_DESC %in% c("Dry", "Wet"),
  d$SURFACE_COND_DESC,
  ifelse(d$SURFACE_COND_DESC %in% c("Unk.", "Unknown"), "Unknown", "Other surface")
)

make_factor <- function(x, reference) {
  x <- as.character(x)
  x[is.na(x) | x == ""] <- "Unknown"
  values <- sort(unique(x))
  if (!(reference %in% values)) stop("Reference category missing: ", reference)
  factor(x, levels = c(reference, setdiff(values, reference)))
}

d$vehicle_group_model <- make_factor(d$vehicle_group, "Car/light vehicle")
d$age_group_model <- make_factor(d$AGE_GROUP, "30-39")
d$speed_zone_model <- make_factor(d$speed_zone_model, "60")
d$weather_model <- make_factor(d$weather_model, "Clear")
d$surface_model <- make_factor(d$surface_model, "Dry")
d$accident_type_model <- make_factor(d$ACCIDENT_TYPE_DESC, "Collision with vehicle")

# Record the model population and the make-up of the heterogeneous Other group.
audit <- data.frame(
  measure = c("Person 1 handoff rows", "Unique accidents", "Serious/fatal outcomes",
              "Lower-severity outcomes", "Other vehicle-group rows",
              "Bicyclists within Other vehicle group"),
  n = c(nrow(d), length(unique(d$ACCIDENT_NO)), sum(d$serious_fatal == 1L),
        sum(d$serious_fatal == 0L), sum(d$vehicle_group_model == "Other"),
        sum(d$vehicle_group_model == "Other" & d$ROAD_USER_TYPE_DESC == "Bicyclists"))
)
write.csv(audit, file.path(output_dir, "model_population_audit.csv"), row.names = FALSE)

road_users <- as.data.frame(table(vehicle_group = d$vehicle_group_model,
                                  road_user_type = d$ROAD_USER_TYPE_DESC))
names(road_users)[3] <- "people_n"
write.csv(road_users, file.path(output_dir, "road_user_composition_by_vehicle_group.csv"),
          row.names = FALSE)

# Split by accident, not by person, so people from one crash do not appear in
# both the training and test sets.
set.seed(20261031)
accident_ids <- unique(d$ACCIDENT_NO)
test_ids <- sample(accident_ids, size = floor(0.20 * length(accident_ids)))
d$model_split <- ifelse(d$ACCIDENT_NO %in% test_ids, "Test", "Training")
training <- d[d$model_split == "Training", ]
test <- d[d$model_split == "Test", ]

split_audit <- do.call(rbind, lapply(c("Training", "Test"), function(split_name) {
  x <- d[d$model_split == split_name, ]
  data.frame(split = split_name, accidents_n = length(unique(x$ACCIDENT_NO)),
             people_n = nrow(x), serious_fatal_n = sum(x$serious_fatal),
             serious_fatal_pct = 100 * mean(x$serious_fatal))
}))
write.csv(split_audit, file.path(output_dir, "train_test_split_audit.csv"), row.names = FALSE)

# Raw results: direct percentages before accounting for the other variables.
raw <- aggregate(serious_fatal ~ vehicle_group_model, data = training,
                 FUN = function(x) c(people_n = length(x), events = sum(x), pct = 100 * mean(x)))
raw_table <- data.frame(
  vehicle_group = raw$vehicle_group_model,
  people_n = raw$serious_fatal[, "people_n"],
  serious_fatal_n = raw$serious_fatal[, "events"],
  serious_fatal_pct = raw$serious_fatal[, "pct"]
)
write.csv(raw_table, file.path(output_dir, "unadjusted_vehicle_group_summary.csv"), row.names = FALSE)

# The first model is the unadjusted comparison. The second is the model that
# answers the approved question using all agreed adjustment variables.
unadjusted_model <- glm(serious_fatal ~ vehicle_group_model,
                         family = binomial(), data = training)
adjusted_model <- glm(
  serious_fatal ~ vehicle_group_model + age_group_model + speed_zone_model +
    weather_model + surface_model + accident_type_model,
  family = binomial(), data = training
)

make_odds_ratio_table <- function(model, model_name) {
  x <- summary(model)$coefficients
  out <- data.frame(
    model = model_name, term = rownames(x), estimate_log_odds = x[, 1],
    standard_error = x[, 2], statistic = x[, 3], p_value = x[, 4],
    stringsAsFactors = FALSE
  )
  out$odds_ratio <- exp(out$estimate_log_odds)
  out$lower_95_ci <- exp(out$estimate_log_odds - 1.96 * out$standard_error)
  out$upper_95_ci <- exp(out$estimate_log_odds + 1.96 * out$standard_error)
  out
}

odds_ratios <- rbind(make_odds_ratio_table(unadjusted_model, "Unadjusted"),
                     make_odds_ratio_table(adjusted_model, "Adjusted"))
write.csv(odds_ratios, file.path(output_dir, "odds_ratios_unadjusted_and_adjusted.csv"),
          row.names = FALSE)

# Adjusted probabilities are calculated for a clear reference scenario. This
# gives an easy-to-explain, like-for-like comparison between vehicle groups.
reference <- data.frame(
  vehicle_group_model = levels(d$vehicle_group_model),
  age_group_model = "30-39", speed_zone_model = "60", weather_model = "Clear",
  surface_model = "Dry", accident_type_model = "Collision with vehicle",
  stringsAsFactors = FALSE
)
reference$vehicle_group_model <- factor(reference$vehicle_group_model,
                                        levels = levels(d$vehicle_group_model))
reference$age_group_model <- factor(reference$age_group_model, levels = levels(d$age_group_model))
reference$speed_zone_model <- factor(reference$speed_zone_model, levels = levels(d$speed_zone_model))
reference$weather_model <- factor(reference$weather_model, levels = levels(d$weather_model))
reference$surface_model <- factor(reference$surface_model, levels = levels(d$surface_model))
reference$accident_type_model <- factor(reference$accident_type_model,
                                        levels = levels(d$accident_type_model))
reference$adjusted_probability <- predict(adjusted_model, newdata = reference, type = "response")
reference$adjusted_probability_pct <- 100 * reference$adjusted_probability
write.csv(reference, file.path(output_dir, "adjusted_probabilities_reference_profile.csv"),
          row.names = FALSE)

# Test whether vehicle-group differences vary across speed zones. Sparse cells
# are written to a table so you can decide whether the interaction is credible.
cell_counts <- as.data.frame(table(vehicle_group = training$vehicle_group_model,
                                   speed_zone = training$speed_zone_model))
names(cell_counts)[3] <- "people_n"
write.csv(cell_counts, file.path(output_dir, "vehicle_group_by_speed_zone_counts.csv"),
          row.names = FALSE)

interaction_model <- try(glm(
  serious_fatal ~ vehicle_group_model * speed_zone_model + age_group_model +
    weather_model + surface_model + accident_type_model,
  family = binomial(), data = training
), silent = TRUE)

if (inherits(interaction_model, "try-error")) {
  writeLines("Interaction model could not be fitted; review sparse cells and report this limitation.",
             file.path(output_dir, "interaction_model_note.txt"))
} else {
  comparison <- as.data.frame(anova(adjusted_model, interaction_model, test = "Chisq"))
  comparison$model <- c("Adjusted main-effects model", "Adjusted interaction model")
  write.csv(comparison, file.path(output_dir, "interaction_model_comparison.csv"),
            row.names = FALSE)
}

# Predictive check using only crashes that were not in the training data.
test$predicted_probability <- predict(adjusted_model, newdata = test, type = "response")
test$predicted_class_0_5 <- ifelse(test$predicted_probability >= 0.5, 1L, 0L)

auc_from_ranks <- function(observed, predicted) {
  positives <- sum(observed == 1L)
  negatives <- sum(observed == 0L)
  if (positives == 0L || negatives == 0L) return(NA_real_)
  ranks <- rank(predicted, ties.method = "average")
  (sum(ranks[observed == 1L]) - positives * (positives + 1) / 2) /
    (positives * negatives)
}

tp <- sum(test$predicted_class_0_5 == 1L & test$serious_fatal == 1L)
tn <- sum(test$predicted_class_0_5 == 0L & test$serious_fatal == 0L)
fp <- sum(test$predicted_class_0_5 == 1L & test$serious_fatal == 0L)
fn <- sum(test$predicted_class_0_5 == 0L & test$serious_fatal == 1L)

performance <- data.frame(
  metric = c("ROC_AUC", "Brier_score", "Accuracy_at_0.5", "Sensitivity_at_0.5", "Specificity_at_0.5"),
  value = c(auc_from_ranks(test$serious_fatal, test$predicted_probability),
            mean((test$serious_fatal - test$predicted_probability)^2),
            mean(test$predicted_class_0_5 == test$serious_fatal),
            tp / (tp + fn), tn / (tn + fp))
)
write.csv(performance, file.path(output_dir, "test_set_performance.csv"), row.names = FALSE)

confusion_matrix <- data.frame(
  actual = c("Lower severity", "Lower severity", "Serious/fatal", "Serious/fatal"),
  predicted = c("Lower severity", "Serious/fatal", "Lower severity", "Serious/fatal"),
  people_n = c(tn, fp, fn, tp)
)
write.csv(confusion_matrix, file.path(output_dir, "test_set_confusion_matrix.csv"), row.names = FALSE)

png(file.path(output_dir, "adjusted_probabilities_reference_profile.png"),
    width = 1200, height = 800, res = 150)
barplot(reference$adjusted_probability_pct, names.arg = reference$vehicle_group_model,
        las = 2, ylab = "Predicted serious/fatal injury probability (%)",
        xlab = "Vehicle group",
        main = "Adjusted probability by vehicle group in a reference crash profile",
        col = "steelblue", ylim = c(0, max(reference$adjusted_probability_pct) * 1.15))
text(seq_len(nrow(reference)), reference$adjusted_probability_pct,
     labels = paste0(round(reference$adjusted_probability_pct, 1), "%"),
     pos = 3, cex = 0.8)
dev.off()

writeLines(c(
  "Person 2 modelling notes",
  "The primary model uses vehicle group, age group, speed zone, weather, road surface and accident type.",
  "Vehicle occupancy and light condition are not in Person 1's supplied analysis file, so this script does not include them.",
  "Special speed-zone codes are combined as Unknown/other rather than interpreted as posted speed limits.",
  "The Other vehicle group is heterogeneous and largely consists of bicyclists; describe this limitation in the report.",
  "The train/test split is by accident to avoid data leakage. The models show association, not causation.",
  "Reference-profile probabilities use: age 30-39, speed zone 60, clear weather, dry surface and collision with vehicle."
), file.path(output_dir, "model_notes.txt"))

saveRDS(unadjusted_model, file.path(output_dir, "unadjusted_model.rds"))
saveRDS(adjusted_model, file.path(output_dir, "adjusted_model.rds"))
writeLines(capture.output(summary(adjusted_model)),
           file.path(output_dir, "adjusted_model_summary.txt"))

cat("Person 2 model analysis complete. Outputs written to", output_dir, "\n")
