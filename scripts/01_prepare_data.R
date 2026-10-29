options(stringsAsFactors = FALSE)

data_dir <- "data"
output_dir <- "outputs/person1"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

read_csv <- function(name) {
  read.csv(file.path(data_dir, name), fileEncoding = "UTF-8-BOM",
           na.strings = c("", "NA"), check.names = FALSE)
}

# The field sets below keep the analysis dataset small and make its contents clear.
person <- read_csv("person.csv")[, c("ACCIDENT_NO", "PERSON_ID", "VEHICLE_ID",
  "AGE_GROUP", "INJ_LEVEL_DESC", "ROAD_USER_TYPE_DESC")]
vehicle <- read_csv("vehicle.csv")[, c("ACCIDENT_NO", "VEHICLE_ID", "VEHICLE_TYPE_DESC")]
accident <- read_csv("accident.csv")[, c("ACCIDENT_NO", "ACCIDENT_DATE",
  "ACCIDENT_TYPE_DESC", "SPEED_ZONE")]
weather_raw <- read_csv("atmospheric_cond.csv")[, c("ACCIDENT_NO",
  "ATMOSPH_COND_SEQ", "ATMOSPH_COND_DESC")]
surface_raw <- read_csv("road_surface_cond.csv")[, c("ACCIDENT_NO",
  "SURFACE_COND_SEQ", "SURFACE_COND_DESC")]

# A person record is unique by crash and person in the supplied file.
stopifnot(!anyDuplicated(person[c("ACCIDENT_NO", "PERSON_ID")]))
stopifnot(!anyDuplicated(vehicle[c("ACCIDENT_NO", "VEHICLE_ID")]))
stopifnot(!anyDuplicated(accident["ACCIDENT_NO"]))

# Weather/surface files can contain multiple observations per crash. Prefer
# sequence 1 as the primary observation, falling back to sequence 0 only when
# sequence 1 is absent. This gives at most one row per crash for each table.
primary_by_crash <- function(x, seq_col, value_col) {
  x <- x[order(x$ACCIDENT_NO, x[[seq_col]] != 1, x[[seq_col]] != 0,
               x[[seq_col]], na.last = TRUE), ]
  x <- x[!duplicated(x$ACCIDENT_NO), c("ACCIDENT_NO", value_col)]
  names(x)[2] <- value_col
  x
}
weather <- primary_by_crash(weather_raw, "ATMOSPH_COND_SEQ", "ATMOSPH_COND_DESC")
surface <- primary_by_crash(surface_raw, "SURFACE_COND_SEQ", "SURFACE_COND_DESC")

# Left joins retain every person row for the join audit. Vehicle-linked records
# form the analysis population because the research question compares vehicles.
joined <- merge(person, vehicle, by = c("ACCIDENT_NO", "VEHICLE_ID"),
                all.x = TRUE, sort = FALSE)
joined <- merge(joined, accident, by = "ACCIDENT_NO", all.x = TRUE, sort = FALSE)
joined <- merge(joined, weather, by = "ACCIDENT_NO", all.x = TRUE, sort = FALSE)
joined <- merge(joined, surface, by = "ACCIDENT_NO", all.x = TRUE, sort = FALSE)

# Broad vehicle groups, with source labels retained in the dataset. Small and
# unusual vehicle types are grouped as Other; unknown/not-applicable stay Unknown.
vehicle_group <- function(x) {
  out <- rep("Other", length(x))
  out[is.na(x) | x %in% c("Not Known", "Not Applicable")] <- "Unknown"
  out[x %in% c("Car", "Station Wagon", "Utility", "Panel Van", "Taxi",
               "Light Commercial Vehicle (Rigid) <= 4.5 Tonnes GVM")] <- "Car/light vehicle"
  out[x %in% c("Motor Cycle", "Motor Scooter", "Moped")] <- "Motorcycle"
  out[x %in% c("Heavy Vehicle (Rigid) > 4.5 Tonnes", "Rigid Truck(Weight Unknown)",
               "Prime Mover (No of Trailers Unknown)", "Prime Mover - Single Trailer",
               "Prime Mover B-Double", "Prime Mover B-Triple", "Prime Mover Only")] <- "Truck/heavy vehicle"
  out[x %in% c("Bus/Coach", "Mini Bus(9-13 seats)")] <- "Bus"
  out
}

joined$vehicle_group <- vehicle_group(joined$VEHICLE_TYPE_DESC)
joined$serious_fatal <- ifelse(joined$INJ_LEVEL_DESC %in% c("Serious injury", "Fatality"),
                               1L,
                               ifelse(joined$INJ_LEVEL_DESC %in% c("Other injury", "Not injured"), 0L, NA_integer_))
joined$serious_fatal_label <- factor(joined$serious_fatal, levels = c(0, 1),
                                     labels = c("Lower severity", "Serious/fatal"))

# Audit counts are written before filtering so dropped/unmatched records are visible.
audit <- data.frame(
  measure = c("Person source rows", "Joined person rows", "Rows without matched vehicle",
              "Rows without matched accident", "Rows with unknown vehicle group",
              "Rows with unclassified injury outcome", "Analysis rows (matched vehicle, known group, classified outcome)"),
  n = c(nrow(person), nrow(joined), sum(is.na(joined$VEHICLE_TYPE_DESC)),
        sum(is.na(joined$ACCIDENT_DATE)), sum(joined$vehicle_group == "Unknown"),
        sum(is.na(joined$serious_fatal)),
        sum(!is.na(joined$VEHICLE_TYPE_DESC) & joined$vehicle_group != "Unknown" &
              !is.na(joined$serious_fatal)))
)
write.csv(audit, file.path(output_dir, "join_and_exclusion_audit.csv"), row.names = FALSE)

analysis <- joined[!is.na(joined$VEHICLE_TYPE_DESC) & joined$vehicle_group != "Unknown" &
                     !is.na(joined$serious_fatal), ]
analysis$vehicle_group <- factor(analysis$vehicle_group,
  levels = c("Car/light vehicle", "Motorcycle", "Truck/heavy vehicle", "Bus", "Other"))
analysis$AGE_GROUP[is.na(analysis$AGE_GROUP)] <- "Unknown"
analysis$SPEED_ZONE <- as.character(analysis$SPEED_ZONE)
analysis$SPEED_ZONE[is.na(analysis$SPEED_ZONE)] <- "Unknown"
analysis$ACCIDENT_TYPE_DESC[is.na(analysis$ACCIDENT_TYPE_DESC)] <- "Unknown"
analysis$ATMOSPH_COND_DESC[is.na(analysis$ATMOSPH_COND_DESC)] <- "Unknown"
analysis$SURFACE_COND_DESC[is.na(analysis$SURFACE_COND_DESC)] <- "Unknown"

write.csv(analysis, file.path(output_dir, "analysis_person_level.csv"), row.names = FALSE)

# Descriptive table uses all included person records, not unique crashes or vehicles.
group_summary <- aggregate(serious_fatal ~ vehicle_group, data = analysis,
                           FUN = function(x) c(n = length(x), events = sum(x), pct = 100 * mean(x)))
summary_table <- data.frame(
  vehicle_group = group_summary$vehicle_group,
  people_n = group_summary$serious_fatal[, "n"],
  serious_fatal_n = group_summary$serious_fatal[, "events"],
  serious_fatal_pct = group_summary$serious_fatal[, "pct"]
)
write.csv(summary_table, file.path(output_dir, "injury_by_vehicle_group.csv"), row.names = FALSE)

zone_table <- aggregate(serious_fatal ~ vehicle_group + SPEED_ZONE, data = analysis,
                        FUN = function(x) c(n = length(x), events = sum(x), pct = 100 * mean(x)))
zone_summary <- data.frame(
  vehicle_group = zone_table$vehicle_group,
  speed_zone = zone_table$SPEED_ZONE,
  people_n = zone_table$serious_fatal[, "n"],
  serious_fatal_n = zone_table$serious_fatal[, "events"],
  serious_fatal_pct = zone_table$serious_fatal[, "pct"]
)
write.csv(zone_summary, file.path(output_dir, "injury_by_vehicle_and_speed_zone.csv"), row.names = FALSE)

png(file.path(output_dir, "injury_by_vehicle_group.png"), width = 1200, height = 800, res = 150)
barplot(summary_table$serious_fatal_pct, names.arg = summary_table$vehicle_group,
        las = 2, ylab = "People with serious/fatal injury (%)",
        xlab = "Vehicle group", main = "Serious/fatal injury percentage by vehicle group",
        col = "steelblue", ylim = c(0, max(summary_table$serious_fatal_pct) * 1.15))
text(seq_len(nrow(summary_table)), summary_table$serious_fatal_pct,
     labels = paste0("n=", summary_table$people_n), pos = 3, cex = 0.75)
dev.off()

cat("Prepared", nrow(analysis), "person-level records. Outputs:", output_dir, "\n")
