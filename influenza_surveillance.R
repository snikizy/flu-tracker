

#  Influenza outbreak surveillance & simulation (MVP)
# Nairobi sub-counties: Westlands, Kibra, Dagoretti
#
# Purpose
#   1. Simulate 90 days of influenza case reporting (July–September 2026).
#   2. Compute positivity rate and 7-day rolling incidence.
#   3. Print epidemiological summaries by sub-county.
#   4. Draw an epidemic curve (daily cases + 7-day moving average).
#   5. Export the line list and the epicurve image next to this script.
#
# Runtime
#   CRAN: tidyverse, ggplot2
#   Install once if needed:
#     install.packages(c("tidyverse", "ggplot2"))
#   Run:
#     Rscript influenza_surveillance.R
#
# Author: Data Analyst Intern Candidate
# Date:   14 October 2026


# 0. Libraries and session

suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
})

set.seed(42)

# Write outputs next to this file when launched with Rscript.
script_dir <- tryCatch(
  {
    cmd_args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("^--file=", cmd_args, value = TRUE)
    if (length(file_arg) == 1L) {
      dirname(normalizePath(sub("^--file=", "", file_arg)))
    } else if (!is.null(sys.frame(1)$ofile)) {
      dirname(normalizePath(sys.frame(1)$ofile))
    } else {
      getwd()
    }
  },
  error = function(e) getwd()
)



# 1. Generate synthetic surveillance data

# 90 days of influenza monitoring across 3 Nairobi sub-counties.
# Incidence follows a Gaussian epi-curve (peak day 45, SD 15 days) plus
# Poisson reporting noise.

generate_flu_data <- function(days = 90) {
  sub_counties <- c("Westlands", "Kibra", "Dagoretti")
  peak_day <- 45
  sigma <- 15

  data_list <- lapply(sub_counties, function(sub_county) {
    timeline <- seq_len(days)
    growth_factor <- exp(-((timeline - peak_day)^2) / (2 * sigma^2))
    base_cases <- 5 + (growth_factor * 40)
    daily_cases <- rpois(days, lambda = base_cases)

    data.frame(
      Date = seq(from = as.Date("2026-07-01"), by = "day", length.out = days),
      Day = timeline,
      SubCounty = sub_county,
      NewCases = daily_cases,
      TestsConducted = rpois(days, lambda = 150),
      stringsAsFactors = FALSE
    )
  })

  master_data <- bind_rows(data_list)

  master_data %>%
    mutate(PositivityRate = round((NewCases / TestsConducted) * 100, 2)) %>%
    group_by(SubCounty) %>%
    mutate(
      RollingAvg7Day = round(
        as.numeric(stats::filter(NewCases, rep(1 / 7, 7), sides = 1)),
        1
      )
    ) %>%
    ungroup()
}

flu_surveillance_data <- generate_flu_data(90)

cat("--- Structured Public Health Surveillance Data Preview ---\n")
print(head(flu_surveillance_data, 10))


# 2. Statistical analysis & epidemiological metrics

county_summary <- flu_surveillance_data %>%
  group_by(SubCounty) %>%
  summarise(
    Total_Cases = sum(NewCases),
    Max_Daily_Cases = max(NewCases),
    Avg_Daily_Positivity = mean(PositivityRate, na.rm = TRUE),
    Peak_7Day_Avg = max(RollingAvg7Day, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n--- Epidemiological Summary by Sub-County ---\n")
print(county_summary)



# 3. Epidemic curve

epicurve_plot <- ggplot(
  flu_surveillance_data,
  aes(x = Date, y = NewCases, color = SubCounty)
) +
  geom_line(alpha = 0.4, linewidth = 0.8) +
  geom_line(aes(y = RollingAvg7Day), linewidth = 1.2, na.rm = TRUE) +
  theme_minimal() +
  labs(
    title = "Influenza Epidemic Curve (Epicurve) & 7-Day Moving Average",
    subtitle = "Health Surveillance Simulation for Nairobi Sub-Counties (Winter/Flu Season 2026)",
    x = "Timeline",
    y = "Number of New Daily Cases",
    color = "Region Monitoring"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "bottom"
  )

png_path <- file.path(script_dir, "influenza_epicurve_2026.png")
csv_path <- file.path(script_dir, "influenza_surveillance_2026.csv")
summary_path <- file.path(script_dir, "influenza_county_summary_2026.csv")

write.csv(flu_surveillance_data, csv_path, row.names = FALSE)
write.csv(county_summary, summary_path, row.names = FALSE)

# Use ragg so Rscript on Windows does not hang on a GUI graphics device.
ragg::agg_png(png_path, width = 10, height = 6, units = "in", res = 150, background = "white")
print(epicurve_plot)
grDevices::dev.off()

if (interactive()) {
  print(epicurve_plot)
}

cat("\nSystem Success: Epidemiological visualization saved as '",
    basename(png_path), "'\n", sep = "")
cat("Line list exported to '", basename(csv_path), "'\n", sep = "")
cat("Sub-county summary exported to '", basename(summary_path), "'\n", sep = "")
