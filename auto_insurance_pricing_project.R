# AUTO INSURANCE PRICING PROJECT
# In RStudio: File > Open File > choose this file, then click Source.
# It creates the practice data, summary table, and chart in your working directory.

set.seed(2026)

n <- 3000
age_group <- sample(c("Young", "Adult", "Senior"), n, replace = TRUE, prob = c(0.25, 0.60, 0.15))
territory <- sample(c("Urban", "Suburban", "Rural"), n, replace = TRUE, prob = c(0.35, 0.45, 0.20))
vehicle_type <- sample(c("Sedan", "SUV"), n, replace = TRUE, prob = c(0.55, 0.45))
exposure <- round(runif(n, 0.70, 1.00), 2)

age_factor <- c(Young = 1.45, Adult = 1.00, Senior = 1.20)[age_group]
territory_factor <- c(Urban = 1.35, Suburban = 1.00, Rural = 0.80)[territory]
vehicle_freq_factor <- c(Sedan = 1.00, SUV = 1.10)[vehicle_type]
claim_count <- rpois(n, 0.11 * age_factor * territory_factor * vehicle_freq_factor * exposure)

vehicle_sev_factor <- c(Sedan = 1.00, SUV = 1.25)[vehicle_type]
territory_sev_factor <- c(Urban = 1.12, Suburban = 1.00, Rural = 0.92)[territory]
mean_severity <- 3600 * vehicle_sev_factor * territory_sev_factor
claim_amount <- ifelse(claim_count == 0, 0,
                       claim_count * rgamma(n, shape = 2.2, scale = mean_severity / 2.2))

auto <- data.frame(policy_id = sprintf("P%04d", 1:n), age_group, territory,
                   vehicle_type, exposure, claim_count, claim_amount = round(claim_amount, 2))
write.csv(auto, "auto_policy_data.csv", row.names = FALSE)

pricing_summary <- function(data, rating_variable) {
  group <- data[[rating_variable]]
  exposure <- tapply(data$exposure, group, sum)
  claims <- tapply(data$claim_count, group, sum)
  losses <- tapply(data$claim_amount, group, sum)
  result <- data.frame(
    rating_variable = rating_variable, group = names(exposure),
    exposure = round(as.numeric(exposure), 2), claims = as.numeric(claims),
    losses = round(as.numeric(losses), 2),
    claim_frequency = round(as.numeric(claims / exposure), 4),
    claim_severity = round(as.numeric(losses / claims), 2),
    pure_premium = round(as.numeric(losses / exposure), 2)
  )
  result$indicated_premium <- round(result$pure_premium / 0.70, 2)
  result
}

by_age <- pricing_summary(auto, "age_group")
by_territory <- pricing_summary(auto, "territory")
by_vehicle <- pricing_summary(auto, "vehicle_type")
summary_table <- rbind(by_age, by_territory, by_vehicle)
write.csv(summary_table, "pricing_summary.csv", row.names = FALSE)
print(summary_table)

portfolio_frequency <- sum(auto$claim_count) / sum(auto$exposure)
png("claim_frequency.png", width = 1000, height = 650)
barplot(by_age$claim_frequency, names.arg = by_age$group,
        col = c("#8ecae6", "#219ebc", "#023047"),
        main = "Claim Frequency by Age Group", ylab = "Claims per Policy Year",
        ylim = c(0, max(by_age$claim_frequency) * 1.2))
abline(h = portfolio_frequency, lty = 2, lwd = 2)
legend("topright", legend = "Portfolio average", lty = 2, bty = "n")
dev.off()

cat("\nDone! Look in your working directory for:\n")
cat("- auto_policy_data.csv\n- pricing_summary.csv\n- claim_frequency.png\n")
