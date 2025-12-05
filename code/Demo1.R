setwd("~/Desktop/uni/Fall Sem 2025-2026/ECO4203/Group Project/")

library(readxl)
library(dplyr)
library(plm)
library(ggplot2)
library(car)

# Load data
data <- read.csv("conjunto_empleo_imss_2015_2022.csv")

# Rename columns
data <- data %>%
  rename(
    entity = cve_entidad,
    state = entidad,
    month = mes,
    sector = grupo,
    sex = sexo,
    age_range = rango_edad,
    salary_range = rango_salarial,
    workers = trabajadores,
    permanents = permanentes,
    eventual = eventuales,
    date = fecha
  )

# Factor conversion
data <- data %>%
  mutate(
    year = as.factor(year),
    month = as.factor(month),
    sector = as.factor(sector),
    sex = as.factor(sex),
    age_range = as.factor(age_range),
    salary_range = as.factor(salary_range),
    state = as.factor(state)
  )

# Aggregate data for unique panel ids
agg_data <- data %>%
  group_by(sector, year, sex, age_range, salary_range) %>%
  summarise(
    workers = sum(workers),
    .groups = 'drop'
  )

# EDA - average and total workers by sector
sector_summary <- agg_data %>%
  group_by(sector) %>%
  summarise(
    avg_workers = mean(workers),
    total_workers = sum(workers),
    .groups = 'drop'
  )
print(sector_summary)

# Plot - avg workers by year and salary range
plot_data <- agg_data %>%
  group_by(year, salary_range) %>%
  summarise(avg_workers = mean(workers), .groups = 'drop')

ggplot(plot_data, aes(x = as.numeric(as.character(year)), y = avg_workers, 
                      color = salary_range, group = salary_range)) +
  geom_line(linewidth = 1) +
  labs(title = "Average Workers by Year and Salary Range",
       x = "Year", y = "Average Number of Workers") +
  theme_minimal()

# Multicollinearity check
agg_data$salary_range_num <- as.numeric(agg_data$salary_range)
agg_data$age_range_num <- as.numeric(agg_data$age_range)

model_vif <- lm(workers ~ salary_range_num + age_range_num, data = agg_data)
vif_values <- vif(model_vif)
print(vif_values)

# Prepare pdata.frame for panel regression
pdata <- pdata.frame(agg_data, index = c("sector", "year"))

# Panel fixed effects regression
model <- plm(workers ~ salary_range + sex + age_range,
             data = pdata,
             model = "within")

summary(model)

# Residual plot - base R safe plotting
res <- as.numeric(residuals(model))
index <- seq_along(res)

plot(index, res, type = "p",
     main = "Residuals Plot",
     ylab = "Residuals",
     xlab = "Index")
abline(h = 0, col = "red")

# Residual plot with ggplot2 (optional)
ggplot(data.frame(index = index, residuals = res), aes(x = index, y = residuals)) +
  geom_point(alpha = 0.5) +
  geom_hline(yintercept = 0, color = "red") +
  labs(title = "Residuals Plot", x = "Index", y = "Residuals") +
  theme_minimal()

# Interaction model: salary range and sex
model_interaction <- plm(workers ~ salary_range * sex + age_range,
                         data = pdata,
                         model = "within")
summary(model_interaction)

# Export sector summary and cleaned data
write.csv(sector_summary, "sector_workers_summary.csv")
write.csv(agg_data, "cleaned_imss_data.csv")
