# Set the workdirectory 

setwd("/Users/mmoheebahmed/Desktop/UCD MA/MA Thesis/Data/R_EN/soepdata")
setwd("/Users/mmoheebahmed/Desktop/UCD MA/MA Thesis/Data") #final_panel & combined_panel
setwd("/Users/mmoheebahmed/Desktop/UCD MA/MA Thesis/Data/R_EN/soepdata/raw")
setwd("/Users/mmoheebahmed/Desktop/UCD MA/MA Thesis/Data/Final Useed Datasets")
getwd()

# Required libraries

library(rio)
library(tidyverse)
library(haven)
library(purrr)
library(dplyr)
library(writexl)
library(ggplot2)
library(tidyr)
library(readr)


# --- Load the file cleanly ---
path <- "asylum applications.csv"

# Read raw with semicolon; no headers
raw <- read_delim(path, delim = ";", col_names = FALSE, trim_ws = TRUE,
                  locale = locale(encoding = "UTF-8"), show_col_types = FALSE)

# Find the row that contains "Year" in the first column
hdr_row <- which(raw[[1]] == "Year")[1]
stopifnot(!is.na(hdr_row))  # fail early if not found

# Keep rows AFTER the header and only the first two columns
# Keep rows AFTER the "Year" header and only the first two columns
refugees_bamf <- raw %>%
  slice((hdr_row + 1):n()) %>%
  select(1:2) %>%                 # keep Year and Count columns only
  rename(year = 1, applications = 2) %>%  # rename by position
  mutate(
    year = suppressWarnings(as.integer(year)),
    applications = readr::parse_number(
      applications,
      locale = locale(grouping_mark = ".", decimal_mark = ",")  # "117.648" -> 117648
    )
  ) %>%
  filter(!is.na(year), !is.na(applications))


# Quick check
head(refugees_bamf)


# Helper for thousand separator formatting
format_number <- function(x) format(x, big.mark = ".", scientific = FALSE)

# Plot
bamf <- refugees_bamf %>%
  filter(year >= 2010) %>%
  ggplot(aes(x = factor(year), y = applications)) +
  geom_col(fill = "#007BFF", width = 0.7) +
  geom_text(aes(label = format_number(applications)),
            vjust = -0.5, size = 3, color = "#333333") +
  scale_y_continuous(labels = format_number, expand = expansion(mult = c(0, 0.1))) +
  labs(
    x = NULL,
    y = "Number of Asylum Applications",
    title = "Figure 1. Registered Asylum Applications in Germany (2010-2024)",
    subtitle = "Total Applications (First and Subsequent)",
    caption = "Source: Federal Office for Migration and Refugees (BAMF, 2024)"
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title = element_text(size = 10, face = "bold", color = "black", margin = margin(t = 10, b = 8)),
    plot.subtitle = element_text(size = 10, face = "bold", color = "black", margin = margin(b = 12)),
    axis.title.y = element_text(color = "black", size = 10, margin = margin(r = 10)),
    axis.title.x = element_text(size = 10, color = "black", hjust = 0.5, margin = margin(t = 15)),
    axis.text.x = element_text(color = "black", size = 10, hjust = 0.5),
    plot.margin = margin(20, 30, 20, 20),
    plot.caption = element_text(size = 9, hjust = 1, margin = margin(t = 15)),
  )

ggsave("bamf.pdf", plot = bamf, height = 7, width = 10, bg = "white")


raw <- read_csv("refugees_applications_by_country.csv", col_names = FALSE, locale = locale(encoding = "UTF-8"))

file <- "refugees_applications_by_country.csv"


refugees_country <- read_delim(
  file, delim = ";", trim_ws = TRUE,
  locale = locale(encoding = "UTF-8"), na = c("", "NA")
)

# Clean possible BOM/space in first col name and force 'country'
names(refugees_country)[1] <- str_replace_all(names(refugees_country)[1], "^\uFEFF", "")
names(refugees_country)[1] <- "country"

# Make year columns numeric (only those that exist)
year_cols <- intersect(as.character(2015:2024), names(refugees_country))
refugees_country <- refugees_country %>%
  mutate(across(all_of(year_cols), ~ readr::parse_double(.)))


raw <- read_delim(file, delim = ";", col_names = FALSE,
                  trim_ws = TRUE, locale = locale(encoding = "UTF-8"))
names(raw) <- as.character(unlist(raw[1, ]))           # first row -> header
refugees_country <- raw[-1, ]
names(refugees_country)[1] <- str_replace_all(names(refugees_country)[1], "^\uFEFF", "")
names(refugees_country)[1] <- "country"

year_cols <- intersect(as.character(2015:2024), names(refugees_country))
refugees_country <- refugees_country %>%
  mutate(across(all_of(year_cols), ~ readr::parse_double(.)))

# identify year columns that actually exist
year_cols <- intersect(as.character(2015:2024), names(refugees_country))

# make year columns numeric (safe if already numeric)
refugees_country <- refugees_country %>%
  mutate(across(all_of(year_cols), ~ suppressWarnings(as.numeric(.))))


df_long <- refugees_country %>%
  pivot_longer(cols = all_of(as.character(2015:2024)),
               names_to = "year", values_to = "applications") %>%
  mutate(year = as.integer(year),
         applications = as.numeric(applications))

top3_totals <- df_long %>%
  group_by(country) %>%
  summarise(total_applications = sum(applications, na.rm = TRUE)) %>%
  arrange(desc(total_applications)) %>%
  slice_head(n = 3) %>%
  mutate(country = recode(country,
                          "Syrien" = "Syria",
                          "Afghanistan" = "Afghanistan",
                          "Irak" = "Iraq"))

format_number <- function(x) format(x, big.mark = ".", scientific = FALSE)

top <- ggplot(top3_totals, aes(x = reorder(country, -total_applications),
                               y = total_applications)) +
  geom_bar(stat = "identity", fill = "#007BFF", width = 0.3)+
  geom_text(aes(label = format_number(total_applications)),
            vjust = -0.5, size = 3, color = "#333333") +
  scale_y_continuous(labels = format_number,
                     expand = expansion(mult = c(0, 0.1))) +
  labs(
    x = NULL,
    y = "Total Asylum Applications (2015-2024)",
    title = "Figure 2. Top 3 Countries of Origin for Asylum Applications (2015-2024)",
    subtitle = "Total Number of First-Time Asylum Applications Registered in Germany",
    caption = "Source: Federal Office for Migration and Refugees (BAMF, 2024)"
  ) + theme_bw(base_size = 13) +
  theme(
    plot.title = element_text(size = 10, face = "bold", color = "black", margin = margin(t = 10, b = 8)),
    plot.subtitle = element_text(size = 10, face = "bold", color = "black", margin = margin(b = 12)),
    axis.title.y = element_text(color = "black", size = 10, margin = margin(r = 10)),
    axis.title.x = element_text(size = 10, color = "black", hjust = 0.5, margin = margin(t = 15)),
    axis.text.x = element_text(color = "black", size = 10, hjust = 0.5),
    plot.margin = margin(20, 30, 20, 20),
    plot.caption = element_text(size = 9, hjust = 1, margin = margin(t = 15)),
  )

ggsave("top_countries.pdf", top, width = 7, height = 6, dpi = 300, bg ="white")


#3 Refugee Decsisions: 

dec <- import("asylum_decisions_2015_2024.csv")
names(dec)
View(dec)

library(ggplot2)
library(tidyr)
library(dplyr)

# Sample data (replace this with your imported CSV if needed)
library(tidyverse)

# Data
asylum_data <- data.frame(
  year = 2015:2024,
  refugee_status = c(137136, 256136, 123909, 41368, 45053, 37818, 32065, 40911, 42525, 37795),
  subsidiary_protection = c(1707, 153700, 98074, 25055, 19419, 18950, 22996, 57532, 71290, 75092),
  deportation_ban = c(2072, 24084, 39659, 9548, 5857, 5702, 4787, 30020, 21462, 20823),
  rejections = c(91514, 173846, 232307, 75395, 54034, 46586, 35071, 49330, 61778, 91940),
  formal_decisions = c(50297, 87967, 109479, 65507, 59951, 36015, 55035, 50880, 64456, 75070)
)

# Reshape and calculate percentages
asylum_long <- asylum_data %>%
  pivot_longer(cols = -year, names_to = "decision_type", values_to = "count") %>%
  group_by(year) %>%
  mutate(percent = count / sum(count) * 100) %>%
  ungroup()

# Plot
dec_trend <- ggplot(asylum_long, aes(x = year, y = percent, color = decision_type)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_y_continuous(
    labels = scales::percent_format(scale = 1),
    breaks = seq(0, 100, by = 25),
    limits = c(0, 100)
  ) +
  scale_x_continuous(breaks = 2015:2024) +
  labs(
    title = "Figure 3. Trends in Asylum Decision Types in Germany (2015-2024)",
    subtitle = "Annual Share by Decision Category",
    x = "Year",
    y = "Percentage of Total Decisions",
    color = "Decision Type",
    caption = "Source: Federal Office for Migration and Refugees (BAMF, 2024)"
  ) +
  scale_color_manual(
    values = c(
      refugee_status = "#1f77b4",
      subsidiary_protection = "#ff7f0e",
      deportation_ban = "#2ca02c",
      rejections = "#d62728",
      formal_decisions = "#9467bd"
    ),
    labels = c(
      "Refugee Status (incl. Geneva Convention)",
      "Subsidiary Protection",
      "Deportation Ban",
      "Rejections",
      "Formal Decisions"
    )
  ) +
  theme_bw(base_size = 13) + 
  theme(
    plot.title = element_text(face = "bold", size = 11, hjust = 0.0,
                              margin = margin(t = 15, b = 10)),
    plot.subtitle = element_text(size = 10, face = "bold", hjust = 0.0,
                                 margin = margin(b = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 15)),
    plot.caption = element_text(size = 9, hjust = 1, margin = margin(t = 15)),
    plot.margin = margin(20, 20, 20, 20),
    legend.position = "bottom",
    legend.title = element_text(size=10),
    legend.text = element_text(size = 10),
    legend.spacing.x = unit(0.5, "cm")
  ) + 
  guides(color = guide_legend(nrow = 2, byrow = TRUE))


ggsave("dec_trend.pdf", plot = dec_trend, height = 7, width = 10, bg = "white")


# The main dataset: 
data_panel <- import("pl_2016_2018.Rdata")
dim(data_panel)

refugee_data <- data_panel %>% filter(sample1 %in% c(30, 31, 34))
unique(as_factor(refugee_data$syear))
unique(as_factor(refugee_data$inputdataset))
nrow(refugee_data)

# Identify only those who participated in the survey in 2016, 2017, and 2018 

# Count how many years each person appeared

respondents_all_three_years <- refugee_data %>%
  group_by(pid) %>%
  summarise(n_years = n_distinct(syear)) %>%
  filter(n_years == 3)  # Keep only those who participated all 3 years

# Join back to get full data for those individuals

panel <- refugee_data %>%
  semi_join(respondents_all_three_years, by = "pid")

View(panel)
dim(panel)

#  Check how many unique individuals?

n_distinct(panel$pid) # the sample size 5283, but for each year is 1761. 

# Number of rows (includes multiple years)

nrow(panel)


# All variables you listed

vars_to_extract <- c(

  "pid", "hid", "cid", "syear", "sample1", "inputdataset",
  "ple0010_h", "pla0009_h", "plh0258_v9",
  "plj0626", "plj0680_h", "plm0529", "plh0007", "plm0564", "plh0011_v2", "plh0012_h",
  "plm0663", "plm0664", "plm0665", "plm0666", "plm0667", "plm0668",
  "plm0672", "plm0673", "plm0674", "plm0675",
  "plj0606", "plj0607", "plj0608", "plj0609", "plj0610", "plj0611",
  "plj0612", "plj0613", "plj0614", "plj0615", "plj0616", "plj0617",
  "plj0618", "plj0619", "plj0620", "plj0621", "plj0622", "plj0623",
  "plj0624", "plj0625"
)

panel_subset <- panel %>% select(any_of(vars_to_extract))

colnames(panel_subset) <- paste0(colnames(panel_subset), "_panel") #to add the  part of the "_panel" to all dataset from panel

dim(panel_subset)

#____________________________________# Related Datasets #_____________________________________#

#1 BioData


biodata <- readRDS("biol.rds")

refugee_bio <- biodata %>% filter(sample1 %in% c(30, 31, 34)) %>% 
  filter(syear %in% c(2016,2017, 2018)) 

respondents_all_three_years_bio <- refugee_bio %>%
  group_by(pid) %>%
  summarise(n_years = n_distinct(syear)) %>%
  filter(n_years == 3)  # Keep only those who participated all 3 years

# Join back to get full data for those individuals

bio_panel <- refugee_bio %>%
  semi_join(respondents_all_three_years_bio, by = "pid")

#  Check how many unique individuals?
n_distinct(bio_panel$pid) # the sample size 5283, but for each year is 1761. 
nrow(bio_panel)
table(bio_panel$syear)

# Only Need the 2016 Wave

bio_panel_2016 <- bio_panel %>% filter(syear==2016)
View(bio_panel_2016)
dim(bio_panel_2016)

# List of 42 variable names

bio_vars <- c(
  #socio_economic
  "pid", "lr3078", "lr3079", "lb0228", "lr3041", "lr3046", "lr3130",
  # Negative Experiences during Escape (wave 2016)
  "lr3122", "lr3123", "lr3124", "lr3125", "lr3126", "lr3127", "lr3128", "lr3129",
  # Reasons for Leaving Home Country (wave 2016)
  "lr3136", "lr3137", "lr3138", "lr3139", "lr3140", "lr3141", "lr3142", "lr3143",
  "lr3144", "lr3145", "lr3146",
  # Political Engagement in Home Country (wave 2016. party identification)
  "lm0623", "lm0624", "lr3147", "lr3148", "lr3149", "lr3150", "lr3151", "lr3152",
  "lr3153", "lr3154", "lr3155", "lr3156", "lr3157", "lr3158", "lr3159", "lr3160",
  "lr3161", "lr3162", "lr3163", "lr3164", "lr3165", "lr3166", "lr3167"
)


# Subset and rename
bio_refugee_subset <- bio_panel_2016[, bio_vars]
colnames(bio_refugee_subset) <- paste0(colnames(bio_refugee_subset), "_bio")

dim(bio_refugee_subset)

#2 Dataset bgp: 

bgp <- readRDS("bgp.rds")
unique(as_factor(bgp$sample1))
refugee_bgp <- bgp %>% filter(sample1 %in% c(30, 31))
unique(as_factor(refugee_bgp$syear)) # only 2016
nrow(refugee_bgp) # 4465 oberservations. 

# Define variables of interest

bgp_vars <- c("pid", "bgp143")

# Subset dataset
bgp_refugee_subset <- refugee_bgp[, bgp_vars]
colnames(bgp_refugee_subset) <- paste0(colnames(bgp_refugee_subset), "_bgp")
View(bgp_refugee_subset)

#3 Dataset bip:

bip <- readRDS("bip.rds")
unique(as_factor(bip$sample1))
refugee_bip <- bip %>% filter(sample1 %in% c(30, 31, 34))
unique(as_factor(refugee_bip$syear)) # only 2018
nrow(refugee_bip) # 4376 observations.
View(refugee_bip)

# Define the variables to extract
bip_vars <- c("pid", "bip_518_q106")

# Subset the dataset
bip_refugee_subset <- refugee_bip[, bip_vars]

#Rename columns to add "_bip" suffix
colnames(bip_refugee_subset) <- paste0(colnames(bip_refugee_subset), "_bip")

View(bip_refugee_subset)

# Combine Data into One Dataset

library(dplyr)

# Step 1: Rename all pid columns to "pid"
bio_refugee_subset <- bio_refugee_subset %>% rename(pid = pid_bio)
bgp_refugee_subset <- bgp_refugee_subset %>% rename(pid = pid_bgp)
bip_refugee_subset <- bip_refugee_subset %>% rename(pid = pid_bip)
panel_subset <- panel_subset %>% rename(pid = pid_panel)

# Step 2: Merge each into the main dataset
panel_combined <- panel_subset %>%
  left_join(bgp_refugee_subset, by = "pid") %>%
  left_join(bip_refugee_subset, by = "pid") %>%
  left_join(bio_refugee_subset, by = "pid")

View(panel_combined)
dim(panel_combined)

write_xlsx(panel_combined, "panel_combined_updated(29_july).xlsx")
write.csv(panel_combined, "panel_combined_updated_29_july.csv", row.names = FALSE)

#__________________________Preparaing the Data for the Model_____________________________________#


library(rio)

data <- import("panel_combined_updated_29_july.csv")
dim(data)
View(data)


# Wave(syear_panel) 

data$wave <- data$syear_panel
table(data$wave, exclude = NULL)
data$wave <- factor(data$wave)
data$wave <- relevel(data$wave, ref = "2017")
wave_dummies <- model.matrix(~ wave, data = data)[, -1]  # drops intercept (wave2017)
colnames(wave_dummies) 
levels(data$wave)

# Violenc Experience
# updating the violence experience variables to include also refugees who got arrested during their escaping route.
data <- data %>%
  mutate(
    violence_exp = ifelse(
      lr3123_bio == 1 | lr3124_bio == 1 | lr3128_bio == 1, 1, 0
    )
  )
table(data$violence_exp, exclude = NULL)

data$violence_exp <- factor(data$violence_exp, levels = c(0, 1), labels = c("No", "Yes"))
data$violence_exp <- relevel(data$violence_exp, ref = "No")
levels(data$violence_exp)
table(data$violence_exp, exclude = NULL)  # 0 = 4059, 1 = 1224


# Violence Home

data <- data %>%
  mutate(
    violence_exp_home = ifelse( lr3138_bio == 1 | lr3139_bio == 1, 1, 0
    ))

table(data$violence_exp_home, exclude = NULL)

data$violence_exp_home <- factor(data$violence_exp_home, levels = c(0, 1), labels = c("No", "Yes"))
data$violence_exp_home <- relevel(data$violence_exp_home, ref = "No")
levels(data$violence_exp_home)
table(data$violence_exp_home, exclude = NULL)  # No = 2100, yes=3183

# Violence General (Home and On Route)

data <- data %>%
  mutate(
    violence_exp_general = ifelse(
      lr3123_bio == 1 | lr3124_bio == 1 | lr3128_bio == 1 | lr3138_bio == 1 | lr3139_bio == 1,
      1,
      0
    )
  )

table(data$violence_exp_general, exclude = NULL) # No = 1677 & 3606

data$violence_exp_general <- factor(data$violence_exp_general, levels = c(0, 1), labels = c("No", "Yes"))
data$violence_exp_general <- relevel(data$violence_exp_general, ref = "No")
levels(data$violence_exp_general)
table(data$violence_exp_general, exclude = NULL) 

# Political Interest Before

# swtiching the variable on political interest before to a dummy one

table(data$bgp143_bgp) # 93 no answer (so missing values)

# swtiching the variable for political interest to a dummy one. Very Strong the ommited one serving as the reference one.

data$political_interest_before <- data$bgp143_bgp
data$political_interest_before[data$political_interest_before == -1] <- NA  # Set missing

# Relevel to make "Not at all" the reference category
data$political_interest_before <- factor(data$bgp143_bgp,
                                       levels = c(4, 3, 2, 1),
                                       labels = c("Not at all", "Not so strong", "Strong", "Very Strong")
)

# Create dummy variables — this will omit "Not at all"
dummy_vars <- model.matrix(~ political_interest_before, data = data)[, -1]

# Check
table(data$political_interest_before, exclude = NULL)
colnames(dummy_vars) # Reference Group: Not At All
levels(data$political_interest_before)

# Political Interest After (General)

table(data$plh0007_panel, data$wave)
data$political_interest_after <- data$plh0007_panel

data$political_interest_after[data$political_interest_after == -1] <- NA

data$political_interest_after <- recode(data$political_interest_after,
                                      `1` = 4,   # Very Strong → 4
                                      `2` = 3,   # Strong → 3
                                      `3` = 2,   # Not so strong → 2
                                      `4` = 1    # Not at all → 1
)

data$political_interest_after <- factor(data$political_interest_after,
                                      levels = c(1, 2, 3, 4),
                                      labels = c("Not at all", "Not so strong", "Strong", "Very Strong"),
                                      ordered = TRUE)

data$political_interest_after <- ifelse(data$wave == 2016, 
                                            NA, 
                                            as.character(data$political_interest_after))

data$political_interest_after <- factor(data$political_interest_after,
                                            levels = c("Not at all", "Not so strong", "Strong", "Very Strong"),
                                            ordered = TRUE)


table(data$political_interest_after, data$wave)


# political interest in Germany: 

table(data$plm0564_panel, data$wave)

data$political_interest_germany <- data$plm0564_panel


data$political_interest_germany[data$political_interest_germany %in% c(-1, -8)] <- NA

data$political_interest_germany <- recode(data$political_interest_germany,
                                        `1` = 4,   # Very Strong → 4
                                        `2` = 3,   # Strong → 3
                                        `3` = 2,   # Not so strong → 2
                                        `4` = 1    # Not at all → 1
)

data$political_interest_germany <- factor(data$political_interest_germany,
                                        levels = c(1, 2, 3, 4),
                                        labels = c("Not at all", "Not so strong", "Strong", "Very Strong"),
                                        ordered = TRUE
)

table(data$political_interest_germany, data$wave)

#_____#

# economic situation: How would you rate your economic situation at that time compared to the situation of others in your country?

table(data$lr3046_bio) # financial status. Applied to everyone. Missing values are 282. 

data$lr3046_bio_clean <- data$lr3046_bio
data$lr3046_bio_clean[data$lr3046_bio_clean == -1] <- NA

data$economic_status_num <- recode(data$lr3046_bio_clean,
                                 `1` = 5,
                                 `2` = 4,
                                 `3` = 3,
                                 `4` = 2,
                                 `5` = 1)

data$economic_status <- factor(data$economic_status_num,
                             levels = c(1, 2, 3, 4, 5),
                             labels = c("far_below_avg", 
                                        "somewhat_below_avg", 
                                        "average", 
                                        "somewhat_above_avg", 
                                        "far_above_avg"))

levels(data$economic_status) # far_below_average is the reference group
table(data$economic_status, useNA = "ifany")
is.factor(data$economic_status)

# income

table(data$lr3041_bio)

data$lr3041_bio_clean <- data$lr3041_bio
data$lr3041_bio_clean[data$lr3041_bio_clean %in% c(-1, -2)] <- NA

# Step 2: Reverse the scale (1 = far_above_avg → 5 = far_below_avg)
data$income_status_num <- dplyr::recode(data$lr3041_bio_clean,
                                      `1` = 5,
                                      `2` = 4,
                                      `3` = 3,
                                      `4` = 2,
                                      `5` = 1)

# Step 3: Create ordered factor with desired reference group first
data$income_status <- factor(data$income_status_num,
                           levels = c(1, 2, 3, 4, 5),
                           labels = c("far_below_avg",
                                      "somewhat_below_avg",
                                      "average",
                                      "somewhat_above_avg",
                                      "far_above_avg"),
                           ordered = TRUE)

# Step 4: Check output
levels(data$income_status) # the reference group is the far below average
table(data$income_status, useNA = "ifany")

# school_type
table(data$lr3079_bio) # what kind of certificate did you leave the school with? (903 doesnt apply, and 39 missing values/ no answer, dont know)

data$lr3079_bio_clean <- data$lr3079_bio
# Set -1 and -2 to NA

data$lr3079_bio_clean[data$lr3079_bio_clean %in% c(-1, -2)] <- NA

data$school_cert <- factor(data$lr3079_bio_clean,
                         levels = c(1, 2, 3, 4, 5),
                         labels = c("no_qualification", 
                                    "middle_school_cert", 
                                    "practical_cert", 
                                    "general_cert", 
                                    "other_school_cert"))

data$school_cert <- relevel(data$school_cert, ref = "no_qualification")

table(data$school_cert, useNA = "ifany")
levels(data$school_cert) # reference group is no_qualifications

# higher education

table(data$lb0228_bio) # yes = 1353, No = 3879, No Answer = 51.

# Religion

table(data$plh0258_v9_panel, data$wave)

data$religion <- data$plh0258_v9_panel
table(data$religion, data$wave)

# Step 1: Extract 2016 values of religion for each pid
religion_2016 <- data %>%
  filter(wave == 2016 & religion != -5) %>%
  select(pid, religion_2016 = religion)

# Step 2: Join these values back to the full dataset
data <- data %>%
  left_join(religion_2016, by = "pid")

# Step 3: Fill in religion from 2016 where missing (including -1), but not if religion is -5
data$religion <- ifelse(data$religion %in% c(-5, NA),
                        data$religion_2016,
                        data$religion)

# Step 4: Drop helper column
data$religion_2016 <- NULL

table(data$religion, data$wave, exclude =NULL)

data$religion[data$religion == -1] <- NA  # only -1 is treated as missing

data$religion <- factor(data$religion,
                      levels = c(4, 5, 6, 7),
                      labels = c("Islamic", "Other religion", "No religion", "Christian"))

data$religion <- relevel(data$religion, ref = "No religion")

table(data$religion, useNA = "ifany")

levels(data$religion) # the first value is the reference group. The reference group is No Religion.

# Citizenship 

table(data$bip_518_q106_bip) # 9 missing values (-2)

data$citizenship <- data$bip_518_q106_bip
table(data$citizenship)

data$citizenship[data$citizenship == -2] <- NA

data$citizenship_grouped <- NA  # initialize

data$citizenship_grouped[data$citizenship == 1] <- "Afghanistan"
data$citizenship_grouped[data$citizenship == 10] <- "Iraq"
data$citizenship_grouped[data$citizenship == 19] <- "Syria"
data$citizenship_grouped[!(data$citizenship %in% c(1, 10, 19)) & !is.na(data$citizenship)] <- "Other"

data$citizenship_grouped <- factor(data$citizenship_grouped,
                                 levels = c("Iraq", "Afghanistan", "Syria", "Other"))

data$citizenship_grouped <- relevel(data$citizenship_grouped, ref = "Afghanistan")
levels(data$citizenship_grouped) # reference group is Iraq
table(data$citizenship_grouped, useNA = "ifany")

# age

table(data$ple0010_h_panel)

data$year_of_birth <- data$ple0010_h_panel

table(data$year_of_birth) # only one value was -3 which means implausabile value. So, i converted  to NA. 

data$year_of_birth[data$year_of_birth == -3] <- NA

sum(!is.na(data$year_of_birth)) # 5282 instead of 5283

# Step 1: Calculate age (assuming data is from 2016, as in IAB-BAMF-SOEP)

data <- data %>%
  mutate(age = 2016 - year_of_birth)

# Step 2: Create the new age_group variable
data <- data %>%
  mutate(age_group = case_when(
    age >= 18 & age <= 25 ~ "18–25",
    age >= 26 & age <= 34 ~ "26–34",
    age >= 35 & age <= 50 ~ "35–50",
    age > 50             ~ "51+",
    TRUE ~ NA_character_  # catch missing or invalid values
  ))

table(data$age_group, useNA = "ifany")


# sex

table(data$pla0009_h_panel, useNA = "ifany")

data$sex <- data$pla0009_h_panel

table(data$sex) # no missing values. 3441 Male, 1842 Female

data$sex <- factor(data$sex,
                 levels = c(1, 2),
                 labels = c("Male", "Female"))

data$sex <- relevel(data$sex, ref = "Female")

levels(data$sex) # reference group female
table(data$sex, useNA= "ifany")

# Residence Permit: 


table(data$plj0680_h_panel, data$wave)

data$residence_permit <- data$plj0680_h_panel

table(data$residence_permit, data$wave, exclude = NULL)

data$residence_permit[data$residence_permit %in% c(-2, -1)] <- NA
table(data$residence_permit, data$wave, exclude = NULL)


data$residence_permit_labeled <- dplyr::case_when(
  data$residence_permit == 1 ~ "Asylum Seeker (Still in Process)",
  data$residence_permit == 2 ~ "Recognized Asylum (§25(1), German Constitutional Asylum)",
  data$residence_permit == 3 ~ "Geneva Refugee/Subsidiary Protection (§25(2))",
  data$residence_permit == 4 ~ "Permanent Residence",
  data$residence_permit == 5 ~ "Tolerated Stay (Duldung)",
  data$residence_permit == 6 ~ "Humanitarian Protection (§22/23, Program-based)",
  data$residence_permit == 7 ~ "Other Humanitarian Protection (§23a/§25(3–5), Individual/Hardship)",
  TRUE ~ NA_character_  # catch anything unexpected or missing
)

data$residence_permit_labeled <- factor(
  data$residence_permit_labeled,
  levels = c(
    "Asylum Seeker (Still in Process)",
    "Recognized Asylum (§25(1), German Constitutional Asylum)",
    "Geneva Refugee/Subsidiary Protection (§25(2))",
    "Humanitarian Protection (§22/23, Program-based)",
    "Other Humanitarian Protection (§23a/§25(3–5), Individual/Hardship)",
    "Tolerated Stay (Duldung)",
    "Permanent Residence"
  )
)

data$residence_permit_labeled<- relevel(data$residence_permit_labeled, ref = "Permanent Residence")


levels(data$residence_permit_labeled)  # Permanent Residence is the reference group
table(data$residence_permit_labeled, useNA = "ifany")
table(data$residence_permit_labeled, data$wave, useNA = "ifany")


# German Language

table(data$plm0529_panel, data$wave)

# Step 1: Replace -1 and -8 with NA
data$plm0529_panel[data$plm0529_panel %in% c(-1, -8)] <- NA

# Step 2: Recode as factor with meaningful labels
data$language_certificate <- factor(
  data$plm0529_panel,
  levels = c(1, 2, 3, 4, 5, 6, 7),  # ensure ordered
  labels = c(
    "A1 (Beginner)",
    "A2 (Elementary)",
    "B1 (Intermediate)",
    "B2 (Upper-Intermediate)",
    "C1 (Advanced)",
    "C2 (Proficient)",
    "No Certificate"
  )
)


data$language_certificate <- relevel(data$language_certificate, ref = "C2 (Proficient)")

table(data$language_certificate, data$wave)
levels(data$language_certificate) # C2.


# Arrivale Date in Germany: 

table(data$lr3130_bio)

# Arrival Year with Droping all Years before 2013
data$arrival_year <- data$lr3130_bio
  
data$arrival_year[data$arrival_year %in% c(-1, 1995:2012)] <- NA

data$arrival_year <- factor(data$arrival_year)
data$arrival_year <- relevel(data$arrival_year, ref = "2013")

table(data$arrival_year, data$wave, exclude = NULL)
table(data$arrival_year,exclude = NULL)
levels(data$arrival_year) # 2013 is the reference group 


#Arrival Periods: 

data$arrival_period <- data$lr3130_bio
table(data$arrival_period)

# Step 1: Replace -1 with NA
data$arrival_period[data$arrival_period == -1] <- NA

# Step 2: Create a new labeled variable
data$arrival_period_grouped <- dplyr::case_when(
  data$arrival_period %in% 1995:2010 ~ "Before 2011",
  !is.na(data$arrival_period)       ~ as.character(data$arrival_period),
  TRUE                               ~ NA_character_  # Preserve NAs
)

# Step 3: Convert to factor (optional, for ordering/analysis)
data$arrival_period_grouped <- factor(data$arrival_period_grouped,
                                      levels = c("Before 2011", "2011", "2012", "2013", "2014", "2015", "2016"))

data$arrival_period_grouped <- factor(data$arrival_period_grouped)
data$arrival_period_grouped <- relevel(data$arrival_period_grouped, ref = "Before 2011")
table(data$arrival_period_grouped)
levels(data$arrival_period_grouped)

# Arrival_Year_Linear: 

data$arrival_year_linear <- data$lr3130_bio

data$arrival_year_linear[data$arrival_year_linear %in% c(-1)] <- NA

table(data$arrival_year_linear, exclude=NULL)

#saving the updated dataset: 

#as csv & excel

write.csv(data, file = "data_12.08.csv", row.names = FALSE)
write_xlsx(data, "data_12.08.xlsx")

# to export the file with NAs
data_export <- data
data_export[is.na(data_export)] <- "NA"
write_xlsx(data_export, "data_with_NA_labels.xlsx")



# The regression model (only violence_experience & political interest in general_after)

# 1. Load required package
library(MASS)
table(data$violence_exp_general)

# 3. Fit the ordinal logistic regression model (only violence and political interest after)

model1 <- polr(political_interest_after ~ violence_exp_general + political_interest_before + wave, data = data, Hess = TRUE)

# 4. View model summary
summary(model1)

# 5. Add p-values manually
ctable <- coef(summary(model1))
p_values <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
results <- cbind(ctable, "p value" = p_values)
print(results)

library(stargazer)
coefs <- ctable[, "Value"]
se <- ctable[, "Std. Error"]
stargazer(model1,
          coef = list(coefs),
          se = list(se),
          p = list(p_values),
          title = "Ordinal Logistic Regression: Political Interest ~ Violence Exposure",
          dep.var.labels = "Political Interest (Ordered)",
          star.cutoffs = c(0.05, 0.01, 0.001),
          digits = 3,
          align = TRUE,
          type = "text", 
          out = "model1_general_violence.txt")



# second model including wave, sex, and year of birth as controls

model2 <- polr(political_interest_after ~ violence_exp_general + political_interest_before + wave +
                 sex + year_of_birth, data = data, Hess = TRUE)


# 3. View summary
summary(model2)

# 4. Add p-values manually
ctable <- coef(summary(model2))
p_values <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
results <- cbind(ctable, "p value" = p_values)
print(results)

# 1. Get model summary
summary2 <- summary(model2)
ctable2 <- coef(summary2)

# 2. Compute p-values
p_values2 <- pnorm(abs(ctable2[, "t value"]), lower.tail = FALSE) * 2

# 3. Extract coefficients and standard errors
coefs2 <- ctable2[, "Value"]
se2 <- ctable2[, "Std. Error"]

# 4. Create stargazer table
stargazer(model2, type = "text",
          coef = list(coefs2),
          se = list(se2),
          p = list(p_values2),
          title = "Ordinal Logistic Regression: Political Interest After ~ Violence Exposure + Wave + Sex + Year of Birth",
          dep.var.labels = "Political Interest After (Ordered)",
          star.cutoffs = c(0.05, 0.01, 0.001),
          digits = 3,
          align = TRUE,
          out = "model2_violence_general.txt")



# model 3

model3 <- polr(political_interest_after ~ violence_exp_general + wave + political_interest_before + sex +
                 year_of_birth + citizenship_grouped + religion, data = data, Hess = TRUE)

# Extract coefficient summary
summary3 <- summary(model3)
ctable3 <- coef(summary3)

# Calculate p-values
p_values3 <- pnorm(abs(ctable3[, "t value"]), lower.tail = FALSE) * 2
coefs3 <- ctable3[, "Value"]
se3 <- ctable3[, "Std. Error"]

stargazer(model3, type = "text",
          coef = list(coefs3),
          se = list(se3),
          p = list(p_values3),
          title = "Model 3_updated: Ordinal Logistic Regression of Political Interest After",
          dep.var.labels = "Political Interest After (Ordered)",
          star.cutoffs = c(0.05, 0.01, 0.001),
          digits = 3,
          align = TRUE, 
          out = "model3_violence_general.txt")


# model 4: 

model4 <- polr(political_interest_after ~ violence_exp_general*citizenship_grouped + 
                 political_interest_before + wave +
                 sex + year_of_birth + religion + 
                 economic_status +  school_cert, 
               data = data, Hess = TRUE)

# Extract model summary
summary4 <- summary(model4)
ctable4 <- coef(summary4)

# Calculate p-values manually
p_values4 <- pnorm(abs(ctable4[, "t value"]), lower.tail = FALSE) * 2
coefs4 <- ctable4[, "Value"]
se4 <- ctable4[, "Std. Error"]

# Stargazer output
stargazer(model4, type = "text",
          coef = list(coefs4),
          se = list(se4),
          p = list(p_values4),
          title = "Model 4: Ordinal Logistic Regression of Political Interest After",
          dep.var.labels = "Political Interest After (Ordered)",
          star.cutoffs = c(0.05, 0.01, 0.001),
          digits = 3,
          align = TRUE,
          out = "model4_violence_general.txt")

#model5

model5 <- polr(political_interest_after ~ violence_exp_general*citizenship_grouped + 
                 political_interest_before + wave +
                 sex + year_of_birth + religion + 
                 economic_status +  school_cert + language_certificate + residence_permit_labeled, 
               data = data, Hess = TRUE)


# Extract model summary
summary5 <- summary(model5)
ctable5 <- coef(summary5)

# Calculate p-values manually
p_values5 <- pnorm(abs(ctable5[, "t value"]), lower.tail = FALSE) * 2
coefs5 <- ctable5[, "Value"]
se5 <- ctable5[, "Std. Error"]

# Stargazer output
stargazer(model5, type = "text",
          coef = list(coefs5),
          se = list(se5),
          p = list(p_values5),
          title = "Model 5: Ordinal Logistic Regression of Political Interest After",
          dep.var.labels = "Political Interest After (Ordered)",
          star.cutoffs = c(0.05, 0.01, 0.001),
          digits = 3,
          align = TRUE,
          out = "model5_violence_general.txt")


# Combine all 5 models: 


library(stargazer)

stargazer(model1, model2, model3, model4, model5,
          type = "html",                    # or "html" / "latex"
          dep.var.labels = "Political Interest After (Ordered)",
          star.cutoffs = c(0.05, 0.01, 0.001),
          digits = 3,
          align = TRUE,
          out = "models_violence_general_final.html")

library(sjPlot)
tab_model(
  model1, model2, model3, model4, model5,
  show.p = TRUE,
  show.ci = FALSE,
  title = "Ordinal Logistic Regression Models: Political Interest in General",
  file = "model_results_general.doc"  # Save as Word
)


# Ploting Interaction Term and Confidence Interval at 95%  for Model 5

mf <- model.frame(model5)                # exact data used by the model
with(mf, table(violence_exp_general, citizenship_grouped))

# drop unused factor levels in the model frame
mf2 <- mf
for(nm in names(mf2)){
  if(is.factor(mf2[[nm]])) mf2[[nm]] <- droplevels(mf2[[nm]])
}

# refit on the cleaned frame
library(MASS)
model5_clean <- polr(formula(model5), data = mf2, Hess = TRUE)

library(effects)
ie <- Effect(c("violence_exp_general","citizenship_grouped"), model5_clean)
plot(ie)  # base R plot

ie_df <- as.data.frame(ie)

ie_long <- ie_df %>%
  tidyr::pivot_longer(
    cols = matches("^(prob|L\\.prob|U\\.prob)\\."),
    names_to   = c("stat","interest_level_raw"),
    names_pattern = "^(prob|L\\.prob|U\\.prob)\\.(.*)$",
    values_to  = "value"
  ) %>%
  dplyr::mutate(
    interest_level = gsub("\\.", " ", interest_level_raw)
  ) %>%
  dplyr::select(-interest_level_raw) %>%
  tidyr::pivot_wider(names_from = stat, values_from = value) %>%
  dplyr::rename(pred_prob = prob, lower = `L.prob`, upper = `U.prob`) %>%
  dplyr::mutate(
    violence_exp_general = factor(violence_exp_general, levels = c("No","Yes")),
    interest_level = factor(interest_level,
                            levels = c("Not at all","Not so strong","Strong","Very Strong")),
    citizenship_grouped = factor(citizenship_grouped,
                                 levels = c("Afghanistan","Iraq","Syria","Other"))
  )

## =========================================================
## 4) Compute combined violence effect (log-odds & OR) BY GROUP automatically
##    (main effect of violence for the reference citizenship + interaction term)
## =========================================================
coefs <- coef(model5_clean)  # slope coefficients (thresholds are in model5_clean$zeta)

# identify the "Yes" level of violence (reference for violence is assumed "No")
lev_vio <- levels(mf2$violence_exp_general)
stopifnot("Yes" %in% lev_vio)  # ensure it exists

# main effect name typically like "violence_exp_generalYes"
main_vio_name <- grep("^violence_exp_general", names(coefs), value = TRUE)
# if multiple (rare), pick the one ending in the Yes level
main_vio_name <- main_vio_name[grepl(paste0("(", paste0(lev_vio[-1], collapse="|"), ")$"),
                                     main_vio_name)]
b_main <- unname(coefs[main_vio_name])

# citizenship levels and reference
lev_cit <- levels(mf2$citizenship_grouped)
ref_cit <- lev_cit[1]  # polr uses first level as reference; you set Afghanistan first above

# build a data frame of combined effects

library(purrr)

effects_summary <- tibble(citizenship_grouped = lev_cit) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    inter_pattern1 = paste0("^violence_exp_general.*:citizenship_grouped", citizenship_grouped, "$"),
    inter_pattern2 = paste0("^citizenship_grouped", citizenship_grouped, ":violence_exp_general.*$"),
    inter_name = list(
      unique(c(
        names(coefs)[grepl(inter_pattern1, names(coefs))],
        names(coefs)[grepl(inter_pattern2, names(coefs))]
      ))
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    b_inter = purrr::map_dbl(inter_name, ~ ifelse(length(.x) == 0, 0, sum(coefs[.x]))),
    log_odds = b_main + b_inter,
    log_odds = ifelse(citizenship_grouped == ref_cit, b_main, log_odds),
    odds_ratio = exp(log_odds)
  ) %>%
  dplyr::select(citizenship_grouped, log_odds, odds_ratio)

## =========================================================
## 5A) Publication-ready probabilities plot (with CIs + annotation)
## =========================================================
p_main <- ggplot(
  ie_long,
  aes(x = violence_exp_general, y = pred_prob,
      color = interest_level, group = interest_level)
) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = interest_level),
              alpha = 0.15, colour = NA) +
  geom_line(size = 0.8) +
  geom_point(size = 1.8) +
  facet_wrap(~ citizenship_grouped, nrow = 2) +
  geom_text(
    data = effects_summary,
    aes(x = 1.55, y = 0.95,
        label = paste0("Log-odds: ", sprintf("%.3f", log_odds),
                       "\nOR: ", sprintf("%.2f", odds_ratio))),
    inherit.aes = FALSE, size = 3.2, hjust = 0.5
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0,1,0.25)) +
  labs(
    title = "Figure 8. Predicted General Political Interest Level by Violence Experience and Citizenship",
    subtitle = "Shaded Bands = 95% CIs. Numbers in Each Facet are the Combined Violence Effect (Log-Odds and Odds Ratio (OR))",
    x = "Violence Experience", y = "Predicted Probability",
    color = "Interest level", fill = "Interest level"
  ) +
theme_bw(base_size = 13) +
  theme(
    plot.title = element_text(size = 11, face = "bold", color = "black", margin = margin(t = 10, b = 8)),
    plot.subtitle = element_text(size = 10, face = "bold", color = "black", margin = margin(b = 12)),
    axis.title.y = element_text(size = 10, margin = margin(r = 10)),
    axis.title.x = element_text(size = 10, color = "black", hjust = 0.5, margin = margin(t = 15)),
    axis.text.x = element_text(color = "black", size = 10, hjust = 0.5),
    legend.position = "right",
    plot.margin = margin(20, 30, 20, 20)
  )


p_main

ggsave("fig_probabilities.pdf", p_main, width = 11, height = 9, dpi = 300, bg ="white")


## =========================================================
## 5B) Difference plot (Yes − No) with ~95% CIs (easy-to-read effect)
##      NOTE: CI for delta is approximate. For exact CIs, simulate from coef VCV.
## =========================================================
delta_df <- ie_long %>%
  select(citizenship_grouped, interest_level, violence_exp_general, pred_prob, lower, upper) %>%
  group_by(citizenship_grouped, interest_level) %>%
  summarise(
    prob_no  = pred_prob[violence_exp_general == "No"],
    prob_yes = pred_prob[violence_exp_general == "Yes"],
    delta    = prob_yes - prob_no,
    se_no    = (upper[violence_exp_general == "No"]  - lower[violence_exp_general == "No"])  / (2*1.96),
    se_yes   = (upper[violence_exp_general == "Yes"] - lower[violence_exp_general == "Yes"]) / (2*1.96),
    se_delta = sqrt(se_no^2 + se_yes^2),
    lower_delta = delta - 1.96*se_delta,
    upper_delta = delta + 1.96*se_delta,
    .groups = "drop"
  )

p_delta <- ggplot(delta_df, aes(x = interest_level, y = delta)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 1.8) +
  geom_errorbar(aes(ymin = lower_delta, ymax = upper_delta), width = 0.12) +
  facet_wrap(~ citizenship_grouped, nrow = 2) +
  labs(
    title = "Effect of Violence Experience on General Political interest level (Yes - No)",
    subtitle = "Points are Differences in Predicted Probability (Yes - No); Bars are ~95% CIs",
    x = "General Political Interest level", y = "Change in Probability (Yes - No)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey95", colour = NA),
    plot.margin = margin(20, 30, 20, 20)
  )

p_delta
ggsave("fig_differences.pdf", p_delta, width = 10, height = 7, bg = "white", dpi = 300)


# Regressioviolence_exp_general# Regression Models: Political Interest in Germany. 

# ----------------------------
# Model 1
# ----------------------------


model1_germany <- polr(political_interest_germany ~ violence_exp_general + political_interest_before + wave, data = data, Hess = TRUE)
summary1_germany <- summary(model1_germany)
ctable1_germany <- coef(summary1_germany)
p_values1_germany <- pnorm(abs(ctable1_germany[, "t value"]), lower.tail = FALSE) * 2
coefs1_germany <- ctable1_germany[, "Value"]
se1_germany <- ctable1_germany[, "Std. Error"]


stargazer(model1_germany,
          coef = list(coefs1_germany), se = list(se1_germany), p = list(p_values1_germany),
          title = "Model 1: Violence Exposure Only",
          dep.var.labels = "Political Interest in Germany (Ordered)",
          star.cutoffs = c(0.05, 0.01, 0.001), digits = 3, align = TRUE,
          type = "text", out = "model1_germany_violence.txt")

# ----------------------------
# Model 2
# ----------------------------

model2_germany <- polr(political_interest_germany ~ violence_exp_general + political_interest_before + wave + sex + year_of_birth, data = data, Hess = TRUE)
summary2_germany <- summary(model2_germany)
ctable2_germany <- coef(summary2_germany)
p_values2_germany <- pnorm(abs(ctable2_germany[, "t value"]), lower.tail = FALSE) * 2
coefs2_germany <- ctable2_germany[, "Value"]
se2_germany <- ctable2_germany[, "Std. Error"]


stargazer(model2_germany,
          coef = list(coefs2_germany), se = list(se2_germany), p = list(p_values2_germany),
          title = "Model 2: + Demographic Controls",
          dep.var.labels = "Political Interest in Germany (Ordered)",
          star.cutoffs = c(0.05, 0.01, 0.001), digits = 3, align = TRUE,
          type = "text", out = "model2_germany_violence.txt")

# ----------------------------
# Model 3 
# ----------------------------

model3_germany <- polr(political_interest_germany ~ violence_exp_general + political_interest_before + wave + 
                                 sex + year_of_birth + citizenship_grouped + religion, data = data, Hess = TRUE)
summary3_germany <- summary(model3_germany)
ctable3_germany <- coef(summary3_germany)
p_values3_germany <- pnorm(abs(ctable3_germany[, "t value"]), lower.tail = FALSE) * 2
coefs3_germany <- ctable3_germany[, "Value"]
se3_germany <- ctable3_germany[, "Std. Error"]

stargazer(model3_germany,
          coef = list(coefs3_germany), se = list(se3_germany), p = list(p_values3_germany),
          title = "Model 3: + Full Demographics",
          dep.var.labels = "Political Interest in Germany (Ordered)",
          star.cutoffs = c(0.05, 0.01, 0.001), digits = 3, align = TRUE,
          type = "text", out = "model3_germany_violence.txt")

# ----------------------------
# Model 4
# ----------------------------

model4_germany <- polr(political_interest_germany ~ violence_exp_general*citizenship_grouped + 
                         political_interest_before +
                         wave + sex + year_of_birth + religion + economic_status + 
                         school_cert, data = data, Hess = TRUE)
summary4_germany <- summary(model4_germany)
ctable4_germany <- coef(summary4_germany)
p_values4_germany <- pnorm(abs(ctable4_germany[, "t value"]), lower.tail = FALSE) * 2
coefs4_germany <- ctable4_germany[, "Value"]
se4_germany <- ctable4_germany[, "Std. Error"]


stargazer(model4_germany,
          coef = list(coefs4_germany), se = list(se4_germany), p = list(p_values4_germany),
          title = "Model 4: + Political Interest Before & Education",
          dep.var.labels = "Political Interest in Germany (Ordered)",
          star.cutoffs = c(0.05, 0.01, 0.001), digits = 3, align = TRUE,
          type = "text", out = "model4_germany_violence.txt")


# ----------------------------
# Model 5
# ----------------------------

model5_germany<- polr(political_interest_germany ~ violence_exp_general*citizenship_grouped +
                         political_interest_before +
                         wave + sex + year_of_birth  + religion + economic_status + 
                         school_cert + language_certificate + residence_permit_labeled, data = data, Hess = TRUE)
summary5_germany <- summary(model5_germany)
ctable5_germany <- coef(summary5_germany)
p_values5_germany <- pnorm(abs(ctable5_germany[, "t value"]), lower.tail = FALSE) * 2
coefs5_germany <- ctable5_germany[, "Value"]
se5_germany <- ctable5_germany[, "Std. Error"]


stargazer(model5_germany,
          coef = list(coefs5_germany), se = list(se5_germany), p = list(p_values5_germany),
          title = "Model 5: + Political Interest Before & Education",
          dep.var.labels = "Political Interest in Germany (Ordered)",
          star.cutoffs = c(0.05, 0.01, 0.001), digits = 3, align = TRUE,
          type = "text", out = "model5_germany_violence.txt")



# ----------------------------
# Compare All Models
# ----------------------------


library(sjPlot)
tab_model(
  model1_germany, model2_germany, model3_germany, model4_germany, model5_germany,
  show.p = TRUE,
  show.ci = FALSE,
  title = "Ordinal Logistic Regression Models: Political Interest in General",
  file = "model_results_germany.doc"  # Save as Word
)



# Interaction Term for the Model 5 in Interest in German Politics: 

mf_germany <- model.frame(model5_germany)                # exact data used by the model
with(mf_germany, table(violence_exp_general, citizenship_grouped))

# drop unused factor levels in the model frame
mf2_germany <- mf_germany
for(nm in names(mf2_germany)){
  if(is.factor(mf2_germany[[nm]])) mf2_germany[[nm]] <- droplevels(mf2_germany[[nm]])
}

# refit on the cleaned frame
library(MASS)
model5_clean_germany <- polr(formula(model5_germany), data = mf2_germany, Hess = TRUE)

library(effects)
ie_germany <- Effect(c("violence_exp_general","citizenship_grouped"), model5_clean_germany)
plot(ie_germany)  # base R plot

ie_df_germany <- as.data.frame(ie_germany)

ie_long_germany <- ie_df_germany %>%
  tidyr::pivot_longer(
    cols = matches("^(prob|L\\.prob|U\\.prob)\\."),
    names_to   = c("stat","interest_level_raw"),
    names_pattern = "^(prob|L\\.prob|U\\.prob)\\.(.*)$",
    values_to  = "value"
  ) %>%
  dplyr::mutate(
    interest_level = gsub("\\.", " ", interest_level_raw)
  ) %>%
  dplyr::select(-interest_level_raw) %>%
  tidyr::pivot_wider(names_from = stat, values_from = value) %>%
  dplyr::rename(pred_prob = prob, lower = `L.prob`, upper = `U.prob`) %>%
  dplyr::mutate(
    violence_exp_general = factor(violence_exp_general, levels = c("No","Yes")),
    interest_level = factor(interest_level,
                            levels = c("Not at all","Not so strong","Strong","Very Strong")),
    citizenship_grouped = factor(citizenship_grouped,
                                 levels = c("Afghanistan","Iraq","Syria","Other"))
  )

## =========================================================
## 4) Compute combined violence effect (log-odds & OR) BY GROUP automatically
## =========================================================
coefs_germany <- coef(model5_clean_germany)  # slope coefficients

# identify the "Yes" level of violence
lev_vio_germany <- levels(mf2_germany$violence_exp_general)
stopifnot("Yes" %in% lev_vio_germany)

# main effect name
main_vio_name_germany <- grep("^violence_exp_general", names(coefs_germany), value = TRUE)
main_vio_name_germany <- main_vio_name_germany[grepl(
  paste0("(", paste0(lev_vio_germany[-1], collapse="|"), ")$"), main_vio_name_germany
)]
b_main_germany <- unname(coefs_germany[main_vio_name_germany])

# citizenship levels and reference
lev_cit_germany <- levels(mf2_germany$citizenship_grouped)
ref_cit_germany <- lev_cit_germany[1]

# build combined effects
library(purrr)
effects_summary_germany <- tibble(citizenship_grouped = lev_cit_germany) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    inter_pattern1_germany = paste0("^violence_exp_general.*:citizenship_grouped", citizenship_grouped, "$"),
    inter_pattern2_germany = paste0("^citizenship_grouped", citizenship_grouped, ":violence_exp_general.*$"),
    inter_name_germany = list(
      unique(c(
        names(coefs_germany)[grepl(inter_pattern1_germany, names(coefs_germany))],
        names(coefs_germany)[grepl(inter_pattern2_germany, names(coefs_germany))]
      ))
    )
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    b_inter_germany = purrr::map_dbl(inter_name_germany, ~ ifelse(length(.x) == 0, 0, sum(coefs_germany[.x]))),
    log_odds_germany = b_main_germany + b_inter_germany,
    log_odds = ifelse(citizenship_grouped == ref_cit_germany, b_main_germany, log_odds_germany),
    odds_ratio = exp(log_odds)
  ) %>%
  dplyr::select(citizenship_grouped, log_odds, odds_ratio)

## =========================================================
## 5A) Publication-ready plot for Germany model
## =========================================================
p_main_germany <- ggplot(
  ie_long_germany,
  aes(x = violence_exp_general, y = pred_prob,
      color = interest_level, group = interest_level)
) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = interest_level),
              alpha = 0.15, colour = NA) +
  geom_line(size = 0.8) +
  geom_point(size = 1.8) +
  facet_wrap(~ citizenship_grouped, nrow = 2) +
  geom_text(
    data = effects_summary_germany,
    aes(x = 1.55, y = 0.95,
        label = paste0("Log-odds: ", sprintf("%.3f", log_odds),
                       "\nOR: ", sprintf("%.2f", odds_ratio))),
    inherit.aes = FALSE, size = 3.2, hjust = 0.5
  ) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0,1,0.25)) +
  labs(
    title = "Figure 9. Predicted German Political Interest Level by Violence Experience and Citizenship",
    subtitle = "Shaded Bands = 95% CIs. Numbers in Each Facet are the Combined Violence Effect (Log-Odds and Odds Ratio(OR))",
    x = "Violence Experience", y = "Predicted Probability",
    color = "Interest level", fill = "Interest level"
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title = element_text(size = 11, face = "bold", color = "black", margin = margin(t = 10, b = 8)),
    plot.subtitle = element_text(size = 10, face = "bold", color = "black", margin = margin(b = 12)),
    axis.title.y = element_text(size = 10, margin = margin(r = 10)),
    axis.title.x = element_text(size = 10, color = "black", hjust = 0.5, margin = margin(t = 15)),
    axis.text.x = element_text(color = "black", size = 10, hjust = 0.5),
    legend.position = "right",
    plot.margin = margin(20, 30, 20, 20)
  )

p_main_germany
ggsave("fig_probabilities_germany.pdf", p_main_germany, width = 11, height = 9, dpi = 300, bg ="white")

## =========================================================
## 5B) Difference plot for Germany model
## =========================================================
delta_df_germany <- ie_long_germany %>%
  select(citizenship_grouped, interest_level, violence_exp_general, pred_prob, lower, upper) %>%
  group_by(citizenship_grouped, interest_level) %>%
  summarise(
    prob_no  = pred_prob[violence_exp_general == "No"],
    prob_yes = pred_prob[violence_exp_general == "Yes"],
    delta    = prob_yes - prob_no,
    se_no    = (upper[violence_exp_general == "No"]  - lower[violence_exp_general == "No"])  / (2*1.96),
    se_yes   = (upper[violence_exp_general == "Yes"] - lower[violence_exp_general == "Yes"]) / (2*1.96),
    se_delta = sqrt(se_no^2 + se_yes^2),
    lower_delta = delta - 1.96*se_delta,
    upper_delta = delta + 1.96*se_delta,
    .groups = "drop"
  )

p_delta_germany <- ggplot(delta_df_germany, aes(x = interest_level, y = delta)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_point(size = 1.8) +
  geom_errorbar(aes(ymin = lower_delta, ymax = upper_delta), width = 0.12) +
  facet_wrap(~ citizenship_grouped, nrow = 2) +
  labs(
    title = "Effect of Violence Experience on German Political Interest Level (Yes - No)",
    subtitle = "Points = Difference in Predicted Probability (Yes - No); Bars = ~95% CIs",
    x = "German Political Interest Level", y = "Change in Probability (Yes - No)"
  ) +
  theme_bw(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey95", colour = NA),
    plot.margin = margin(20, 30, 20, 20)
  )

p_delta_germany
ggsave("fig_differences_germany.pdf", p_delta_germany, width = 10, height = 7, bg = "white", dpi = 300)




table(data$political_interest_after, data$wave)
table(data$political_interest_germany, data$wave)

table(data$political_interest_before, data$sex)
table(data$political_interest_after, data$sex)
table(data$residence_permit_labeled, data$political_interest_before, exclude = NULL)
table(data$residence_permit_labeled, data$political_interest_after, exclude = NULL)
table(data$residence_permit_labeled, data$political_interest_germany, exclude = NULL)


# Basic 3-way frequency table

table(data$violence_exp_general, data$citizenship, exclude =NULL)
table(data$violence_exp_general, data$political_interest_before, exclude = NULL)
table(data$violence_exp_general, data$political_interest_germany, exclude = NULL)
table(data$violence_exp_general, data$citizenship_grouped, exclude = NULL)
table(data$violence_exp_general, data$sex, exclude = NULL)
table(data$violence_exp_general, data$religion, exclude = NULL)



round(100 * prop.table(table(data$violence_exp_general, data$citizenship_grouped)), 1)

table(data$citizenship)

#, data$citizenship_grouped, exclude = NULL)), 1)

table(data$violence_exp_general, exclude = NULL)

round(100 * prop.table(table(data$violence_exp_general)), 1)

round(100 * prop.table(table(data$violence_exp_general, data$political_interest_before)), 1)
round(100 * prop.table(table(data$violence_exp_general, data$political_interest_after)), 1)
round(100 * prop.table(table(data$violence_exp_general, data$political_interest_germany)), 1)


library(ggplot2)

cats <- c("Not at all","Not so strong","Strong","Very Strong")

no_before  <- c(19.2,  8.7,  2.4, 1.4)
no_after   <- c(17.9,  8.6,  3.7, 1.5)

yes_before <- c(36.4, 19.8,  7.6, 4.4)
yes_after  <- c(32.9, 20.4,  9.7, 5.2)

# Differences (After − Before)
no_diff  <- no_after  - no_before
yes_diff <- yes_after - yes_before

diff_df <- data.frame(
  Violence = rep(c("No","Yes"), each = 4),
  Interest = rep(cats, 2),
  Diff     = c(no_diff, yes_diff)
)

library(ggplot2)

dodge <- position_dodge(width = 0.6)

diff_general <- ggplot(diff_df, aes(x = Interest, y = Diff, fill = Violence)) +
  geom_col(width = 0.6, position = dodge) +
  geom_hline(yintercept = 0, color = "black") +
  # Labels outside the bars
  geom_text(
    aes(
      label = sprintf("%+.1f", Diff),
      vjust = ifelse(Diff >= 0, -0.6, 1.4)
    ),
    position = dodge,
    size = 3.6
  ) +
  scale_fill_manual(values = c("#377eb8", "#e41a1c"), name = "Violence Exposure") +
  labs(
    x = "Political Interest",
    y = "Percentage-Point Change",
    title = "Figure 6. Change in Political Interest Before vs. After Migration - General Politics",
    caption = "Source: IAB-BAMF-SOEP Refugee Survey (Waves 2016-2018)"
  ) +
  # Fixed scale from -6 to 6 with breaks -6, -3, 0, 3, 6
  scale_y_continuous(
    breaks = seq(-4, 4, 2),
    limits = c(-4.5, 4.5),
    expand = expansion(mult = c(0.02, 0.12))
  ) +
  coord_cartesian(clip = "off") +
  theme_bw(base_size = 13) +
  theme(
    plot.title = element_text(size = 11, face = "bold", color = "black", margin = margin(t = 10, b = 8)),
    plot.subtitle = element_text(size = 10, face = "bold", color = "black", margin = margin(b = 12)),
    axis.title.y = element_text(size = 10, margin = margin(r = 10)),
    axis.title.x = element_text(size = 10, color = "black", hjust = 0.5, margin = margin(t = 15)),
    axis.text.x = element_text(color = "black", size = 10, hjust = 0.5),
    legend.position = "right",
    plot.caption = element_text(size = 8, hjust = 1, margin = margin(t = 12)),
    plot.margin = margin(20, 30, 20, 20)
  )
ggsave(
  "change_in_political_interest_general_politics.pdf",
  plot = diff_general,
  width = 9, height = 7, dpi = 320, bg = "white"
)



# differences between before and after in Germany: 

library(ggplot2)

# Categories and groups
cats_germany <- c("Not at all", "Not so strong", "Strong", "Very Strong")

# Percentages from your screenshot (BEFORE)
no_before_germany  <- c(19.2,  8.7,  2.4, 1.4)
yes_before_germany <- c(36.4, 19.8,  7.6, 4.4)

# Percentages from your screenshot (GERMANY)
no_germany  <- c(16.2,  8.9,  5.2, 1.5)
yes_germany <- c(30.3, 20.4, 13.2, 4.4)

# Calculate percentage-point differences (Germany − Before)
no_diff_germany  <- no_germany  - no_before_germany
yes_diff_germany <- yes_germany - yes_before_germany

# Put into a data frame
diff_df_germany <- data.frame(
  Violence = rep(c("No","Yes"), each = length(cats)),
  Interest = rep(cats_germany, 2),
  Diff     = c(no_diff_germany, yes_diff_germany)
)

library(ggplot2)

dodge <- position_dodge(width = 0.6)

diff_german <- ggplot(diff_df_germany, aes(x = Interest, y = Diff, fill = Violence)) +
  geom_col(width = 0.6, position = dodge) +
  geom_hline(yintercept = 0, color = "black") +
  # Labels outside the bars
  geom_text(
    aes(
      label = sprintf("%+.1f", Diff),
      vjust = ifelse(Diff >= 0, -0.6, 1.4)
    ),
    position = dodge,
    size = 3.6
  ) +
  scale_fill_manual(values = c("#377eb8", "#e41a1c"), name = "Violence Exposure") +
  labs(
    x = "Political Interest",
    y = "Percentage-Point Change",
    title = "Figure 7. Change in Political Interest Before vs. After Migration - German Politics",
    caption = "Source: IAB-BAMF-SOEP Refugee Survey (Waves 2016-2018)"
  ) +
  # Fixed scale from -6 to 6 with breaks -6, -3, 0, 3, 6
  scale_y_continuous(
    breaks = seq(-6, 6, 3),
    limits = c(-6.5, 6.5),
    expand = expansion(mult = c(0.02, 0.12))
  ) +
  coord_cartesian(clip = "off") +
  theme_bw(base_size = 13) +
  theme(
    plot.title = element_text(size = 11, face = "bold", color = "black", margin = margin(t = 10, b = 8)),
    plot.subtitle = element_text(size = 10, face = "bold", color = "black", margin = margin(b = 12)),
    axis.title.y = element_text(size = 10, margin = margin(r = 10)),
    axis.title.x = element_text(size = 10, color = "black", hjust = 0.5, margin = margin(t = 15)),
    axis.text.x = element_text(color = "black", size = 10, hjust = 0.5),
    legend.position = "right",
    plot.caption = element_text(size = 8, hjust = 1, margin = margin(t = 12)),
    plot.margin = margin(20, 30, 20, 20)
  )


ggsave(
  "change_in_political_interest_german_politics.pdf",
  plot = diff_german,
  width = 9, height = 7, dpi = 320, bg = "white"
)

# Figures 4 & 5


acolSums(data[, c("lr3123_bio", "lr3124_bio", "lr3128_bio", "lr3138_bio", "lr3139_bio")] == 1, na.rm = TRUE)





colSums(data[, c("lr3123_bio", "lr3124_bio", "lr3128_bio", "lr3138_bio", "lr3139_bio")] == 1, na.rm = TRUE)


violence_counts <- colSums(data[, c("lr3123_bio", "lr3124_bio", "lr3128_bio", "lr3138_bio", "lr3139_bio")] == 1, na.rm = TRUE)

data.frame(
  Item = c(
    "Sexual Violence", 
    "Physical Attack", 
    "Imprisment", 
    "Persecution", 
    "Discrimination"
  ),
  Count = as.vector(violence_counts)
)

total_n <- nrow(data)

data.frame(
  Item = c(
    "Sexual Violence", 
    "Physical Attack", 
    "Imprisment", 
    "Persecution", 
    "Discrimination"
  ),
  Count = as.vector(violence_counts),
  Percent = round(100 * as.vector(violence_counts) / total_n, 1)
)

# Step 1: Create data with grouping

library(ggplot2)
library(dplyr)
library(scales)


# Step 1: Data – Replace this with your actual counts
df_vio <- data.frame(
  Item = c("Sexual Violence", "Physical Attack", "Imprisonment", "Persecution", "Discrimination"),
  Count = as.vector(violence_counts),  # Replace with actual vector of counts
  Group = c("During Flight", "During Flight", "During Flight", "At Home", "At Home")
)

# Step 2: Compute percentages within each group
df_vio <- df_vio %>%
  group_by(Group) %>%
  mutate(
    Percent = round(100 * Count / sum(Count), 1)
  ) %>%
  arrange(desc(Percent), .by_group = TRUE) %>%
  mutate(Item = factor(Item, levels = unique(Item))) %>%
  ungroup()

# Step 3: Create the percentage plot

dodge <- position_dodge(width = 0.5)
violence_plot <- ggplot(df_vio, aes(x = Item, y = Percent, fill = Group)) +
  geom_col(width = 0.3, position = dodge) +
  geom_text(aes(label = paste0(Percent, "%")), vjust = -0.4, size = 3.5) +
  scale_fill_manual(values = c("During Flight" = "#377eb8", "At Home" = "grey30")) +
  facet_wrap(~ Group, scales = "free_x") +
  labs(
    title = "Figure 4. Reported Experiences of Exposure to Violence Among Refugees by Context",
    subtitle = "Percentages of Reported Experiences Grouped by Context",
    x = "Types of Violence",
    y = "Percentage of Respondents Reporting This Experience",
    fill = "Violence Context",
    caption = "Source: IAB-BAMF-SOEP Refugee Survey (Waves 2016-2018)"
  ) +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  theme_bw(base_size = 13) +
  theme(
    plot.title = element_text(size = 12, face = "bold", color = "black", margin = margin(t = 10, b = 8)),
    plot.subtitle = element_text(size = 11, face = "bold", color = "black", margin = margin(b = 12)),
    axis.title.y = element_text(size = 11, margin = margin(r = 10)),
    axis.title.x = element_text(size = 11, color = "black", hjust = 0.5, margin = margin(t = 15)),
    axis.text.x = element_text(color = "black", size = 11, hjust = 0.5),
    legend.position = "none",
    plot.caption = element_text(size = 7, hjust = 1, margin = margin(t = 12)),
    plot.margin = margin(20, 30, 20, 20)
  )


# Step 4: Export (optional)
ggsave("violence_types_by_context_percentage.pdf", plot = violence_plot, width = 10, height = 7, bg = "white")

#figure 5
rm(try)
try <- data

try <- try %>%
  mutate(
    sexual_violence = lr3123_bio,
    physical_attack = lr3124_bio,
    imprisonment    = lr3128_bio,
    persecution     = lr3139_bio,
    discrimination  = lr3138_bio
  )


top3_countries <- try %>%
  filter(!is.na(citizenship_grouped), citizenship_grouped != "Other") %>%
  count(citizenship_grouped, sort = TRUE) %>%
  slice_head(n = 3) %>%
  pull(citizenship_grouped)


# Add grouping and rename items
violence_long <- try %>%
  filter(citizenship_grouped %in% top3_countries) %>%
  mutate(
    `Sexual Violence`   = lr3123_bio,
    `Physical Attack`   = lr3124_bio,
    `Imprisonment`      = lr3128_bio,
    Discrimination      = lr3138_bio,
    Persecution         = lr3139_bio
  ) %>%
  dplyr::select(citizenship_grouped, `Sexual Violence`, `Physical Attack`,
                Imprisonment, Discrimination, Persecution) %>%
  pivot_longer(-citizenship_grouped, names_to = "Violence_Type", values_to = "Selected") %>%
  group_by(citizenship_grouped, Violence_Type) %>%
  summarise(
    Count = sum(Selected == 1, na.rm = TRUE),
    Total = n(),
    Percent = round(100 * Count / Total, 1),
    .groups = "drop"
  ) %>%
  mutate(
    Context = case_when(
      Violence_Type %in% c("Discrimination", "Persecution") ~ "At Home",
      TRUE ~ "During Flight"
    )
  )


# Reorder violence types: start with Persecution
violence_long$Violence_Type <- factor(
  violence_long$Violence_Type,
  levels = c("Persecution", "Discrimination", "Imprisonment", "Physical Attack", "Sexual Violence")
)


violence_long$citizenship_grouped <- factor(
  violence_long$citizenship_grouped,
  levels = c("Syria", "Afghanistan", "Iraq")
)


# Reorder countries (Syria, Afghanistan, Iraq)
violence_long <- violence_long %>%
  mutate(
    citizenship_grouped = factor(citizenship_grouped, levels = c("Syria", "Afghanistan", "Iraq")),
    Violence_Type = factor(Violence_Type, levels = c("Persecution", "Discrimination", "Imprisonment", "Physical Attack", "Sexual Violence"))
  )

# Plot
violence_types_top_countries <- ggplot(violence_long, aes(x = citizenship_grouped, y = Percent, fill = Context)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = paste0(Percent, "%")), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 3.2) +
  facet_wrap(~ Violence_Type, nrow = 1, strip.position = "top") +
  scale_fill_manual(values = c("During Flight" = "#377eb8", "At Home" = "grey30")) +
  labs(
    title = "Figure 5. Reported Experiences of Violence by Type and Context Among Refugees",
    subtitle = "Comparison by Citizenship Group (Top 3 Countries) and Context of Experience",
    x = "Citizenship Group",
    y = "Percent of Respondents",
    fill = "Violence Context",
    caption = "Source: IAB-BAMF-SOEP Refugee Survey (Waves 2016-2018)"
  ) +
  theme_bw(base_size = 13) +
  theme(
    plot.title = element_text(size = 12, face = "bold", color = "black",  margin = margin(t = 10, b = 5)),
    plot.subtitle = element_text(size = 11, face = "bold", color = "black",  margin = margin(b = 10)),
    axis.title.x = element_text(size = 11, color = "black", margin = margin(t = 10)),
    axis.title.y = element_text(size = 11, color = "black", margin = margin(r = 10)),
    axis.text.x = element_text(size = 11, color = "black"),
    axis.text.y = element_text(size = 11, color = "black"),
    legend.position = "top",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    strip.text = element_text(size = 10, face = "bold"),
    plot.caption = element_text(size = 8, hjust = 1, margin = margin(t = 10)),
    plot.margin = margin(20, 25, 20, 20)
  )

# Save the plot (optional)
ggsave("figure5_violence_by_type_context_country.pdf", plot = violence_types_top_countries, width = 12, height = 9, bg = "white")





table(data$violence_exp_general, exclude = NULL)

round(100 * prop.table(table(data$violence_exp_general, data$political_interest_before)), 1)
round(100 * prop.table(table(data$violence_exp_general, data$political_interest_after)), 1)
round(100 * prop.table(table(data$violence_exp_general, data$political_interest_germany)), 1)

round(100 * prop.table(table(data$religion, data$persecution)), 1)
round(100 * prop.table(table(data$religion, data$discrimination)), 1)
round(100 * prop.table(table(data$religion, data$imprisment)), 1)
round(100 * prop.table(table(data$religion, data$physical_violence)), 1)
round(100 * prop.table(table(data$religion, data$sexual_violence)), 1)


round(100 * prop.table(table(data$sex)), 1)

data$physical_violence <- data$lr3124_bio
table(data$physical_violence)




data$physical_violence[data$physical_violence == -2] <- NA  # Set missing


data$imprisment  <- data$lr3128_bio
table(data$imprisment)
data$imprisment[data$imprisment == -2] <- NA  # Set missing

table(data$sex, data$sexual_violence)


data$sexual_violence <- data$lr3123_bio
data$sexual_violence[data$sexual_violence == -2] <- NA  # Set missing


round(100 * prop.table(table(data$sex)), 1)


round(100 * prop.table(table(data$violence_exp_general, data$religion)), 1)

round(100 * prop.table(table(data$religion)), 1)





 # Persecution 
table(data$lr3138_bio)

data$persecution <- data$lr3138_bio
data$persecution[data$persecution == -2] <- NA  # Set missing


# Persecution
# Keep 1 = Yes, change -2 to 0 (No)
data$persecution <- ifelse(data$lr3138_bio == -2, 0, data$lr3138_bio)

# Create a table of country by reported persecution
table_persecution <- table(data$citizenship_grouped, data$persecution)

table(data$persecution)

# Chi-squared test
chisq.test(table_persecution)


# Discrimination 

table(data$lr3139_bio)

data$discrimination <- data$lr3139_bio
data$discrimination <- ifelse(data$lr3139_bio == -2, 0, data$lr3139_bio)
table(data$discrimination)


# Create a table of country by reported persecution (1 = yes, 0 = no)
table_discrimination <- table(
  data$citizenship_grouped, 
  data$discrimination  # assuming this is the "persecution" variable
)
chisq.test(table_discrimination)

# Sexual Violence

table(data$lr3123_bio)

data$sexual <- data$lr3123_bio
data$sexual <- ifelse(data$lr3123_bio == -2, 0, data$lr3123_bio)
table(data$sexual)


# Create a table of country by reported persecution (1 = yes, 0 = no)
table_sexual <- table(
  data$citizenship_grouped, 
  data$sexual  # assuming this is the "persecution" variable
)
chisq.test(table_sexual)


# Physical Violence: lr3124_bio

table(data$lr3124_bio)

data$physical <- data$lr3124_bio
data$physical <- ifelse(data$lr3124_bio == -2, 0, data$lr3124_bio)
table(data$physical)


# Create a table of country by reported persecution (1 = yes, 0 = no)
table_physical <- table(
  data$citizenship_grouped, 
  data$physical  # assuming this is the "persecution" variable
)
chisq.test(table_physical)

# Being in Jail lr3128

table(data$lr3128_bio)

data$jail <- data$lr3128_bio
data$jail <- ifelse(data$lr3128_bio == -2, 0, data$lr3128_bio)
table(data$jail)


# Create a table of country by reported persecution (1 = yes, 0 = no)
table_jail <- table(
  data$citizenship_grouped, 
  data$jail  # assuming this is the "persecution" variable
)
chisq.test(table_jail)

table(data$violence_exp_general, data$political_interest_after, exclude = NULL)

table(data$political_interest_before, data$political_interest_after, exclude = NULL)



# Perform the chi-squared test
chisq.test(table_discrimination)

#___

# Individual Rgeression Analyses Tables: 

# General Political Interest 

#1 Sexual Violence: lr3123_bio

table(data$lr3123_bio, exclude = NULL)

data <- data %>%
  mutate(sexual_violence_model = ifelse(lr3123_bio == 1, 1, 0),
        sexual_violence_model = factor(sexual_violence_model, levels = c(0, 1), labels = c("No", "Yes")))

data$sexual_violence_model <- relevel(data$sexual_violence_model, ref = "No")

table(data$sexual_violence_model)

m5_sexual_violence <- polr(
  political_interest_after ~ sexual_violence_model * citizenship_grouped +
    political_interest_before + wave +
    sex + year_of_birth + religion +
    economic_status + school_cert + language_certificate + residence_permit_labeled,
  data = data, Hess = TRUE
)


# Extract model summary
summary5_sexual_violence <- summary(m5_sexual_violence)
ctable5_sexual_violence <- coef(summary5_sexual_violence)

# Calculate p-values manually
p_values5_sexual_violence <- pnorm(abs(ctable5_sexual_violence [, "t value"]), lower.tail = FALSE) * 2
coefs5_sexual_violence  <- ctable5_sexual_violence [, "Value"]
se5_sexual_violence <- ctable5_sexual_violence [, "Std. Error"]

# Stargazer output
stargazer(m5_sexual_violence, type = "html",
          coef = list(coefs5_sexual_violence),
          se = list(se5_sexual_violence),
          p = list(p_values5_sexual_violence),
          title = "Model 5: Sexual Violence",
          dep.var.labels = "Political Interest After (Ordered)",
          star.cutoffs = c(0.05, 0.01, 0.001),
          digits = 3,
          align = TRUE,
          out = "model5_sexual_violence.html")


#2 Physical Violence: lr3124_bio 

table(data$lr3124_bio, exclude = NULL)

data <- data %>%
  mutate(physical_violence_model = ifelse(lr3124_bio == 1, 1, 0),
         physical_violence_model = factor(physical_violence_model, levels = c(0, 1), labels = c("No", "Yes")))

data$physcial_violence_model <- relevel(data$physical_violence_model, ref = "No")



m5_physical_violence <- polr(
  political_interest_after ~ physcial_violence_model * citizenship_grouped +
    political_interest_before + wave +
    sex + year_of_birth + religion +
    economic_status + school_cert + language_certificate + residence_permit_labeled,
  data = data, Hess = TRUE
)


# Extract model summary
summary5_physical_violence <- summary(m5_physical_violence)
ctable5_physical_violence <- coef(summary5_physical_violence)

# Calculate p-values manually
p_values5_physical_violence <- pnorm(abs(ctable5_physical_violence [, "t value"]), lower.tail = FALSE) * 2
coefs5_physical_violence  <- ctable5_physical_violence [, "Value"]
se5_physical_violence <- ctable5_physical_violence [, "Std. Error"]

# Stargazer output
stargazer(m5_physical_violence, type = "html",
          coef = list(coefs5_physical_violence),
          se = list(se5_physical_violence),
          p = list(p_values5_physical_violence),
          title = "Model 5: Physical Violence",
          dep.var.labels = "Political Interest After (Ordered)",
          star.cutoffs = c(0.05, 0.01, 0.001),
          digits = 3,
          align = TRUE,
          out = "model5_physical_violence.html")


#3 lr3128_bio <- imprisonment 

table(data$lr3128_bio, exclude = NULL)


data <- data %>%
  mutate(imprisonment_model = ifelse(lr3128_bio == 1, 1, 0),
         imprisonment_model = factor(imprisonment_model, levels = c(0, 1), labels = c("No", "Yes")))

data$imprisonment_model <- relevel(data$imprisonment_model, ref = "No")

table(data$imprisonment_model, exclude = NULL)

m5_imprisonment <- polr(
  political_interest_after ~ imprisonment_model * citizenship_grouped +
    political_interest_before + wave +
    sex + year_of_birth + religion +
    economic_status + school_cert + language_certificate + residence_permit_labeled,
  data = data, Hess = TRUE
)


# Extract model summary
summary5_imprisonment <- summary(m5_imprisonment)
ctable5_imprisonment <- coef(summary5_imprisonment)

# Calculate p-values manually
p_values5_imprisonment <- pnorm(abs(ctable5_imprisonment [, "t value"]), lower.tail = FALSE) * 2
coefs5_imprisonment  <- ctable5_imprisonment [, "Value"]
se5_imprisonment <- ctable5_imprisonment [, "Std. Error"]

# Stargazer output
stargazer(m5_imprisonment, type = "html",
          coef = list(coefs5_imprisonment),
          se = list(se5_imprisonment),
          p = list(p_values5_imprisonment),
          title = "Model 5: Imprisonment",
          dep.var.labels = "Political Interest After (Ordered)",
          star.cutoffs = c(0.05, 0.01, 0.001),
          digits = 3,
          align = TRUE,
          out = "model5_imprisonment.html")

#4 Persecution lr3138_bio 

table(data$lr3138_bio, exclude = NULL)


data <- data %>%
  mutate(persecution_model = ifelse(lr3138_bio == 1, 1, 0),
         persecution_model = factor(persecution_model, levels = c(0, 1), labels = c("No", "Yes")))

data$persecution_model <- relevel(data$persecution_model, ref = "No")

table(data$persecution_model, exclude = NULL)

m5_persecution <- polr(
  political_interest_after ~ persecution_model * citizenship_grouped +
    political_interest_before + wave +
    sex + year_of_birth + religion +
    economic_status + school_cert + language_certificate + residence_permit_labeled,
  data = data, Hess = TRUE
)


# Extract model summary
summary5_persecution <- summary(m5_persecution)
ctable5_persecution <- coef(summary5_persecution)

# Calculate p-values manually
p_values5_persecution <- pnorm(abs(ctable5_persecution [, "t value"]), lower.tail = FALSE) * 2
coefs5_persecution  <- ctable5_persecution [, "Value"]
se5_persecution <- ctable5_persecution [, "Std. Error"]

# Stargazer output
stargazer(m5_persecution, type = "html",
          coef = list(coefs5_persecution),
          se = list(se5_persecution),
          p = list(p_values5_persecution),
          title = "Model 5: persecution",
          dep.var.labels = "Political Interest After (Ordered)",
          star.cutoffs = c(0.05, 0.01, 0.001),
          digits = 3,
          align = TRUE,
          out = "model5_persecution.html") 


#4 Discriminaion lr3139_bio 

table(data$lr3139_bio, exclude = NULL)


data <- data %>%
  mutate(dis_model = ifelse(lr3139_bio == 1, 1, 0),
         dis_model = factor(dis_model, levels = c(0, 1), labels = c("No", "Yes")))

data$dis_model <- relevel(data$dis_model, ref = "No")

table(data$dis_model, exclude = NULL)


m5_dis <- polr(
  political_interest_after ~ dis_model * citizenship_grouped +
    political_interest_before + wave +
    sex + year_of_birth + religion +
    economic_status + school_cert + language_certificate + residence_permit_labeled,
  data = data, Hess = TRUE
)


# Extract model summary
summary5_dis <- summary(m5_dis)
ctable5_dis <- coef(summary5_dis)

# Calculate p-values manually
p_values5_dis <- pnorm(abs(ctable5_dis [, "t value"]), lower.tail = FALSE) * 2
coefs5_dis  <- ctable5_dis [, "Value"]
se5_dis <- ctable5_dis[, "Std. Error"]

# Stargazer output
stargazer(m5_dis, type = "html",
          coef = list(coefs5_dis),
          se = list(se5_dis),
          p = list(p_values5_dis),
          title = "Model 5: Discrimination",
          dep.var.labels = "Political Interest After (Ordered)",
          star.cutoffs = c(0.05, 0.01, 0.001),
          digits = 3,
          align = TRUE,
          out = "model5_discrimination.html") 


       
# German Political Interest 

#1 Sexual Violence: lr3123_bio

m5_sexual_violence_germany <- polr(
  political_interest_germany ~ sexual_violence_model * citizenship_grouped +
    political_interest_before + wave +
    sex + year_of_birth + religion +
    economic_status + school_cert + language_certificate + residence_permit_labeled,
  data = data, Hess = TRUE
)


# Extract model summary
summary5_sexual_violence_germany <- summary(m5_sexual_violence_germany)
ctable5_sexual_violence_germany <- coef(summary5_sexual_violence_germany)

# Calculate p-values manually
p_values5_sexual_violence_germany <- pnorm(abs(ctable5_sexual_violence_germany [, "t value"]), lower.tail = FALSE) * 2
coefs5_sexual_violence_germany  <- ctable5_sexual_violence_germany [, "Value"]
se5_sexual_violence_germany <- ctable5_sexual_violence_germany [, "Std. Error"]

# Stargazer output
stargazer(m5_sexual_violence_germany, type = "html",
          coef = list(coefs5_sexual_violence_germany),
          se = list(se5_sexual_violence_germany),
          p = list(p_values5_sexual_violence_germany),
          title = "Model 5: Sexual Violence_Germany",
          dep.var.labels = "German Political Interest (Ordered)",
          star.cutoffs = c(0.05, 0.01, 0.001),
          digits = 3,
          align = TRUE,
          out = "model5_sexual_violence_germany.html")


#2 Physical Violence: lr3124_bio 

m5_physical_violence_germany <- polr(
  political_interest_germany ~ physcial_violence_model * citizenship_grouped +
    political_interest_before + wave +
    sex + year_of_birth + religion +
    economic_status + school_cert + language_certificate + residence_permit_labeled,
  data = data, Hess = TRUE
)


# Extract model summary
summary5_physical_violence_germany <- summary(m5_physical_violence_germany)
ctable5_physical_violence_germany <- coef(summary5_physical_violence_germany)

# Calculate p-values manually
p_values5_physical_violence_germany <- pnorm(abs(ctable5_physical_violence_germany [, "t value"]), lower.tail = FALSE) * 2
coefs5_physical_violence_germany  <- ctable5_physical_violence_germany [, "Value"]
se5_physical_violence_germany <- ctable5_physical_violence_germany [, "Std. Error"]

# Stargazer output
stargazer(m5_physical_violence_germany, type = "html",
          coef = list(coefs5_physical_violence_germany),
          se = list(se5_physical_violence_germany),
          p = list(p_values5_physical_violence_germany),
          title = "Model 5: Physical Violence_Germany",
          dep.var.labels = "German Interest After (Ordered)",
          star.cutoffs = c(0.05, 0.01, 0.001),
          digits = 3,
          align = TRUE,
          out = "model5_physical_violence_germany.html")


#3 lr3128_bio <- imprisonment 

m5_imprisonment_germany <- polr(
  political_interest_germany ~ imprisonment_model * citizenship_grouped +
    political_interest_before + wave +
    sex + year_of_birth + religion +
    economic_status + school_cert + language_certificate + residence_permit_labeled,
  data = data, Hess = TRUE
)


# Extract model summary
summary5_imprisonment_germany <- summary(m5_imprisonment_germany)
ctable5_imprisonment_germany <- coef(summary5_imprisonment_germany)

# Calculate p-values manually
p_values5_imprisonment_germany <- pnorm(abs(ctable5_imprisonment_germany [, "t value"]), lower.tail = FALSE) * 2
coefs5_imprisonment_germany  <- ctable5_imprisonment_germany [, "Value"]
se5_imprisonment_germany <- ctable5_imprisonment_germany [, "Std. Error"]

# Stargazer output
stargazer(m5_imprisonment_germany, type = "html",
          coef = list(coefs5_imprisonment_germany),
          se = list(se5_imprisonment_germany),
          p = list(p_values5_imprisonment_germany),
          title = "Model 5: Imprisonment_Germany",
          dep.var.labels = "German Political Interest (Ordered)",
          star.cutoffs = c(0.05, 0.01, 0.001),
          digits = 3,
          align = TRUE,
          out = "model5_imprisonment_germany.html")

#4 Persecution lr3138_bio 

m5_persecution_germany <- polr(
  political_interest_germany ~ persecution_model * citizenship_grouped +
    political_interest_before + wave +
    sex + year_of_birth + religion +
    economic_status + school_cert + language_certificate + residence_permit_labeled,
  data = data, Hess = TRUE
)


# Extract model summary
summary5_persecution_germany <- summary(m5_persecution_germany)
ctable5_persecution_germany <- coef(summary5_persecution_germany)

# Calculate p-values manually
p_values5_persecution_germany <- pnorm(abs(ctable5_persecution_germany [, "t value"]), lower.tail = FALSE) * 2
coefs5_persecution_germany  <- ctable5_persecution_germany [, "Value"]
se5_persecution_germany <- ctable5_persecution_germany [, "Std. Error"]

# Stargazer output
stargazer(m5_persecution_germany, type = "html",
          coef = list(coefs5_persecution_germany),
          se = list(se5_persecution_germany),
          p = list(p_values5_persecution_germany),
          title = "Model 5: persecution_germany",
          dep.var.labels = "German Political Interest After (Ordered)",
          star.cutoffs = c(0.05, 0.01, 0.001),
          digits = 3,
          align = TRUE,
          out = "model5_persecution_germany.html") 


#5 Discriminaion lr3139_bio 

m5_dis_germany <- polr(
  political_interest_germany ~ dis_model * citizenship_grouped +
    political_interest_before + wave +
    sex + year_of_birth + religion +
    economic_status + school_cert + language_certificate + residence_permit_labeled,
  data = data, Hess = TRUE
)


# Extract model summary
summary5_dis_germany <- summary(m5_dis_germany)
ctable5_dis_germany <- coef(summary5_dis_germany)

# Calculate p-values manually
p_values5_dis_germany <- pnorm(abs(ctable5_dis_germany [, "t value"]), lower.tail = FALSE) * 2
coefs5_dis_germany  <- ctable5_dis_germany [, "Value"]
se5_dis_germany <- ctable5_dis_germany[, "Std. Error"]

# Stargazer output
stargazer(m5_dis_germany, type = "html",
          coef = list(coefs5_dis_germany),
          se = list(se5_dis_germany),
          p = list(p_values5_dis_germany),
          title = "Model 5: Discrimination_Germany",
          dep.var.labels = "German Political Interest After (Ordered)",
          star.cutoffs = c(0.05, 0.01, 0.001),
          digits = 3,
          align = TRUE,
          out = "model5_discrimination_germany.html") 


