# Set the workdirectory

setwd("/Users/mmoheebahmed/Desktop/UCD MA/MA Thesis/Data/R_EN/soepdata")
setwd("/Users/mmoheebahmed/Desktop/UCD MA/MA Thesis/Data/R_EN/soepdata/raw")
setwd("/Users/mmoheebahmed/Desktop/UCD MA/MA Thesis/Data/R_EN/soepdata/eu-silc-like-panel")
getwd()

# Load the base libraries

library(tidyverse)
library(tidyr)
library(dplyr)
library(ggplot2)
library(rio)

refugee_history <- readRDS("refugspell.rds")
View(refugee_history)
names(refugee_hisotry)
class(refugee_history$rexit)
unique(as_factor(refugee_hisotry$move))

bgpgen <- readRDS("bgpgen.rds")
names(bgpgen)
View(bgpgen)
unique(as_factor(bgpgen$status_asyl))
class(bgpgen$status_asyl)

library(haven)
unique(as_factor(bgpgen$status_asyl))
dim(bgpgen)

biodata <- readRDS("biol.rds")
dim(biodata)
names(biodata)
View(biodata)
library(haven)
unique(as_factor(biodata$syear))


# filter the data to include only the required survey: 

# Load necessary library (for as_factor if you used haven)
library(haven)
library(dplyr)


bamf_refugee <- biodata %>% filter(sample1 %in% c(30, 31, 34, 41, 47, 48))

unique(as_factor(bamf_refugee$syear))

table(as_factor(bamf_refugee$syear))
View(bamf_refugee)

# Identify only those who participated in the survey in 2016, 2017, and 2018 


# Step 1: Filter only 2016, 2017, 2018 observations
bamf_16_17_18 <- bamf_refugee %>%
  filter(syear %in% c(2016, 2017, 2018))

# Step 2: Count how many years each person appeared
respondents_all_three_years <- bamf_16_17_18 %>%
  group_by(pid) %>%
  summarise(n_years = n_distinct(syear)) %>%
  filter(n_years == 3)  # Keep only those who participated all 3 years

# Step 3: Join back to get full data for those individuals
bamf_panel_16_17_18 <- bamf_16_17_18 %>%
  semi_join(respondents_all_three_years, by = "pid")

View(bamf_panel_16_17_18)
dim(bamf_panel_16_17_18)

# How many unique individuals?
n_distinct(bamf_panel_16_17_18$pid) # the sample size 5283, but for each year is 1761. 

# Number of rows (includes multiple years)
nrow(bamf_panel_16_17_18)

# Frequency per year
table(bamf_panel_16_17_18$syear)


#________________________________________#
# Trial: Identify those who participated in the panel in the years of 2016, 2017, 2018, 2020 and 2021.# O Sample.

library(dplyr)

# Step 1: Filter only 2016, 2017, 2018, 2020, 2021 observations
bamf_16_17_18_20_21 <- bamf_refugee %>%
  filter(syear %in% c(2016, 2017, 2018, 2020, 2021))

# Step 2: Identify those who participated in all 5 years
respondents_all_five_years <- bamf_16_17_18_20_21 %>%
  group_by(pid) %>%
  summarise(n_years = n_distinct(syear)) %>%
  filter(n_years == 5)

# Step 3: Join back to get full data for those individuals in those years
bamf_panel_16_to_21 <- bamf_16_17_18_20_21 %>%
  semi_join(respondents_all_five_years, by = "pid")

# Count of unique individuals
n_distinct(bamf_panel_16_to_21$pid) # 0

# # Number of rows (includes multiple years)
nrow(bamf_panel_16_to_21) # 0

# Count of observations by year
table(bamf_panel_16_to_21$syear) # 0

# Trial: Identify those who participated in the panel in the years of 2016, 2017, 2018, and 2020.# O Sample.


library(dplyr)

# Step 1: Filter only 2016, 2017, 2018, and 2020 observations
bamf_16_17_18_20 <- bamf_refugee %>%
  filter(syear %in% c(2016, 2017, 2018, 2020))

# Step 2: Identify those who participated in all 4 years
respondents_all_four_years <- bamf_16_17_18_20 %>%
  group_by(pid) %>%
  summarise(n_years = n_distinct(syear)) %>%
  filter(n_years == 4)

# Step 3: Join back to get full data for those individuals in those years
bamf_panel_16_to_20 <- bamf_16_17_18_20 %>%
  semi_join(respondents_all_four_years, by = "pid")

# Number of unique individuals
n_distinct(bamf_panel_16_to_20$pid) # 0

# Distribution across years
table(bamf_panel_16_to_20$syear) # 0

#________________________________________________________#

# our dataset: bamf_panel_16_17_18

# Extract the Variables Names in Excel Table for Easy Access:

library(tibble)
library(purrr)

var_labels <- tibble(
  variable = names(bamf_panel_16_17_18),
  label = map_chr(bamf_panel_16_17_18, function(x) {
    lbl <- attr(x, "label")
    if (is.null(lbl)) return(NA_character_)
    if (length(lbl) > 1) return(paste(lbl, collapse = " | "))
    return(lbl)
  })
)

View(var_labels)

# Load the package
library(openxlsx)

# Write to Excel
write.xlsx(var_labels, file = "variable_labels.xlsx")

#__________________Some Descripitive Data___________________

library(dplyr)
library(ggplot2)

# Step 1: Filter to only 2016 respondents
escape_2016 <- bamf_panel_16_17_18 %>%
  filter(syear == 2016)

# Step 2: Define variables and their labels (from codebook)
experience_vars <- c("lr3122", "lr3123", "lr3124", "lr3125", "lr3126", "lr3127", "lr3128", "lr3129")
experience_labels <- c(
  "Fraud or Exploitation",
  "Sexual Harassment",
  "Physical Assaults",
  "Shipwreck",
  "Robbery",
  "Extortion",
  "Incarceration",
  "None of These"
)

# Step 3: Build frequency table for 'Yes' (code = 1)
freq_df <- data.frame(
  variable = experience_labels,
  count = sapply(experience_vars, function(var) {
    sum(escape_2016[[var]] == 1, na.rm = TRUE)
  })
)

# Step 4: Plot
neg_experience <- ggplot(freq_df, aes(x = reorder(variable, count), y = count)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = count), hjust = -0.2, size = 4) +  # Add count labels
  coord_flip() +
  labs(
    title = "Negative Experiences During Escape (2016 Respondents)",
    x = "Experience",
    y = "Number of Respondents"
  ) +
  theme_minimal()

ggsave("Negative_Experience_During_Escape.pdf", plot = neg_experience, height = 15, width = 15, bg = "white")

# To Double-Check Whether Any of Those who selected the Non of These Selected Anything Else.

# List of the 7 actual experiences (excluding "None of These")
experiences <- c("lr3122", "lr3123", "lr3124", "lr3125", "lr3126", "lr3127", "lr3128")

# Create a flag for anyone who selected "None of These"
escape_2016 <- bamf_panel_16_17_18 %>%
  filter(syear == 2016) %>%
  mutate(
    none_selected = lr3129 == 1,
    any_others_selected = rowSums(across(all_of(experiences), ~ . == 1), na.rm = TRUE) > 0
  )

# Find inconsistencies: those who selected "None of These" AND another experience
inconsistent <- escape_2016 %>%
  filter(none_selected & any_others_selected)

# Count how many
n_inconsistent <- nrow(inconsistent)
n_none_total <- sum(escape_2016$lr3129 == 1, na.rm = TRUE)


cat("Number of people who selected 'None of These':", n_none_total, "\n")
cat("Number of inconsistent responses (also selected other experiences):", n_inconsistent, "\n")
#______________________________#

# Checking Question 36 about the main reasons for leaving their home country. 


# Filter 2016 data
escape_2016 <- bamf_panel_16_17_18 %>%
  filter(syear == 2016) 

# Create logical vectors for each variable
other_reason_1 <- escape_2016$lr3136 == 1
other_reason_2 <- escape_2016$lr3146 == 1

# Count overlap
sum(other_reason_1, na.rm = TRUE)         # How many selected lr3136 # 1387 ones
sum(other_reason_2, na.rm = TRUE)         # How many selected lr3146 # 226 ones
sum(other_reason_1 & other_reason_2, na.rm = TRUE)  # How many selected both # 147 ones. 


library(dplyr)
library(ggplot2)

# 1. Filter for 2016 respondents
escape_2016 <- bamf_panel_16_17_18 %>% 
  filter(syear == 2016)

# 2. Define variables and their labels
reason_vars <- c("lr3136", "lr3137", "lr3138", "lr3139", "lr3140", 
                 "lr3141", "lr3142", "lr3143", "lr3144", "lr3145", "lr3146")

reason_labels <- c(
  "Fear of War",
  "Fear of Forced Recruitment",
  "Persecution",
  "Discrimination",
  "Poor Living Conditions",
  "Economic Situation",
  "Wanted to Be with Family",
  "Family Sent Me",
  "Family Members Left",
  "Friends Left",
  "Other Reasons"
)

# 3. Calculate frequencies of "Yes" responses
reason_freq <- data.frame(
  Reason = reason_labels,
  Count = sapply(reason_vars, function(var) {
    sum(escape_2016[[var]] == 1, na.rm = TRUE)
  })
)

# 4. Plot with labels on bars
escaping_reasons <- ggplot(reason_freq, aes(x = reorder(Reason, Count), y = Count)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = Count), hjust = -0.2) +
  coord_flip() +
  labs(
    title = "Reasons for Leaving Country of Origin (2016 Respondents)",
    x = "Reason",
    y = "Number of Respondents"
  ) +
  theme_minimal()

ggsave("Escaping Reasons.pdf", plot = escaping_reasons, height = 10, width= 13, bg = "white")


#_______#
# This question doesnt apply to the refugee survey. 

# 1. Filter data for 2016
migration_2016 <- bamf_panel_16_17_18 %>% 
  filter(syear == 2016)

# 2. Define variable names and their descriptive labels
migration_vars <- c("lb0031", "lb0032", "lb0033", "lb0034", "lb0035", "lb0036")

migration_labels <- c(
  "Better Life",
  "Work/Money",
  "Freedom",
  "Family",
  "Poverty",
  "Persecution/War"
)

# 3. Calculate frequencies of "Yes" responses (coded as 1)
migration_freq <- data.frame(
  Reason = migration_labels,
  Count = sapply(migration_vars, function(var) {
    sum(migration_2016[[var]] == 1, na.rm = TRUE)
  })
)

head(migration_2016[, migration_vars])


for (var in migration_vars) {
  cat("\n---", var, "---\n")
  print(table(migration_2016[[var]], useNA = "ifany"))
}

sum(migration_2016$lb0031 == 1, na.rm = TRUE)



library(dplyr)

# List of variables to check
vars_to_check <- c(
  "lb0021", "lb0022", 
  "lb0885_h", "lb0885_v1", "lb0885_v2", 
  "lb1447", 
  "sex_h", "sex_v1", "sex_v2"
)

# Print frequency tables for each variable
for (var in vars_to_check) {
  if (var %in% colnames(bamf_panel_16_17_18)) {
    cat("\n==== Variable:", var, "====\n")
    print(table(bamf_panel_16_17_18[[var]], useNA = "ifany"))
  }
}



# Vector of variable names
vars_to_check <- c(
  "lb0548", "lb0549", "lb0550",
  "lb0551", "lb0552", "lb0553",
  "lb0554", "lb0555", "lb0556"
)

# Loop through and print value frequencies
for (var in vars_to_check) {
  cat("==== Variable:", var, "====\n")
  print(table(bamf_panel_16_17_18[[var]], useNA = "always"))
  cat("\n")
}



#_________#


#________Testing different datsets_________________#

# Other Poteinal Datasets

refugee_history <- readRDS("bioimmig.rds") 
View(refugee_history) # Not related 
names(refugee_history)

cog <- readRDS("cog_refu.rds")
View(cog) #Not related 

trust <- readRDS("trust.rds")
View(trust) # Not related

document <- readRDS("more_docu.rds")
View(document) # More on Language Skillsa and Study for Refugees and Others.

gen <- readRDS("pgen.rds") # on country of origin, and work experience, current work, and income.
View(gen) 


design <- readRDS("design.rds") # Not related 
View(design)


cog1 <- readRDS("cogdj.rds") # Not related
View(cog1)

cog2 <- readRDS("cognit.rds") # Not related
View(cog2)

specific <- readRDS("pbiospe.rds") # Not related
View(specific)

exit <- readRDS("pbr_exit.rds") # can be related to some extent but not on politics. 
View(exit)

brutto <- readRDS("pbrutto.rds") # Not related
View(brutto)

bath <- readRDS("ppathl.rds") # Should be Checked again
View(bath)

politics <- readRDS("plueckel.rds") # Important but doesnt have anything to do with politics.
View(politics)


pl <- readRDS("pl.rds") # Cannot load it yet! 

pgen <- readRDS("pgen.rds")
View(pgen)

health <- readRDS("vpl.rds") # Not related 
View(health)

life <- readRDS("lifespell.rds") # Not related. 
View(life)

local <- readRDS("more_local.rds") # Not related
View(local)

kal <- readRDS("pkal.rds") # Not related
View(kal)

person <- readRDS("lee2person.rds") # Not related
View(person)

insturment <- readRDS("instrumentation.rds") # Not related
View(insturment)

event <- readRDS("lkal.rds") # Not related
View(event)

pfile <- readRDS("P-File.rds") # Not related
View(pfile)

vpluecke <- readRDS("vpluecke.rds") # Not relared
View(vpluecke) 

vpl <- readRDS("vp.rds") # Not related
View(vpl)

exit_row <- readRDS("exit.rds") # Not related
View(exit_row)

ev <-readRDS("ev20.rds") # Not related
View(ev)

bg <- readRDS("bgvp.rds") # Not related
View(bg)

bgpluecke <- readRDS("bgpluecke.rds") # Not related
View(bgpluecke)

bgpkal <- readRDS("bgpkal.rds") # Not related
View(bgpkal)

bgp <- readRDS("bgp.rds") # On Asylum Experience and Decision
View(bgp)

bgpgen <- readRDS("bgpgen.rds") # Not related
View(bgpgen)

bgpequiv <- readRDS("bgpequiv.rds")
View(bgpequiv)


bh <- readRDS("bh.rds")
View(bh)
#_______#

library(rio)

data <- import("pl_2016_2018.Rdata")
View(data)