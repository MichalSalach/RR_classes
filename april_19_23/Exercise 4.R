#### Path ####
setwd("april_19_23")

#### Libraries ####
library(readxl)
library(Hmisc)
library(stringr)
library(dplyr)

#### Data ####

# Import data from the O*NET database, at ISCO-08 occupation level.
# The original data uses a version of SOC classification, but the data we load here
# are already cross-walked to ISCO-08 using: https://ibs.org.pl/en/resources/occupation-classifications-crosswalks-from-onet-soc-to-isco/

# The O*NET database contains information for occupations in the USA, including
# the tasks and activities typically associated with a specific occupation.

task_data <- read.csv("Data/onet_tasks.csv")
# isco08 variable is for occupation codes
# the t_* variables are specific tasks conducted on the job

# read employment data from Eurostat
# These datasets include quarterly information on the number of workers in specific
# 1-digit ISCO occupation categories. (Check here for details: https://www.ilo.org/public/english/bureau/stat/isco/isco08/)

for (i in 1:9) {
  var <- paste0("isco", i)
  sheet <- paste0("ISCO", i)
  assign(var, read_excel("Data/Eurostat_employment_isco.xlsx", sheet = sheet))
}

#### Parameters ####

countries <- c("Belgium", "Spain", "Poland", "Italy", "Sweden")

#### Data preparation ####

# This will calculate worker totals in each of the chosen countries.

for (country in countries) {
  total_country <- 0

  for (i in 1:9) {
    df <- get(paste0("isco", i))
    total_country <- total_country + df[, country]
  }

  assign(paste0("total_", country), total_country[, ])
}

# Let's merge all these datasets. We'll need a column that stores the occupation categories:

for (i in 1:9) {
  df <- get(paste0("isco", i))
  df[, "ISCO"] <- i
  assign(paste0("isco", i), df)
}

# and this gives us one large file with employment in all occupations.
all_data <- rbind(isco1, isco2, isco3, isco4, isco5, isco6, isco7, isco8, isco9)

# We have 9 occupations and the same time range for each, so we an add the totals by
# adding a vector that is 9 times the previously calculated totals

for (country in countries) {
  all_data[, paste0("total_", country)] <- rep(get(paste0("total_", country)), 9)

  # And this will give us shares of each occupation among all workers in a period-country:
  all_data[, paste0("share_", country)] <- all_data[, country] / all_data[, paste0("total_", country)]
}

# Now let's look at the task data. We want the first digit of the ISCO variable only

task_data$isco08_1dig <- str_sub(task_data$isco08, 1, 1) %>% as.numeric()

# And we'll calculate the mean task values at a 1-digit level
# (more on what these tasks are below)

aggdata <- aggregate(task_data,
  by = list(task_data$isco08_1dig),
  FUN = mean, na.rm = TRUE
)
aggdata$isco08 <- NULL

# Let's combine the data.

combined <- left_join(all_data, aggdata, by = c("ISCO" = "isco08_1dig"))

# Let's move a group-specific procedure to a function:

agg_data_by_group <- function(task_items, group_name) {
  tryCatch(
    {

      # Traditionally, the first step is to standardise the task values using weights
      # defined by share of occupations in the labour force. This should be done separately
      # for each country. Standardisation -> getting the mean to 0 and std. dev. to 1.
      # Let's do this for each of the variables that interests us.

      for (item in task_items) {
        for (country in countries) {
          var_name <- paste0("std_", country, "_t_", item)
          temp_mean <- wtd.mean(combined[, paste0("t_", item)], combined[, paste0("share_", country)])
          temp_sd <- wtd.var(combined[, paste0("t_", item)], combined[, paste0("share_", country)]) %>% sqrt()

          combined[, var_name] <- (combined[, paste0("t_", item)] - temp_mean) / temp_sd
        }
      }

      # The next step is to calculate the `classic` task content intensity, i.e.
      # how important is a particular general task content category in the workforce
      # Here, we're looking at non-routine cognitive analytical tasks, as defined
      # by David Autor and Darron Acemoglu:

      for (country in countries) {
        combined[paste0(country, "_", group_name)] <- 0
        for (item in task_items) {
          combined[paste0(country, "_", group_name)] <- combined[paste0(country, "_", group_name)] +
            combined[paste0("std_", country, "_t_", item)]
        }
      }

      # And we standardise group in a similar way.

      for (country in countries) {
        temp_mean <- wtd.mean(
          combined[, paste0(country, "_", group_name)],
          combined[, paste0("share_", country)]
        )
        temp_sd <- wtd.var(
          combined[, paste0(country, "_", group_name)],
          combined[, paste0("share_", country)]
        ) %>% sqrt()

        combined[, paste0("std_", country, "_", group_name)] <- (combined[, paste0(country, "_", group_name)] - temp_mean) / temp_sd

        # Finally, to track the changes over time, we have to calculate a country-level mean.

        # Step 1: multiply the value by the share of such workers.
        combined[, paste0("multip_", country, "_", group_name)] <- combined[, paste0("std_", country, "_", group_name)] * combined[, paste0("share_", country)]

        # Step 2: sum it up (it basically becomes another weighted mean)
        assign(
          paste0("agg_", country),
          aggregate(combined[, paste0("multip_", country, "_", group_name)],
            by = list(combined$TIME),
            FUN = sum,
            na.rm = TRUE
          )
        )

        # We can plot it now!
        agg_country <- get(paste0("agg_", country))
        plot(agg_country[, 2], xaxt = "n", ylab = paste("agg. multip.", country, group_name))
        axis(1, at = seq(1, 40, 3), labels = agg_country$Group.1[seq(1, 40, 3)])
      }
    },
    error = function(cond) stop("Wrong selection of categories.")
  )
}

#### Results ####

# We'll be interested in tracking the intensity of Non-routine cognitive analytical tasks
# Using a framework reminiscent of the work by David Autor ('4A2a4', '4A2b2', '4A4a1').

# Therefore, these are the categories we're primarily interested in:
# Non-routine cognitive analytical
# 4.A.2.a.4 Analyzing Data or Information
# 4.A.2.b.2	Thinking Creatively
# 4.A.4.a.1	Interpreting the Meaning of Information for Others

# These are some other categories:
# Routine manual
# 4.A.3.a.3	Controlling Machines and Processes
# 4.C.2.d.1.i	Spend Time Making Repetitive Motions
# 4.C.3.d.3	Pace Determined by Speed of Equipment

agg_data_by_group(c("4A2a4", "4A2b2", "4A4a1"), "NRCA")
agg_data_by_group(c("4A3a3", "4C2d1i", "4C3d3"), "RM")

# Get rid of unnecessary files:
rm(i, var, country, sheet, df)
