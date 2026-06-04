# plotting what the biomass looks like between years
library(dplyr)

# FWP hunting take ------------------------------------------------------------
# adding FWP hunting estimates

source("scripts/fwp_hunting_estimates.R")

# adding column for rolled over biomass form previous day
fwp_take <- daily_count %>%
  arrange(year, month, day) %>% 
  mutate(fwp_bms1 = if_else(!is.na(lag(year)), # if it isn't the first row, which doesn't have a previous row
                            if_else(year == lag(year),  # if the previous row is from the same winter
                                    final_take_bms + lag(final_take_bms) * 0.25, # multiple previous row by 0.25 and add as a rollover from previous day
                                    final_take_bms), #(ifelse2) else, just use the biomass for that day
                            final_take_bms))  #(ifelse1) else, just use the biomass for that day

# plotting biomass numbers
fwp_take %>% 
  ggplot(aes(x = as.Date(paste(2000, md, sep = "-")), y = fwp_bms1, 
             group = year, col = factor(year))) +
  geom_line()

# Tribal bison take --------------------------------------------------------------
# adding daily bison take values from NPS Bison Project
# days without recorded values are remaining as 0 since they didn't start survey efforts until bison moved out of the park and became available to take

# reading in daily take data
bison_take <- readr::read_csv("data/clean/bison_daily_take.csv") %>%
  rename(bison_take = take) %>% 
  mutate(
    date = mdy(date),
    md = format(date,"%m-%d"),
    # creating biomass column
    bison_biomass = bison_take*2.15,
    # creating winter year column
    winter_year = if_else(month(date) %in% c(11,12), year(date), year(date) - 1)) %>% 
  # creating slight rollover of biomass from previous day
  arrange(date) %>% 
  mutate(bison_bms1 = if_else(!is.na(lag(date)), # if it isn't the first row, which doesn't have a previous row
                              if_else(winter_year == lag(winter_year),  # if the previous row is from the same winter
                                      bison_biomass + lag(bison_biomass) * 0.25, # multiple previous row by 0.25 and add as a rollover from previous day
                                      bison_biomass), # (ifelse2) else, just use the biomass for that day
                              bison_biomass))  # (ifelse1) else, just use the biomass for that day

# plotting biomass numbers
# Create a sort key so December comes before Jan-March
df_plot <- bison_take %>%
  mutate(
    month = as.integer(substr(md, 1, 2)),
    sort_order = ifelse(month == 12, month - 12, month),  # Dec = 0, Jan = 1, Feb = 2, Mar = 3
    winter_year = as.factor(winter_year)
  ) %>%
  arrange(winter_year, sort_order)

# Set x-axis labels in chronological order
md_levels <- df_plot %>%
  arrange(sort_order, date) %>%
  pull(md) %>%
  unique()

df_plot <- df_plot %>%
  mutate(md = factor(md, levels = md_levels))

ggplot(df_plot, aes(x = md, y = bison_bms1, color = winter_year, group = winter_year)) +
  geom_line() +
  scale_x_discrete(breaks = md_levels[seq(1, length(md_levels), by = 7)]) +  # show every 7th label to avoid crowding
  labs(
    x = "Date (Month-Day)",
    y = "Bison BMS1",
    color = "Winter Year",
    title = "Bison BMS1 by Winter Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# plotting together -------------------------------------------------------

