library(tidyverse)

tbs <- read_csv('data/authorities_and_expenditures.csv')

################################################################################
# POP Quiz
################################################################################

# Summarize total spending per year in billion $
# Make data into long.
# Group by year
# Then do a summarize.
# The as.integer() is bonus points...
total_spending <- tbs %>%
  gather(year, expenditures, starts_with('20')) %>%
  mutate(year = as.integer(year)) %>%
  group_by(year) %>%
  summarize(total_exp = sum(expenditures) / 1e9)

# Summarize selected spending of departments
# Make data into long
# Group by organization and year
# Ungroup (optional)
# Filter the following departments.
# Make a expenditures_billion column (scientific notation totally optional)
# Remove the plain 'vanilla' expenditures variable.
selected_spending <- tbs %>%
  gather(year, expenditures, starts_with('20')) %>%
  mutate(year = as.integer(year)) %>%
  group_by(organization, year) %>%
  summarize(expenditures = sum(expenditures)) %>%
  ungroup %>%
  filter(organization %in% c(
    'Department of Finance',
    'Department of Employment and Social Development',
    'Department of National Defence',
    'Department of Indian Affairs and Northern Development'
  )) %>%
  mutate(expenditures_billion = expenditures / 1e9) %>%
  select(-expenditures)

# Graph selected spending into a line graph, group by organization
# Fix scale to something more reasonable.
# Throw the leggend on the bottom.
selected_spending %>%
  ggplot(aes(
    year, expenditures_billion, group = organization, color = organization
  )) +
  scale_y_continuous(limits = c(0, 100)) +
  geom_line() +
  labs(x = 'Year', y = 'Expenditures (Billion $)', color = '') +
  theme_bw() +
  theme(legend.position = 'bottom')

################################################################################
# END QUIZ
################################################################################

# Some notes, the top was 'given' to you, 4 largest departments (in 2017).
# But how # would you find this information out without knowing it ahead of
# time?
spending_all <- tbs %>%
  gather(year, expenditures, starts_with('20')) %>%
  mutate(year = as.integer(year)) %>%
  group_by(organization, year) %>%
  summarize(expenditures = sum(expenditures)) %>%
  ungroup

spending_large_orgs <- spending_all %>%
  filter(year == 2017) %>%
  arrange(-expenditures) %>%
  # Slice says take row X to Y
  slice(1:4) %>%
  # Pull takes a data frame row into vector.
  pull(organization)

smart_spending <- spending_all %>%
  filter(organization %in% spending_large_orgs) %>%
  mutate(expenditures_billion = expenditures / 1e9) %>%
  select(-expenditures)

# You can do all of this in one go!
spending_all_in_one <- tbs %>%
  gather(year, expenditures, starts_with('20')) %>%
  mutate(year = as.integer(year)) %>%
  group_by(organization, year) %>%
  summarize(expenditures = sum(expenditures)) %>%
  ungroup %>%
  filter(
    organization %in% c(
	  # The dot . in a pipe means 'this object'. So not only does the pipe
	  # throw in the object as the first argument of the next function, it
	  # allows you to use the . to refer to it again elsewhere.
      filter(., year == 2017) %>%
        arrange(-expenditures) %>%
        slice(1:4) %>%
        pull(organization)
    )
  ) %>%
  mutate(expenditures_billion = expenditures / 1e9) %>%
  select(-expenditures)

