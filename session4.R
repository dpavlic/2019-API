library(tidyverse)

e <- read_csv('data/14100064.csv') %>% 
  # You can rename the variables in a selection.
  select(
    year = REF_DATE,
    geography = GEO, 
    wage_type = Wages, 
    type_work = "Type of work", 
    naics = "North American Industry Classification System (NAICS)", 
    sex = Sex, 
    age_group = "Age group",
    uom = UOM,
    value = VALUE
  )

er <- e %>% 
  filter(
    year %in% 2011:2016,
    geography == 'Canada',
    wage_type == 'Median hourly wage rate',
    type_work == 'Full-time employees',
    uom == 'Current dollars',
    sex != "Both sexes",
    age_group == "25 to 54 years"
  ) %>% 
  select(-geography, -wage_type, -type_work, -uom, -age_group) %>% 
  arrange(-value)

wide <- er %>% 
  spread(naics, value)

long <- wide %>% 
  gather(naics, value, -year, -sex)

ggplot(er, aes(year, value)) +
  geom_point(aes(color = naics, shape = sex))

# Lots of information... but who can make sense of it??!!!
# Keep messaging simple, to the point!
er %>% 
  filter(naics == 'Total employees, all industries') %>% 
  ggplot(aes(year, value)) +
    geom_line()
  
# What happened here?...
er %>% 
  filter(naics == 'Total employees, all industries') %>% 
  ggplot(aes(year, value, group = sex, color = sex)) +
    geom_line() +
    # Keep your scales always in mind!
    scale_y_continuous(limits = c(0, 35)) +
    # Quick and dirty...
    #xlab('Year') 
    #ylab('Median Wage') +
    # OR...
    labs(x = 'Year', y = 'Median Hourly Wage', color = '') +
    theme_bw() +
    theme(legend.position = 'bottom') 
 
bars <- e %>% 
  filter(
    year %in% 2011:2016,
    geography == 'Canada',
    type_work == 'Full-time employees',
    wage_type == 'Total employees, all wages',
    sex == "Both sexes",
    age_group == "25 to 54 years",
    uom == 'Persons',
    naics != 'Total employees, all industries'
  ) 

ggplot(bars, aes(year, value, fill = naics)) + 
  geom_bar(stat = 'identity', position = 'dodge')

# LESS IS OFTEN MORE.
bars %>% 
  filter(year == 2016) %>% 
  ggplot(aes(naics, value)) + 
  geom_bar(stat = 'identity') +
  coord_flip() +
  labs(y = 'Employees (000s)', x = '')

# One cool trick... actually... two cool tricks...
bars %>% 
  filter(year == 2016) %>% 
  arrange(-value) %>% 
  mutate(naics2 = factor(naics, naics)) %>% 
  ggplot(aes(naics2, value / 1000)) +
    geom_bar(stat = 'identity') +
    coord_flip() +
    labs(y = 'Employees (millions)', x = '')
