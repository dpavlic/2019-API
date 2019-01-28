# Create a New Project ---------------------------------------------------------
# File -> New Project -> analytics_public_policy
# Create a data directory...
# Call the first script lesson_1.R

# Style guide ------------------------------------------------------------------
# YouCANMAnaGEWITHOUTaSTYLEguidebutITGETSmessy... Here is a link. You do not
# need to use exactly this style, but always _STRIVE_ to make your code as
# readable and presentable as possible. You _WILL_ be marked on this.
# https://style.tidyverse.org/

# Libraries & Data -------------------------------------------------------------
library(gapminder)
library(tidyverse)

# Although gapminder is loaded, it is 'hidden' from view at
# the moment.
# Remember: we can use the data() function to show us what
#           datasets are available to us.
gap <- gapminder

# What does this look like. Also look at the Data generated in the Environment
# column. While here, also notice how the History column fills up with
# information. Also, note that we can remove all objects we've created with
# the broom icon... OR use the rm() function.
View(gap)
colnames(gap) #or
names(gap)
ncol(gap)
nrow(gap)
dim(gap)

# dplyr basics -----------------------------------------------------------------
# We will use Excel to drive home how this works.

# To choose only certain rows based on the condition, we use dplyr's filter
# function. As with any dplyr function, the first argument to filter is the
# dataset you want to operate on. This is followed by the conditions on which
# to filter on. 
# NOTE THE DOUBLE EQUALS.
gap_africa <- filter(gap, continent == 'Africa')

# What about multiple conditions?
gap_africa <- filter(gap, continent == 'Africa')
gap_africa1980 <- filter(gap_africa, year > 1980)

# or... do it all in one go with the AND (&) operator.
gap_africa1980 <- filter(gap, continent == 'Africa' & year > 1980)

# there is also the OR (|) operator. Let's select year 1952 OR 1957.
gap_1950s <- filter(gap, year == 1952 | year == 1957)

# To choose only certain columns with dplyr, use the select function. As before
# the first argument is the name of the dataset, followed by columns you want
# to select.
gap_somecols <- select(gap, country, continent, year)

# You can also use - to choose which columns NOT to select. This will return
# everything EXCEPT the country and continent columns
gap_somecols2 <- select(gap, -country, -continent)

# To add columns with dplyr you use the mutate function. Once again, the first
# argument is the dataset we want to operate on (add a column to), followed by
# one or more columns we want to add. The example below adds a column pop2 to
# the dataset.
gap2 <- mutate(gap, pop2 = pop * 2)

# What about sorting data? We can use arrange.
gap_sorted_ascending <- arrange(gap, year)
# _or_ descending
gap_sorted_descending <- arrange(gap, -year)

# The pipe operator allows us to chain dplyr operations together. It looks
# like this %>% and the rule of the pipe operator is this: the object on the
# left-hand side (for our purporses, the dataset on the left-hand side) is
# passed as the FIRST argument of the function of the right-hand side. That
# is why all dplyr functions ask for data as the first argument, so we can
# efficiently chain operations together.

# This is how it looks without the pipe operator
gap_africa <- filter(gap, continent == 'Africa')

# And with the pipe operator:
gap_africa <- gap %>% 
  filter(continent == 'Africa')

# Of course, in that example, there is no advantage to the pipe. The
# advantage starts to make itself more apparent when we need to do several
# operations at once. Here, we take the original gap dataset, choose only
# the rows where continent is Africa, select the pop and continent columns
# and finally create a new column pop2, all in one 'go'.
gap_pipe <- gap %>%
  filter(continent == 'Africa') %>%
  select(pop, continent) %>%
  mutate(pop2 = pop * 2)

# Let's now do a quick exercise. Filter the gapminder dataset to Europe or
# Asia, year 1952, 1962, 1972, 1992, 2002. Select country, continent, year,
# pop colums, leaving out lifeExp and gdpPercap. Sort by year, descending.
gap %>% 
  filter(continent == 'Europe') %>% 
  filter(
    year == 1952 | year == 1962 | year == 1972 | year == 1992 | year == 2002
  ) %>% 
  select(country, continent, year, pop) %>% 
  arrange(-year)
