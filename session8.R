library(tidyverse)

# POP QUIZ 2 ###################################################################
exp <- read_csv('data/exports_naics_3341-3342.csv', skip = 8, na = ' ')

exps <- exp %>%
  slice(1:222) %>%
  rename(country = X1) %>%
  gather(year, dollars, -country) %>%
  mutate(year = as.integer(year))

top4 <- exps %>%
  filter(year == 2016) %>%
  arrange(-dollars) %>%
  filter(country != 'Sub-Total') %>%
  slice(2:5) %>%
  pull(country)

exps %>%
  filter(country %in% top4) %>%
  mutate(dollars_mil = dollars / 1e6) %>%
  select(-dollars) %>%
  ggplot(aes(year, dollars_mil)) +
  geom_line() +
  facet_wrap(~ country) +
  labs(x = 'Year', y = 'Dollars ($ Million)') +
  ggtitle('Computer and Peripheral Manufacturing Exports') +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

# CONTINUING FUNCTIONS #########################################################

# Let's load some trade data.
file_path1 <- 'data/exports_naics_3341-3342.csv'
file_path2 <- 'data/exports_naics_3343-3344.csv'
file_path3 <- 'data/exports_naics_3345-3346.csv'

d1 <- read_csv(file_path1, skip = 8, na = ' ')
d2 <- read_csv(file_path2, skip = 8, na = ' ')
d3 <- read_csv(file_path3, skip = 8, na = ' ')

# We have to do this 3 times and every time for any trade data we get from the
# government, but we will always use arguments skip = 8, and
# na.strings = ' '. Well we can define our own function. For example:
read_trade <- function(file_path) {
  read_csv(file_path, skip = 8, na = ' ')
}

# Then we can just do:
d1 <- read_trade(file_path1)
d2 <- read_trade(file_path2)
d3 <- read_trade(file_path3)

# This is still quite bad... The solution? map()
# map applies the function to each member of a collection (vector, list, etc)
# and returns the result in a list.
trade_files <- list.files('data', full.names = TRUE) %>% 
  str_subset('naics')

# We can now map these into a list of dataframes and then bind the rows.
trade_dfs <- map(trade_files, read_trade) %>% 
  bind_rows

# or all in one go; map_dfr essentially is the same thing but binds the rows
# for you because it is such a common operation.
trade_dfs <- map_dfr(trade_files, read_trade)
