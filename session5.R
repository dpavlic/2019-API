library(tidyverse)

ef <- read_csv('data/earnings_female_S.csv')
em <- read_csv('data/earnings_male_S.csv')

ef
em

# Let's do some basic review on the ef summary data.
# Filter for only engineering students
# Turn the wide data to long tidy data.
# Provide total + mean earnings by year.
ef %>%
  filter(field == 'Engineering') %>%
  gather(year, earnings, starts_with('yr_')) %>%
  group_by(year) %>%
  summarize(
    total_earnings = sum(earnings),
    mean_earnings = mean(earnings)
  )

# What happened here?... there are NAs in data. We will cover this in more
# detail later, but computation on NAs will produce... NAs.
ef %>%
  filter(field == 'Engineering') %>%
  gather(year, earnings, starts_with('yr_')) %>%
  group_by(year) %>%
  summarize(
    total_earnings = sum(earnings, na.rm = TRUE),
    mean_earnings = mean(earnings, na.rm = TRUE)
  )
# or filter...

# 3 (well 4) new things to learn:
  # 1. bind_rows() binds / STACKS a collection or a list of data frames together
  # 2. case_when() allows us to introduce conditional logic.
  # 3. str_replace_all() replaces portions of a string with another value.
  # 4. as.integer() coerces string to integer (max 2.147 billion)
  #    TIP: use as.double() if you need more than that...
bind_rows(em, ef)

# what is the issue here ... we have a male column and a female column, how can
# we get rid of that...?
# case_when
efm <- bind_rows(ef, em) %>%
  mutate(
    sex = case_when(
      # Short circuit logic. First one takes all.
      female == 1 ~ 'Female',
      male == 1 ~ 'Male'
      # Optional
      # TRUE ~ 'some_value'
      # This is a catch all -- if you do not match in the above list... return
      # this value.
    )
  ) %>%
  select(-female, -male) %>%
  gather(year, earnings, starts_with('yr_')) %>%
  mutate(
    year = str_replace(year, fixed('yr_'), ''),
    year = as.integer(year)
  )

efm %>%
  filter(field == 'Engineering') %>%
  group_by(year, sex) %>%
  summarise(earnings = mean(earnings, na.rm = TRUE)) %>%
  ungroup() %>%
  # We will divide the earnings in order to give a cleaner scale, and use sex
  # for both group and colour. The final bit will be then to facet the results
  # by field. Then on to graph tidying.
  ggplot(aes(year, earnings / 1000, colour = sex, group = sex)) +
  geom_line() +
  scale_y_continuous(limits = c(25, 100)) +
  ggtitle('Earnings of University Graduates') +
  labs(x = 'Year', y = 'Earnings (thousand $)') +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.1, 0.9),
    legend.background = element_blank(),
    legend.key = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

# Now again with fields...
efm %>%
  group_by(year, field, sex) %>%
  summarise(earnings = mean(earnings, na.rm = TRUE)) %>%
  ungroup() %>%
  # We will divide the earnings in order to give a cleaner scale, and use sex
  # for both group and colour. The final bit will be then to facet the results
  # by field. Then on to graph tidying.
  ggplot(aes(year, earnings / 1000, colour = sex, group = sex)) +
  geom_line() +
  facet_wrap(~ field) + # nrow, ncol
  scale_y_continuous(limits = c(25, 100)) +
  ggtitle('Earnings of University Graduates') +
  labs(x = 'Year', y = 'Earnings (thousand $)') +
  theme_bw() +
  theme(
    legend.title = element_blank(),
    legend.position = c(0.1, 0.9),
    legend.background = element_blank(),
    legend.key = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )
