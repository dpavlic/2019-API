library(tidyverse)

################################################################################
# Back to basics. The language of R.
################################################################################

# Atomic Classes

# R has 5 basic atomic classes
# Logical, Integer, Numeric, Complex, Character

TRUE # and FALSE
1L
40.4
4i # Never going to come up.
"String"

# vector ---> We have implicitly dealt with this for a long time
# list ---> We have implicitly dealt with this for a long time
# matrix (won't talk about it here...)
# data frame ---> This is pretty much what we've dealt with
# factors

# Vectors
c(1, 2, 3)

# Vectors are... vectorized
c(1, 2, 3) + 1
c(1, 2, 3) + c(2, 3, 5)

c('Biz', 'Bla', 'Boo') %>%
  str_replace('B', 'F')

# Vectors are one type of a class.
c(TRUE, FALSE)
c("Hi", "Guys")
# However...
c(1, TRUE)
c(TRUE, 3, "Whoops")

# This leads to implicit conversion... That's a problem
TRUE > 0
3 > TRUE
"Hi" > 1

# ARRRRRRRRRRRRRRRRRRGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGGG. This is a bit of
# a design problem in R. Not much one can do about it except be aware of it.

# A list is an arbitrary collection. You don't need to have the same atomic
# classes within a list.
list(TRUE, 3, "Whoops")

# Lists can hold vectors of elemnts... or nested lists.
list(TRUE, c(1, 2, 3))
list(TRUE, c(1, 2, 3), list(1, 'BOO'))

# we can name elements of a list:
some_list <- list(a = TRUE, b = c(1, 2, 3))
some_list$b
# vs
some_list[[2]] # this is your only choice when a list is unnamed...

# matrix... we will not cover yet (if at all)

# Finally a data frame. A data frame is just a special list... one where
# all elements of the list are vectors of the same length.
df1 <- list(
  a = c(1, 2, 3),
  b = c('A', 'B', 'C')
) %>%
  as.data.frame()

df2 <- data.frame(a = c(1, 2, 3), b = c('A', 'B', 'C'))
all.equal(df1, df2)

# Data frames how column AND row names -- but tibbles of tidyverse dispense
# with rownames as an unnecessary thing.

# Factors
# As we discussed... factors are 'classifications' of things. Once set, they
# are no longer like strings.
c('A', 'B', 'C') %>%
  str_replace('A', 'Z')

# When we do it as a factor...
factor(c('A', 'B', 'C')) %>%
  str_replace('A', 'Z') # factor is lost

# Levels, if not specified, will automatically be an alphabetical list of
# factors passed.
factor(c('A', 'B', 'C'))

# UNLESS specified otherwise!
factor(c('A', 'B', 'C'), levels = c('B', 'C', 'A'))
# We can even specify levels that are not yet there...
factor(c('A', 'B', 'C'), levels = c('B', 'C', 'A', 'Z'))

# Let's look at a partical use of this again and break it down.
# (this is just copy - pasted) from before...
tbs <- read_csv('data/authorities_and_expenditures.csv')

spending_all_in_one <- tbs %>%
  gather(year, expenditures, starts_with('20')) %>%
  mutate(year = as.integer(year)) %>%
  group_by(organization, year) %>%
  summarize(expenditures = sum(expenditures)) %>%
  ungroup %>%
  filter(
    organization %in% c(
      filter(., year == 2017) %>%
        arrange(-expenditures) %>%
        slice(1:4) %>%
        pull(organization)
    )
  ) %>%
  mutate(expenditures_billion = expenditures / 1e9) %>%
  select(-expenditures)

spending_all_in_one %>%
  ggplot(aes(
    year, expenditures_billion, group = organization, color = organization
  )) +
  scale_y_continuous(limits = c(0, 100)) +
  geom_line() +
  labs(x = 'Year', y = 'Expenditures (Billion $)', color = '') +
  theme_bw() +
  theme(legend.position = 'bottom')

# But... we want the order of organizations to match the highest expenditures...
org_order <- spending_all_in_one %>%
  filter(year == 2017) %>%
  arrange(-expenditures_billion) %>%
  pull(organization)

spending_all_in_one %>%
  mutate(organization = factor(organization, levels = org_order)) %>%
  ggplot(aes(
    year, expenditures_billion, group = organization, color = organization
  )) +
  scale_y_continuous(limits = c(0, 100)) +
  geom_line() +
  labs(x = 'Year', y = 'Expenditures (Billion $)', color = '') +
  theme_bw() +
  theme(legend.position = 'bottom')

# all in one
spending_all_in_one %>%
  mutate(organization = factor(
    organization,
    levels = filter(., year == 2017) %>%
      arrange(-expenditures_billion) %>%
      pull(organization)
  )) %>%
  ggplot(aes(
    year, expenditures_billion, group = organization, color = organization
  )) +
  scale_y_continuous(limits = c(0, 100)) +
  geom_line() +
  labs(x = 'Year', y = 'Expenditures (Billion $)', color = '') +
  theme_bw() +
  theme(legend.position = 'bottom')

library(magrittr)
library(tidyverse)
library(stringr)

# Conditionals -----------------------------------------------------------------

# We start with a concept not entirely unfamiliar. The if condition. We have
# seen if variants already in mutate(), notably the ifelse, and the case_when.
# if functions around a similar principle.

# For example, let's check if a number is a multiple of 3. For example, is 2
# a multiple of 3? Let's check our grade 4 math. 3 goes into 2 zero times,
# giving us a remainder of 2.

4 %% 3

# But how can we say, if a number is a multiple of 3, print that a number is
# a multiple of 3.

if (4 %% 3 == 0) {
  print('Number is a multiple of 3.')
}

# This will return NOTHING, since 2 is not a multiple of 3. Maybe we want to
# add something to print if the number is not a multiple of 3.
if (4 %% 3 == 0) {
  print('Number is a multiple of 3.')
} else {
  print('Number is not a multiple of 3.')
}

# What if we want to check if a number is a multiple of 3, and if it is not,
# THEN check if it is a multiple of 2, then if it's neither, print it's not
# a multiple of either 2 or 3.
if (4 %% 3 == 0) {
  print('Number is a multiple of 3.')
} else if (4 %% 2 == 0) {
  print('Number is not a multiple of 3, but is a multiple of 2.')
} else {
  print('Number is not a multiple of 3 or 2.')
}

# This is all well and good, but we manually enter a number here. Let's say
# we go back to the original problem, and try to find out if the number is a
# multiple of 3? But we want to find the answer for any arbitrary number we
# input?

# Functions --------------------------------------------------------------------

# The answer to the quandary in the previous section is: FUNCTIONS. You know
# these things because you've been using them all along in R. Functions produce
# an output for a given set of inputs.

# Almost everything in R is a function. You know those operators you're using?
# Just sneakily hidden functions.
`+`(3, 2)
`*`(5, 10)
`:`(1, 5)

# Extra examples...
example <- c('a', 'b', 'c')
example[1]
`[`(example, 1)
`<-`(some_var, 'hello')
some_var

# The last line is returned, which also happens to be the first and only line
# of this function. The last evaluated line in R is automatically
# returned, or you can manually use the return function.
add_two_numbers <- function(x, y) {
  x + y
}

# OR, if this helps you:
add_two_numbers <- function(x, y) {
  return(x + y)
}

# Let's go back to our multiple example. We're going to create a new function
# is_multiple, which requires a number. In addition, we will create an OPTIONAL
# parameter, multiple, which specifies what multiple a number should be of,
# with a DEFAULT value of 2. This argument is therefore NOT required, and if
# not entered will default to 2. If a number is a multiple, it will return
# the number back, otherwise it will return a NA value.
is_multiple <- function(x, multiple = 2) {
  if (x %% multiple == 0) {
    x
  } else {
    NA
  }
}

is_multiple(2)
is_multiple(3)

# To change the multiple, you can do any of the following.
# If the optional argument name is not specified, arguments proceed by the
# order of the position the parameters are defined in.
is_multiple(3, multiple = 3)
is_multiple(multiple = 3, 3)
is_multiple(3, 3)
