library(tidyverse)

# CONTINUING FUNCTIONS #########################################################

# From last week...
read_trade <- function(file_path) {
  read_csv(file_path, skip = 8, na = ' ')
}

# Let's load some trade data.
tf <- map_dfr(list.files('data', 'naics', full.names = TRUE), read_trade)

# Great... except, we have a dangling read_trade function we are likely never
# going to use again. It's not something we will keep using over and over again.
# But we'll come back to that in a moment? What would we use again?

x <- c(1, 5, 10, 15, NA)
sum(x)
sum(x, na.rm = TRUE)

# Three dots in a function definition mean, any number of parameters, which
# we can then pass to another function.
sum_available <- function(...) {
  sum(..., na.rm = TRUE)
}
sum(x)
sum_available(x)

# Something like that... you may use a whole bunch of times. BUT, that is not
# what is at play with our trade example.
tf <- map_dfr(list.files('data', 'naics', full.names = TRUE), read_trade)

# It becomes a bit annoying to have to define a one-off function just to use it
# in a map call. But there is an answer to that. Anonymous functions!!!!
tf <- map_dfr(
  list.files('data', 'naics', full.names = TRUE),
  function(x) read_csv(x, skip = 8, na = ' ')
)

# You define an UNNAMED (anonymous) function right within and it works just
# fine. Note that you can curly brace it and have the function be multiple
# lines, but you don't have to if it's just one expression.
tf <- map_dfr(
  list.files('data', 'naics', full.names = TRUE),
  function(x) {
    read_csv(x, skip = 8, na = ' ')
  }
)

# Note also that you don't have to go through the trouble of writing function(x)
# either because this is such a common thing that map() provides a shortcut
# anonymous function that allows you to refer to the argument as either
# a dot, an .x, or ..1 preceded by a tilde
tf <- map_dfr(
  list.files('data', 'naics', full.names = TRUE),
  ~ read_csv(.x, skip = 8, na = ' ')
)

# Function pitfalls...
# Beware that in addition to a pure function, there is also an impure function.
# This function produces side effects and may not always produce the same set
# of outputs for a given set of inputs.
add_numbers_badly <- function(x, y) {
  x + y + z
}

add_numbers_badly(1, 2)

z <- 1

add_numbers_badly(1, 2)

z <- 4

add_numbers_badly(1, 2)

# Unless there's a very good reason, avoid writing impure functions. They
# introduce a rich source of bugs, and lots of potential head banging.

# Note that variables in the global scope can come inside the function, but
# this doesn't work the other way around.
rm(z)
scope_function <- function(x, y) {
  z <- 3
  x + y + 1
}

scope_function(1, 2)

# but:
z
# object 'y' not found. You defined it in the function but it does not leak.
# Scopes in R are lexical. We don't have to worry about this too much, but
# they 'float' up, where R will look up the value of the parent environment,
# not the calling environment.
# http://adv-r.had.co.nz/Environments.html for all you ever (did not want) to
# know about environments if you want light bed time reading you will not be
# tested on in this course.
y <- 5
f1 <- function(x) {
  y <- 1
  f2 <- function(z) {
    y + z
  }
  f2(3)
}

f1(5)

# WORKSHOP #####################################################################
tf <- map_dfr(
  list.files('data', 'naics', full.names = TRUE),
  ~ read_csv(.x, skip = 8, na = ' ')
)

View(tf)

# We have an actual category filled in as an observation? How do we fix that.
tf2 <- tf %>%
  rename(country = X1) %>%
  mutate(
    is_naics = str_detect(country, 'NAICS'),
    naics = if_else(is_naics, country, NA_character_)
  ) %>%
  fill(naics) %>%
  filter(
    !is_naics,
    !country %in% c('Sub-Total', '', 'Total All Countries'),
    !str_detect(country, 'Source')
  ) %>%
  select(-is_naics) %>%
  mutate(naics = word(naics, 4, -1)) %>%
  gather(year, dollars, -country, -naics) %>%
  mutate(dollars_mil = dollars / 1e9)

# Note we have NAs... Remember, when summarizing or doing graphs,
# this makes a difference.
