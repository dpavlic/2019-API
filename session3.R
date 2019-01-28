# Loading up a file. We will start with .csv files.
# Create a data directory in your project file and then put
# transfer_payments_treasury_board.csv into it. Now let's load it.
# https://raw.githubusercontent.com/dpavlic/2019-API/master/data/transfer_payments_treasury_board_s3.csv

library(tidyverse)

# read_csv NOT!!! read.csv
tr <- read_csv('data/transfer_payments_treasury_board_s3.csv')

unique(tr$organization)
unique(tr$transfer_payment_type)
unique(tr$transfer_payment_name)

# or in a table...
tr %>% 
  distinct(organization)

# Review on the new dataset.
ncol(tr)
nrow(tr)
dim(tr)

tr2 <- tr %>% 
  filter(
    transfer_payment_type == 'Grant' | transfer_payment_type == 'Contribution'
  ) %>% 
  select(organization, transfer_payment_type, exp_1718) %>% 
  mutate(exp_1718_mil = exp_1718 / 10000000) %>% 
  arrange(organization, -exp_1718_mil)

# Quick tips on making this slightly simpler. The %in% operator.
tr2_alt <- tr %>% 
  filter(transfer_payment_type %in% c('Grant', 'Contribution')) %>% 
  select(organization, transfer_payment_type, exp_1718) %>% 
  mutate(exp_1718_mil = exp_1718 / 10000000) %>% 
  arrange(organization, -exp_1718_mil)

# You'll get the same thing.
all.equal(tr2, tr2_alt)

# Grouping and summarizing data
tr_g <- tr %>% 
  group_by(transfer_payment_type)

is_grouped_df(tr)
is_grouped_df(tr_g)

# Summaries
tr %>% 
  summarize(mean_exp1718 = mean(exp_1718))

# Some other examples?
# We can look at summarize help for inspiration. n(), min, max, median, mode
# Some examples?
tr %>% 
  summarize(
    count = n(),
    min_exp1718 = min(exp_1718),
    mean_exp1718 = mean(exp_1718),
    max_exp1718 = max(exp_1718)
  )

# Where this type of thing really shines is when things are grouped. Let them
# come up with grouping examples.
tr %>% 
  group_by(transfer_payment_type) %>% 
  summarize(
    count = n(),
    min_exp1718 = min(exp_1718),
    mean_exp1718 = mean(exp_1718),
    max_exp1718 = max(exp_1718)
  )

# Gathering 
tr_long <- tr %>% 
  gather(exp_year, exp_value, contains('exp_')) 

# Spreading... let's go back.
# What happens if we don't group by?
tr_wide <- tr_long %>% 
  group_by(organization, transfer_payment_type, transfer_payment_name) %>% 
  spread(exp_year, exp_value) %>% 
  ungroup

all.equal(tr, tr_wide)

# need some gather / spread examples.