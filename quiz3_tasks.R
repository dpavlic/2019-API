# Load up housing starts

# Task 1
# Select:
#   GEO
#   GeoUID
#   Rename `Housing estimates` to housing_estimates
#   `Apartment and other unit types`
#   `Row units`
#   `Semi-detached units`
#   `Single-detached units`
#  Gather up:
#   `Apartment and other unit types`
#   `Row units`
#   `Semi-detached units`
#   `Single-detached units`
#  into a key 'unit_type' and 'starts' as values
#  filter housing_estimates to 'Housing starts'
#  remove housing_estimates

# Task 2
# Summarize total starts by group GEO
# arrange total, descending
# slice 4 largest
# pull GEO vector into a variable geo_filter

# Task 3
# Filter GEO to geo_filter (use %in%)
# Make a bar plot, x = unit type, y = starts
# facet by GEO
# Labels + title

