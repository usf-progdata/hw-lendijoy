library(dplyr)
library(tidyr)
library(tibble)

dat_bfi <- psychTools::bfi
key_bfi <- psychTools::bfi.keys

head(dat_bfi)


# ==================================================

# getting rid of rownames
dat_bfi <- dat_bfi %>%
  rownames_to_column(var = ".id")

head(dat_bfi)
filter(dat_bfi, .id == 61617)


# ==================================================

# converting between data.frame and tibble

## data.frame (base R)
psychTools::bfi

## tibble (tibble/tidyverse)
as_tibble(psychTools::bfi, rownames = ".id")

dat_bfi <- as_tibble(dat_bfi)

print(dat_bfi, n = 30)

## tibbles are nice, but some packages (e.g., lavaan) don't play well with them
## In that case, convert back to data.frame
as.data.frame(dat_bfi)


# ==================================================

# recode()
select(dat_bfi, .id, gender, education)

## Let's recode the categorical variables
dict <- psychTools::bfi.dictionary %>%
  as_tibble(rownames = "item")

# Remember how mutate() and summary() have the form:
#   mutate(.data, new_column = computation)
#
# recode() is backwards:
#   recode(.x, old = new)

dat_bfi %>%
  mutate(
    gender = recode(gender, "1" = "man", "2" = "female")
  ) %>%
  select(.id, gender, education)

## note that for numeric values, you need to wrap them in "quotes" or `backticks`
## That's not necessary for character values
palmerpenguins::penguins %>%
  mutate(sex = recode(sex, male = "Male", female = "Female"))

## Let's look at a few more recode options
?recode

dat_bfi %>%
  mutate(
    education = recode(education, "1" = "Some HS", "2" = "HS", "3" = "Some College", "4" = "College", "5" = "Graduate degree")
  ) %>%
  select(.id, gender, education)

## Let's say we want just "HS or less" versus "more than HS"
dat_bfi %>%
  mutate(
    education = recode(education, "1" = "HS", "2" = "HS", .default = "More than HS")
  ) %>%
  select(.id, gender, education)

## Let's say we want to convert NA values to an explict value
dat_bfi %>%
  mutate(
    education = recode(education, "1" = "HS", "2" = "HS", .default = "More than HS", .missing = "(Unknown)")
  ) %>%
  select(.id, gender, education)

# tidyr::replace_na()

## If we just want to replace NA values, use `tidyr::replace_na()`

dat_bfi %>%
  mutate(
    education = replace_na(education, replace = "(Unknown)")
  ) %>%
  select(.id, gender, education)




# reverse coding variables
print(dict, n = 30)

reversed <- c("A1", "C4", "C5", "E1", "E2", "O2", "O5")

reversed <- dict %>%
  filter(Keying == -1) %>%
  pull(item)

dat_bfi %>%
  mutate(A1r = recode(A1, "6" = 1, "5" = 2, "4" = 3, "3" = 4, "2" = 5, "1" = 6)) %>%
  select(A1, A1r)


dat_bfi %>%
  mutate(A1r = 7 - A1) %>%
  select(A1, A1r)

dat_bfi %>%
  mutate(
    across(all_of(reversed),
           ~ recode(.x, "6" = 1, "5" = 2, "4" = 3, "3" = 4, "2" = 5, "1" = 6),
           .names = "{.col}r")
  ) %>%
  select(A1, A1r)


# Now you try:

## 1. Use the psychTools::bfi (or psych::bfi) data
## 2. Recode gender to 'man', 'women', '(no response)'
## 3. Recode education to "Some HS", "HS", "Some College", "College", "Graduate degree", "(no response)"
## 4. Compute a new variable `hs_grad` with levels "no" and "yes"
## 5. Reverse code the -1 items, as indicated in psychTools::bfi.dictionary or psych::bfi.key
