# Advent of code 2020
# Francesc Català-Moll

library(tidyverse)
library(glue)

# Part 1 ------------------------------------------------------------------

# In this list, the two entries that sum to 2020 are 1721 and 299. Multiplying
# them together produces 1721 * 299 = 514579, so the correct answer is 514579.
#
# Of course, your expense report is much larger. Find the two entries that sum
# to 2020; what do you get if you multiply them together?


## =========================
## Option 1

dat <- read_tsv("data/inputdata_01.txt", col_names = "values") %>% pull()

checked <- c()
for (it in seq_along(dat)) {
    diff <- 2020 - dat[it]
    inters <- intersect(diff, checked)

    if (length(inters) == 0) {
        checked[it] <- dat[it]
    } else {
        print(glue("result: {dat[it]} + {inters} = {dat[it] + inters}"))
        print(glue("result: {dat[it]} * {inters} = {dat[it] * inters}"))
        break
    }
}

rm(list = ls())

## =========================
## Option 2

dat <- read_tsv("data/inputdata_01.txt", col_names = "values") %>% pull()
for (it in seq_along(dat)) {
    diff <- 2020 - dat[it]
    if (diff %in% dat) {
        print(glue("result: {dat[it] * diff}" ))
        break
    }
}

rm(list = ls())

# Part 2 ------------------------------------------------------------------

# Using the above example again, the three entries that sum to 2020 are 979, 366,
# and 675. Multiplying them together produces the answer, 241861950. In your
# expense report, what is the product of the three entries that sum to 2020?

library(tidyverse)

dat <- read_tsv("data/inputdata_01.txt", col_names = "values") %>% pull()

for (it in seq_along(dat)) {
    sum <- dat + dat[it]
    sum <- sum[sum < 2020]

    for (it2 in seq_along(sum)) {
        diff <- 2020 - sum[it2]
        if (diff %in% dat) {
            r <- c(dat[it], sum[it2] - dat[it], diff)
            break
        }
    }

    if (exists("r")) {
        print(glue("result: {r[1] * r[2] * r[3]}"))
        break
    }
}






