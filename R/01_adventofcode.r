# Advent of code 2020
# Francesc Català-Moll

# In this list, the two entries that sum to 2020 are 1721 and 299. Multiplying
# them together produces 1721 * 299 = 514579, so the correct answer is 514579.
#
# Of course, your expense report is much larger. Find the two entries that sum
# to 2020; what do you get if you multiply them together?

library(tidyverse)

dat <- read_tsv("data/inputdata_01.txt", col_names = "values") %>% pull()

checked <- c()
for (it in seq_along(dat)) {
    diff <- 2020 - dat[it]
    inters <- intersect(diff, checked)

    if (length(inters) == 0) {
        checked[it] <- dat[it]
    } else {
        print(glue::glue("result: {dat[it]} + {inters} = {dat[it] + inters}"))
        print(glue::glue("result: {dat[it]} * {inters} = {dat[it] * inters}"))
        break
    }
}






