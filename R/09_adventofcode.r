
# Part 1 ------------------------------------------------------------------

# The first step of attacking the weakness in the XMAS data is to find the first
# number in the list (after the preamble) which is not the sum of two of the 25
# numbers before it. What is the first number that does not have this property?

library(tidyverse)
library(glue)

detect_no_property <- function(dat, window) {
    skip <- 0
    result <- 0
    for (i in (window + 1):nrow(dat)) {
        inp <- dat %>% pull(values)
        seq <- inp %>% .[(i-window):(i-1)]
        for (z in seq_along(seq)) {
            rest <- inp[i] - seq[z]
            if (length(intersect(rest, seq[-z])) != 0) {
                skip <- 1
                break
            }
            if (skip != 0 & z == window) { result <- inp[i] }
        }
        if (result != 0) { break }
    }
    result
}

read_csv("data/inputdata_09.txt", col_names = "values") %>%
    detect_no_property(window = 25)

# Part 2 ------------------------------------------------------------------

# In this list, adding up all of the numbers from 15 through 40 produces the
# invalid number from step 1, 127. (Of course, the contiguous set of numbers in
# your actual list might be much longer.)
#
# To find the encryption weakness, add together the smallest and largest number
# in this contiguous range; in this example, these are 15 and 47, producing 62.
#
# What is the encryption weakness in your XMAS-encrypted list of numbers?

encript_weakness <- function(dat, window = 25) {
    expected <- dat %>% detect_no_property(window = window)
    values <- dat %>% pull(values)

    for (i in 1:(length(values) - 1)) {
        sum <- values[i]
        it <- i
        list <- values[i]
        while(sum < expected) {
            it <- it + 1
            sum <- sum + values[it]
            list <- c(list, values[it])
        }

        if (sum == expected & it != i) {
            res <- min(list) + max(list)
            break
        }

    }
    res
}

read_csv("data/inputdata_09.txt", col_names = "values") %>%
    encript_weakness()









