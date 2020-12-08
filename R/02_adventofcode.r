library(tidyverse)

# Part 1 ------------------------------------------------------------------

# Each line gives the password policy and then the password. The password policy
# indicates the lowest and highest number of times a given letter must appear for
# the password to be valid. For example, 1-3 a means that the password must
# contain a at least 1 time and at most 3 times.
#
# In the above example, 2 passwords are valid. The middle password, cdefg, is
# not; it contains no instances of b, but needs at least 1. The first and third
# passwords are valid: they contain one a or nine c, both within the limits of
# their respective policies.
#
# How many passwords are valid according to their policies?

dat <- "data/inputdata_02.txt" %>%
    read_delim(delim = " ", col_names = c("exp_times", "letter", "psw")) %>%
    separate(exp_times, c("min", "max"), convert = TRUE) %>%
    mutate(letter = str_remove(letter, ":"))

dat %>%
    mutate(count = str_count(psw, letter)) %>%
    filter(count >= min & count <= max) %>%
    nrow()

# Part 2 ------------------------------------------------------------------

# Each policy actually describes two positions in the password, where 1 means
# the first character, 2 means the second character, and so on. (Be careful;
# Toboggan Corporate Policies have no concept of "index zero"!) Exactly one of
# these positions must contain the given letter. Other occurrences of the letter
# are irrelevant for the purposes of policy enforcement.
#
# How many passwords are valid according to the new interpretation of the
# policies?

checker <- function(str, pattern, pos1, pos2) {
    positions <- str_locate_all(str, pattern)[[1]][ ,1]
    len <- length(intersect(positions, c(pos1, pos2)))
    res <- ifelse(len == 1, 1, 0)
    res
}

dat %>%
    rowwise() %>%
    mutate(condition = checker(psw, letter, min, max)) %>%
    filter(condition == 1) %>%
    nrow()
