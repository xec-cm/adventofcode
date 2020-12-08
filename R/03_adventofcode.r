library(tidyverse)

# Part 1 ------------------------------------------------------------------

# Starting at the top-left corner of your map and following a slope of right 3
# and down 1, how many trees would you encounter?

dat <- read_lines("data/inputdata_03.txt") %>%
    map(function(x) {
         x %>%
            str_replace_all("#", "1") %>%
            str_replace_all("\\.", "0") %>%
            str_split("") %>%
            unlist() %>%
            as.numeric() %>%
            matrix(nrow = 1)
    }) %>%
    reduce(rbind)

count_trees <- function(dat, right, down) {
    dat <- round((nrow(dat) * right / ncol(dat)) + down) %>%
        seq_len() %>%
        map(function(x) { dat }) %>%
        reduce(cbind)

    r <- 1
    c <- 1
    res <- 0
    while(r <= nrow(dat)) {
        res <- res + dat[r, c]
        r <- r + down
        c <- c + right
    }
    res
}

count_trees(dat, 3, 1)

# Part 2 ------------------------------------------------------------------

# What do you get if you multiply together the number of trees encountered on
# each of the listed slopes?

list(c(1, 1), c(3, 1), c(5, 1), c(7, 1), c(1, 2)) %>%
    map_dbl(function(x) { count_trees(dat, x[[1]], x[[2]]) }) %>%
    prod()

