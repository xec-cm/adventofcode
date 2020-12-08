
# Part 1 ------------------------------------------------------------------

# For each group, count the number of questions to which anyone answered "yes".
# What is the sum of those counts?

preprocess <- function(dat) {
    res <- list()
    int <- list()
    group <- 1
    for (i in seq_along(dat)) {
        if (str_count(dat[i]) == 0) {
            res[[str_c("g_", group)]] <- int
            group <- group + 1
            int <- list()
        } else if (i == length(dat)) {
            word <- str_split(dat[i], "") %>% unlist()
            for (z in seq_along(word)) {
                int[word[z]] <- 1
            }
            res[[str_c("g_", group)]] <- int
        } else {
            word <- str_split(dat[i], "") %>% unlist()
            for (z in seq_along(word)) {
                int[[word[[z]]]] <- 1
            }
        }
    }
    res
}

read_lines("data/inputdata_06.txt") %>%
    preprocess() %>%
    map_dbl(function(x) reduce(x, sum)) %>%
    sum()

# Part 2 ------------------------------------------------------------------

# For each group, count the number of questions to which everyone answered "yes".
# What is the sum of those counts?

preprocess <- function(dat) {
    res <- list()
    int <- list()
    idiv <- 0
    for (i in seq_along(dat)) {
        if (str_count(dat[i]) == 0) {
            res[[str_c("indiv_",i,"_",idiv)]] <- int
            idiv <- 0
            int <- list()
        } else if (i == length(dat)) {
            word <- str_split(dat[i], "") %>% unlist()
            for (z in seq_along(word)) {
                int[[word[[z]]]] <- ifelse(word[[z]] %in% names(int), int[[word[[z]]]] + 1, 1)
            }
            idiv <- idiv + 1
            res[[str_c("indiv_",i,"_",idiv)]] <- int
        } else {
            word <- str_split(dat[i], "") %>% unlist()
            for (z in seq_along(word)) {
                int[[word[[z]]]] <- ifelse(word[[z]] %in% names(int), int[[word[[z]]]] + 1, 1)
            }
            idiv <- idiv + 1
        }
    }
    res
}

read_lines("data/inputdata_06.txt") %>%
    preprocess() %>%
    map2_dbl(names(.), function(x, name) {
        indiv <- str_remove(name, ".*_") %>% as.numeric()
        res <- x %>% unlist() %>% .[. == indiv] %>% length()
        res
    }) %>%
    sum()
