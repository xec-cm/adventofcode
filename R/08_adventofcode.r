# Part 1 ------------------------------------------------------------------

# Run your copy of the boot code. Immediately before any instruction is executed
# a second time, what value is in the accumulator?

counter <- function(dat) {
    acc <- 0
    visited <- c()
    idx <- 1
    len <- length(dat)

    while (!idx %in% visited) {
        if (idx >= len) { break }
        visited <- c(visited, idx)
        word <- str_remove_all(dat[idx], " .*")
        value <- as.numeric(str_remove_all(dat[idx], ".* "))
        if (word == "nop") {
            idx <- idx + 1
        } else if (word == "jmp") {
            idx <- idx + value
        } else if (word == "acc") {
            acc <- acc + value
            idx <- idx + 1
        }
    }
    tibble(count = acc, idx)
}

read_lines("data/inputdata_08.txt") %>%
    counter()

# Part 2 ------------------------------------------------------------------

# Fix the program so that it terminates normally by changing exactly one jmp
# (to nop) or nop (to jmp). What is the value of the accumulator after the
# program terminates?

reparer <- function(dat) {
    out_df <- tibble()
    for (i in seq_along(dat)) {
        n_dat <- dat
        w <- n_dat[i]
        if (str_detect(w, "nop")) {
            n_dat[i] <- str_replace(n_dat[i], "nop", "jmp")
        } else if (str_detect(w, "jmp")) {
            n_dat[i] <- str_replace(n_dat[i], "jmp", "nop")
        }
        res <- counter(n_dat)
        out_df <- bind_rows(out_df, res)
    }
    out_df
}

read_lines("data/inputdata_08.txt") %>%
    reparer() %>%
    filter(idx == max(idx)) %>%
    pull(count)
