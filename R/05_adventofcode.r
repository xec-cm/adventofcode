# Part 1 ------------------------------------------------------------------

# As a sanity check, look through your list of boarding passes. What is the
# highest seat ID on a boarding pass?

binary_converter <- function(dat) {

    max_row <- 127
    min_row <- 0
    max_col <- 8
    min_col <- 0

    dat <- str_split(dat, "") %>% unlist()
    for (i in seq_along(dat)) {
        if (dat[i] == "F") {
            max_row <- round((max_row + min_row) / 2)
        } else if (dat[i] == "B") {
            min_row <- round((max_row + min_row) / 2)
        } else if (dat[i] == "R") {
            min_col <- round((max_col + min_col) / 2)
        } else if (dat[i] == "L") {
            max_col <- round((max_col + min_col) / 2)
        }
    }

    tibble(row = min_row, col = min_col, ID = row * 8 + col)
}

read_lines("data/inputdata_05.txt") %>%
    map_dfr(binary_converter) %>%
    pull(ID) %>%
    max()

# Part 2 ------------------------------------------------------------------

# Ding! The "fasten seat belt" signs have turned on. Time to find your seat.
#
# It's a completely full flight, so your seat should be the only missing boarding
# pass in your list. However, there's a catch: some of the seats at the very
# front and back of the plane don't exist on this aircraft, so they'll be missing
# from your list as well.
#
# Your seat wasn't at the very front or back, though; the seats with IDs +1 and
# -1 from yours will be in your list.
#
# What is the ID of your seat?

find_empty <- function(dat) {
    ids <- min(dat$ID):max(dat$ID)
    setdiff(ids, dat$ID)
}

read_lines("data/inputdata_05.txt") %>%
    map_dfr(binary_converter) %>%
    find_empty()





