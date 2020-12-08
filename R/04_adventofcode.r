# Part 1 ------------------------------------------------------------------

# The fourth passport is missing two fields, cid and byr. Missing cid is fine,
# but missing any other field is not, so this passport is invalid. According to
# the above rules, your improved system would report 2 valid passports.
# Count the number of valid passports - those that have all required fields.
# Treat cid as optional. In your batch file, how many passports are valid?

porcess_lines <- function(dat) {
    out <- tibble()
    exp <- c(ecl = NA, pid = NA, eyr = NA, hcl = NA, byr = NA, iyr = NA, cid = NA, hgt = NA)
    len <- length(dat)
    for (i in seq_along(dat)) {
        if (str_count(dat[i]) == 0) {
            out <- bind_rows(out, exp)
            exp <- c(ecl = NA, pid = NA, eyr = NA, hcl = NA, byr = NA, iyr = NA, cid = NA, hgt = NA)
        } else if (i == len) {
            to_ingest <- dat[i] %>% str_split(":") %>% unlist() %>% str_split(" ") %>% unlist()
            for (z in seq(1, length(to_ingest), 2)) {
                exp[to_ingest[z]] <- to_ingest[z + 1]
            }
            out <- bind_rows(out, exp)
        } else {
            to_ingest <- dat[i] %>% str_split(":") %>% unlist() %>% str_split(" ") %>% unlist()
            for (z in seq(1, length(to_ingest), 2)) {
                exp[to_ingest[z]] <- to_ingest[z + 1]
            }
        }
    }
    out
}

read_lines("data/inputdata_04.txt") %>%
    porcess_lines() %>%
    select(-cid) %>%
    drop_na() %>%
    nrow()


# Part 2 ------------------------------------------------------------------

# Count the number of valid passports - those that have all required fields and
# valid values. Continue to treat cid as optional. In your batch file, how many
# passports are valid?

read_lines("data/inputdata_04.txt") %>%
    porcess_lines() %>%
    select(-cid) %>%
    mutate(unit = str_remove(hgt, "\\d+"),
           hgt = str_remove(hgt, unit),
           across(c(pid, eyr, hgt, iyr, byr), as.numeric)) %>%
    filter(byr >= 1920 & byr <= 2002) %>%
    filter(iyr >= 2010 & iyr <= 2020) %>%
    filter(eyr >= 2020 & eyr <= 2030) %>%
    filter(ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth")) %>%
    filter(floor(log10(pid)) + 1 == 9) %>%
    filter((unit == "cm" & hgt >= 150 & hgt <= 193) | (unit == "in" & hgt >= 59 & hgt <= 76)) %>%
    filter(str_detect(hcl, "^#")) & length(str_split(hcl, "") %>% unlist()) == 7) %>%
    drop_na() %>%
    nrow()







