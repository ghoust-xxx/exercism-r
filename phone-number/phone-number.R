parse_phone_number <- function(number_string) {
    digits <- strsplit(number_string, "")[[1]]
    i <- 0
    out <- ""
    for (x in digits) {
        if (x == "+" || x == " " || x == "(" || x == ")" || x == "." || x == "-") {
            next
        }
        if (!grepl("[0-9]", x)[1]) {
            return(NULL)
        }
        if (i == 0 && x == "1") {
            next
        }
        if ((i == 0 || i == 3) && (x == "0" || x == "1")) {
            return(NULL)
        }
        out <- paste0(out, x)
        i <- i + 1
    }
    if (nchar(out) != 10) {
        return(NULL)
    }

    return(out)
}
