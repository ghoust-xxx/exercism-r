hamming <- function(strand1, strand2) {
    if (nchar(strand1) != nchar(strand2)) {
        stop("error")
    }
    if (nchar(strand1) == 0) {
        return(0)
    }
    s1 <- strsplit(strand1, "")[[1]]
    s2 <- strsplit(strand2, "")[[1]]
    cnt <- 0
    for (i in 1:nchar(strand1)) {
        if (s1[i] != s2[i]) {
            cnt <- cnt + 1
        }
    }

    cnt
}
