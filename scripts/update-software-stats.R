# Fetch monthly download statistics for the packages in data/software.yml.
#
# Bioconductor: number of distinct IP addresses that downloaded the package
#   each month (https://bioconductor.org/packages/stats/).
# CRAN: number of downloads each month from the Posit CRAN mirror
#   (https://cranlogs.r-pkg.org).
#
# Writes data/software-stats.json. Run from the project root:
#   Rscript scripts/update-software-stats.R

library(jsonlite)
library(yaml)

months <- 12
out_file <- "data/software-stats.json"

this_month <- as.Date(format(Sys.Date(), "%Y-%m-01"))
first_month <- seq(this_month, by = "-1 month", length.out = months + 1)[months + 1]

fetch_bioc <- function(pkg) {
    url <- sprintf("https://bioconductor.org/packages/stats/bioc/%1$s/%1$s_stats.tab", pkg)
    tab <- read.delim(url, colClasses = "character")
    tab <- tab[tab$Month != "all", ]
    tab$date <- as.Date(sprintf("%s-%02d-01", tab$Year, match(tab$Month, month.abb)))
    tab <- tab[tab$date >= first_month & tab$date < this_month, ]
    tab <- tab[order(tab$date), ]
    list(month = format(tab$date, "%Y-%m"), downloads = as.integer(tab$Nb_of_distinct_IPs))
}

fetch_cran <- function(pkg) {
    url <- sprintf("https://cranlogs.r-pkg.org/downloads/daily/%s:%s/%s",
                   first_month, this_month - 1, pkg)
    days <- fromJSON(url)$downloads[[1]]
    month <- substr(days$day, 1, 7)
    totals <- tapply(days$downloads, month, sum)
    list(month = names(totals), downloads = as.integer(totals))
}

software <- read_yaml("data/software.yml")
pkgs <- Filter(function(p) p$repository %in% c("Bioconductor", "CRAN") && p$type == "Package", software)

stats <- lapply(pkgs, function(p) {
    fetch <- if (p$repository == "CRAN") fetch_cran else fetch_bioc
    st <- tryCatch(fetch(p$name), error = function(e) NULL)
    if (is.null(st) || !length(st$downloads)) {
        warning("No statistics for ", p$name)
        return(NULL)
    }
    c(st, source = p$repository)
})
names(stats) <- vapply(pkgs, `[[`, character(1), "name")
stats <- Filter(Negate(is.null), stats)

write_json(
    list(updated = format(Sys.Date()), packages = stats),
    out_file, auto_unbox = TRUE, pretty = TRUE
)
message("Wrote statistics for ", length(stats), " packages.")
