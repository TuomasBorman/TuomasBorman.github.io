# Update the publication list from ORCID.
#
# 1. Fetch the list of works from the public ORCID API.
# 2. For every work with a DOI, fetch BibTeX and CSL-JSON metadata from doi.org.
# 3. Write data/publications.bib (for download) and data/publications.json
#    (used to render publications.qmd).
#
# Run from the project root:  Rscript scripts/update-publications.R

library(httr2)
library(jsonlite)

orcid <- "0000-0002-8563-8884"

# DOIs that are listed in ORCID but should not appear on the website.
exclude_dois <- character(0)

bib_file <- "data/publications.bib"
json_file <- "data/publications.json"

get <- function(url, accept) {
    request(url) |>
        req_headers(Accept = accept) |>
        req_user_agent("TuomasBorman.github.io publication updater") |>
        req_retry(max_tries = 3) |>
        req_perform()
}

# ---- 1. Works from ORCID ----------------------------------------------------
works <- get(
    sprintf("https://pub.orcid.org/v3.0/%s/works", orcid), "application/json"
) |> resp_body_json()

orcid_works <- lapply(works$group, function(g) {
    s <- g[["work-summary"]][[1]]
    ids <- g[["external-ids"]][["external-id"]]
    doi <- Filter(function(x) x[["external-id-type"]] == "doi", ids)
    list(
        doi = if (length(doi)) tolower(doi[[1]][["external-id-value"]]) else NA,
        title = s$title$title$value,
        year = s[["publication-date"]]$year$value %||% NA,
        journal = s[["journal-title"]]$value %||% NA,
        type = s$type
    )
})

# ---- 2. Metadata from doi.org -----------------------------------------------
fetch_work <- function(w) {
    if (is.na(w$doi)) {
        message("No DOI, using ORCID summary: ", w$title)
        return(list(
            csl = list(
                title = w$title, `container-title` = w$journal,
                issued = list(`date-parts` = list(list(as.integer(w$year)))),
                type = w$type
            ),
            bib = NULL
        ))
    }
    message("Fetching ", w$doi)
    url <- paste0("https://doi.org/", w$doi)
    csl <- tryCatch(
        get(url, "application/vnd.citationstyles.csl+json") |> resp_body_json(),
        error = function(e) NULL
    )
    bib <- tryCatch(
        get(url, "application/x-bibtex") |> resp_body_string(),
        error = function(e) NULL
    )
    if (is.null(csl)) {
        warning("Could not fetch metadata for ", w$doi)
        csl <- list(
            title = w$title, DOI = w$doi, `container-title` = w$journal,
            issued = list(`date-parts` = list(list(as.integer(w$year))))
        )
    }
    list(csl = csl, bib = bib)
}

orcid_works <- Filter(function(w) !w$doi %in% tolower(exclude_dois), orcid_works)
fetched <- lapply(orcid_works, fetch_work)

# ---- 3. Simplify and write --------------------------------------------------
first <- function(x) if (length(x)) unlist(x)[[1]] else NA

simplify <- function(csl) {
    date <- unlist(
        (csl$published %||% csl$issued %||% csl$created)$`date-parts`[[1]]
    )
    authors <- vapply(csl$author %||% list(), function(a) {
        trimws(paste(a$given %||% "", a$family %||% a$literal %||% ""))
    }, character(1))
    authors <- authors[nzchar(authors)]
    preprint <- identical(csl$type, "posted-content") ||
        identical(csl$subtype, "preprint") || identical(csl$type, "preprint")
    container <- first(csl$`container-title`)
    if (preprint && (is.na(container) || container == "")) {
        container <- csl$institution[[1]]$name %||% csl$publisher %||% NA
    }
    list(
        title = gsub("\\s+", " ", first(csl$title)),
        authors = authors,
        year = as.integer(date[1]),
        month = if (length(date) > 1) as.integer(date[2]) else NA,
        journal = container,
        volume = csl[["volume"]] %||% NA,
        issue = csl[["issue"]] %||% NA,
        pages = csl[["page"]] %||% NA,
        doi = tolower(csl[["DOI"]] %||% NA),
        type = if (preprint) "preprint" else csl[["type"]] %||% NA,
        preprint = preprint
    )
}

pubs <- lapply(fetched, function(x) simplify(x$csl))
ord <- order(
    -vapply(pubs, function(p) p$year %||% 0L, integer(1)),
    -vapply(pubs, function(p) ifelse(is.na(p$month), 0L, p$month), integer(1))
)
pubs <- pubs[ord]

write_json(
    list(orcid = orcid, updated = format(Sys.Date()), publications = pubs),
    json_file, auto_unbox = TRUE, pretty = TRUE, na = "null"
)

# Give every entry a unique, readable key, e.g. Borman2025HoloFoodR
bib_key <- function(p) {
    family <- sub(".* ", "", p$authors[1] %||% "Anon")
    word <- strsplit(p$title, "[^[:alnum:]]+")[[1]]
    word <- word[nzchar(word)][1]
    key <- paste0(family, p$year, word)
    gsub("[^A-Za-z0-9]", "", iconv(key, to = "ASCII//TRANSLIT"))
}
keys <- make.unique(vapply(pubs, bib_key, character(1)), sep = "_")
bibs <- vapply(fetched[ord], function(x) x$bib %||% "", character(1))
bibs <- trimws(sub("^(\\s*@\\w+\\{)[^,]*,", "\\1KEY,", bibs))
bibs <- mapply(function(b, k) sub("KEY,", paste0(k, ","), b, fixed = TRUE), bibs, keys)
bibs <- unname(bibs[nzchar(bibs)])
writeLines(c(
    sprintf("%% Publications of Tuomas Borman (ORCID %s)", orcid),
    "% Generated automatically by scripts/update-publications.R. Do not edit.",
    "",
    paste(bibs, collapse = "\n\n")
), bib_file)

message("Wrote ", length(pubs), " publications.")
