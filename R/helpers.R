# Helpers that turn the files in data/ into Markdown for the pages.
# The same Markdown is used for the HTML pages and the PDF (Typst) versions.

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x

is_pdf <- function() isTRUE(knitr::pandoc_to("typst"))

# Push the rest of a line to the right margin in the PDF (ignored in HTML).
pdf_fill <- "`#h(1em)#h(1fr)`{=typst}"

heading <- function(text, level = 2) cat(strrep("#", level), " ", text, "\n\n", sep = "")

icon <- function(name) {
    if (name == "orcid") {
        if (is_pdf()) return('`#box(baseline: 15%, image("assets/orcid.svg", height: 0.9em))`{=typst}')
        return('<img src="assets/orcid.svg" class="orcid-icon" alt="ORCID">')
    }
    if (is_pdf()) "" else sprintf('<i class="bi bi-%s"></i>', name)
}

# ---- Dates ------------------------------------------------------------------

# "2025", "2025-08" or "2025-08-25" -> "2025", "Aug 2025", "25 Aug 2025"
fmt_date <- function(x, day = TRUE) {
    x <- as.character(x)
    if (is.na(x) || x == "") return("")
    if (tolower(x) == "present") return("present")
    p <- as.integer(strsplit(x, "-")[[1]])
    if (length(p) == 1) return(as.character(p[1]))
    if (length(p) == 2 || !day) return(paste(month.abb[p[2]], p[1]))
    paste(p[3], month.abb[p[2]], p[1])
}

fmt_range <- function(start, end = NULL, day = TRUE) {
    start <- as.character(start %||% "")
    end <- as.character(end %||% "")
    if (end == "" || end == start) return(fmt_date(start, day))
    s <- strsplit(start, "-")[[1]]
    e <- strsplit(end, "-")[[1]]
    # Same month: "25–29 Aug 2025"
    if (day && length(s) == 3 && length(e) == 3 && all(s[1:2] == e[1:2])) {
        return(sprintf("%d–%s", as.integer(s[3]), fmt_date(end)))
    }
    paste(fmt_date(start, day), "–", fmt_date(end, day))
}

# Sortable number from a partial date, e.g. "2025-08" -> 20250800
date_key <- function(x) {
    if (tolower(as.character(x)) == "present") return(99999999)
    p <- as.integer(strsplit(as.character(x), "-")[[1]])
    p <- c(p, 0, 0)[1:3]
    p[1] * 10000 + p[2] * 100 + p[3]
}

# ---- Last updated -----------------------------------------------------------

# Date of the last git commit that changed any of `files`; uncommitted edits
# count as today.
last_updated_date <- function(files) {
    dates <- vapply(files, function(file) {
        run <- function(...) suppressWarnings(tryCatch(
            system2("git", c(...), stdout = TRUE, stderr = FALSE),
            error = function(e) character(0)
        ))
        git <- run("log", "-1", "--format=%cs", "--", file)
        dirty <- run("status", "--porcelain", "--", file)
        if (length(git) && nzchar(git[1]) && !length(dirty)) git[1] else format(Sys.Date())
    }, character(1))
    max(dates)
}

last_updated <- function(files) fmt_date(last_updated_date(files))

updated_note <- function(files) {
    cat(sprintf("[Last updated %s]{.updated}\n\n", last_updated(files)))
}

# ---- CV ---------------------------------------------------------------------

cv_header <- function(cv) {
    links <- vapply(cv$contact, function(c) {
        if (is_pdf()) sprintf("%s [%s](%s)", icon(c$icon), c$text, c$url)
        else sprintf('<a href="%s">%s %s</a>', c$url, icon(c$icon), c$text)
    }, character(1))
    if (is_pdf()) {
        cat(sprintf("**%s**\\\n", cv$headline), paste(links, collapse = " · "), "\n\n")
    } else {
        cat('<div class="cv-contact">', links, "</div>\n\n")
    }
}

cv_entry <- function(e) {
    period <- fmt_range(e$start, e$end, day = FALSE)
    org <- paste(c(e$org, e$location), collapse = ", ")
    cat("::: {.cv-entry}\n")
    cat(sprintf("[**%s**]{.cv-title} %s [%s]{.cv-date}\n\n", e$title, pdf_fill, period))
    if (length(org)) cat(sprintf("[%s]{.cv-org}\n\n", org))
    if (length(e$bullets)) cat(paste("-", unlist(e$bullets)), sep = "\n")
    cat("\n:::\n\n")
}

cv_sections <- function(cv, level = 2) {
    for (s in cv$sections) {
        heading(s$title, level)
        for (e in s$entries) cv_entry(e)
        if (length(s$items)) cat(paste("-", unlist(s$items)), sep = "\n")
        cat("\n\n")
    }
}

# Entries of a section of the standard CV, e.g. cv_entries(cv, "Education")
cv_entries <- function(cv, section, filter = NULL) {
    s <- Filter(function(s) s$title == section, cv$sections)[[1]]
    current <- vapply(s$entries, function(e) identical(e$end, "present"), logical(1))
    switch(filter %||% "all",
        current = s$entries[current],
        previous = , completed = s$entries[!current],
        s$entries
    )
}

# Horizontal timeline of work and education for the home page: one bar per
# entry on a shared year axis, so overlapping periods run in parallel.
timeline <- function(cv, sections = c("Work experience", "Education")) {
    # "2023-01" -> 2023.0; a year alone covers the whole year
    to_num <- function(x, end = FALSE) {
        x <- as.character(x %||% "present")
        if (x == "present") {
            d <- as.POSIXlt(Sys.Date())
            return(1900 + d$year + d$yday / 365)
        }
        p <- as.integer(strsplit(x, "-")[[1]])
        if (length(p) == 1) return(p + end)
        p[1] + (p[2] - 1 + end) / 12
    }
    entries <- lapply(sections, cv_entries, cv = cv)
    starts <- unlist(lapply(unlist(entries, recursive = FALSE), function(e) to_num(e$start)))
    from <- floor(min(starts))
    to <- floor(to_num("present")) + 1
    pos <- function(x) round(100 * (x - from) / (to - from), 2)

    n <- to - from
    years <- from:(to - 1)
    track <- function(x = "") sprintf('<div class="g-track">%s</div>', x)

    rows <- function(i) {
        kind <- if (sections[i] == "Education") "edu" else "work"
        for (e in entries[[i]]) {
            left <- pos(to_num(e$start))
            width <- pos(to_num(e$end, end = TRUE)) - left
            ongoing <- if (identical(e$end, "present")) " g-ongoing" else ""
            bar <- sprintf('<span class="g-bar g-%s%s" style="left:%s%%;width:%s%%"></span>',
                           kind, ongoing, left, width)
            cat(sprintf(
                '<div class="g-label" title="%s, %s: %s"><b>%s</b><span>%s</span></div>%s\n',
                e$title, e$org, fmt_range(e$start, e$end, day = FALSE),
                e$short %||% e$title, e$org, track(bar)
            ))
        }
    }
    group <- function(i) cat(sprintf('<div class="g-group">%s</div>%s\n', sections[i], track()))

    # First section above the year axis, second section below it
    cat(sprintf('<div class="gantt" style="--years:%d">\n', n))
    group(1)
    rows(1)
    axis <- paste(sprintf("<span>%s</span>", years), collapse = "")
    cat(sprintf('<div></div><div class="g-axis">%s</div>\n', axis))
    group(2)
    rows(2)
    cat("</div>\n\n")
}

# ---- Research Council of Finland CV -----------------------------------------

rcf_cv <- function(rcf, cv, level = 2) {
    for (s in rcf$sections) {
        heading(s$title, level)
        if (length(s$fields)) {
            for (f in s$fields) {
                value <- if (identical(f$value, "auto")) last_updated(rcf$date_from) else f$value
                cat(sprintf("%s: %s\\\n", f$name, value))
            }
            cat("\n")
        }
        if (length(s$use)) for (e in cv_entries(cv, s$use, s$filter)) cv_entry(e)
        for (e in s$entries) cv_entry(e)
        if (length(s$text)) cat(s$text, "\n\n")
        if (identical(s$auto, "teaching")) cat("-", teaching_summary(), "\n")
        if (length(s$items)) cat(paste("-", unlist(s$items)), sep = "\n")
        cat("\n\n")
    }
}

# ---- Publications -----------------------------------------------------------

me <- "Tuomas Borman"

# Publication classification of the Finnish Ministry of Education and Culture,
# used in Research Council of Finland publication lists.
pub_categories <- c(
    A = "Peer-reviewed scientific articles",
    B = "Non-peer-reviewed scientific writings",
    C = "Scientific books (monographs)",
    D = "Publications intended for professional communities",
    E = "Publications intended for the general public",
    F = "Public artistic and design activities",
    G = "Theses",
    H = "Patents and invention disclosures",
    I = "Audiovisual material, ICT software"
)

pub_category <- function(type) {
    switch(type %||% "",
        "journal-article" = , "article-journal" = , "paper-conference" = ,
        "proceedings-article" = , "chapter" = , "book-chapter" = "A",
        "book" = , "monograph" = "C",
        "thesis" = , "dissertation" = "G",
        "software" = "I",
        "preprint" = "preprint",
        "B"
    )
}

clean_markup <- function(x) {
    x <- gsub("</?(i|em)>", "*", x)
    x <- gsub("</?(b|strong)>", "**", x)
    x <- gsub("<sub>(.*?)</sub>", "~\\1~", x, perl = TRUE)
    x <- gsub("<sup>(.*?)</sup>", "^\\1^", x, perl = TRUE)
    gsub("<[^>]+>", "", x)
}

fmt_authors <- function(a) {
    a <- unlist(a)
    a[a == me] <- sprintf("**%s**", me)
    paste(a, collapse = ", ")
}

# Authors (year). Title. Journal volume(issue), pages. DOI
pub_reference <- function(p) {
    venue <- p$journal %||% p$venue %||% ""
    if (nzchar(venue) && is.null(p$venue)) venue <- sprintf("*%s*", venue)
    if (!is.null(p$volume)) {
        venue <- paste0(venue, " ", p$volume, if (!is.null(p$issue)) sprintf("(%s)", p$issue))
    }
    if (!is.null(p$pages)) venue <- paste0(venue, ", ", gsub("-", "–", p$pages))
    link <- if (!is.null(p$doi)) sprintf("<https://doi.org/%s>", p$doi)
            else if (!is.null(p$url)) sprintf("<%s>", p$url)
    parts <- c(
        if (length(p$authors)) sprintf("%s%s.", fmt_authors(p$authors),
                                       if (!is.null(p$year)) sprintf(" (%s)", p$year) else ""),
        sprintf("%s.", sub("\\.$", "", clean_markup(p$title))),
        if (nzchar(venue)) sprintf("%s.", venue),
        link
    )
    paste(parts, collapse = " ")
}

read_pubs <- function(file = "data/publications.json", manual = "data/publications-manual.yml") {
    d <- jsonlite::read_json(file)
    m <- yaml::read_yaml(manual)
    pubs <- lapply(d$publications, function(p) {
        p$category <- unlist(m$categories)[p$doi %||% ""] %||% pub_category(p$type)
        p
    })
    pubs <- c(pubs, m$publications)
    pubs[order(-vapply(pubs, function(p) as.numeric(p$year %||% 0), numeric(1)))]
}

pub_list <- function(pubs = read_pubs(), level = 2) {
    cats <- vapply(pubs, function(p) p$category, character(1))
    for (k in names(pub_categories)) {
        if (!any(cats == k)) next
        heading(sprintf("%s. %s", k, pub_categories[[k]]), level)
        cat(paste0(seq_len(sum(cats == k)), ". ", vapply(pubs[cats == k], pub_reference, character(1))),
            sep = "\n")
        cat("\n\n")
    }
    # Preprints are not part of the classification; shown on the web page only
    if (any(cats == "preprint") && !is_pdf()) {
        heading("Preprints", level)
        cat("Manuscripts not yet accepted for publication. Not included in the PDF.\n\n")
        cat(paste0(seq_len(sum(cats == "preprint")), ". ",
                   vapply(pubs[cats == "preprint"], pub_reference, character(1))), sep = "\n")
        cat("\n\n")
    }
}

pub_stats <- function(file = "data/publications.json") {
    d <- jsonlite::read_json(file)
    list(n = length(d$publications), updated = fmt_date(d$updated))
}

# ---- Presentations ----------------------------------------------------------

talk_group <- function(type) {
    type <- tolower(type)
    if (grepl("poster", type)) return("poster")
    if (grepl("workshop|course|school|lecture|training", type)) return("teaching")
    "talk" # talks, webinars and demos
}

read_talks <- function(file = "data/presentations.yml") {
    talks <- yaml::read_yaml(file)
    talks <- lapply(talks, function(t) {
        t$date <- as.character(t$date)
        t$group <- talk_group(t$type)
        t
    })
    talks[order(-vapply(talks, function(t) date_key(t$date), numeric(1)))]
}

talk_stats <- function(talks) {
    g <- vapply(talks, `[[`, character(1), "group")
    c(teaching = sum(g == "teaching"), talk = sum(g == "talk"), poster = sum(g == "poster"))
}

teaching_summary <- function(talks = read_talks()) {
    teaching <- Filter(function(t) t$group == "teaching", talks)
    roles <- vapply(teaching, function(t) t$role %||% "", character(1))
    years <- substr(vapply(teaching, `[[`, character(1), "date"), 1, 4)
    sprintf(
        "%d workshops, courses and lectures since %s: lead instructor in %d and co-instructor in %d. Full list: <https://tuomasborman.github.io/presentations.html>",
        length(teaching), min(years), sum(roles == "Lead instructor"), sum(roles == "Co-instructor")
    )
}

talk_entry <- function(t) {
    when <- fmt_range(t$date, t$end)
    what <- if (length(t$title)) sprintf("**%s**", t$title) else sprintf("**%s**", t$event %||% t$type)
    if (!is.null(t$url)) what <- sprintf("[%s](%s)", what, t$url)
    where <- paste(c(if (length(t$title)) t$event, t$location), collapse = ", ")
    lead <- identical(t$role, "Lead instructor")
    extra <- paste(c(if (!lead) t$role, t$duration), collapse = " · ")

    if (is_pdf()) {
        details <- paste(c(t$role, t$duration), collapse = " · ")
        cat(sprintf("- %s — %s. %s%s\n", t$type, what, where,
                    if (nzchar(details)) paste0(" (", details, ")") else ""))
        return(invisible())
    }
    cat(sprintf('::: {.talk data-group="%s"}\n', t$group))
    cat(sprintf("[%s]{.talk-date}\n\n", when))
    cat("::: {.talk-body}\n")
    cat(sprintf("[%s]{.badge .talk-%s} %s%s\\\n", t$type, t$group, what,
                if (lead) " [Lead instructor]{.badge .talk-lead}" else ""))
    cat(sprintf("[%s]{.talk-where}", where))
    if (nzchar(extra)) cat(sprintf("\\\n[%s]{.talk-extra}", extra))
    cat("\n:::\n:::\n\n")
}

talk_list <- function(talks = read_talks()) {
    years <- substr(vapply(talks, `[[`, character(1), "date"), 1, 4)
    for (y in unique(years)) {
        if (!is_pdf()) cat("::: {.talk-year}\n")
        heading(y)
        for (t in talks[years == y]) talk_entry(t)
        if (is_pdf()) cat("\n") else cat(":::\n\n")
    }
}

# ---- Software ---------------------------------------------------------------

anchor <- function(name) paste0("sw-", tolower(gsub("[^A-Za-z0-9]", "", name)))

fmt_num <- function(x) formatC(round(x), format = "d", big.mark = ",")

# Bar chart of monthly downloads (distinct IPs) as inline SVG
downloads_chart <- function(st) {
    n <- length(st$downloads)
    w <- 300; h <- 56; base <- 50; top <- 4
    slot <- w / n; bw <- slot - 4
    ymax <- max(st$downloads)
    label <- function(m) {
        d <- as.integer(strsplit(m, "-")[[1]])
        paste(month.abb[d[2]], d[1])
    }
    bars <- vapply(seq_len(n), function(i) {
        bh <- max(1.5, (base - top) * st$downloads[i] / ymax)
        x <- (i - 1) * slot + 2
        r <- min(3, bw / 2, bh)
        y <- base - bh
        # Rounded top, square base
        path <- sprintf("M%.1f,%.1fv%.1fa%.1f,%.1f 0 0 1 %.1f,-%.1fh%.1fa%.1f,%.1f 0 0 1 %.1f,%.1fv%.1fz",
                        x, base, -(bh - r), r, r, r, r, bw - 2 * r, r, r, r, r, bh - r)
        sprintf('<g class="dl-col"><title>%s: %s downloads</title><rect class="dl-hit" x="%.1f" y="0" width="%.1f" height="%g"/><path class="dl-bar" d="%s"/></g>',
                label(st$month[i]), fmt_num(st$downloads[i]), (i - 1) * slot, slot, base, path)
    }, character(1))
    sprintf(paste0(
        '<svg class="dl-chart" viewBox="0 0 %g %g" role="img" aria-label="Monthly downloads from %s to %s, between %s and %s per month">',
        '%s<line class="dl-axis" x1="0" x2="%g" y1="%g" y2="%g"/>',
        '<text class="dl-label" x="0" y="%g">%s</text><text class="dl-label" x="%g" y="%g" text-anchor="end">%s</text></svg>'),
        w, h + 8, label(st$month[1]), label(st$month[n]), fmt_num(min(st$downloads)), fmt_num(max(st$downloads)),
        paste(bars, collapse = ""), w, base + .5, base + .5,
        h + 6, label(st$month[1]), w, h + 6, label(st$month[n]))
}

sw_links <- function(p) {
    badges <- if (isTRUE(p$bioc)) sprintf(paste0(
        '<span class="sw-build">Release <a href="https://bioconductor.org/checkResults/release/bioc-LATEST/%1$s"><img src="https://bioconductor.org/shields/build/release/bioc/%1$s.svg" alt="Release build status"></a></span>',
        '<span class="sw-build">Devel <a href="https://bioconductor.org/checkResults/devel/bioc-LATEST/%1$s"><img src="https://bioconductor.org/shields/build/devel/bioc/%1$s.svg" alt="Devel build status"></a></span>'
    ), p$name) else ""
    sprintf('<a href="https://github.com/%s"><i class="bi bi-github"></i> GitHub</a>%s', p$github, badges)
}

software_entry <- function(p, stats = NULL) {
    st <- stats[[p$name]]
    cat(sprintf('::: {.sw #%s}\n', anchor(p$name)))
    cat("::: {.sw-info}\n")
    cat(sprintf("[[%s](%s)]{.sw-name} [%s]{.badge .badge-neutral}\\\n",
                p$name, p$url, p$repository))
    cat(sprintf("[%s]{.sw-desc}\n\n", p$description))
    cat(sprintf("[%s]{.sw-links}\n", sw_links(p)))
    cat(":::\n")
    if (!is.null(st)) {
        cat('::: {.sw-stats}\n')
        cat(sprintf('[%s]{.stat-number} [downloads/month]{.stat-label}\n\n', fmt_num(stats::median(st$downloads))))
        cat(downloads_chart(st), "\n")
        cat(":::\n")
    }
    cat(":::\n\n")
}

software_list <- function(file = "data/software.yml", stats_file = "data/software-stats.json") {
    sw <- yaml::read_yaml(file)
    sw <- sw[order(match(vapply(sw, `[[`, character(1), "role"), c("Maintainer", "Contributor")),
                   tolower(vapply(sw, `[[`, character(1), "name")))]
    stats <- if (file.exists(stats_file)) jsonlite::read_json(stats_file, simplifyVector = TRUE) else NULL
    roles <- vapply(sw, `[[`, character(1), "role")
    for (r in c("Maintainer", "Contributor")) {
        heading(r)
        if (r == "Maintainer" && !is.null(stats)) {
            first <- stats$packages[[1]]$month
            cat(sprintf(paste(
                "[Downloads per month, %s to %s. For Bioconductor packages this is the number of",
                "distinct IP addresses ([source](https://bioconductor.org/packages/stats/)); for CRAN",
                "packages, all downloads from the Posit CRAN mirror ([source](https://cranlogs.r-pkg.org)).",
                "The large number is the median month. Hover a bar for the exact number.]{.updated}\n\n"),
                fmt_date(first[1]), fmt_date(first[length(first)])))
        }
        for (p in sw[roles == r]) software_entry(p, stats$packages)
    }
}
