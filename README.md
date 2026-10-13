# tuomasborman.github.io

Personal website built with [Quarto](https://quarto.org) and published with GitHub Pages.

## Updating content

| What | Edit | Page |
|---|---|---|
| About page | `index.qmd` | `index.html` |
| CV (also the timeline on the About page) | `data/cv.yml` | `cv.html` + `cv.pdf` |
| CV, Research Council of Finland format | `data/cv-rcf.yml` (reuses entries from `data/cv.yml`) | tab on `cv.html` + `cv-rcf.pdf` |
| Software | `data/software.yml` | `software.html` |
| Presentations | `data/presentations.yml` | `presentations.html` + `presentations.pdf` |
| Publications | nothing (ORCID), plus `data/publications-manual.yml` for software, theses etc. | `publications.html` + `publications.pdf` |

Push to `main` and the site rebuilds automatically. The "Last updated" date on the
CV and presentations pages is the date of the last commit to the data file.

## Publications

Publications come from [ORCID](https://orcid.org/0000-0002-8563-8884). Every Monday,
a GitHub Action runs `scripts/update-publications.R`, which

1. lists works in ORCID,
2. fetches BibTeX and metadata for each DOI from doi.org,
3. writes `data/publications.bib` and `data/publications.json`,

and commits and republishes the site if something changed. To update right away, run
the *Update publications* workflow from the Actions tab, or locally:

```sh
Rscript scripts/update-publications.R
```

The list is grouped by the publication classes (A–I) used by the
[Research Council of Finland](https://www.aka.fi/julkaisuluettelo/). Journal articles go
to A automatically; preprints are shown on the web page only. To hide a work, add its
DOI to `exclude_dois` in the script.

## Software download statistics

The Software page shows monthly downloads for the Bioconductor packages, from
Bioconductor. The site is rebuilt every Monday, and the build runs
`scripts/update-software-stats.R` to refresh `data/software-stats.json`.

## Preview locally

```sh
quarto preview
```

Requires Quarto and the R packages `knitr`, `rmarkdown`, `yaml`, `jsonlite` and `httr2`.

## Layout

- `_quarto.yml`: site settings and navigation
- `R/helpers.R`: turns the files in `data/` into page content
- `assets/`: picture, styles (`styles.scss`, `dark.scss`) and PDF style (`pdf.typ`)
- `.github/workflows/`: build and publish, and the weekly publication update
