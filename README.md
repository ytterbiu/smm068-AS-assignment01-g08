# MSc AS - Term 2: SMM068 Financial Economics — Group Project

Term 2 group project 01 (out of 2 total this term) for Financial Economics
(50% of coursework grade - 10% of module).

- Group 08 working directory
- HTML report: https://ytterbiu.github.io/smm068-AS-assignment01-g08/

## Formatting guide

Making notes here for collaboration using rmd format:

- If using `align*` ensure that it isn't wrapped in math mode
- Add `<!-- prettier-ignore -->` before `align*` environment
- Avoid putting + or - at the start of a new line within an `align*` environment
- Use `$$ (maths) $$` for display math mode for consistency, rather than
  `\[ and \]`
- This character `−` raises warning in X&#x018E;LaTeX, have replaced throughout
  with `-`

## Structure

To be inserted.

The directory structure is as follows

```{bash}
.
├── CHANGELOG.md
├── Q1.csv
├── README.md
├── air.toml
├── answer.lua
├── cm2-assessment01-group08.pdf
├── cm2-assessment01-group08.rmd
├── cm2-assessment01-group08.tex
├── coursework_CM2_250120.pdf
├── fig
│   ├── ...
├── preamble.tex
├── references.bib
├── smm068-AS-assignment01-g08.Rproj
└── style.css
```

## Contents

To be inserted.

## Requirements

- R (≥ 4.x)
- Packages: at minimum **rmarkdown** (others as used in the Rmd)

For R Markdown install core package:

```{r}
install.packages("rmarkdown")
```

Additional packages used are

```{r}
# to be inserted / updated
install.packages("ggplot2")
install.packages("patchwork")
install.packages("kableExtra")
```

- Optional packages: `htmltools` (required only if rendering to HTML)

## Useful operations

### Render outputs (HTML / PDF / Word)

#### Render a single R Markdown file to multiple formats:

```{r}
rmarkdown::render("cm2-assessment01-group08.Rmd", output_format = "all")
```

#### Render to a specific format:

```{r}
rmarkdown::render("cm2-assessment01-group08.Rmd", output_format = "html_document")
rmarkdown::render("cm2-assessment01-group08.Rmd", output_format = "pdf_document")
rmarkdown::render("cm2-assessment01-group08.Rmd", output_format = "word_document")
```

#### Render everything in a directory (not used here):

```{r}
files <- list.files(pattern = "\\.Rmd$", ignore.case = TRUE)
for (f in files) rmarkdown::render(f, output_format = "all")
```

### Extract R code from an Rmd (purl)

Create a `.R` script from an `.Rmd`:

```{r}
knitr::purl("report.Rmd", documentation = 0)
```

### Debug: find non-ASCII characters

Useful if PDF/LaTeX builds start to fail without clear errors and if you suspect
things like smart quotes or odd dashes.

```{r}
tools::showNonASCIIfile("report.Rmd")
```

### Run all code chunks (for debugging)

```{r}
knitr::knit("report.Rmd")
```

### Session Info

```{r}
sessionInfo()
```

## Acknowledgements

To be inserted.
