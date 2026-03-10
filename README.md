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
# --- Data Fetching ---
library(quantmod) # for downloading

# --- Data Manipulation & Plotting ---
library(tidyverse) # loads dplyr, tidyr, ggplot2, purrr (& others)
library(scales) # to express portfolio weights as percentage
library(patchwork) # for combining multiple plots into one figure

# --- Optimisation & Maths (Q2) ---
library(rootSolve) # finds roots of (fairly straightforward) equations
library(optimx) #  handles non-linear unconstrained optimization (as well as box-constrained).
library(quadprog) # solves quadratic programming problem

# --- Reporting & Tables ---
library(kableExtra) # for general tables
library(DT) # for HTML interactive data tables
library(plotly) # for interactive html plots
library(pander) # for writing to pandoc
```

- Optional packages: `htmltools` (required only if rendering to HTML)

## Useful operations

### Render outputs (HTML / PDF / Word)

#### Render a single R Markdown file to multiple formats:

```{r}
rmarkdown::render("filename.Rmd", output_format = "all")
```

```{r}
render_clean("filename.Rmd", output_format = "all")
```

#### Render to a specific format:

```{r}
rmarkdown::render("filename.Rmd", output_format = "html_document")
rmarkdown::render("filename.Rmd", output_format = "pdf_document")
rmarkdown::render("filename.Rmd", output_format = "word_document")
```

#### Render everything in a directory (not used here):

```{r}
files <- list.files(pattern = "\\.Rmd$", ignore.case = TRUE)
for (f in files) rmarkdown::render(f, output_format = "all")
```

### Extract R code from an Rmd (purl)

Create a `.R` script from an `.Rmd`:

```{r}
knitr::purl("filename.Rmd", documentation = 0)
```

### Debug: find non-ASCII characters

Useful if PDF/LaTeX builds start to fail without clear errors and if you suspect
things like smart quotes or odd dashes.

```{r}
tools::showNonASCIIfile("filename.Rmd")
```

### Run all code chunks (for debugging)

```{r}
knitr::knit("filename.Rmd")
```

### Session Info

```{r}
sessionInfo()
```

## References

> Zvi Bodie, Alex Kane, and Alan J. Marcus. Investments ISE. The McGraw Hill Series in Finance, Insurance,
> and Real Estate. McGraw Hill, 13th edition, 2024. ISBN 9781266085963. URL https://www.mheducation.co.uk/investments-ise-9781266085963-emea-group.

> Andrew Clare, James Seaton, Peter N. Smith, and Stephen Thomas. The trend is our friend: Risk parity,
> momentum and trend following in global asset allocation. Journal of Behavioral and Experimental
> Finance, 9:63–80, 2016. ISSN 2214-6350. doi: 10.1016/j.jbef.2016.01.002. URL https://www.sciencedirect.com/science/article/pii/S2214635016000083.

> Iqbal Owadally, Chul Jang, and Andrew Clare. Optimal investment for a retirement plan with deferred
> annuities. Insurance: Mathematics and Economics, 98:51–62, 2021. doi: 10.1016/j.insmatheco.2021.
> 02.001. URL https://www.sciencedirect.com/science/article/pii/S0167668721000196.
