# Changelog

## Unreleased

### To Do

- Ask Iqbal about GenAI declaration

#### Q1

- Add draft supplemental methodology describing the stock selection process in
  more detail and adding a bit of background information on the companies -
  potentially in a table?

#### Q2

- TBC

#### Q3

- Add description of what the pdf/cdf distribution means by method of comparison
  for both figures (saving space for Q3g) - i.e., higher probability of
  more/less extreme results / higher excess kurtosis(?)
- Look into adding table comparing the pdf & cdf of asset 1 & 2 (`kable`)

- BE note: asked question in week 03: can link this back to an investors utility function - their utility function would capture this
  - potential to include this in the appendix in more detail (can't see disadvantage of doing this...)
  - We can work out the expected utility using different functions score
    $$
    \mathbb{E}[U] = \sum P(x) \cdot U(x)
    $$
  - Then calculate this for the three different utility functions (& put results in a table)
    - Utility functions: Quadratic, Logarithmic, & Power
    - Thoughts on calculating this for each approach, putting in a table, then referring to this in the answer to last part of Q3?

### Changed

## 2026-02-01

- Create `ggplot` versions of the figures, complete with figure captions.
- Test 2x2 or 2x1 plot format for difference and PDF/CDF ease of comparison.

## 2026-01-30 & 2026-01-31

- Add functionality to avoid repeated downloads from yahoo (can change this back
  for the R code submission).
  - Change `aligned` to `align*` throughout.
- Wrap `array` in display math (`$$ ... $$`) so it renders in PDF and HTML.
- Switch `round()` to `sprintf()` for fixed decimal formatting and PDF
  rendering.
- Fix array spacing - possibly create custom environment for answers for QOL.

### Fixed
