# Corpus of equations

`corpus.csv`: 457 equations used by `test-corpus.R`, built by
`data-raw/corpus/build.R` (scripts and downloaded sources in
`data-raw/corpus/`, not part of the package).

| column | content |
|---|---|
| `id` | identifier, stable in its source |
| `source`, `ref` | origin and exact place (file, test, page) |
| `category` | domain or family of syntax |
| `display` | display style (`TRUE`) or inline |
| `tex` | the LaTeX equation, without `$` delimiters |
| `size_ok`, `word_ok` | expected result of `equation_size()` and `transform_mathjax(to = "mml")` |
| `reason` | why the equation is expected to fail |
| `cran` | also tested on CRAN (two equations of each category) |

## Sources and licenses

- `equatiomatic-tests`, `equatiomatic-docs`: equations written by
  'equatiomatic' 0.4.9 (Daniel Anderson, Andrew Heiss, Jay Sumners;
  <https://github.com/datalorax/equatiomatic>), license CC BY 4.0
  (<https://creativecommons.org/licenses/by/4.0/>): the expected outputs
  of its tests (`tests/testthat/_snaps/*.md`) and the outputs of
  `extract_eq()` in its vignettes and examples. Changes: the `$$`
  delimiters are removed, duplicates are kept once.
- `katex-tests`: the test cases of the screenshots of KaTeX
  (`test/screenshotter/ss_data.yaml`, <https://github.com/KaTeX/KaTeX>),
  MIT license, see `licenses/KaTeX-LICENSE`.
- `matplotlib-tests`: the expressions of `math_tests` in
  `lib/matplotlib/tests/test_mathtext.py` of matplotlib
  (<https://github.com/matplotlib/matplotlib>), each `$...$` on its own row,
  matplotlib license, see `licenses/matplotlib-LICENSE`.
- `classics`: classic formulas written for this corpus; `ref` is a page
  that gives the formula.
- `equatags-word`: equations already compared with Microsoft Word, written
  for this corpus.

## Expected failures

19 equations: commands added to KaTeX after 0.16.9 (the version of the
package 'katex'), macros defined by a KaTeX test, the HTML extension of
KaTeX, invalid commands written on purpose, the mathtext dialect of
matplotlib. When one of them starts to work (for example after an update of
'katex'), the test says so: run `data-raw/corpus/build.R` again.
