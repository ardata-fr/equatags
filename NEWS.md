# equatags 0.3.0

- In 'Office Open XML Math' output, the argument of sums, products and
integrals is now written inside the operator: it was written after it, and Word
showed an empty placeholder in the operator.
- In 'Office Open XML Math' output, delimiters such as the parentheses of a
matrix or `\left( \right)` are now stretchy delimiters: they kept the height of
a single line.
- The XSL stylesheet is read once per session instead of once per equation.

# equatags 0.2.2

- add a `strict` argument to transform_mathjax() ("ignore" by default) to turn off ennoying warnings when using accented characters in equations.

# equatags 0.2.1

- add a `display` argument to transform_mathjax() to allow generating display or inline equations. It is set to FALSE by default (inline equation). This changes the behaviour that was previously to generate display equations only.
- delete unused mathjax-npm related tools.

# equatags 0.2.0

-   drop the node.js dependency in favor of package katex.

# equatags 0.1.1

-   use tools::R_user_dir instead of rappdirs::user_data_dir as it's a condition to keep the package on CRAN ("This is using \^\~/.local/share/equatags , prohibited by the CRAN policy. Please replace the use of rappdirs by the allowed procedure documented there.")
