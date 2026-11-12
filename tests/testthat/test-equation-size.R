# Sizes are checked against the data of the font (advances of glyphs) and the
# rules of TeX (spaces between atoms), not against another engine. The font
# "STIX Two Math" is shipped with 'gdtools': the tests run everywhere.

# advance of a character, in inches, at `size` points
# advance of the glyph of a character, at the script level `level` (the
# glyph of the feature 'ssty' in scripts), in inches at size `size`
advance_in <- function(ch, size = 12, font = gdtools::stixmath_file(), level = 0) {
  f <- math_font(font)
  f$adv[f$ssty(f$gid(ch), level) + 1] * size / f$upem / 72
}

test_that("a letter is as wide as its math italic glyph", {
  s <- equation_size("x")
  # U+1D465 MATHEMATICAL ITALIC SMALL X
  expect_equal(s$width, advance_in("\U0001D465"))
  expect_gt(s$height, 0)
})

test_that("binary operators and relations get the spaces of TeX", {
  em <- 12 / 72
  a <- advance_in("\U0001D44E")
  b <- advance_in("\U0001D44F")
  # medium space (4/18 em) on each side of a binary operator
  expect_equal(
    equation_size("a+b")$width,
    a + advance_in("+") + b + 2 * 4 / 18 * em
  )
  # thick space (5/18 em) on each side of a relation
  expect_equal(
    equation_size("a=b")$width,
    a + advance_in("=") + b + 2 * 5 / 18 * em
  )
  # a leading minus sign is not a binary operator
  expect_equal(
    equation_size("-a")$width,
    advance_in("\u2212") + a
  )
})

test_that("styles use the mathematical letters of the font", {
  # bold, italic and bold-italic letters are glyphs of the font (Unicode
  # mathematical alphanumeric symbols), there is no bold or italic file
  expect_equal(equation_size("\\mathbf{x}")$width, advance_in("\U0001D431"))
  expect_equal(equation_size("\\textbf{x}")$width, advance_in("\U0001D431"))
  expect_equal(equation_size("\\textit{x}")$width, advance_in("\U0001D465"))
  expect_equal(equation_size("\\alpha")$width, advance_in("\U0001D6FC"))
  expect_equal(equation_size("\\boldsymbol{\\alpha}")$width, advance_in("\U0001D736"))
  expect_equal(equation_size("\\mathbf{\\Omega}")$width, advance_in("\U0001D6C0"))
  expect_equal(equation_size("\\mathbb{R}")$width, advance_in("\u211D"))
})

test_that("a fraction has the space of the null delimiters on both sides", {
  em <- 12 / 72
  # inline style: numerator and denominator one level down (70%, glyphs of
  # the script style), the space (0.12 em) at the size of the fraction
  w <- 0.7 * max(advance_in("\U0001D44E", level = 1), advance_in("\U0001D44F", level = 1))
  expect_equal(equation_size("\\frac{a}{b}")$width, w + 2 * 0.12 * em)
  # display style: numerator and denominator in text style
  w <- max(advance_in("\U0001D44E"), advance_in("\U0001D44F"))
  expect_equal(equation_size("\\frac{a}{b}", display = TRUE)$width, w + 2 * 0.12 * em)
  # no such space in a stack with delimiters (\binom)
  expect_lt(
    equation_size("\\binom{a}{b}", display = TRUE)$width,
    equation_size("\\left(\\frac{a}{b}\\right)", display = TRUE)$width
  )
})

test_that("the italic correction of a large operator moves its subscript only", {
  # MathML Core: the superscript is not moved by the italic correction of a
  # large operator, its subscript is moved back by it
  f <- math_font(gdtools::stixmath_file())
  g <- f$gid("\u222B")
  v <- f$variants(g)
  large <- v$gid[which(v$size >= f$const[["displayOperatorMinHeight"]])[1]]
  ic <- f$italic(large) * 12 / f$upem / 72
  expect_gt(ic, 0)
  # scripts wider than the italic correction
  with_sup <- equation_size("\\int^{abc}", display = TRUE)$width
  with_sub <- equation_size("\\int_{abc}", display = TRUE)$width
  expect_equal(with_sup - with_sub, ic)
})

test_that("strikes keep the size of their content", {
  x <- equation_size("x")
  for (tex in c("\\cancel{x}", "\\bcancel{x}", "\\xcancel{x}", "\\sout{x}")) {
    expect_equal(equation_size(tex), x)
  }
  skip_if(getRversion() < "4.3.0")
  expect_true("lines" %in% names(equation_grob("\\cancel{x}")$children))
})

test_that("a frame adds its space and its rule on every side", {
  em <- 12 / 72
  pad <- (0.3 + 0.04) * em
  x <- equation_size("x+1", display = TRUE)
  boxed <- equation_size("\\boxed{x+1}", display = TRUE)
  expect_equal(boxed$width, x$width + 2 * pad)
  expect_equal(boxed$height, x$height + pad)
  expect_equal(boxed$depth, x$depth + pad)
})

test_that("overset and stackrel put their script above the base", {
  # KaTeX wraps them in a token element (mo) to give them the class of the
  # base: the content is laid out, not the text of the token
  eq <- equation_size("=")
  over <- equation_size("\\overset{!}{=}")
  expect_equal(over$width, eq$width)
  expect_gt(over$height, eq$height)
  expect_gt(equation_size("\\stackrel{def}{=}")$height, eq$height)
})

test_that("arrows and braces are stretched to the width of their base", {
  base <- equation_size("ABCD", display = TRUE)
  arrow <- equation_size("\\overrightarrow{ABCD}", display = TRUE)
  expect_equal(arrow$width, base$width, tolerance = 0.02)
  expect_gt(arrow$height, base$height)
  base <- equation_size("a+b+c", display = TRUE)
  brace <- equation_size("\\overbrace{a+b+c}^{n}", display = TRUE)
  expect_gte(brace$width, base$width)
  expect_lt(brace$width, base$width * 1.1)
  expect_gt(brace$height, base$height)
  under <- equation_size("\\underbrace{a+b+c}_{n}", display = TRUE)
  expect_gt(under$depth, base$depth)
})

test_that("the arrow of xrightarrow is as wide as its label, 3 em at least", {
  em <- 12 / 72
  short <- equation_size("\\xrightarrow{f}", display = TRUE)
  expect_gte(short$width, 3 * em)
  # the label at the script level
  label <- equation_size("\\scriptstyle\\text{a very long label}", display = TRUE)$width
  long <- equation_size("\\xrightarrow{\\text{a very long label}}", display = TRUE)
  # the label is padded with 0.6 em (scripts at 70%)
  expect_equal(long$width, label + 0.6 * 0.7 * em, tolerance = 0.02)
})

test_that("underline is a rule under the base", {
  f <- math_font(gdtools::stixmath_file())
  u <- function(name) f$const[[name]] * 12 / f$upem / 72
  x <- equation_size("x+y", display = TRUE)
  under <- equation_size("\\underline{x+y}", display = TRUE)
  expect_equal(under$width, x$width)
  expect_equal(
    under$depth,
    x$depth + u("underbarVerticalGap") + u("underbarRuleThickness") + u("underbarExtraDescender")
  )
})

test_that("cells are aligned as their column", {
  f <- math_font(gdtools::stixmath_file())
  x_of <- function(tex, ch) {
    b <- layout_equation(tex, f, display = TRUE)
    b$glyphs$x[b$glyphs$gid == f$gid(ch)]
  }
  # aligned: right then left columns, the equal signs are aligned
  eq <- x_of("\\begin{aligned} a &= b + c \\\\ cc &= d \\end{aligned}", "=")
  expect_length(eq, 2)
  expect_equal(eq[1], eq[2])
  # array{lcr}: left column, left edges aligned
  left <- c(
    x_of("\\begin{array}{lcr} a & b & c \\\\ dd & e & f \\end{array}", "\U0001D44E"),
    x_of("\\begin{array}{lcr} a & b & c \\\\ dd & e & f \\end{array}", "\U0001D451")
  )
  expect_equal(left[1], min(left[-1]))
  # cases: left aligned columns
  one <- x_of("\\begin{cases} 1 & x \\\\ 22 & y \\end{cases}", "1")
  two <- x_of("\\begin{cases} 1 & x \\\\ 22 & y \\end{cases}", "2")
  expect_equal(one, min(two))
})

test_that("colours are drawn and do not change the size", {
  expect_equal(equation_size("\\color{red} \\frac{a}{b}"), equation_size("\\frac{a}{b}"))
  # \colorbox: a background with 3pt (0.3 em) of space on each side
  em <- 12 / 72
  x <- equation_size("\\text{x}")
  box <- equation_size("\\colorbox{yellow}{x}")
  expect_equal(box$width, x$width + 0.6 * em)
  skip_if(getRversion() < "4.3.0")
  g <- equation_grob("a + \\textcolor{blue}{\\frac{b}{\\color{red} c}}", colour = "black")
  # colours of the glyphs, as stored by glyphInfo() (#RRGGBBAA)
  cols <- g$children$glyphs$glyphInfo$glyphs$colour
  expect_setequal(unique(cols), c("#000000FF", "#0000FFFF", "#FF0000FF"))
  expect_identical(as.character(g$children$rules$gp$fill), "blue")
  expect_true("fills" %in% names(equation_grob("\\colorbox{yellow}{x}")$children))
})

test_that("middle delimiters are stretched with the spaces given by KaTeX", {
  em <- 12 / 72
  # \middle| gets 0.05 em on each side, a | inside \left( \right) none
  middle <- equation_size("\\left( a \\middle| b \\right)")$width
  plain <- equation_size("\\left( a | b \\right)")$width
  # (the stretched bar may be a little wider than the plain one)
  expect_equal(middle - plain, 0.1 * em, tolerance = 0.1)
  # stretched as the other delimiters
  tall <- equation_size("\\left( x \\middle| \\frac{a}{b} \\right)")
  short <- equation_size("\\left( x | \\frac{a}{b} \\right)")
  expect_gt(tall$height + tall$depth, 0)
  expect_gte(tall$width, short$width)
})

test_that("sizes are proportional to the font size", {
  x <- c("\\frac{a+b}{c}", "\\sum_{i=1}^{n} x_i", "\\sqrt{x^2+y^2}")
  s12 <- equation_size(x, size = 12)
  s24 <- equation_size(x, size = 24)
  expect_equal(s24$width, 2 * s12$width)
  expect_equal(s24$height + s24$depth, 2 * (s12$height + s12$depth))
})

test_that("display style puts limits above and below large operators", {
  inline <- equation_size("\\sum_{i=1}^{n} x_i", display = FALSE)
  display <- equation_size("\\sum_{i=1}^{n} x_i", display = TRUE)
  expect_gt(display$height + display$depth, inline$height + inline$depth)
  expect_lt(display$width, inline$width)
})

test_that("structures are taller than their content", {
  s <- equation_size(c(
    "a",
    "\\frac{a}{b}",
    "\\sqrt{a}",
    "\\left( \\frac{a}{b} \\right)",
    "\\begin{pmatrix} a & b \\\\ c & d \\end{pmatrix}",
    "\\hat{a}"
  ))
  total <- s$height + s$depth
  expect_true(all(total[-1] > total[1]))
  # the delimiters are stretched around the fraction
  expect_gt(total[4], total[2])
})

test_that("missing and unsupported equations give NA", {
  expect_warning(
    # \tag is refused by KaTeX outside of display mode
    s <- equation_size(c("a", NA, "x \\tag{1}")),
    "can not be measured"
  )
  expect_false(is.na(s$width[1]))
  expect_true(all(is.na(s$width[2:3])))
})

test_that("arguments are checked", {
  expect_error(equation_size(1), "character")
  expect_error(equation_size("a", size = -1), "positive")
  expect_error(equation_size("a", display = NA), "TRUE or FALSE")
})

test_that("the default font is the file shipped with gdtools", {
  x <- c("\\frac{a+b}{c}", "\\sum_{i=1}^{n} x_i")
  expect_identical(
    equation_size(x),
    equation_size(x, font = gdtools::stixmath_file())
  )
  # the same file found by name
  name <- "equatags-test-stix-math"
  gdtools::register_stixmath(name = name)
  expect_identical(equation_size(x), equation_size(x, font = name))
})

test_that("a font without MATH table is refused", {
  path <- systemfonts::match_fonts("sans")$path
  skip_if(is.na(path))
  f <- tryCatch(math_font("sans"), error = function(e) e)
  skip_if(!inherits(f, "error"), "the default sans font has a MATH table")
  expect_match(conditionMessage(f), "MATH")
})

test_that("the grob has the size of the equation", {
  skip_if(getRversion() < "4.3.0")
  x <- "\\frac{a+b}{c} = \\sqrt{x}"
  s <- equation_size(x, size = 20)
  g <- equation_grob(x, size = 20)
  expect_s3_class(g, "equation_grob")
  expect_equal(grid::convertWidth(grid::grobWidth(g), "in", TRUE), s$width)
  expect_equal(
    grid::convertHeight(grid::grobHeight(g), "in", TRUE),
    s$height + s$depth
  )
  expect_true(all(c("glyphs", "rules") %in% names(g$children)))
  expect_error(equation_grob(c("a", "b")), "single")
})

test_that("layouts are cached by style and expression, not by size", {
  f <- math_font(gdtools::stixmath_file())
  tex <- "\\frac{q^2}{\\sqrt{z_k}}"
  s1 <- equation_size(tex, size = 10)
  expect_true(exists(paste0("T", tex), envir = f$layouts, inherits = FALSE))
  expect_false(exists(paste0("D", tex), envir = f$layouts, inherits = FALSE))
  s2 <- equation_size(tex, size = 20)
  expect_equal(s2$width, 2 * s1$width)
  expect_equal(s2$height, 2 * s1$height)
  equation_size(tex, display = TRUE)
  expect_true(exists(paste0("D", tex), envir = f$layouts, inherits = FALSE))
  # errors are not cached
  expect_warning(equation_size("\\frac{a}"))
  expect_false(exists("T\\frac{a}", envir = f$layouts, inherits = FALSE))
})

test_that("rules are drawn and sized as in TeX", {
  f <- math_font(gdtools::stixmath_file())
  em <- f$upem
  b <- layout_equation("\\rule{1em}{1em}", f)
  expect_equal(c(b$w, b$h, b$d), c(em, em, 0))
  expect_equal(nrow(b$fills), 1)
  expect_equal(b$fills$h, em)
  # raised by 3pt (0.3 em) and lowered by 2pt (0.2 em)
  b <- layout_equation("\\rule[3pt]{1em}{0.2em}", f)
  expect_equal(c(b$h, b$d), c(0.5 * em, 0))
  b <- layout_equation("\\rule[-2pt]{1em}{0.2em}", f)
  expect_equal(c(b$h, b$d), c(0, 0.2 * em))
})

test_that("lines between the columns and rows of tables are drawn", {
  f <- math_font(gdtools::stixmath_file())
  plain <- layout_equation("\\begin{array}{cc} a & b \\\\ c & d \\end{array}", f)
  solid <- layout_equation("\\begin{array}{c|c} a & b \\\\ \\hline c & d \\end{array}", f)
  # one vertical and one horizontal rule, no space added
  expect_equal(nrow(solid$rules), 2)
  expect_equal(c(solid$w, solid$h, solid$d), c(plain$w, plain$h, plain$d))
  v <- solid$rules[solid$rules$h > solid$rules$w, ]
  expect_equal(v$h, plain$h + plain$d)
  h <- solid$rules[solid$rules$w > solid$rules$h, ]
  expect_equal(h$w, plain$w)
  # dashed lines are made of several dashes
  dashed <- layout_equation("\\begin{array}{c:c} a & b \\\\ \\hdashline c & d \\end{array}", f)
  expect_gt(nrow(dashed$rules), 2)
})

test_that("backgrounds move with the content of a frame", {
  f <- math_font(gdtools::stixmath_file())
  alone <- layout_equation("\\colorbox{yellow}{x}", f)
  boxed <- layout_equation("\\boxed{\\colorbox{yellow}{x}}", f)
  shift <- boxed$glyphs$x - alone$glyphs$x
  expect_gt(shift, 0)
  expect_equal(boxed$fills$x, alone$fills$x + shift)
})

test_that("frames of arrays are drawn on the right sides, against the table", {
  f <- math_font(gdtools::stixmath_file())
  t <- 0.04 * f$upem
  plain <- layout_equation("\\begin{array}{cc} a & b \\end{array}", f)
  half <- 0.5 * f$upem # half of the columnspacing of 1em written by KaTeX
  # vertical lines at the edges: KaTeX writes notation="top bottom"
  v <- layout_equation("\\begin{array}{|c|c|} a & b \\end{array}", f)
  expect_equal(v$w, plain$w + 2 * (half + t))
  expect_equal(c(v$h, v$d), c(plain$h + t, plain$d + t))
  r <- v$rules
  expect_equal(sum(r$h > r$w), 3) # left, middle and right
  expect_equal(sum(r$w > r$h), 0)
  # \hline at the top and the bottom: KaTeX writes notation="left right"
  h <- layout_equation("\\begin{array}{c} \\hline a \\\\ \\hline \\end{array}", f)
  r <- h$rules
  expect_equal(sum(r$w > r$h), 2)
  expect_equal(sum(r$h > r$w), 0)
  # inner horizontal rules reach the vertical rules of the frame
  g <- layout_equation("\\begin{array}{|c|} \\hline a \\\\ \\hline b \\\\ \\hline \\end{array}", f)
  r <- g$rules[g$rules$w > g$rules$h, ]
  inner <- r[r$x > 0, ]
  expect_equal(nrow(inner), 1)
  expect_equal(c(inner$x, inner$w), c(t, g$w - 2 * t))
})

test_that("empty cells around numbered environments take no space", {
  s <- equation_size(
    c("x", "\\begin{equation} x \\end{equation}", "\\begin{gather} x \\\\ y \\end{gather}"),
    display = TRUE
  )
  expect_equal(s$width[2], s$width[1])
  expect_equal(s$width[3], s$width[1], tolerance = 0.2)
})

test_that("vertical bars used as delimiters are not spaced as relations", {
  s <- equation_size(c(
    "|x|", "\\lvert x \\rvert", "\\bigl| x \\bigr|", "\\left| x \\right|",
    "a \\mid b", "a|b", "\\lvert -x \\rvert", "|-x|"
  ))
  expect_equal(s$width[2], s$width[1])
  expect_equal(s$width[4], s$width[1])
  # \bigl uses a larger bar
  expect_lt(s$width[3], 1.1 * s$width[1])
  # \mid is a relation
  expect_gt(s$width[5], s$width[6])
  # minus after an opening bar is unary; after "|", an ordinary atom as in
  # TeX, it is binary
  expect_lt(s$width[7], s$width[8])
})

test_that("operators get the class of the table of symbols of KaTeX", {
  em <- 12 / 72
  # extra width of `a op b` compared with `a`, `b` and the operator alone
  spaces <- function(op) {
    equation_size(paste0("a ", op, " b"))$width -
      equation_size(paste0("{", op, "}"))$width -
      equation_size("ab")$width
  }
  for (op in c("\\wedge", "\\vee", "\\bullet", "\\ominus", "\\times")) {
    expect_equal(spaces(op), 2 * 4 / 18 * em, label = op)
  }
  for (op in c("\\Leftarrow", "\\ni", "\\perp", "\\vdash", "\\prec", "\\ne", "\\notin", "\\not\\subset")) {
    expect_equal(spaces(op), 2 * 5 / 18 * em, label = op)
  }
  # \cdotp is punctuation: a thin space after it only
  expect_equal(spaces("\\cdotp"), 3 / 18 * em)
  # \bigvee is a large operator: limits above and below in display style
  inline <- equation_size("\\bigvee_{i=1}^n x_i")
  display <- equation_size("\\bigvee_{i=1}^n x_i", display = TRUE)
  expect_gt(display$height, inline$height)
  expect_lt(display$width, inline$width)
})

test_that("the frame of an array extends the lines of the table only", {
  f <- math_font(gdtools::stixmath_file())
  half <- 0.5 * f$upem # half of the columnspacing of 1em written by KaTeX
  alone <- layout_equation("\\overline{abc}", f)
  b <- layout_equation("\\begin{array}{|c|} \\overline{abc} \\end{array}", f)
  # the overline keeps the width of its base, in the cell
  bar <- b$rules[b$rules$w > b$rules$h, ]
  expect_equal(bar$w, alone$rules$w)
  expect_gt(bar$x, half)
  expect_null(b$rules$table)
})
