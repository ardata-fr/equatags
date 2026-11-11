# Rules of the layout that are not reached by the other tests: each test
# checks the result against the rule (TeX, OpenType 'MATH' table, MathML),
# not against a stored value.

stix <- function() math_font(gdtools::stixmath_file())
text_style <- function(display = FALSE) {
  list(level = 0, display = display, cramped = FALSE)
}
# children of a MathML fragment written without namespace
mml_kids <- function(x) xml2::xml_children(xml2::read_xml(x))
# a copy of the font with some of its functions replaced, and its own cache
font_with <- function(...) {
  f <- stix()
  f[names(list(...))] <- list(...)
  f$layouts <- new.env(parent = emptyenv())
  f
}
adv <- function(f, ch) f$adv[f$gid(ch) + 1]

test_that("math alphabets map digits and Greek variants, other characters are kept", {
  expect_equal(math_variant("\u03f5", "italic"), "\U0001d716")
  expect_equal(math_variant("1", "bold"), "\U0001d7cf")
  expect_equal(math_variant("1", "double-struck"), "\U0001d7d9")
  expect_equal(math_variant("+", "bold"), "+")
  expect_equal(math_variant("ab", "bold"), "ab")
  expect_equal(math_variant("x", "normal"), "x")
  f <- stix()
  expect_equal(layout_equation("\\epsilon", f)$w, adv(f, "\U0001d716"))
  expect_equal(layout_equation("\\mathbf{1}", f)$w, adv(f, "\U0001d7cf"))
  expect_equal(layout_equation("\\mathbb{1}", f)$w, adv(f, "\U0001d7d9"))
})

test_that("characters missing from the font take no space", {
  f <- stix()
  expect_equal(f$gid("\u65e5"), 0)
  expect_equal(layout_equation("\\text{\u65e5}", f)$w, 0)
})

test_that("a binary operator before a closing delimiter is an ordinary atom", {
  f <- stix()
  expect_equal(
    layout_equation("(a+)", f)$w,
    adv(f, "(") + adv(f, "\U0001d44e") + adv(f, "+") + adv(f, ")")
  )
})

test_that("a name of function is an operator followed by a thin space", {
  f <- stix()
  # the invisible function application written by KaTeX takes no space
  expect_equal(
    layout_equation("\\sin x", f)$w,
    adv(f, "s") + adv(f, "i") + adv(f, "n") + 3 / 18 * f$upem + adv(f, "\U0001d465")
  )
})

test_that("phantoms keep the space of their content, without drawing it", {
  f <- stix()
  x <- layout_equation("x", f)
  p <- layout_equation("\\phantom{x}", f)
  expect_equal(c(p$w, p$h, p$d), c(x$w, x$h, x$d))
  expect_equal(nrow(p$glyphs), 0)
  v <- layout_equation("\\vphantom{x}", f)
  expect_equal(c(v$w, v$h), c(0, x$h))
  # \mathrlap draws its content without width, \smash without height
  l <- layout_equation("\\mathrlap{x}", f)
  expect_equal(c(l$w, nrow(l$glyphs)), c(0, 1))
  s <- layout_equation("\\smash{x}", f)
  expect_equal(c(s$w, s$h, s$d), c(x$w, 0, 0))
})

test_that("the degree of a root is placed with the kerns of the MATH table", {
  f <- stix()
  sq <- layout_equation("\\sqrt{x}", f)
  rt <- layout_equation("\\sqrt[3]{x}", f)
  deg_w <- adv(f, "3") * f$const[["scriptScriptPercentScaleDown"]] / 100
  kerns <- f$const[["radicalKernBeforeDegree"]] + deg_w + f$const[["radicalKernAfterDegree"]]
  expect_equal(rt$w, sq$w + max(kerns, 0))
  expect_equal(nrow(rt$glyphs), nrow(sq$glyphs) + 1)
  # the bottom of the degree is at radicalDegreeBottomRaisePercent of the
  # height of the radical sign
  g <- rt$glyphs
  rad <- g[g$gid != f$gid("3") & g$gid != f$gid("\U0001d465"), ]
  ink <- f$ink(rad$gid)
  sign_bottom <- rad$y + ink[3]
  sign_height <- ink[4] - ink[3]
  deg_y <- g$y[g$gid == f$gid("3")]
  expect_equal(
    deg_y - sign_bottom,
    f$const[["radicalDegreeBottomRaisePercent"]] / 100 * sign_height
  )
})

test_that("MathML elements that are not supported give an error", {
  node <- xml2::read_xml("<mfoo/>")
  expect_error(layout_node(node, stix(), text_style()), "not supported")
})

test_that("binomials keep the minimum gap between their rows", {
  f <- stix()
  # a fraction above: in text style, the shifts of the MATH table leave a
  # gap too small
  kids <- mml_kids(
    "<mfrac linethickness=\"0px\"><mfrac><mi>a</mi><mi>b</mi></mfrac><mi>c</mi></mfrac>"
  )
  for (display in c(FALSE, TRUE)) {
    st <- text_style(display)
    cst <- if (display) with_style(st, display = FALSE) else sub_style(st)
    top <- layout_node(kids[[1]], f, cst)
    bot <- layout_node(kids[[2]], f, with_style(cst, cramped = TRUE))
    b <- layout_stack(kids, f, st)
    up <- b$h - top$h
    down <- b$d - bot$d
    gap <- (up - top$d) - (bot$h - down)
    gap_min <- K(f, st, if (display) "stackDisplayStyleGapMin" else "stackGapMin")
    if (display) {
      # the larger shifts of the display style are enough
      expect_gt(gap, gap_min)
    } else {
      # the rows are moved apart, half up and half down
      expect_equal(gap, gap_min)
      expect_equal(up - K(f, st, "stackTopShiftUp"), down - K(f, st, "stackBottomShiftDown"))
    }
  }
  # one letter in each row: the shifts of the MATH table
  st <- text_style()
  kids <- mml_kids("<mfrac linethickness=\"0px\"><mi>a</mi><mi>c</mi></mfrac>")
  top <- layout_node(kids[[1]], f, sub_style(st))
  bot <- layout_node(kids[[2]], f, sub_style(st, cramped = TRUE))
  b <- layout_stack(kids, f, st)
  expect_equal(b$h - top$h, K(f, st, "stackTopShiftUp"))
  expect_equal(b$d - bot$d, K(f, st, "stackBottomShiftDown"))
})

test_that("scripts keep the gaps of the MATH table", {
  f <- stix()
  st <- text_style()
  ss <- sub_style(st)
  # a tall subscript and a deep superscript
  kids <- mml_kids("<msubsup><mi>x</mi><mi>A</mi><mi>g</mi></msubsup>")
  b <- layout_scripts(kids[[1]], kids[[2]], kids[[3]], f, st)
  sub <- layout_node(kids[[2]], f, with_style(ss, cramped = TRUE))
  sup <- layout_node(kids[[3]], f, ss)
  # glyphs: base, superscript, subscript
  u <- b$glyphs$y[2]
  v <- -b$glyphs$y[3]
  tol <- 1e-9
  expect_gte((u - sup$d) - (sub$h - v), K(f, st, "subSuperscriptGapMin") - tol)
  expect_gte(u - sup$d, K(f, st, "superscriptBottomMaxWithSubscript") - tol)
  # the superscript is raised to superscriptBottomMaxWithSubscript, the
  # subscript is raised as much
  sup_only <- layout_scripts(kids[[1]], NULL, kids[[3]], f, st)
  expect_gt(u, sup_only$glyphs$y[2])
})

test_that("limits of names of functions are scripts outside of the display style", {
  f <- stix()
  inline <- layout_equation("\\lim_{x} y", f, display = FALSE)
  display <- layout_equation("\\lim_{x} y", f, display = TRUE)
  # inline, the limit is a subscript: the equation is wider and less deep
  expect_gt(inline$w, display$w)
  expect_lt(inline$d, display$d)
})

test_that("limits asked with \\limits stay below in the text style", {
  f <- stix()
  limits <- layout_equation("\\lim\\limits_{x} y", f, display = FALSE)
  display <- layout_equation("\\lim_{x} y", f, display = TRUE)
  expect_equal(c(limits$w, limits$h, limits$d), c(display$w, display$h, display$d))
})

test_that("labels under a stretched arrow are placed below it", {
  f <- stix()
  over <- layout_equation("\\xrightarrow{f}", f)
  both <- layout_equation("\\xrightarrow[g]{f}", f)
  expect_equal(both$h, over$h)
  expect_gt(both$d, over$d + K(f, text_style(), "stretchStackGapBelowMin"))
})

test_that("delimiters without assembly use their largest size variant", {
  f <- stix()
  g <- f$gid("\u27e8")
  expect_null(f$assembly(g))
  v <- f$variants(g)
  b <- layout_equation(paste0(
    "\\left\\langle \\begin{matrix}", paste(letters[1:10], collapse = "\\\\"),
    "\\end{matrix} \\right\\rangle"
  ), f)
  expect_true(v$gid[nrow(v)] %in% b$glyphs$gid)
  expect_gt(b$h + b$d, max(v$size, na.rm = TRUE))
})

test_that("empty delimiters have the size of the characters", {
  f <- stix()
  b <- layout_equation("\\left( \\right)", f)
  expect_equal(b$w, adv(f, "(") + adv(f, ")"))
  expect_equal(b$cls, "inner")
})

test_that("horizontal braces use a size variant, an assembly or the widest variant", {
  f <- stix()
  brace <- function(b, base) nrow(b$glyphs) - base
  # a short base: one glyph, the first variant wide enough
  short <- layout_equation("\\overbrace{a}", f)
  expect_equal(brace(short, 1), 1)
  expect_gte(short$w, adv(f, "\U0001d44e"))
  # a long base: an assembly
  long <- layout_equation("\\overbrace{abcdefghijklmnop}", f)
  expect_gt(brace(long, 16), 1)
  # a font without assembly: the widest variant, narrower than the base
  f2 <- font_with(assembly = function(g, horizontal = FALSE) NULL)
  b <- layout_equation("\\overbrace{abcdefghijklmnop}", f2)
  expect_equal(brace(b, 16), 1)
  v <- f$hvariants(f$gid("\u23de"))
  expect_true(v$gid[nrow(v)] %in% b$glyphs$gid)
})

test_that("elements under the base other than underline are limits", {
  f <- stix()
  x <- layout_equation("x", f)
  for (tex in c("\\utilde{x}", "\\undergroup{x}")) {
    b <- layout_equation(tex, f)
    expect_gt(b$d, x$d, label = tex)
    expect_equal(nrow(b$rules), 0, label = tex)
    expect_gt(nrow(b$glyphs), 1, label = tex)
  }
})

test_that("a vertical strike is a rule in the middle of the content", {
  f <- stix()
  node <- xml2::read_xml("<menclose notation=\"verticalstrike\"><mi>x</mi></menclose>")
  b <- layout_node(node, f, text_style())
  t <- f$const[["fractionRuleThickness"]]
  expect_equal(nrow(b$rules), 1)
  expect_equal(b$rules$x + b$rules$w / 2, b$w / 2)
  expect_equal(c(b$rules$y, b$rules$h), c(-b$d, b$h + b$d))
  expect_equal(b$rules$w, t)
})

test_that("an accent is centered on a base of several glyphs", {
  f <- stix()
  b <- layout_equation("\\hat{ab}", f)
  acc <- b$glyphs[nrow(b$glyphs), ]
  expect_equal(acc$x + f$top_accent(acc$gid), b$w / 2)
})

test_that("an accent without attachment point is centered on its advance", {
  f <- font_with(top_accent = function(g) NA_real_)
  b <- layout_equation("\\hat{x}", f)
  acc <- b$glyphs[nrow(b$glyphs), ]
  expect_equal(acc$x + f$adv[acc$gid + 1] / 2, b$w / 2)
})

test_that("wide accents use the widest variant that fits the base", {
  f <- stix()
  acc_w <- function(tex) {
    b <- layout_equation(tex, f)
    f$adv[b$glyphs$gid[nrow(b$glyphs)] + 1]
  }
  expect_gt(acc_w("\\widehat{xyzxyz}"), acc_w("\\widehat{x}"))
  expect_lte(acc_w("\\widehat{xyzxyz}"), layout_equation("xyzxyz", f)$w * 1.1)
})

test_that("an overline uses the constants of the MATH table without the overline glyph", {
  stix_gid <- stix()$gid
  f <- font_with(gid = function(ch) if (identical(ch, "\u203e")) 0L else stix_gid(ch))
  x <- layout_equation("x", f)
  b <- layout_equation("\\overline{x}", f)
  r <- b$rules
  expect_equal(r$y, x$h + f$const[["overbarVerticalGap"]])
  expect_equal(r$h, f$const[["overbarRuleThickness"]])
  expect_equal(b$h, r$y + r$h + f$const[["overbarExtraAscender"]])
})
