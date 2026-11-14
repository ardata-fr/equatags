# Rules of the layout that are not reached by the other tests: each test
# checks the result against the rule (TeX, OpenType 'MATH' table, MathML),
# not against a stored value.

stix <- function() math_font(gdtools::stixmath_file())
text_style <- function(display = FALSE) {
  list(level = 0, display = display, cramped = FALSE, size = 1)
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
  # the degree is two script levels down: glyph of the script style 2
  three <- f$ssty(f$gid("3"), 2)
  deg_w <- f$adv[three + 1] * f$const[["scriptScriptPercentScaleDown"]] / 100
  kerns <- f$const[["radicalKernBeforeDegree"]] + deg_w + f$const[["radicalKernAfterDegree"]]
  expect_equal(rt$w, sq$w + max(kerns, 0))
  expect_equal(nrow(rt$glyphs), nrow(sq$glyphs) + 1)
  # the bottom of the degree is at radicalDegreeBottomRaisePercent of the
  # height of the radical sign
  g <- rt$glyphs
  rad <- g[g$gid != three & g$gid != f$gid("\U0001d465"), ]
  ink <- f$ink(rad$gid)
  sign_bottom <- rad$y + ink[3]
  sign_height <- ink[4] - ink[3]
  deg_y <- g$y[g$gid == three]
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

test_that("\\big, \\Big, \\bigg and \\Bigg have their own size, whatever the content", {
  f <- stix()
  em <- f$upem
  first_gid <- function(tex) layout_equation(tex, f)$glyphs$gid[1]
  total <- function(tex) {
    b <- layout_equation(tex, f)
    b$h + b$d
  }
  # minsize of KaTeX: 1.2, 1.8, 2.4 and 3 em
  sizes <- c(big = 1.2, Big = 1.8, bigg = 2.4, Bigg = 3)
  for (cmd in names(sizes)) {
    expect_gte(total(paste0("\\", cmd, "(")), sizes[[cmd]] * em)
  }
  expect_gt(first_gid("\\big( x"), first_gid("( x"))
  # the content does not change the size
  expect_equal(first_gid("\\big( \\frac{a}{b}"), first_gid("\\big( x"))
  expect_equal(first_gid("\\big( \\sum_{i=1}^{n} \\frac{a}{b}"), first_gid("\\big( x"))
  # as the base of scripts
  base <- function(tex) layout_equation(tex, f)$glyphs$gid[3]
  expect_equal(base("\\big( x \\big)^2"), base("\\big( x \\big)"))
  expect_gt(base("\\big( x \\big)^2"), base("( x )^2"))
  # content of the row: \left ... \right are stretched around it
  expect_gte(total("\\left( \\Bigg( x \\right)"), total("\\Bigg( x"))
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

test_that("\\llap, \\rlap and \\clap take no width and overlap their neighbours", {
  f <- stix()
  y <- layout_equation("y", f)
  for (tex in c("\\llap{x}y", "\\rlap{x}y", "\\clap{x}y")) {
    expect_equal(layout_equation(tex, f)$w, y$w, info = tex)
  }
  w <- adv(f, "\U0001d465") # italic x of mi
  first_x <- function(tex) layout_equation(tex, f)$glyphs$x[1]
  expect_equal(first_x("\\mathllap{x}y"), -w)
  expect_equal(first_x("\\mathrlap{x}y"), 0)
  expect_equal(first_x("\\mathclap{x}y"), -w / 2)
})

test_that("spacing accents are drawn with the combining accents of the font", {
  f <- stix()
  accents <- c(
    "\\acute{x}" = "\u0301", "\\grave{x}" = "\u0300", "\\hat{x}" = "\u0302",
    "\\bar{x}" = "\u0304", "\\check{x}" = "\u030c", "\\breve{x}" = "\u0306",
    "\\tilde{x}" = "\u0303", "\\dot{x}" = "\u0307", "\\ddot{x}" = "\u0308",
    "\\mathring{x}" = "\u030a"
  )
  for (tex in names(accents)) {
    g <- layout_equation(tex, f)$glyphs$gid
    expect_true(f$gid(accents[[tex]]) %in% g, info = tex)
  }
})

test_that("hooked arrows are stretched with the assembly of the font", {
  f <- stix()
  for (tex in c("\\xhookrightarrow{f}", "\\xhookleftarrow{f}")) {
    b <- layout_equation(tex, f)
    # the arrow is made of several parts, 3 em long (minsize) less the side
    # bearings of its ends; with a long label, as wide as \xrightarrow
    expect_gt(nrow(b$glyphs), 2, label = tex)
    expect_gt(b$w, 2.5 * f$upem, label = tex)
    expect_equal(
      layout_equation(sub("f}", "abcdefghij}", tex, fixed = TRUE), f)$w,
      layout_equation(sub("hook", "", sub("f}", "abcdefghij}", tex, fixed = TRUE)), f)$w,
      tolerance = 0.1, label = tex
    )
  }
})

test_that("group accents use the size variants of the font", {
  f <- stix()
  for (ch in c("\u23e0", "\u23e1")) {
    v <- f$hvariants(f$gid(ch))
    tex <- paste0(if (ch == "\u23e0") "\\overgroup" else "\\undergroup", "{abc}")
    g <- layout_equation(tex, f)$glyphs$gid
    expect_true(any(v$gid[-1] %in% g), label = tex)
  }
})

test_that("\\utilde stretches the combining tilde below, \\widetilde the one above", {
  f <- stix()
  below <- f$hvariants(f$gid("\u0330"))$gid
  above <- f$hvariants(f$gid("\u0303"))$gid
  b <- layout_equation("\\utilde{abc}", f)
  expect_true(any(below %in% b$glyphs$gid))
  # placed under the base as \\underbrace, not as a limit at script size
  expect_lt(b$d, layout_equation("\\underbrace{abc}", f)$d)
  expect_gt(b$d, layout_equation("abc", f)$d)
  expect_true(any(above %in% layout_equation("\\widetilde{abc}", f)$glyphs$gid))
})

# --- script style (feature 'ssty' of the 'GSUB' table) -------------------------

# bytes of a 'GSUB' table, written from the OpenType specification
u16b <- function(...) {
  v <- c(...)
  as.raw(rbind(v %/% 256, v %% 256))
}
u32b <- function(v) as.raw(c(v %/% 16777216, (v %/% 65536) %% 256, (v %/% 256) %% 256, v %% 256))
gsub_coverage <- function(g) c(u16b(1, length(g)), u16b(g))
gsub_single1 <- function(g, delta) c(u16b(1, 6, delta), gsub_coverage(g))
gsub_single2 <- function(g, subs) {
  c(u16b(2, 6 + 2 * length(subs), length(subs)), u16b(subs), gsub_coverage(g))
}
gsub_alternate <- function(g, alts) {
  n <- length(alts)
  sets <- lapply(alts, function(a) u16b(length(a), a))
  offsets <- 6 + 2 * n + c(0, cumsum(lengths(sets)))[seq_len(n)]
  c(u16b(1, 6 + 2 * n + sum(lengths(sets)), n), u16b(offsets), unlist(sets), gsub_coverage(g))
}
gsub_extension <- function(type, sub) c(u16b(1, type), u32b(8), sub)
gsub_lookup <- function(type, sub) c(u16b(type, 0, 1, 8), sub)
# a list of tables preceded by their count and their offsets
gsub_list <- function(parts) {
  offsets <- 2 + 2 * length(parts) + c(0, cumsum(lengths(parts)))[seq_along(parts)]
  c(u16b(length(parts)), u16b(offsets), unlist(parts))
}
gsub_table <- function(features, lookups) {
  tables <- lapply(features, function(i) u16b(0, length(i), i))
  offsets <- 2 + 6 * length(features) + c(0, cumsum(lengths(tables)))[seq_along(tables)]
  records <- unlist(Map(function(tag, o) c(charToRaw(tag), u16b(o)), names(features), offsets))
  feature_list <- c(u16b(length(features)), records, unlist(tables))
  lookup_list <- gsub_list(lookups)
  c(u16b(1, 0, 0, 10, 10 + length(feature_list)), feature_list, lookup_list)
}

test_that("the script style is read from single, alternate and extension substitutions", {
  r <- gsub_table(
    features = list(liga = 0L, ssty = 1:5),
    lookups = list(
      gsub_lookup(1, gsub_single1(9, 1)), # feature liga: ignored
      gsub_lookup(1, gsub_single1(1, 10)), # single, format 1: 1 -> 11
      gsub_lookup(1, gsub_single2(c(2, 3), c(20, 30))), # single, format 2
      gsub_lookup(3, gsub_alternate(c(4, 5), list(c(40, 41), 50))), # alternates
      gsub_lookup(7, gsub_extension(3, gsub_alternate(c(6, 1), list(c(60, 61), c(99, 98)))))
    )
  )
  s <- read_ssty(r, 0, 100)
  expect_equal(s$alt1[c(1, 2, 3, 4, 5, 6) + 1], c(11, 20, 30, 40, 50, 60))
  # level 2: second alternate, or the only one
  expect_equal(s$alt2[c(1, 2, 3, 4, 5, 6) + 1], c(11, 20, 30, 41, 50, 61))
  # the first substitution of a glyph is kept: glyph 1 is 11, not 99; the
  # lookups of other features are ignored (glyph 9)
  expect_equal(s$alt1[9 + 1], NA_integer_)
  expect_equal(sum(!is.na(s$alt1)), 6)
  # no 'GSUB' table
  expect_true(all(is.na(read_ssty(r, NULL, 10)$alt1)))
})

test_that("scripts use the glyphs of the script style of the font", {
  f <- stix()
  x <- f$gid("\U0001d465")
  a <- f$gid("\U0001d44e")
  b <- f$gid("\U0001d44f")
  expect_false(f$ssty(a, 1) == a)
  expect_false(f$ssty(b, 2) == f$ssty(b, 1))
  expect_equal(f$ssty(a, 0), a)
  g <- layout_equation("x^{a^b}", f)$glyphs$gid
  expect_equal(g, c(x, f$ssty(a, 1), f$ssty(b, 2)))
  # the prime of x' is the prime of the script style, lower than the prime
  # of the text style raised as a superscript
  p <- f$gid("\u2032")
  expect_equal(layout_equation("x'", f)$glyphs$gid, c(x, f$ssty(p, 1)))
  expect_lt(f$ink(f$ssty(p, 1))[3], f$ink(p)[3])
  # an accent in a script is the accent of the script style
  acc <- f$gid("\u0302")
  expect_true(f$ssty(acc, 1) %in% layout_equation("x^{\\hat{a}}", f)$glyphs$gid)
})

test_that("a font without script style keeps the glyphs of the text style", {
  f <- font_with(ssty = function(g, level) g)
  g <- layout_equation("x^{a^b}", f)$glyphs$gid
  expect_equal(g, c(f$gid("\U0001d465"), f$gid("\U0001d44e"), f$gid("\U0001d44f")))
})

test_that("\\overlinesegment and \\underlinesegment are laid out as \\overline and \\underline", {
  f <- stix()
  expect_equal(layout_equation("\\overlinesegment{AB}", f), layout_equation("\\overline{AB}", f))
  expect_equal(layout_equation("\\underlinesegment{AB}", f), layout_equation("\\underline{AB}", f))
})

test_that("an empty \\big delimiter is a null delimiter, as in KaTeX", {
  f <- stix()
  st <- text_style()
  with_null <- layout_equation("a \\bigl. b", f)
  without <- layout_equation("a b", f)
  expect_equal(with_null$w - without$w, null_delimiter_space * em_size(f, st))
  expect_equal(c(with_null$h, with_null$d), c(without$h, without$d))
  # an ordinary atom: a binary operator after it keeps its spaces
  expect_equal(
    layout_equation("a \\bigr. + b", f)$w - layout_equation("a \\bigr. b", f)$w,
    layout_equation("a + b", f)$w - layout_equation("a b", f)$w
  )
})

test_that("an accent on nothing is the accent character, with its width", {
  f <- stix()
  b <- layout_equation("\\text{a\\^{}b}", f)
  hat <- f$gid("ˆ")
  expect_true(hat %in% b$glyphs$gid)
  expect_equal(b$w, adv(f, "a") + adv(f, "ˆ") + adv(f, "b"))
  # the accent starts after "a", it is not drawn over it
  expect_equal(b$glyphs$x[b$glyphs$gid == hat], adv(f, "a"))
})

test_that("\\label is removed: it shows nothing", {
  f <- stix()
  expect_equal(layout_equation("\\label{eq: m} y = x", f), layout_equation("y = x", f))
})

test_that("sizing commands scale the glyphs, with the factor of the normal size", {
  f <- stix()
  x <- layout_equation("x", f)
  huge <- layout_equation("\\Huge x", f)
  expect_equal(huge$w, 2.488 * x$w)
  expect_equal(huge$h, 2.488 * x$h)
  expect_equal(layout_equation("\\tiny x", f)$w, 0.5 * x$w)
  # the factor of \tiny in \Huge is the one of \tiny, not 0.5 * 2.488
  b <- layout_equation("\\Huge{a {\\tiny b}}", f)
  expect_equal(b$w, 2.488 * layout_equation("a", f)$w + 0.5 * layout_equation("b", f)$w)
  # scripts and scripts of scripts have the sizes of KaTeX: \huge, \LARGE
  expect_equal(layout_equation("x^{\\Huge 2}", f)$glyphs$scale[2], 2.074)
  expect_equal(layout_equation("x^{y^{\\Huge 2}}", f)$glyphs$scale[3], 1.728)
})

test_that("a line break starts a new line, aligned on the left", {
  f <- stix()
  em <- f$upem
  a <- layout_equation("a", f)
  b <- layout_equation("b", f)
  ab <- layout_equation("a \\\\ b", f)
  expect_equal(ab$w, max(a$w, b$w))
  # baseline of the first line, next baseline 1.2 em below
  expect_equal(ab$h, a$h)
  expect_equal(ab$d, 1.2 * em + b$d)
  expect_equal(ab$glyphs$x, c(0, 0))
  expect_equal(ab$glyphs$y, c(0, -1.2 * em))
  expect_equal(layout_equation("a \\newline b", f)$d, ab$d)
  # tall lines are 0.1 em apart
  fr <- layout_equation("\\dfrac{1}{2}", f)
  two <- layout_equation("\\dfrac{1}{2} \\\\ \\dfrac{1}{2}", f)
  expect_equal(two$d, fr$d + max(1.2 * em, fr$d + fr$h + 0.1 * em))
})

test_that("line breaks are only used in the row of the equation, as in KaTeX", {
  f <- stix()
  em <- f$upem
  one_line <- layout_equation("a+{b c}+d", f)
  # in a group or a fraction, the break is ignored
  expect_equal(layout_equation("a+{b \\\\ c}+d", f)[c("w", "h", "d")], one_line[c("w", "h", "d")])
  expect_equal(
    layout_equation("\\frac{a \\\\ b}{c}", f)[c("w", "h", "d")],
    layout_equation("\\frac{a b}{c}", f)[c("w", "h", "d")]
  )
  # \\[1em] adds 1 em after the line
  ab <- layout_equation("a \\\\ b", f)
  expect_equal(layout_equation("a \\\\[1em] b", f)$d, ab$d + em)
})

test_that("\\vcenter centers its content on the math axis", {
  f <- stix()
  axis <- f$const[["axisHeight"]]
  x <- layout_equation("x", f)
  b <- layout_equation("\\vcenter{x}", f)
  expect_equal(b$w, x$w)
  # x is moved up: its middle is on the axis, its depth becomes negative
  # and the depth of the row is 0, as in TeX
  expect_equal(b$glyphs$y, axis - (x$h - x$d) / 2)
  expect_equal(b$h, x$h + axis - (x$h - x$d) / 2)
  expect_equal(b$d, 0)
  fr <- layout_equation("\\vcenter{\\frac{a}{b}}", f)
  expect_equal((fr$h - fr$d) / 2, axis)
})

test_that("\\pmb draws the glyphs twice, moved by the text shadow of KaTeX", {
  f <- stix()
  em <- f$upem
  x <- layout_equation("x+y", f)
  b <- layout_equation("\\pmb{x+y}", f)
  expect_equal(b[c("w", "h", "d")], x[c("w", "h", "d")])
  n <- nrow(x$glyphs)
  expect_equal(nrow(b$glyphs), 2 * n)
  expect_equal(b$glyphs$gid, rep(x$glyphs$gid, 2))
  expect_equal(b$glyphs$x[n + seq_len(n)], x$glyphs$x + 0.02 * em)
  expect_equal(b$glyphs$y[n + seq_len(n)], x$glyphs$y - 0.01 * em)
})

test_that("large operators without limits are bigger in display style", {
  f <- stix()
  inline <- layout_equation("\\int x", f, display = FALSE)
  display <- layout_equation("\\int x", f, display = TRUE)
  expect_gte(display$h + display$d, f$const[["displayOperatorMinHeight"]])
  expect_gt(display$h + display$d, inline$h + inline$d)
  expect_equal(layout_equation("\\displaystyle\\int x", f, display = FALSE)$h, display$h)
})

test_that("limits written by KaTeX in display mode are scripts outside of the display style", {
  f <- stix()
  # the sum of the denominator has its limit as a subscript (\nolimits)
  keep <- c("w", "h", "d")
  expect_equal(
    layout_equation("\\frac{1}{\\sum_{j} x}", f, display = TRUE)[keep],
    layout_equation("\\frac{1}{\\sum\\nolimits_{j} x}", f, display = TRUE)[keep]
  )
  expect_equal(
    layout_equation("\\frac{1}{\\lim_{x} y}", f, display = TRUE)[keep],
    layout_equation("\\frac{1}{\\lim\\nolimits_{x} y}", f, display = TRUE)[keep]
  )
  # in the display style, limits stay below
  expect_gt(
    layout_equation("\\sum_{j} x", f, display = TRUE)$d,
    layout_equation("\\sum\\nolimits_{j} x", f, display = TRUE)$d
  )
})

test_that("\\cfrac keeps the display style in its numerator and denominator", {
  f <- stix()
  expect_equal(
    layout_equation("\\cfrac{1}{1+\\cfrac{1}{x}}", f)[c("w", "h", "d")],
    layout_equation("\\frac{\\displaystyle 1}{\\displaystyle 1+\\frac{\\displaystyle 1}{\\displaystyle x}}", f)[c("w", "h", "d")]
  )
})

test_that("\\raisebox raises its content with its height and depth", {
  f <- stix()
  b <- layout_equation("\\text{b}", f)
  r <- layout_equation("\\raisebox{0.5em}{b}", f)
  expect_equal(r$h, b$h + 0.5 * f$upem)
  expect_equal(r$glyphs$y, 0.5 * f$upem)
})

test_that("delimiters of binomials have at least the size of TeX", {
  f <- stix()
  em <- f$upem
  d <- layout_equation("\\binom{n}{k}", f, display = TRUE)
  expect_gte(d$h + d$d, 2.39 * em)
  t <- layout_equation("\\binom{n}{k}", f, display = FALSE)
  expect_gte(t$h + t$d, 1.01 * em)
})

test_that("\\big| is an ordinary atom: no space around it", {
  f <- stix()
  b <- layout_equation("x\\big|y", f)
  bar <- layout_equation("\\big|", f)
  expect_equal(b$w, layout_equation("x", f)$w + bar$w + layout_equation("y", f)$w)
})
