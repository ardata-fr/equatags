# Layout of a MathML tree (written by KaTeX) with the rules of TeX (The
# TeXbook, appendix G) and the constants of the OpenType 'MATH' table of the
# font. Values are in font units at a size of 1 em = units per em.
#
# Role of this file
#
# It computes where each glyph and each rule of an equation goes. It is used
# by the two user functions of R/equation_size.R, and by nothing else:
#
# - equation_size(): size of the box of each equation (width, height, depth);
# - equation_grob(): the same box, drawn with grid (glyphs, rules, lines).
#
# transform_mathjax() (R/write_mathjax.R) does not use it: Word and
# PowerPoint lay out the Office Open XML Math themselves.
#
# Chain of calls
#
#   equation_size() / equation_grob()           R/equation_size.R
#     math_font()        reads the font file    R/math_font.R
#     layout_equation()  cache by font, style and expression
#       layout_tex()     LaTeX -> MathML with katex::katex_mathml()
#         layout_node()  one MathML element -> one box (recursive),
#                        then its colour and background
#           layout_element()  dispatch on the name of the element
#
# The TeX class of an operator (binary, relation...) comes from the table of
# symbols of KaTeX, copied in R/math_classes.R (generated, do not edit).
#
# From LaTeX to the layout functions
#
# The functions work on the MathML of KaTeX, not on LaTeX. To find the code
# of a LaTeX construct, look at the MathML KaTeX writes for it
# (katex::katex_mathml("\\frac{a}{b}")), then at this table. The tests of
# tests/testthat/test-math-layout.R are named after the LaTeX constructs.
#
#   LaTeX                        MathML of KaTeX               function
#   ---------------------------  ----------------------------  ----------------------
#   a, x, \alpha, \sin, 12       mi, mn                        token_box()
#   \text{...}                   mtext                         token_box()
#   +, =, (, \le                 mo                            layout_mo()
#   \frac, \dfrac, \tfrac        mfrac                         layout_frac()
#   \binom                       mfrac linethickness="0px"     layout_stack()
#   x^2, x_i, x_i^2              msup, msub, msubsup           layout_scripts()
#   \sum_i^n (display style)     munderover                    layout_underover()
#   \lim_x (display style),      munder                        layout_underover()
#   \sum\limits_i
#   \overbrace, \underbrace,     mover, munder + stretchy mo   layout_underover()
#   \overgroup, \undergroup,
#   \utilde
#   \xrightarrow{f},             mover, stretchy base          layout_stretched_base()
#   \xhookrightarrow{f}
#   \hat, \widehat, \overline,   mover accent="true"           layout_accent()
#   \overrightarrow
#   \underline                   munder accentunder="true"     layout_accent_under()
#   \sqrt, \sqrt[n]              msqrt, mroot                  layout_radical()
#   \left( \right), \middle      row with fence or stretchy mo layout_row()
#   \big(, \Big(, \bigg(         mo stretchy minsize maxsize   layout_mo()
#   matrix, pmatrix, array,      mtable                        layout_table()
#   aligned, cases
#   \boxed, \fbox, \cancel,      menclose                      layout_enclose()
#   frame of an array
#   \colorbox, \rule, \llap,     mpadded                       layout_padded()
#   \rlap, \clap, labels of
#   \xrightarrow
#   \quad, \hspace, \,           mspace or mtext of spaces     layout_element(), token_box()
#   \phantom                     mphantom                      layout_element()
#   \displaystyle, \color        mstyle                        layout_element(), layout_node()
#   \overset, \stackrel          mo around an mover            layout_element()
#   \mathbf, \mathbb, \mathcal   mathvariant                   math_variant()
#
# Surprises of KaTeX, handled where they are used
#
# - Inline style: limits of large operators are scripts (\sum_i^n is an
#   msubsup). With \limits, a single limit is an munder or an mover
#   (\lim\limits_x, \sum\limits_i), but two limits are still an msubsup
#   (\sum\limits_i^n): the layout can not tell them from scripts.
# - \dfrac is an mfrac in an mstyle displaystyle="true"; \tfrac is a plain
#   mfrac.
# - |x| is written with mi U+2223; \lvert, \left| with mo U+2223 (the
#   character of the relation \mid), marked stretchy="false" or fence="true"
#   (see is_bar_delimiter() and hlist()).
# - \sin, \lim are an mi of several letters followed by an invisible mo
#   (U+2061, function application): see token_box() and layout_mo().
# - \rule writes the height and depth of its mpadded without sign, but they
#   are relative (see layout_padded()); the frame of an array is a menclose
#   around the mtable with its sides exchanged (see layout_enclose()).
# - Numbered environments add empty cells around the rows (see
#   layout_table()).
#
# Sources
#
# - D. E. Knuth, The TeXbook, appendix G "Generating boxes from formulas":
#   styles and cramped styles, script sizes, large operators centered on the
#   axis (rule 13), limits (rule 13a), radicals (rule 11, half of the excess
#   of the radical sign added to the gap), fractions and stacks (rule 15,
#   with the space of the null delimiters on both sides of a fraction,
#   15e),
#   scripts (rules 18a to 18f, no drop of the baselines for a character
#   base), and chapter 18 for the spaces between atoms (table of thin,
#   medium and thick spaces, only in text and display styles) and the binary
#   operators that become ordinary atoms.
# - The TeXbook, chapter 17: size of the delimiters of \left and \right
#   (\delimiterfactor 901, \delimitershortfall 5pt).
# - LaTeX: minimum height of the rows of arrays, \arraystretch times
#   \baselineskip, 70% above the baseline and 30% below (\strut).
# - LaTeX: space and rule of \fbox (\fboxsep 3pt, \fboxrule 0.4pt); the
#   package 'cancel' (strikes do not change the size of the content).
# - OpenType specification, 'MATH' table: meaning of the constants (gaps,
#   shifts, rule thicknesses, accentBaseHeight, radicalDegreeBottomRaisePercent
#   "in proportion to the height of the radical sign"), size variants and
#   glyph assemblies (minConnectorOverlap, connector lengths, extenders).
#   https://learn.microsoft.com/en-us/typography/opentype/spec/math
# - MathML Core (W3C): use of the 'MATH' table constants for MathML elements,
#   e.g. 3.3.3.1 for the target size of the radical sign (base ink height +
#   gap + rule thickness), 3.4.1.2 and 3.4.1.3 for the italic correction of
#   scripts (the superscript of a large operator is not moved by it, its
#   subscript is moved back), and the `mathvariant` transforms.
#   https://w3c.github.io/mathml-core/
# - OpenType specification, 'GSUB' table, feature 'ssty': the glyphs drawn
#   for scripts (script level 1) and scripts of scripts (level 2); MathML
#   Core applies it in scripts.
# - Unicode, Mathematical Alphanumeric Symbols (U+1D400 to U+1D7FF) and the
#   letterlike symbols filling its holes (U+210E...).
#
# The layout is written from these documents. No code was taken from other
# implementations (KaTeX, textshaping, HarfBuzz, browsers, Word).
#
# A box is a list: w, h, d (width, height above the baseline, depth below),
# cls (TeX atom class), italic (italic correction), is_char (box of a single
# character), glyphs (data.frame gid, x, y, scale), rules (data.frame x, y,
# w, h, filled rectangles such as fraction bars) and lines (data.frame x0,
# y0, x1, y1, thickness, segments such as the strokes of \cancel).
#
# A style is a list: level (0 for text or display, 1 for scripts, 2 for
# scripts of scripts), display (display style) and cramped.

# The empty box is built once: its data.frames are only copied when a box
# is changed.
empty_box_value <- list(
  w = 0, h = 0, d = 0, cls = "ord", italic = 0, is_char = FALSE,
  glyphs = data.frame(gid = integer(), x = numeric(), y = numeric(), scale = numeric()),
  rules = data.frame(x = numeric(), y = numeric(), w = numeric(), h = numeric()),
  lines = data.frame(
    x0 = numeric(), y0 = numeric(), x1 = numeric(), y1 = numeric(),
    thickness = numeric()
  ),
  fills = data.frame(x = numeric(), y = numeric(), w = numeric(), h = numeric())
)
empty_box <- function() empty_box_value

# Move all the items of a box (its size does not change).
shift_box <- function(b, dx, dy) {
  if (nrow(b$glyphs)) {
    b$glyphs$x <- b$glyphs$x + dx
    b$glyphs$y <- b$glyphs$y + dy
  }
  if (nrow(b$rules)) {
    b$rules$x <- b$rules$x + dx
    b$rules$y <- b$rules$y + dy
  }
  if (nrow(b$lines)) {
    b$lines[c("x0", "x1")] <- b$lines[c("x0", "x1")] + dx
    b$lines[c("y0", "y1")] <- b$lines[c("y0", "y1")] + dy
  }
  if (nrow(b$fills)) {
    b$fills$x <- b$fills$x + dx
    b$fills$y <- b$fills$y + dy
  }
  b
}

# Rows of two tables of items (glyphs, rules, lines or fills), with the
# columns of both: a column missing in one of them (colour) is NA there.
bind_items <- function(a, b) {
  for (nm in setdiff(names(b), names(a))) {
    a[[nm]] <- if (nrow(a)) NA else b[[nm]][0]
  }
  for (nm in setdiff(names(a), names(b))) {
    b[[nm]] <- if (nrow(b)) NA else a[[nm]][0]
  }
  rbind(a, b[names(a)])
}

# Colour of MathML (mathcolor, mathbackground, border of \fcolorbox), as
# written in LaTeX: a CSS colour, named or hexadecimal. CSS names are read
# in css_colours (R/css_colours.R): some are unknown to R ("teal") and
# others have another value there ("green", "gray"). Returns NA, with a
# warning, for a colour unknown to CSS and to R.
css_colour <- function(x) {
  if (is.na(x)) {
    return(x)
  }
  x <- trimws(x)
  key <- tolower(x)
  if (key %in% names(css_colours)) {
    return(unname(css_colours[key]))
  }
  if (grepl("^[0-9a-fA-F]{6}$", x)) {
    # KaTeX accepts hexadecimal colours without "#"
    x <- paste0("#", x)
  }
  ok <- tryCatch({
    grDevices::col2rgb(x)
    TRUE
  }, error = function(e) FALSE)
  if (!ok) {
    warning("unknown colour \"", x, "\", it is ignored.", call. = FALSE)
    return(NA_character_)
  }
  x
}

# Colour of the items of a box that have no colour yet (inner colours take
# precedence): mathcolor of MathML (\color, \textcolor).
paint_box <- function(b, colour) {
  for (part in c("glyphs", "rules", "lines")) {
    d <- b[[part]]
    if (!nrow(d)) next
    if (!"col" %in% names(d)) d$col <- NA_character_
    d$col[is.na(d$col)] <- colour
    b[[part]] <- d
  }
  b
}

# Background of a box: mathbackground of MathML (\colorbox).
fill_box <- function(b, colour) {
  b$fills <- bind_items(
    data.frame(x = 0, y = -b$d, w = b$w, h = b$h + b$d, col = colour),
    b$fills
  )
  b
}

# Empty space of width `space` on both sides of a box.
add_side_space <- function(b, space) {
  b <- shift_box(b, space, 0)
  b$w <- b$w + 2 * space
  b
}

# Add the glyphs, rules and lines of `b`, moved by (dx, dy), to `a`.
merge_items <- function(a, b, dx = 0, dy = 0) {
  b <- shift_box(b, dx, dy)
  a$glyphs <- bind_items(a$glyphs, b$glyphs)
  a$rules <- bind_items(a$rules, b$rules)
  a$lines <- bind_items(a$lines, b$lines)
  a$fills <- bind_items(a$fills, b$fills)
  a
}

# --- styles -------------------------------------------------------------------
#
# The style decides the size of the glyphs (scale_of()) and which constants
# of the font are used (display or not, cramped or not).

# A copy of the style `st` with some of its values changed.
with_style <- function(st, ...) {
  new <- list(...)
  st[names(new)] <- new
  st
}

# Scale of the glyphs: 1 in text and display styles, the percentages of the
# font for scripts (x^2) and scripts of scripts (x^{2^n}).
scale_of <- function(f, st) {
  c(
    1,
    f$const[["scriptPercentScaleDown"]] / 100,
    f$const[["scriptScriptPercentScaleDown"]] / 100
  )[min(st$level, 2) + 1]
}
# A constant of the 'MATH' table at the size of the style.
K <- function(f, st, name) f$const[[name]] * scale_of(f, st)
# Size of an em at the size of the style, in font units.
em_size <- function(f, st) f$upem * scale_of(f, st)
# Style of the scripts, limits and fractions of a style: one level smaller,
# never display.
sub_style <- function(st, cramped = st$cramped) {
  list(level = st$level + 1, display = FALSE, cramped = cramped)
}

# --- characters ---------------------------------------------------------------

# Mathematical alphanumeric symbols (block U+1D400) for `mathvariant`.
alpha_start <- c(
  bold = 0x1D400, italic = 0x1D434, "bold-italic" = 0x1D468, script = 0x1D49C,
  "bold-script" = 0x1D4D0, fraktur = 0x1D504, "double-struck" = 0x1D538,
  "bold-fraktur" = 0x1D56C, "sans-serif" = 0x1D5A0, monospace = 0x1D670
)
# Holes of the block: characters encoded elsewhere.
alpha_holes <- list(
  italic = c(h = 0x210E),
  script = c(
    B = 0x212C, E = 0x2130, F = 0x2131, H = 0x210B, I = 0x2110, L = 0x2112,
    M = 0x2133, R = 0x211B, e = 0x212F, g = 0x210A, o = 0x2134
  ),
  fraktur = c(C = 0x212D, H = 0x210C, I = 0x2111, R = 0x211C, Z = 0x2128),
  "double-struck" = c(
    C = 0x2102, H = 0x210D, N = 0x2115, P = 0x2119, Q = 0x211A, R = 0x211D,
    Z = 0x2124
  )
)
# Mathematical Greek letters (capital and small) by `mathvariant`.
greek_start <- list(
  bold = c(capital = 0x1D6A8, small = 0x1D6C2),
  italic = c(capital = 0x1D6E2, small = 0x1D6FC),
  "bold-italic" = c(capital = 0x1D71C, small = 0x1D736)
)
# Italic of the Greek symbol variants.
greek_italic_variants <- c(
  "\u03F5" = 0x1D716, "\u03D1" = 0x1D717, "\u03D5" = 0x1D719,
  "\u03D6" = 0x1D71B, "\u03F1" = 0x1D71A
)

# Character of `ch` in the alphabet `variant` (\mathbf{x}, \mathbb{R},
# \mathcal{A}, italic letters of mi).
math_variant <- function(ch, variant) {
  if (variant == "normal") {
    return(ch)
  }
  cp <- utf8ToInt(ch)
  if (length(cp) != 1) {
    return(ch)
  }
  hole <- alpha_holes[[variant]][ch]
  if (length(hole) && !is.na(hole)) {
    return(intToUtf8(hole))
  }
  start <- alpha_start[variant]
  if (!is.na(start)) {
    if (cp >= 65 && cp <= 90) {
      return(intToUtf8(start + cp - 65))
    }
    if (cp >= 97 && cp <= 122) {
      return(intToUtf8(start + 26 + cp - 97))
    }
  }
  # Greek: capitals U+0391 to U+03A9 and small letters U+03B1 to U+03C9 keep
  # their order in the mathematical blocks
  greek <- greek_start[[variant]]
  if (!is.null(greek)) {
    if (cp >= 0x391 && cp <= 0x3A9) {
      return(intToUtf8(greek[["capital"]] + cp - 0x391))
    }
    if (cp >= 0x3B1 && cp <= 0x3C9) {
      return(intToUtf8(greek[["small"]] + cp - 0x3B1))
    }
  }
  if (variant == "italic" && ch %in% names(greek_italic_variants)) {
    return(intToUtf8(greek_italic_variants[[ch]]))
  }
  if (variant == "bold" && cp >= 48 && cp <= 57) {
    return(intToUtf8(0x1D7CE + cp - 48))
  }
  if (variant == "double-struck" && cp >= 48 && cp <= 57) {
    return(intToUtf8(0x1D7D8 + cp - 48))
  }
  ch
}

# Large operators (\sum, \int, \bigcup...): class "op" of the table of
# symbols of KaTeX (R/math_classes.R).
op_large <- names(math_classes)[math_classes == "op"]
# Invisible operators of KaTeX (function application, invisible times...).
op_invisible <- c("\u2061", "\u2062", "\u2063", "\u2064")

# Vertical bars used as delimiters (\lvert, \rvert, \lVert, \bigl|,
# \left|): KaTeX writes them with the characters of the relations \mid and
# \parallel, but as stretchy or fence operators. Their class, opening or
# closing, is set by their position in the row (see hlist()).
bar_chars <- c("\u2223", "\u2225")
is_bar_delimiter <- function(node) {
  xml2::xml_name(node) == "mo" && xml2::xml_text(node) %in% bar_chars &&
    (identical(xml2::xml_attr(node, "stretchy"), "false") ||
      identical(xml2::xml_attr(node, "fence"), "true"))
}

# TeX class of an operator, from the table of symbols of KaTeX
# (R/math_classes.R): a negated relation written with the combining long
# solidus U+0338 (\not\subset) has the class of its relation; punctuation
# that is also a binary operator (\cdotp, U+22C5) is marked as a separator by
# KaTeX.
mo_class <- function(ch, node = NULL) {
  if (!is.null(node) && identical(xml2::xml_attr(node, "separator"), "true")) {
    return("punct")
  }
  ch <- sub("\u0338$", "", ch)
  cls <- if (nchar(ch) == 1) math_classes[ch] else NA
  if (is.na(cls)) "ord" else unname(cls)
}

# Box of one glyph: its advance width, the height and depth of its ink. In
# scripts, the glyph is the one the font draws for the script level (feature
# 'ssty' of the 'GSUB' table, as MathML Core does): wider and bolder, a prime
# placed for a superscript.
glyph_box <- function(f, st, gid) {
  gid <- f$ssty(gid, st$level)
  s <- scale_of(f, st)
  ink <- f$ink(gid)
  b <- empty_box()
  b$w <- f$adv[gid + 1] * s
  b$h <- max(ink[4], 0) * s
  b$d <- max(-ink[3], 0) * s
  b$italic <- f$italic(gid) * s
  b$is_char <- TRUE
  b$glyphs <- data.frame(gid = gid, x = 0, y = 0, scale = s)
  b
}

# Box of characters written side by side, without kerning (names of
# functions, numbers, \text{...}).
text_box <- function(f, st, chars) {
  b <- empty_box()
  for (ch in chars) {
    gid <- f$gid(ch)
    if (gid == 0) {
      next # missing or invisible character: no width
    }
    g <- glyph_box(f, st, gid)
    b <- merge_items(b, g, dx = b$w)
    b$h <- max(b$h, g$h)
    b$d <- max(b$d, g$d)
    b$w <- b$w + g$w
    b$italic <- g$italic
  }
  b$is_char <- length(chars) == 1
  b
}

# --- spacing ------------------------------------------------------------------

# Space of a null delimiter, on each side of a fraction, in em (TeX: the
# \nulldelimiterspace of 1.2pt, 0.12 em in 10pt type).
null_delimiter_space <- 0.12

# Space between atoms (TeX), in mu (1/18 em); negative values are only used in
# text and display styles, not in scripts.
atom_spacing <- matrix(
  c(
     0,  1, -2, -3,  0,  0,  0, -1,
     1,  1,  0, -3,  0,  0,  0, -1,
    -2, -2,  0,  0, -2,  0,  0, -2,
    -3, -3,  0,  0, -3,  0,  0, -3,
     0,  0,  0,  0,  0,  0,  0,  0,
     0,  1, -2, -3,  0,  0,  0, -1,
    -1, -1,  0, -1, -1, -1, -1, -1,
    -1,  1, -2, -3, -1,  0, -1, -1
  ),
  nrow = 8,
  byrow = TRUE,
  dimnames = rep(list(c("ord", "op", "bin", "rel", "open", "close", "punct", "inner")), 2)
)

# Space between two atoms of classes `left` and `right` (a + b, a = b,
# f(x), a, b), from atom_spacing.
atom_space <- function(f, st, left, right) {
  v <- atom_spacing[left, right]
  if (v < 0) {
    if (st$level > 0) {
      return(0)
    }
    v <- -v
  }
  c(0, 3, 4, 5)[v + 1] / 18 * em_size(f, st)
}

# Width in font units of a length in em ("0.1667em").
em_value <- function(x, f, st) {
  if (is.na(x) || !grepl("^[+-]?[0-9.]+(em|pt)$", x)) {
    return(0)
  }
  v <- as.numeric(sub("^[+]", "", sub("(em|pt)$", "", x)))
  # a point of TeX in 10pt type: a tenth of em
  if (grepl("pt$", x)) {
    v <- v / 10
  }
  v * em_size(f, st)
}

# Boxes side by side, with the spaces between atoms: every row of an
# equation goes through it (via layout_row()).
hlist <- function(f, st, boxes) {
  boxes <- Filter(function(b) !identical(b$cls, "skip"), boxes)
  if (!length(boxes)) {
    return(empty_box())
  }
  cls <- vapply(boxes, function(b) b$cls, "")
  n <- length(cls)
  # a bar delimiter opens at the start of the row or after an operator, it
  # closes otherwise (as the "|" of TeX in the rows of \left and \right)
  for (i in seq_len(n)) {
    if (cls[i] == "bar") {
      opens <- i == 1 || cls[i - 1] %in% c("bin", "op", "rel", "open", "punct")
      cls[i] <- if (opens) "open" else "close"
    }
  }
  # a binary operator first or after bin, op, rel, open or punct, or before
  # rel, close or punct, is an ordinary atom
  for (i in seq_len(n)) {
    if (cls[i] == "bin" && (i == 1 || cls[i - 1] %in% c("bin", "op", "rel", "open", "punct"))) {
      cls[i] <- "ord"
    }
  }
  for (i in seq_len(n)) {
    if (cls[i] == "bin" && (i == n || cls[i + 1] %in% c("rel", "close", "punct"))) {
      cls[i] <- "ord"
    }
  }
  out <- empty_box()
  for (i in seq_len(n)) {
    if (i > 1) {
      out$w <- out$w + atom_space(f, st, cls[i - 1], cls[i])
    }
    b <- boxes[[i]]
    out <- merge_items(out, b, dx = out$w)
    out$w <- out$w + b$w
    out$h <- max(out$h, b$h)
    out$d <- max(out$d, b$d)
  }
  out$italic <- boxes[[n]]$italic
  out$cls <- if (n == 1) boxes[[1]]$cls else "ord"
  out$is_char <- n == 1 && boxes[[1]]$is_char
  out
}

# --- elements -----------------------------------------------------------------

# Box of a MathML element, with its colour (\color, \textcolor) and its
# background (\colorbox). Every element goes through it.
layout_node <- function(node, f, st) {
  nm <- xml2::xml_name(node)
  kids <- xml2::xml_children(node)
  b <- layout_element(node, nm, kids, f, st)
  colour <- css_colour(xml2::xml_attr(node, "mathcolor"))
  if (!is.na(colour)) {
    b <- paint_box(b, colour)
  }
  background <- css_colour(xml2::xml_attr(node, "mathbackground"))
  if (!is.na(background)) {
    b <- fill_box(b, background)
  }
  b
}

# Dispatch on the name of the MathML element: see the table at the top of
# this file. Small elements are laid out here: mstyle (\displaystyle,
# \scriptstyle), mphantom (\phantom), mspace (\quad, \hspace, \rule).
# An element that is not handled is an error: equation_size() turns it into
# NA with a warning.
layout_element <- function(node, nm, kids, f, st) {
  if (nm %in% c("mo", "mi", "mn") && length(kids) > 0) {
    # KaTeX wraps some constructs in a token element to give them the class
    # of their base (\overset, \stackrel, \underset): their content is laid
    # out, with the class of the content
    return(layout_row(kids, f, st))
  }
  switch(
    nm,
    mstyle = {
      ds <- xml2::xml_attr(node, "displaystyle")
      sl <- xml2::xml_attr(node, "scriptlevel")
      if (!is.na(ds)) st$display <- ds == "true"
      if (!is.na(sl)) st$level <- as.integer(sl)
      layout_row(kids, f, st)
    },
    math = ,
    semantics = ,
    mrow = layout_row(kids[xml2::xml_name(kids) != "annotation"], f, st),
    mpadded = layout_padded(node, kids, f, st),
    mphantom = {
      # the space of its content, nothing drawn
      b <- layout_row(kids, f, st)
      b$glyphs <- b$glyphs[0, ]
      b$rules <- b$rules[0, ]
      b$lines <- b$lines[0, ]
      b$fills <- b$fills[0, ]
      b
    },
    menclose = layout_enclose(node, kids, f, st),
    msqrt = layout_radical(kids, NULL, f, st),
    mroot = layout_radical(kids[1], kids[[2]], f, st),
    mtable = layout_table(node, f, st),
    mspace = {
      # its background is drawn on its height and depth (\rule)
      b <- empty_box()
      b$w <- em_value(xml2::xml_attr(node, "width"), f, st)
      b$h <- em_value(xml2::xml_attr(node, "height"), f, st)
      b$d <- em_value(xml2::xml_attr(node, "depth"), f, st)
      b
    },
    mi = token_box(node, f, st, is_mi = TRUE),
    mn = ,
    mtext = token_box(node, f, st),
    mo = layout_mo(node, f, st),
    mfrac = layout_frac(kids, f, st, node),
    msub = layout_scripts(kids[[1]], kids[[2]], NULL, f, st),
    msup = layout_scripts(kids[[1]], NULL, kids[[2]], f, st),
    msubsup = layout_scripts(kids[[1]], kids[[2]], kids[[3]], f, st),
    munderover = layout_underover(kids[[1]], kids[[2]], kids[[3]], f, st),
    munder = if (identical(xml2::xml_attr(node, "accentunder"), "true")) {
      layout_accent_under(kids[[1]], kids[[2]], f, st)
    } else {
      layout_underover(kids[[1]], kids[[2]], NULL, f, st)
    },
    mover = if (identical(xml2::xml_attr(node, "accent"), "true")) {
      layout_accent(kids[[1]], kids[[2]], f, st)
    } else {
      layout_underover(kids[[1]], NULL, kids[[2]], f, st)
    },
    stop("MathML element <", nm, "> is not supported.", call. = FALSE)
  )
}

# Token elements mi, mn and mtext: their characters, in the alphabet given by
# `mathvariant` (\mathbf, \textbf...). In mi, a letter is italic by default
# and a name of function (several letters) is upright, of class "op".
token_box <- function(node, f, st, is_mi = FALSE) {
  chars <- strsplit(xml2::xml_text(node), "")[[1]]
  variant <- xml2::xml_attr(node, "mathvariant")
  if (is.na(variant) && is_mi && length(chars) == 1) {
    variant <- "italic"
  }
  if (!is.na(variant)) {
    chars <- vapply(chars, math_variant, "", variant = variant, USE.NAMES = FALSE)
  }
  b <- text_box(f, st, chars)
  b$cls <- if (is_mi && length(chars) > 1) "op" else "ord"
  b
}

# Operators, relations, delimiters and punctuation (+, =, (, \le, ,):
# their TeX class decides the spaces around them (see hlist()). Invisible
# operators of KaTeX take no space. Stretchy delimiters are not laid out
# here but in layout_row().
layout_mo <- function(node, f, st) {
  ch <- xml2::xml_text(node)
  if (ch %in% op_invisible) {
    b <- empty_box()
    b$cls <- "skip"
    return(b)
  }
  if (is_fixed_delimiter(node)) {
    if (!nzchar(ch)) {
      # \bigl.: a null delimiter, as in KaTeX (a space, no height). KaTeX
      # writes \bigl. and \bigr. the same way: an ordinary atom, so that a
      # binary operator after it keeps its spaces, as after \bigr. in TeX
      b <- empty_box()
      b$w <- null_delimiter_space * em_size(f, st)
      b$cls <- "ord"
      return(b)
    }
    # \big(, \Bigg|: their own size, in a row or as the base of scripts
    # (\big)^2)
    return(stretchy_delimiter(node, f, st, 0))
  }
  b <- text_box(f, st, strsplit(ch, "")[[1]])
  b$cls <- if (is_bar_delimiter(node)) "bar" else mo_class(ch, node)
  explicit_spaces(b, node, f, st)
}

# lspace and rspace of an operator (\mid, \middle of KaTeX): an ordinary atom
# with these margins.
explicit_spaces <- function(b, node, f, st) {
  ls <- xml2::xml_attr(node, "lspace")
  rs <- xml2::xml_attr(node, "rspace")
  if (!is.na(ls) || !is.na(rs)) {
    l <- em_value(ls, f, st)
    r <- em_value(rs, f, st)
    b <- shift_box(b, l, 0)
    b$w <- b$w + l + r
    b$cls <- "ord"
  }
  b
}

# \sum, \int, \prod, \bigcup...: a bigger glyph in display style.
is_large_op <- function(node) {
  xml2::xml_name(node) == "mo" && xml2::xml_text(node) %in% op_large
}

# Box moved so that its middle is on the math axis (the height of the bar of
# fractions): large operators, delimiters, tables.
center_on_axis <- function(b, f, st) {
  shift <- (b$h - b$d) / 2 - K(f, st, "axisHeight")
  b <- shift_box(b, 0, -shift)
  b$h <- b$h - shift
  b$d <- b$d + shift
  b
}

# Glyph of the first size variant at least `size` big (`v` from f$variants()
# or f$hvariants(), sizes multiplied by `scale`), NA if none.
first_variant <- function(v, size, scale = 1) {
  ok <- which(!is.na(v$size) & v$size * scale >= size)
  if (length(ok)) v$gid[ok[1]] else NA_integer_
}

# Large operator: in display style, the first size variant at least as high as
# displayOperatorMinHeight; centered on the math axis.
large_op <- function(node, f, st) {
  g <- f$gid(xml2::xml_text(node))
  if (st$display) {
    v <- f$variants(g)
    g <- first_variant(v, f$const[["displayOperatorMinHeight"]])
    if (is.na(g)) g <- v$gid[nrow(v)]
  }
  b <- center_on_axis(glyph_box(f, st, g), f, st)
  b$cls <- "op"
  b$is_char <- FALSE
  b$large <- TRUE
  b
}

# Boxes stacked vertically and centered on the width of the widest one: the
# baseline of box i is moved by dy[i], and horizontally by dx[i] after
# centering. The height and depth are those of the stacked boxes.
vstack <- function(boxes, dy, dx = numeric(length(boxes))) {
  w <- max(vapply(boxes, function(b) b$w, 0))
  out <- empty_box()
  for (i in seq_along(boxes)) {
    out <- merge_items(out, boxes[[i]], dx = (w - boxes[[i]]$w) / 2 + dx[i], dy = dy[i])
  }
  out$w <- w
  out$h <- max(mapply(function(b, y) b$h + y, boxes, dy))
  out$d <- max(mapply(function(b, y) b$d - y, boxes, dy))
  out
}

# Base of scripts or limits: a large operator gets its size and is centered
# on the axis.
base_box <- function(node, f, st) {
  if (is_large_op(node)) large_op(node, f, st) else layout_node(node, f, st)
}

# Fractions (\frac, \dfrac, \tfrac, \cfrac): numerator and denominator
# around the bar, on the math axis (TeX, rule 15). A fraction without bar
# (\binom, \choose) is a stack.
layout_frac <- function(kids, f, st, node = NULL) {
  lt <- if (is.null(node)) NA else xml2::xml_attr(node, "linethickness")
  if (!is.na(lt) && suppressWarnings(as.numeric(gsub("[a-z]", "", lt))) %in% 0) {
    return(layout_stack(kids, f, st))
  }
  # numerator and denominator: text style in display style, one level down
  # otherwise
  cst <- if (st$display) with_style(st, display = FALSE) else sub_style(st)
  num <- layout_node(kids[[1]], f, cst)
  den <- layout_node(kids[[2]], f, with_style(cst, cramped = TRUE))
  axis <- K(f, st, "axisHeight")
  t <- K(f, st, "fractionRuleThickness")
  if (st$display) {
    up <- K(f, st, "fractionNumeratorDisplayStyleShiftUp")
    down <- K(f, st, "fractionDenominatorDisplayStyleShiftDown")
    gap_num <- K(f, st, "fractionNumDisplayStyleGapMin")
    gap_den <- K(f, st, "fractionDenomDisplayStyleGapMin")
  } else {
    up <- K(f, st, "fractionNumeratorShiftUp")
    down <- K(f, st, "fractionDenominatorShiftDown")
    gap_num <- K(f, st, "fractionNumeratorGapMin")
    gap_den <- K(f, st, "fractionDenominatorGapMin")
  }
  up <- max(up, axis + t / 2 + gap_num + num$d)
  down <- max(down, den$h + gap_den - (axis - t / 2))
  b <- vstack(list(num, den), c(up, -down))
  b$rules <- bind_items(b$rules, data.frame(x = 0, y = axis - t / 2, w = b$w, h = t))
  b$cls <- "inner"
  # the null delimiters of a fraction (TeX, rule 15e)
  add_side_space(b, null_delimiter_space * em_size(f, st))
}

# Stack without bar (\binom).
layout_stack <- function(kids, f, st) {
  cst <- if (st$display) with_style(st, display = FALSE) else sub_style(st)
  top <- layout_node(kids[[1]], f, cst)
  bot <- layout_node(kids[[2]], f, with_style(cst, cramped = TRUE))
  if (st$display) {
    up <- K(f, st, "stackTopDisplayStyleShiftUp")
    down <- K(f, st, "stackBottomDisplayStyleShiftDown")
    gap <- K(f, st, "stackDisplayStyleGapMin")
  } else {
    up <- K(f, st, "stackTopShiftUp")
    down <- K(f, st, "stackBottomShiftDown")
    gap <- K(f, st, "stackGapMin")
  }
  excess <- gap - ((up - top$d) - (bot$h - down))
  if (excess > 0) {
    up <- up + excess / 2
    down <- down + excess / 2
  }
  b <- vstack(list(top, bot), c(up, -down))
  b$cls <- "inner"
  b
}

# Superscripts and subscripts (x^2, x_i, x_i^2), and the limits of large
# operators outside of the display style (\sum_i^n inline) (TeX, rules 18a
# to 18f).
layout_scripts <- function(base_node, sub_node, sup_node, f, st) {
  base <- base_box(base_node, f, st)
  sub <- if (!is.null(sub_node)) layout_node(sub_node, f, sub_style(st, cramped = TRUE))
  sup <- if (!is.null(sup_node)) layout_node(sup_node, f, sub_style(st))
  ss <- sub_style(st)
  # no drop of the baselines for a character base (TeX, rule 18a)
  u_drop <- if (base$is_char) 0 else base$h - K(f, ss, "superscriptBaselineDropMax")
  d_drop <- if (base$is_char) 0 else base$d + K(f, ss, "subscriptBaselineDropMin")
  if (!is.null(sup)) {
    u <- max(
      u_drop,
      K(f, st, if (st$cramped) "superscriptShiftUpCramped" else "superscriptShiftUp"),
      sup$d + K(f, st, "superscriptBottomMin")
    )
  }
  if (!is.null(sub)) {
    v <- max(d_drop, K(f, st, "subscriptShiftDown"), sub$h - K(f, st, "subscriptTopMax"))
  }
  if (!is.null(sub) && !is.null(sup)) {
    gap <- (u - sup$d) - (sub$h - v)
    if (gap < K(f, st, "subSuperscriptGapMin")) {
      v <- v + K(f, st, "subSuperscriptGapMin") - gap
    }
    lim <- K(f, st, "superscriptBottomMaxWithSubscript")
    if ((u - sup$d) < lim) {
      delta <- lim - (u - sup$d)
      u <- u + delta
      v <- v - delta
    }
  }
  b <- base
  w <- base$w
  ic <- base$italic
  if (!is.null(sup)) {
    # the italic correction moves the superscript, except for large
    # operators, whose italic correction moves the subscript (MathML Core
    # 3.4.1.2 and 3.4.1.3)
    sup_x <- base$w + if (isTRUE(base$large)) 0 else ic
    b <- merge_items(b, sup, dx = sup_x, dy = u)
    w <- max(w, sup_x + sup$w)
    b$h <- max(b$h, u + sup$h)
    b$d <- max(b$d, sup$d - u)
  }
  if (!is.null(sub)) {
    sx <- base$w - if (isTRUE(base$large)) ic else 0
    b <- merge_items(b, sub, dx = sx, dy = -v)
    w <- max(w, sx + sub$w)
    b$h <- max(b$h, sub$h - v)
    b$d <- max(b$d, v + sub$d)
  }
  b$w <- w + K(f, st, "spaceAfterScript")
  b$cls <- base$cls
  b$is_char <- FALSE
  b$italic <- 0
  b
}

# Limits above and below a base (\sum_{i=1}^n and \lim_{x} in display style,
# \limits), braces (\overbrace, \underbrace), \overset and \underset
# (TeX, rule 13a). A stretchy arrow as base (\xrightarrow) goes to
# layout_stretched_base().
layout_underover <- function(base_node, under_node, over_node, f, st) {
  # KaTeX writes scripts (msub, msubsup) for the limits that are not above
  # and below: munder and munderover are limits in every style (\limits)
  if (is_h_stretchy(base_node)) {
    return(layout_stretched_base(base_node, under_node, over_node, f, st))
  }
  base <- base_box(base_node, f, st)
  ss <- sub_style(st)
  # a stretchy over or under element (\overbrace, \underbrace) gets the width
  # of the base, at the size of the base
  under_ch <- if (!is.null(under_node)) stretchy_under_char(under_node)
  under_stretched <- !is.null(under_ch)
  over_stretched <- !is.null(over_node) && is_h_stretchy(over_node)
  under <- if (under_stretched) {
    stretch_glyph_h(f, st, under_ch, base$w)
  } else if (!is.null(under_node)) {
    layout_node(under_node, f, with_style(ss, cramped = TRUE))
  }
  over <- if (over_stretched) {
    stretch_glyph_h(f, st, xml2::xml_text(over_node), base$w)
  } else if (!is.null(over_node)) {
    layout_node(over_node, f, ss)
  }
  # limits of large operators are moved by half their italic correction
  ic <- if (isTRUE(base$large)) base$italic else 0
  boxes <- list(base)
  dy <- 0
  dx <- 0
  if (!is.null(over)) {
    rise <- if (over_stretched) {
      stretch_stack_shift(f, st, base, over, over = TRUE)
    } else {
      base$h + max(K(f, st, "upperLimitBaselineRiseMin"), K(f, st, "upperLimitGapMin") + over$d)
    }
    boxes <- c(boxes, list(over))
    dy <- c(dy, rise)
    dx <- c(dx, ic / 2)
  }
  if (!is.null(under)) {
    drop <- if (under_stretched) {
      -stretch_stack_shift(f, st, base, under, over = FALSE)
    } else {
      base$d + max(K(f, st, "lowerLimitBaselineDropMin"), K(f, st, "lowerLimitGapMin") + under$h)
    }
    boxes <- c(boxes, list(under))
    dy <- c(dy, -drop)
    dx <- c(dx, -ic / 2)
  }
  b <- vstack(boxes, dy, dx)
  b$cls <- base$cls
  b$is_char <- FALSE
  b
}

# A stretchy base with scripts above and below (\xrightarrow{f}): the scripts
# are laid out first, the base gets their width, at least its `minsize`;
# scripts are placed with the shifts and gaps of stacks with stretched
# elements (OpenType 'MATH': stretchStackTopShiftUp, stretchStackGapAboveMin,
# stretchStackBottomShiftDown, stretchStackGapBelowMin).
layout_stretched_base <- function(base_node, under_node, over_node, f, st) {
  ss <- sub_style(st)
  over <- if (!is.null(over_node)) layout_node(over_node, f, ss)
  under <- if (!is.null(under_node)) layout_node(under_node, f, with_style(ss, cramped = TRUE))
  ch <- xml2::xml_text(base_node)
  target <- max(
    em_value(xml2::xml_attr(base_node, "minsize"), f, st),
    if (!is.null(over)) over$w else 0,
    if (!is.null(under)) under$w else 0
  )
  base <- stretch_glyph_h(f, st, ch, target)
  boxes <- list(base)
  dy <- 0
  if (!is.null(over)) {
    u <- max(K(f, st, "stretchStackTopShiftUp"), base$h + K(f, st, "stretchStackGapAboveMin") + over$d)
    boxes <- c(boxes, list(over))
    dy <- c(dy, u)
  }
  if (!is.null(under)) {
    v <- max(K(f, st, "stretchStackBottomShiftDown"), base$d + K(f, st, "stretchStackGapBelowMin") + under$h)
    boxes <- c(boxes, list(under))
    dy <- c(dy, -v)
  }
  b <- vstack(boxes, dy)
  b$cls <- mo_class(ch, base_node)
  b$is_char <- FALSE
  b
}

# --- stretchy delimiters --------------------------------------------------------

is_stretchy <- function(node) {
  xml2::xml_name(node) == "mo" &&
    (identical(xml2::xml_attr(node, "fence"), "true") ||
      identical(xml2::xml_attr(node, "stretchy"), "true"))
}

# Vertical glyph with a total size (height + depth) of at least `target`, in
# units of the style: the first size variant big enough, otherwise a glyph
# assembly; the baseline is the one of the glyph (callers center it).
stretch_glyph <- function(f, st, ch, target) {
  s <- scale_of(f, st)
  g <- f$gid(ch)
  v <- f$variants(g)
  gv <- first_variant(v, target, s)
  if (!is.na(gv)) {
    return(glyph_box(f, st, gv))
  }
  a <- f$assembly(g)
  if (is.null(a)) {
    return(glyph_box(f, st, v$gid[nrow(v)]))
  }
  parts <- assemble_parts(a, target / s, f$min_overlap)
  y <- parts$offset
  ink <- t(vapply(parts$gid, f$ink, numeric(4)))
  b <- empty_box()
  b$glyphs <- data.frame(gid = parts$gid, x = 0, y = y * s, scale = s)
  b$w <- max(f$adv[parts$gid + 1]) * s
  b$h <- max(y + ink[, 4]) * s
  b$d <- -min(y + ink[, 3]) * s
  b
}

# Parts of a glyph assembly reaching the size `target` (font units): the
# extenders are repeated as needed, the overlaps between parts are spread
# to fit the size, bounded by minConnectorOverlap and by the lengths of the
# connectors (OpenType 'MATH', GlyphAssembly). Returns the parts with their
# offset along the direction of the assembly.
assemble_parts <- function(a, target, overlap_min) {
  for (n_rep in 0:100) {
    times <- ifelse(a$extender, n_rep, 1L)
    parts <- a[rep(seq_len(nrow(a)), times), ]
    if (sum(parts$advance) - (nrow(parts) - 1) * overlap_min >= target) break
  }
  n <- nrow(parts)
  overlap_max <- pmin(parts$end[-n], parts$start[-1])
  wanted <- if (n > 1) (sum(parts$advance) - target) / (n - 1) else 0
  overlap <- pmax(overlap_min, pmin(wanted, overlap_max))
  parts$offset <- c(0, cumsum(parts$advance[-n] - overlap))
  parts
}

# Horizontal glyph at least `target` wide (units of the style): the first
# horizontal size variant wide enough, otherwise a horizontal glyph
# assembly, otherwise the widest variant. The box spans the ink of the
# glyphs horizontally, from x = 0.
stretch_glyph_h <- function(f, st, ch, target) {
  s <- scale_of(f, st)
  g <- f$gid(ch)
  v <- f$hvariants(g)
  gv <- first_variant(v, target, s)
  a <- f$assembly(g, horizontal = TRUE)
  if (!is.na(gv)) {
    parts <- data.frame(gid = gv, offset = 0)
  } else if (!is.null(a)) {
    parts <- assemble_parts(a, target / s, f$min_overlap)
  } else {
    parts <- data.frame(gid = v$gid[nrow(v)], offset = 0)
  }
  ink <- t(vapply(parts$gid, f$ink, numeric(4)))
  left <- min(parts$offset + ink[, 1])
  b <- empty_box()
  b$glyphs <- data.frame(gid = parts$gid, x = (parts$offset - left) * s, y = 0, scale = s)
  b$w <- (max(parts$offset + ink[, 2]) - left) * s
  b$h <- max(ink[, 4]) * s
  b$d <- -min(ink[, 3]) * s
  b
}

# Characters stretched horizontally over or under a base: arrows, braces,
# brackets and bars.
h_stretchy_chars <- c(
  "\u2192", "\u2190", "\u2194", "\u21D2", "\u21D0", "\u21D4", "\u21A6",
  "\u21C0", "\u21BC", "\u21C1", "\u21BD", "\u23DE", "\u23DF", "\u23B4",
  "\u23B5", "\u23DC", "\u23DD", "\u203E", "_",
  # \xhookrightarrow, \xhookleftarrow, \overgroup, \undergroup
  "\u21AA", "\u21A9", "\u23E0", "\u23E1"
)

is_h_stretchy <- function(node) {
  xml2::xml_name(node) == "mo" &&
    identical(xml2::xml_attr(node, "stretchy"), "true") &&
    xml2::xml_text(node) %in% h_stretchy_chars
}

# Characters stretched under a base with another character of the font: the
# tilde of \utilde, written "~" by KaTeX, is the combining tilde below
# (U+0330), which has horizontal size variants. "~" is not in
# h_stretchy_chars: over the base, it is the wide accent \widetilde, laid out
# by layout_accent().
under_stretchy_chars <- c("~" = "\u0330")

# Character to stretch under a base for the element `node`, NULL if the
# element is not stretched.
stretchy_under_char <- function(node) {
  ch <- xml2::xml_text(node)
  if (is_h_stretchy(node)) {
    return(ch)
  }
  if (xml2::xml_name(node) == "mo" &&
    identical(xml2::xml_attr(node, "stretchy"), "true") &&
    ch %in% names(under_stretchy_chars)) {
    return(under_stretchy_chars[[ch]])
  }
  NULL
}

# A stretched glyph placed above (`over = TRUE`) or below a base, with the
# gaps of stacks with stretched elements (OpenType 'MATH':
# stretchStackGapBelowMin, stretchStackGapAboveMin). Returns the offset of
# the baseline of the glyph.
stretch_stack_shift <- function(f, st, base, glyph, over = TRUE) {
  if (over) {
    base$h + K(f, st, "stretchStackGapBelowMin") + glyph$d
  } else {
    -(base$d + K(f, st, "stretchStackGapAboveMin") + glyph$h)
  }
}

# Delimiters of a fixed size: \big, \Big, \bigg and \Bigg are written by
# KaTeX with minsize = maxsize (1.2, 1.8, 2.4 and 3 em), whatever their
# content. They are laid out by layout_mo(), not stretched by layout_row().
is_fixed_delimiter <- function(node) {
  is_stretchy(node) && !is.na(xml2::xml_attr(node, "minsize"))
}

# Vertical stretchy delimiter centered on the math axis, with a total size
# (height + depth) of `target`, kept between its minsize and maxsize
# (MathML Core, 3.2.4: stretching of operators).
stretchy_delimiter <- function(node, f, st, target) {
  lo <- em_value(xml2::xml_attr(node, "minsize"), f, st)
  hi <- em_value(xml2::xml_attr(node, "maxsize"), f, st)
  target <- max(target, lo)
  if (hi > 0) target <- min(target, hi)
  ch <- xml2::xml_text(node)
  b <- center_on_axis(stretch_glyph(f, st, ch, target), f, st)
  b$cls <- if (is_bar_delimiter(node)) "bar" else mo_class(ch, node)
  # \middle: margins given by KaTeX
  explicit_spaces(b, node, f, st)
}

# A row with stretchy delimiters (\left ... \right, matrices): size of the
# delimiters as in TeX (delimiterfactor 901, delimitershortfall of 0.5 em).
# Delimiters of a fixed size (\big) are content of the row, as in TeX.
layout_row <- function(kids, f, st) {
  stretchy <- vapply(kids, function(k) is_stretchy(k) && !is_fixed_delimiter(k), TRUE)
  boxes <- vector("list", length(kids))
  for (i in which(!stretchy)) {
    boxes[[i]] <- layout_node(kids[[i]], f, st)
  }
  if (any(stretchy)) {
    others <- boxes[!stretchy]
    axis <- K(f, st, "axisHeight")
    delta <- if (length(others)) {
      max(vapply(others, function(b) max(b$h - axis, b$d + axis), 0))
    } else {
      0
    }
    target <- max(2 * delta * 0.901, 2 * delta - 0.5 * em_size(f, st))
    for (i in which(stretchy)) {
      boxes[[i]] <- stretchy_delimiter(kids[[i]], f, st, target)
    }
  }
  out <- hlist(f, st, boxes)
  if (any(stretchy) && stretchy[1]) {
    out$cls <- "inner"
  }
  out
}

# --- radicals -------------------------------------------------------------------

# Radical. Target size of the radical sign: ink height and depth of the base
# + gap + rule thickness (MathML Core 3.3.3.1). When the sign found is taller,
# half of the excess is added to the gap (The TeXbook, appendix G, rule 11).
# The bottom of the degree is at radicalDegreeBottomRaisePercent of the height
# of the radical sign (OpenType 'MATH' specification).
layout_radical <- function(kids, degree_node, f, st) {
  body <- layout_row(kids, f, with_style(st, cramped = TRUE))
  gap <- K(f, st, if (st$display) "radicalDisplayStyleVerticalGap" else "radicalVerticalGap")
  t <- K(f, st, "radicalRuleThickness")
  extra <- K(f, st, "radicalExtraAscender")
  target <- body$h + body$d + gap + t
  rad <- stretch_glyph(f, st, "\u221A", target)
  excess <- (rad$h + rad$d) - target
  if (excess > 0) {
    gap <- gap + excess / 2
  }
  top <- body$h + gap + t
  dy <- top - rad$h
  b <- empty_box()
  x0 <- 0
  if (!is.null(degree_node)) {
    deg <- layout_node(degree_node, f, list(level = st$level + 2, display = FALSE, cramped = FALSE))
    before <- K(f, st, "radicalKernBeforeDegree")
    after <- K(f, st, "radicalKernAfterDegree")
    raise <- f$const[["radicalDegreeBottomRaisePercent"]] / 100 * (rad$h + rad$d)
    b <- merge_items(b, deg, dx = before, dy = dy - rad$d + raise)
    x0 <- max(before + deg$w + after, 0)
    b$h <- dy - rad$d + raise + deg$h
  }
  b <- merge_items(b, rad, dx = x0, dy = dy)
  b <- merge_items(b, body, dx = x0 + rad$w)
  b$rules <- bind_items(b$rules, data.frame(x = x0 + rad$w, y = body$h + gap, w = body$w, h = t))
  b$w <- x0 + rad$w + body$w
  b$h <- max(b$h, top + extra)
  b$d <- max(body$d, rad$d - dy)
  b$cls <- "ord"
  b
}

# --- tables -----------------------------------------------------------------------

# Alignment of the cells of each column: `columnalign` of the table, its last
# value repeated for the other columns (MathML), "center" by default; the
# `columnalign` of a cell takes precedence.
column_alignments <- function(node, nc) {
  align <- xml2::xml_attr(node, "columnalign")
  align <- if (is.na(align)) "center" else strsplit(trimws(align), "\\s+")[[1]]
  align[pmin(seq_len(nc), length(align))]
}

# x offset of a cell of width `w` in a column at `x` of width `col_w`
cell_offset <- function(align, x, col_w, w) {
  switch(align, left = x, right = x + col_w - w, x + (col_w - w) / 2)
}

# Lines between the columns or the rows of a table (`columnlines`,
# `rowlines`): one value per gap, the last one repeated (MathML), "none" by
# default.
table_lines <- function(node, name, n) {
  v <- xml2::xml_attr(node, name)
  if (n < 1 || is.na(v)) {
    return(rep("none", max(n, 0)))
  }
  v <- strsplit(trimws(v), "\\s+")[[1]]
  v[pmin(seq_len(n), length(v))]
}

# A solid or dashed rule from (x, y), of length `len` and thickness `t`,
# horizontal or vertical. Dashes of 4pt separated by 4pt, the default values
# of the LaTeX package 'arydshln' (\dashlinedash, \dashlinegap).
table_rule <- function(x, y, len, t, style, vertical, f, st) {
  if (style == "dashed") {
    dash <- 0.4 * em_size(f, st)
    from <- seq(0, max(len - dash, 0), by = 2 * dash)
    size <- pmin(dash, len - from)
  } else {
    from <- 0
    size <- len
  }
  if (vertical) {
    data.frame(x = x, y = y + from, w = t, h = size)
  } else {
    data.frame(x = x + from, y = y, w = size, h = t)
  }
}

# Space between the columns of a table: `columnspacing`, 0.8 em by default
# (MathML).
column_spacing <- function(node, f, st) {
  v <- xml2::xml_attr(node, "columnspacing")
  em_value(if (is.na(v)) "0.8em" else v, f, st)
}

# Matrices and arrays (matrix, pmatrix, array, aligned, cases, gathered):
# cells aligned by column, rows at least as high as a strut, lines between
# columns and rows (array{c|c}, \hline), centered on the math axis. The
# delimiters of pmatrix or cases are around the table, in layout_row(); its
# frame (array{|c|}) is in layout_enclose().
layout_table <- function(node, f, st) {
  rows <- xml2::xml_find_all(node, "./*[local-name()='mtr']")
  # KaTeX writes empty cells around the equations of the environments that
  # can be numbered (class "mtr-glue" and "mml-eqn-num"), to center them on
  # a page and place the number: they are not laid out
  mtds <- lapply(rows, function(r) {
    xml2::xml_find_all(r, paste0(
      "./*[local-name()='mtd'][not((@class='mtr-glue' or @class='mml-eqn-num')",
      " and count(node()) = 0)]"
    ))
  })
  cells <- lapply(mtds, function(r) {
    lapply(r, function(cell) layout_row(xml2::xml_children(cell), f, st))
  })
  nc <- max(lengths(cells))
  col_align <- column_alignments(node, nc)
  col_w <- vapply(seq_len(nc), function(j) {
    max(vapply(cells, function(r) if (j <= length(r)) r[[j]]$w else 0, 0))
  }, 0)
  # minimum height of the rows, as the strut of TeX and KaTeX (arrayskip of
  # 1.2 em, 70% above the baseline, 30% below); the space between rows is in
  # the strut, `rowspacing` is not used
  skip <- 1.2 * em_size(f, st)
  row_h <- vapply(cells, function(r) max(c(0.7 * skip, vapply(r, function(cell) cell$h, 0))), 0)
  row_d <- vapply(cells, function(r) max(c(0.3 * skip, vapply(r, function(cell) cell$d, 0))), 0)
  col_space <- column_spacing(node, f, st)
  total <- sum(row_h + row_d)
  col_x <- c(0, cumsum(col_w + col_space))[seq_len(nc)]
  b <- empty_box()
  y <- 0 # top of the table, going down
  for (i in seq_along(cells)) {
    base <- y - row_h[i]
    for (j in seq_along(cells[[i]])) {
      cell <- cells[[i]][[j]]
      align <- xml2::xml_attr(mtds[[i]][[j]], "columnalign")
      if (is.na(align)) align <- col_align[j]
      b <- merge_items(b, cell, dx = cell_offset(align, col_x[j], col_w[j], cell$w), dy = base)
    }
    y <- base - row_d[i]
  }
  # lines between columns and rows, without space of their own (\arrayrulewidth
  # of LaTeX, 0.4pt)
  t <- fbox_rule * em_size(f, st)
  width <- sum(col_w) + col_space * (nc - 1)
  col_lines <- table_lines(node, "columnlines", nc - 1)
  for (j in seq_along(col_lines)) {
    if (col_lines[j] == "none") next
    x <- col_x[j] + col_w[j] + (col_space - t) / 2
    b$rules <- bind_items(b$rules, table_rule(x, -total, total, t, col_lines[j], TRUE, f, st))
  }
  row_lines <- table_lines(node, "rowlines", length(cells) - 1)
  row_y <- -cumsum(row_h + row_d)
  for (i in seq_along(row_lines)) {
    if (row_lines[i] == "none") next
    hline <- table_rule(0, row_y[i] - t / 2, width, t, row_lines[i], FALSE, f, st)
    # the table of the line, for the frame of an array (see layout_enclose())
    hline$table <- xml2::xml_path(node)
    b$rules <- bind_items(b$rules, hline)
  }
  # centered on the math axis
  top <- total / 2 + K(f, st, "axisHeight")
  b <- shift_box(b, 0, top)
  b$h <- top
  b$d <- total - top
  b$w <- width
  b
}

# --- padding and accents under the base -------------------------------------------

# mpadded (KaTeX writes it for the labels of \xrightarrow, \colorbox,
# \fcolorbox, \rule, \llap, \rlap, \clap): `width` is a length in em,
# absolute or relative to the content ("+0.6em"); `lspace` moves the content
# to the right inside this width, by a length in em or by a multiple of the
# width of the content ("-1width" for \llap, "-0.5width" for \clap).
#
# \colorbox and \fcolorbox: KaTeX writes height="+6pt" voffset="3pt", which
# raises the content by 3pt in MathML; KaTeX (HTML) and LaTeX keep the
# content on the baseline, with the space of \fboxsep above and under it:
# the voffset is added to the depth instead of moving the content. The frame
# of \fcolorbox is written as CSS (style="border: 0.04em solid navy"): its
# rules are drawn around the box, which grows by their thickness.
layout_padded <- function(node, kids, f, st) {
  b <- layout_row(kids, f, st)
  content_w <- b$w
  # \rule: KaTeX raises the rule with an mpadded whose height and depth are
  # meant to be added to those of the rule (TeX raises the box), but writes
  # them without sign ("0.3em" for "+0.3em"); they are read as relative
  is_rule <- length(kids) == 1 && xml2::xml_name(kids[[1]]) == "mspace" &&
    !is.na(xml2::xml_attr(kids[[1]], "mathbackground"))
  # a length, absolute or relative to the current value ("+6pt")
  padded <- function(name, current) {
    v <- xml2::xml_attr(node, name)
    if (is.na(v)) {
      current
    } else if (is_rule || grepl("^[+-]", v)) {
      current + em_value(v, f, st)
    } else {
      em_value(v, f, st)
    }
  }
  b$w <- padded("width", b$w)
  b$h <- padded("height", b$h)
  b$d <- padded("depth", b$d)
  ls <- xml2::xml_attr(node, "lspace")
  lspace <- if (!is.na(ls) && grepl("^[+-]?[0-9.]+width$", ls)) {
    as.numeric(sub("width$", "", ls)) * content_w
  } else {
    em_value(ls, f, st)
  }
  voffset <- em_value(xml2::xml_attr(node, "voffset"), f, st)
  border <- padded_border(xml2::xml_attr(node, "style"), f, st)
  boxed <- !is.na(xml2::xml_attr(node, "mathbackground")) || !is.null(border)
  if (boxed && voffset != 0) {
    b$h <- b$h - voffset
    b$d <- b$d + voffset
    voffset <- 0
  }
  if (lspace != 0 || voffset != 0) {
    b <- shift_box(b, lspace, voffset)
  }
  if (!is.null(border)) {
    b <- frame_box(b, border$thickness, border$colour)
  }
  b
}

# Frame of \fcolorbox, written by KaTeX in the style of the mpadded
# ("border: 0.04em solid navy"): its thickness and its colour, or NULL.
padded_border <- function(style, f, st) {
  if (is.na(style)) {
    return(NULL)
  }
  m <- regmatches(
    style,
    regexec("border:\\s*([0-9.]+(em|pt))\\s+solid\\s+([^;]+)", style)
  )[[1]]
  if (!length(m)) {
    return(NULL)
  }
  list(thickness = em_value(m[2], f, st), colour = css_colour(trimws(m[4])))
}

# A frame of rules of thickness `t` around a box, which grows by `t` on
# every side.
frame_box <- function(b, t, colour) {
  b <- shift_box(b, t, 0)
  b$w <- b$w + 2 * t
  b$h <- b$h + t
  b$d <- b$d + t
  total <- b$h + b$d
  frame <- data.frame(
    x = c(0, b$w - t, 0, 0),
    y = c(-b$d, -b$d, -b$d, b$h - t),
    w = c(t, t, b$w, b$w),
    h = c(total, total, t, t),
    col = colour
  )
  b$rules <- bind_items(b$rules, frame)
  b
}

# munder accentunder: \underline is a rule under the base, with the gap, the
# thickness and the extra descender of the font (OpenType 'MATH':
# underbarVerticalGap, underbarRuleThickness, underbarExtraDescender); other
# elements under the base are laid out as munder.
layout_accent_under <- function(base_node, acc_node, f, st) {
  ch <- xml2::xml_text(acc_node)
  if (!ch %in% c("\u203E", "_", "\u00AF")) {
    return(layout_underover(base_node, acc_node, NULL, f, st))
  }
  b <- layout_node(base_node, f, st)
  gap <- K(f, st, "underbarVerticalGap")
  t <- K(f, st, "underbarRuleThickness")
  b$rules <- bind_items(b$rules, data.frame(x = 0, y = -b$d - gap - t, w = b$w, h = t))
  b$d <- b$d + gap + t + K(f, st, "underbarExtraDescender")
  b$is_char <- FALSE
  b
}

# --- enclosures -------------------------------------------------------------------

# Space between the content and the frame of \fbox and \boxed, and thickness
# of the frame, in em (LaTeX: \fboxsep of 3pt and \fboxrule of 0.4pt, in 10pt
# type).
fbox_sep <- 0.3
fbox_rule <- 0.04

# menclose: frames (\boxed, \fbox) and strikes (\cancel, \bcancel, \xcancel,
# \sout). Strikes do not change the size of the content, as with the LaTeX
# package 'cancel'; a frame adds its space and its rule on every side.
# Notations not listed below are ignored: the content is laid out alone.
layout_enclose <- function(node, kids, f, st) {
  b <- layout_row(kids, f, st)
  notation <- xml2::xml_attr(node, "notation")
  notation <- if (is.na(notation)) character() else strsplit(notation, "\\s+")[[1]]
  t <- K(f, st, "fractionRuleThickness")

  # strikes, over the content
  if ("updiagonalstrike" %in% notation) {
    b$lines <- bind_items(b$lines, data.frame(x0 = 0, y0 = -b$d, x1 = b$w, y1 = b$h, thickness = t))
  }
  if ("downdiagonalstrike" %in% notation) {
    b$lines <- bind_items(b$lines, data.frame(x0 = 0, y0 = b$h, x1 = b$w, y1 = -b$d, thickness = t))
  }
  if ("horizontalstrike" %in% notation) {
    axis <- K(f, st, "axisHeight")
    b$rules <- bind_items(b$rules, data.frame(x = 0, y = axis - t / 2, w = b$w, h = t))
  }
  if ("verticalstrike" %in% notation) {
    b$rules <- bind_items(b$rules, data.frame(x = (b$w - t) / 2, y = -b$d, w = t, h = b$h + b$d))
  }

  # frame
  sides <- intersect(notation, c("left", "right", "top", "bottom"))
  if (any(c("box", "roundedbox") %in% notation)) {
    sides <- c("left", "right", "top", "bottom")
  }
  # frame of an array (vertical lines and \hline at its edges): KaTeX writes
  # a menclose around the mtable, with the sides exchanged (top for left,
  # bottom for right and the reverse). As in LaTeX, the rules are drawn
  # outside of the space of half a column (\arraycolsep) on each side of the
  # table, without the space of \fbox; horizontal rules of the table go
  # through this space.
  is_array <- length(kids) == 1 && xml2::xml_name(kids[[1]]) == "mtable" &&
    !any(c("box", "roundedbox") %in% notation)
  if (is_array && length(sides)) {
    swap <- c(top = "left", left = "top", bottom = "right", right = "bottom")
    sides <- unname(swap[sides])
  }
  if (length(sides)) {
    s <- em_size(f, st)
    rule <- fbox_rule * s
    pad <- fbox_sep * s + rule
    vpad <- pad
    if (is_array) {
      half <- column_spacing(kids[[1]], f, st) / 2
      pad <- half + rule
      vpad <- rule
      # horizontal lines of this table (not of a table or a rule in a cell)
      full <- !is.null(b$rules$table) & b$rules$table %in% xml2::xml_path(kids[[1]])
      b$rules$x[full] <- -half
      b$rules$w[full] <- b$w + 2 * half
    }
    w <- b$w + 2 * pad
    bottom <- -b$d - vpad
    height <- b$h + b$d + 2 * vpad
    frame <- data.frame(
      side = c("left", "right", "bottom", "top"),
      x = c(0, w - rule, 0, 0),
      y = c(bottom, bottom, bottom, bottom + height - rule),
      w = c(rule, rule, w, w),
      h = c(height, height, rule, rule)
    )
    frame <- frame[frame$side %in% sides, c("x", "y", "w", "h")]
    inner <- shift_box(b, pad, 0)
    b$glyphs <- inner$glyphs
    b$lines <- inner$lines
    b$fills <- inner$fills
    b$rules <- bind_items(inner$rules, frame)
    b$w <- w
    b$h <- b$h + vpad
    b$d <- b$d + vpad
  }
  b$cls <- "ord"
  b$is_char <- FALSE
  b
}

# --- accents ----------------------------------------------------------------------

# Spacing accents written by KaTeX, and the combining accents of the font.
accent_combining <- c(
  "^" = "\u0302", "\u02C9" = "\u0304", "\u00AF" = "\u0304", "~" = "\u0303",
  "\u02D9" = "\u0307", "\u00A8" = "\u0308", "\u02C7" = "\u030C",
  "\u02D8" = "\u0306", "\u00B4" = "\u0301", "`" = "\u0300", "\u02DA" = "\u030A",
  # \acute and \grave
  "\u02CA" = "\u0301", "\u02CB" = "\u0300"
)

# Attachment point of an accent on a box: the one of its glyph when it has a
# single glyph, otherwise its center.
attach_x <- function(f, b) {
  if (nrow(b$glyphs) == 1) {
    a <- f$top_accent(b$glyphs$gid)
    if (!is.na(a)) {
      return(b$glyphs$x + a * b$glyphs$scale)
    }
  }
  b$w / 2
}

# Accents over a base: \hat, \tilde, \bar, \dot, \vec... (a glyph placed on
# the attachment point of the base), \widehat, \widetilde (the widest
# variant that fits), \overline (a rule), \overrightarrow (an arrow
# stretched to the width of the base).
layout_accent <- function(base_node, acc_node, f, st) {
  base <- layout_node(base_node, f, with_style(st, cramped = TRUE))
  ch <- xml2::xml_text(acc_node)
  stretchy <- identical(xml2::xml_attr(acc_node, "stretchy"), "true")
  if (!stretchy && base$w == 0 && !nrow(base$glyphs) && !nrow(base$rules)) {
    # accent on nothing (\^{} in a text, \hat{}): the accent character
    # itself, with its width, as in LaTeX
    return(text_box(f, st, ch))
  }
  b <- base
  b$is_char <- FALSE
  if (ch %in% c("\u203E", "\u00AF") && stretchy) {
    # \overline: the overline U+203E stretched over the base, placed as an
    # accent (MathML Core, mover accent): a rule with the position and the
    # thickness of the glyph, raised above accentBaseHeight with the base
    g <- f$gid("\u203E")
    if (g > 0) {
      s <- scale_of(f, st)
      ink <- f$ink(g) * s
      dy <- max(0, base$h - K(f, st, "accentBaseHeight"))
      y <- dy + ink[3]
      t <- ink[4] - ink[3]
      top <- dy + ink[4]
    } else {
      y <- base$h + K(f, st, "overbarVerticalGap")
      t <- K(f, st, "overbarRuleThickness")
      top <- y + t + K(f, st, "overbarExtraAscender")
    }
    b$rules <- bind_items(b$rules, data.frame(x = 0, y = y, w = base$w, h = t))
    b$h <- max(base$h, top)
    return(b)
  }
  if (is_h_stretchy(acc_node)) {
    # arrows over the base (\overrightarrow): stretched to the width of the
    # base, placed as a stretched element above it
    acc <- stretch_glyph_h(f, st, ch, base$w)
    rise <- stretch_stack_shift(f, st, base, acc, over = TRUE)
    b <- vstack(list(base, acc), c(0, rise))
    b$cls <- base$cls
    return(b)
  }
  comb <- if (ch %in% names(accent_combining)) accent_combining[[ch]] else ch
  g <- f$gid(comb)
  s <- scale_of(f, st)
  if (stretchy) {
    # wide accent: widest horizontal variant not wider than the base
    v <- f$hvariants(g)
    ok <- which(!is.na(v$size) & v$size * s <= base$w * 1.1)
    if (length(ok)) g <- v$gid[max(ok)]
  }
  # the glyph of the script level, for its attachment point and its ink
  g <- f$ssty(g, st$level)
  acc <- glyph_box(f, st, g)
  a_acc <- f$top_accent(g)
  if (is.na(a_acc)) a_acc <- f$adv[g + 1] / 2
  # above accentBaseHeight, the accent is raised with the base
  dy <- max(0, base$h - K(f, st, "accentBaseHeight"))
  b <- merge_items(b, acc, dx = attach_x(f, base) - a_acc * s, dy = dy)
  b$h <- max(base$h, dy + f$ink(g)[4] * s)
  b
}

# --- entry point --------------------------------------------------------------

# Box of an equation written in LaTeX: called by equation_size() and
# equation_grob() (R/equation_size.R). Boxes are in font units and do not
# depend on the font size: they are kept for the session in the environment
# of the font, by style and expression. Errors are not kept.
layout_equation <- function(tex, f, display = FALSE) {
  key <- paste0(if (display) "D" else "T", tex)
  b <- f$layouts[[key]]
  if (is.null(b)) {
    b <- layout_tex(tex, f, display)
    f$layouts[[key]] <- b
  }
  b
}

# LaTeX -> MathML of KaTeX -> box. KaTeX errors (unknown commands) are
# raised here.
layout_tex <- function(tex, f, display) {
  html <- fix_katex_mathml(katex_mathml(prepare_tex(tex), displayMode = display))
  doc <- xml2::read_html(html)
  math <- xml2::xml_find_first(doc, "//math")
  sem <- xml2::xml_find_first(math, "./semantics")
  node <- if (!inherits(sem, "xml_missing")) sem else math
  b <- layout_node(node, f, list(level = 0, display = display, cramped = FALSE))
  # the tables of the horizontal lines are only used by layout_enclose()
  b$rules$table <- NULL
  b
}
