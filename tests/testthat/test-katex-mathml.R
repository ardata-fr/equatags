# Contract with the MathML written by KaTeX (package 'katex').
#
# The layout (R/math_layout.R) does not read LaTeX but the MathML written by
# KaTeX, and relies on the forms checked below. Each test is named after the
# function that relies on the form: when an update of 'katex' changes one of
# them, the failing test says which function to update.

katex_doc <- function(tex, display = FALSE) {
  xml2::read_html(katex_mathml(tex, displayMode = display))
}
find_first <- function(tex, xpath, display = FALSE) {
  xml2::xml_find_first(katex_doc(tex, display), xpath)
}
attr_of <- function(tex, xpath, name, display = FALSE) {
  xml2::xml_attr(find_first(tex, xpath, display), name)
}

test_that("layout_tex(): the equation is in math/semantics, with a TeX annotation", {
  doc <- katex_doc("a+b")
  expect_length(xml2::xml_find_all(doc, "//math/semantics"), 1)
  expect_equal(
    xml2::xml_text(xml2::xml_find_first(doc, "//semantics/annotation")),
    "a+b"
  )
})

test_that("layout_frac(): fractions without bar have a linethickness of 0", {
  for (tex in c("\\binom{a}{b}", "a \\choose b", "\\genfrac{(}{)}{0pt}{}{a}{b}")) {
    expect_equal(attr_of(tex, "//mfrac", "linethickness"), "0px")
  }
  expect_true(is.na(attr_of("\\frac{a}{b}", "//mfrac", "linethickness")))
})

test_that("layout_element(): \\dfrac and \\displaystyle are an mstyle with displaystyle", {
  expect_equal(attr_of("\\dfrac{a}{b}", "//mstyle[mfrac]", "displaystyle"), "true")
  expect_equal(attr_of("\\displaystyle x", "//mstyle", "displaystyle"), "true")
  expect_equal(attr_of("\\scriptstyle x", "//mstyle", "scriptlevel"), "1")
})

test_that("is_large_op(): large operators are an mo with a character of op_large", {
  for (tex in c("\\sum", "\\prod", "\\int", "\\oint", "\\bigcup", "\\bigoplus")) {
    node <- find_first(tex, "//semantics/*[1]//mo | //semantics/mo")
    expect_true(is_large_op(node), info = tex)
  }
})

test_that("layout_scripts(), layout_underover(): limits depend on the style and on \\limits", {
  first_name <- function(tex, display) {
    xml2::xml_name(find_first(tex, "//semantics/mrow/*[1]", display))
  }
  expect_equal(first_name("\\sum_{i}^{n}", FALSE), "msubsup")
  expect_equal(first_name("\\sum_{i}^{n}", TRUE), "munderover")
  expect_equal(first_name("\\lim_{x}", FALSE), "msub")
  expect_equal(first_name("\\lim_{x}", TRUE), "munder")
  # \limits: one limit is under or over the base, two limits are scripts
  expect_equal(first_name("\\lim\\limits_{x}", FALSE), "munder")
  expect_equal(first_name("\\sum\\limits_{i}", FALSE), "munder")
  expect_equal(first_name("\\sum\\limits^{n}", FALSE), "mover")
  expect_equal(first_name("\\sum\\limits_{i}^{n}", FALSE), "msubsup")
})

test_that("token_box(), layout_mo(): names of functions are an mi followed by an invisible mo", {
  doc <- katex_doc("\\sin x")
  expect_equal(xml2::xml_text(xml2::xml_find_first(doc, "//mi")), "sin")
  expect_true(xml2::xml_text(xml2::xml_find_first(doc, "//mi/following-sibling::mo[1]")) %in% op_invisible)
  doc <- katex_doc("\\operatorname{argmax} x")
  expect_equal(xml2::xml_text(xml2::xml_find_first(doc, "//mi")), "argmax")
})

test_that("is_bar_delimiter(): bars used as delimiters are an mo of U+2223 or U+2225", {
  for (tex in c("\\lvert x \\rvert", "\\lVert x \\rVert", "\\left| x \\right|", "\\left\\| x \\right\\|")) {
    node <- find_first(tex, "//mo")
    expect_true(is_bar_delimiter(node), info = tex)
  }
  # the relation \mid is not a delimiter
  expect_false(is_bar_delimiter(find_first("a \\mid b", "//mo")))
  # |x| is an mi, laid out as an ordinary character
  expect_equal(xml2::xml_name(find_first("|x|", "//semantics/mrow/*[1]")), "mi")
})

test_that("explicit_spaces(): \\middle has lspace and rspace", {
  node <- find_first("\\left( a \\middle| b \\right)", "//mo[2]")
  expect_false(is.na(xml2::xml_attr(node, "lspace")))
  expect_false(is.na(xml2::xml_attr(node, "rspace")))
})

test_that("mo_class(): \\cdotp is an mo marked as separator", {
  expect_equal(attr_of("a \\cdotp b", "//mo", "separator"), "true")
})

test_that("is_stretchy(): delimiters of \\left and \\right are an mo with fence", {
  expect_equal(attr_of("\\left( x \\right)", "//mo", "fence"), "true")
  expect_equal(attr_of("\\begin{pmatrix}a\\end{pmatrix}", "//mo", "fence"), "true")
})

test_that("is_fixed_delimiter(): \\big and its variants have minsize = maxsize", {
  sizes <- c(big = "1.2em", Big = "1.8em", bigg = "2.4em", Bigg = "3em")
  for (cmd in names(sizes)) {
    tex <- paste0("\\", cmd, "( x")
    node <- find_first(tex, "//mo")
    expect_true(is_fixed_delimiter(node), info = tex)
    expect_equal(xml2::xml_attr(node, "minsize"), sizes[[cmd]], info = tex)
    expect_equal(xml2::xml_attr(node, "maxsize"), sizes[[cmd]], info = tex)
  }
})

test_that("layout_accent(): accents are an mover accent with a character of accent_combining", {
  for (tex in c("\\hat{x}", "\\bar{x}", "\\check{x}", "\\breve{x}", "\\tilde{x}",
                "\\mathring{x}", "\\ddot{x}", "\\dot{x}")) {
    expect_equal(attr_of(tex, "//mover", "accent"), "true", info = tex)
    ch <- xml2::xml_text(find_first(tex, "//mover/mo"))
    expect_true(ch %in% names(accent_combining), info = tex)
  }
  # \vec is written with the combining arrow itself
  expect_equal(xml2::xml_text(find_first("\\vec{x}", "//mover/mo")), "\u20d7")
})

test_that("layout_accent(): wide accents and \\overline are stretchy", {
  expect_equal(attr_of("\\widehat{xy}", "//mover/mo", "stretchy"), "true")
  expect_equal(attr_of("\\widetilde{xy}", "//mover/mo", "stretchy"), "true")
  node <- find_first("\\overline{x}", "//mover/mo")
  expect_equal(xml2::xml_text(node), "\u203e")
  expect_equal(xml2::xml_attr(node, "stretchy"), "true")
})

test_that("layout_accent_under(): \\underline is an munder accentunder with U+203E", {
  expect_equal(attr_of("\\underline{x}", "//munder", "accentunder"), "true")
  expect_equal(xml2::xml_text(find_first("\\underline{x}", "//munder/mo")), "\u203e")
})

test_that("is_h_stretchy(): braces and arrows over or under a base use h_stretchy_chars", {
  for (tex in c("\\overbrace{x}", "\\underbrace{x}", "\\overrightarrow{x}",
                "\\overleftarrow{x}", "\\overleftrightarrow{x}", "\\Overrightarrow{x}",
                "\\underrightarrow{x}", "\\overrightharpoon{x}")) {
    node <- find_first(tex, "//mover/mo | //munder/mo")
    expect_true(is_h_stretchy(node), info = tex)
  }
})

test_that("layout_stretched_base(): extensible arrows have a minsize and a label in an mpadded", {
  for (tex in c("\\xrightarrow{f}", "\\xleftarrow{f}", "\\xRightarrow{f}",
                "\\xLeftarrow{f}", "\\xleftrightarrow{f}", "\\xmapsto{f}")) {
    base <- find_first(tex, "//mover/*[1]")
    expect_true(is_h_stretchy(base), info = tex)
    expect_false(is.na(xml2::xml_attr(base, "minsize")), info = tex)
    expect_equal(xml2::xml_name(find_first(tex, "//mover/*[2]")), "mpadded", info = tex)
  }
})

test_that("layout_element(): \\overset and \\stackrel wrap an mover in an mo", {
  for (tex in c("\\overset{a}{=}", "\\stackrel{a}{=}")) {
    expect_equal(xml2::xml_name(find_first(tex, "//semantics/mrow/*[1]")), "mo", info = tex)
    expect_equal(xml2::xml_name(find_first(tex, "//semantics/mrow/mo/*[1]")), "mover", info = tex)
  }
  expect_equal(xml2::xml_name(find_first("\\underset{a}{=}", "//semantics/mrow/mo/*[1]")), "munder")
})

test_that("layout_padded(): \\rule writes its height in an mspace and unsigned lengths", {
  expect_equal(attr_of("\\rule{1em}{2pt}", "//mpadded/mspace", "mathbackground"), "black")
  expect_equal(attr_of("\\rule{1em}{2pt}", "//mpadded/mspace", "height"), "0.2em")
  # raised rule: the length has no sign, but is relative
  expect_equal(attr_of("\\rule[1pt]{1em}{2pt}", "//mpadded", "height"), "0.1em")
})

test_that("layout_padded(): \\colorbox is an mpadded with a background and signed lengths", {
  node <- find_first("\\colorbox{red}{x}", "//mpadded")
  expect_equal(xml2::xml_attr(node, "mathbackground"), "red")
  expect_match(xml2::xml_attr(node, "width"), "^[+]")
  expect_match(xml2::xml_attr(node, "height"), "^[+]")
})

test_that("layout_node(): colours are a mathcolor attribute", {
  expect_equal(attr_of("\\color{red}{x}", "//*[@mathcolor]", "mathcolor"), "red")
  expect_equal(attr_of("\\textcolor{blue}{x}", "//*[@mathcolor]", "mathcolor"), "blue")
})

test_that("layout_enclose(): frames and strikes are an menclose", {
  expect_equal(attr_of("\\boxed{x}", "//menclose", "notation"), "box")
  expect_equal(attr_of("\\fbox{x}", "//menclose", "notation"), "box")
  expect_equal(attr_of("\\cancel{x}", "//menclose", "notation"), "updiagonalstrike")
  expect_equal(attr_of("\\bcancel{x}", "//menclose", "notation"), "downdiagonalstrike")
  expect_match(attr_of("\\xcancel{x}", "//menclose", "notation"), "updiagonalstrike")
  expect_equal(attr_of("\\sout{x}", "//menclose", "notation"), "horizontalstrike")
})

test_that("layout_enclose(): the frame of an array is an menclose with its sides exchanged", {
  # vertical lines at the edges are written as top and bottom
  expect_equal(attr_of("\\begin{array}{|c|}a\\end{array}", "//menclose[mtable]", "notation"), "top bottom")
  # \hline at the edges are written as left and right
  expect_equal(
    attr_of("\\begin{array}{c}\\hline a\\\\\\hline\\end{array}", "//menclose[mtable]", "notation"),
    "left right"
  )
})

test_that("table_lines(): lines between columns and rows are columnlines and rowlines", {
  expect_equal(attr_of("\\begin{array}{c|c}a&b\\end{array}", "//mtable", "columnlines"), "solid")
  expect_equal(attr_of("\\begin{array}{c:c}a&b\\end{array}", "//mtable", "columnlines"), "dashed")
  expect_equal(attr_of("\\begin{array}{c}a\\\\\\hline b\\end{array}", "//mtable", "rowlines"), "solid")
  expect_equal(attr_of("\\begin{array}{c}a\\\\\\hdashline b\\end{array}", "//mtable", "rowlines"), "dashed")
})

test_that("layout_table(): numbered environments add empty cells to the rows", {
  doc <- katex_doc("\\begin{equation}a\\end{equation}", display = TRUE)
  cls <- xml2::xml_attr(xml2::xml_find_all(doc, "//mtr[1]/mtd"), "class")
  expect_true(all(c("mtr-glue", "mml-eqn-num") %in% cls))
})

test_that("layout_element(): phantoms and spaces are mphantom and mspace", {
  expect_false(inherits(find_first("\\phantom{x}", "//mphantom"), "xml_missing"))
  expect_equal(attr_of("a\\quad b", "//mspace", "width"), "1em")
  expect_equal(attr_of("a\\hspace{2em} b", "//mspace", "width"), "2em")
})

test_that("math_variant(): math alphabets are a mathvariant known by math_variant()", {
  variants <- c(
    "\\mathbf{x}" = "bold", "\\mathbb{R}" = "double-struck", "\\mathcal{A}" = "script",
    "\\mathfrak{A}" = "fraktur", "\\boldsymbol{x}" = "bold-italic",
    "\\mathsf{x}" = "sans-serif", "\\mathtt{x}" = "monospace", "\\mathrm{x}" = "normal"
  )
  for (tex in names(variants)) {
    v <- attr_of(tex, "//*[@mathvariant]", "mathvariant")
    expect_equal(v, variants[[tex]], info = tex)
    expect_true(v == "normal" || v %in% names(alpha_start), info = tex)
  }
})

test_that("layout_padded(): \\llap, \\rlap and \\clap are an mpadded of width 0", {
  lspace <- c("\\llap{x}" = "-1width", "\\rlap{x}" = NA, "\\clap{x}" = "-0.5width")
  for (tex in names(lspace)) {
    expect_equal(attr_of(tex, "//mpadded", "width"), "0px", info = tex)
    expect_equal(attr_of(tex, "//mpadded", "lspace"), lspace[[tex]], info = tex)
  }
})
