# Reading of an OpenType math font: glyph advances ('hmtx'), ink boxes of the
# glyphs (outlines read by 'systemfonts', by glyph index, so that size
# variants without a Unicode character can be measured) and the 'MATH' table
# (constants, italic corrections, top accent attachments, vertical and
# horizontal size variants, vertical glyph assemblies). Values are in font
# units.
#
# Sources
#
# - OpenType specification, 'MATH' table: MathConstants, MathGlyphInfo
#   (MathItalicsCorrectionInfo, MathTopAccentAttachment), MathVariants
#   (MathGlyphConstruction, GlyphAssembly, GlyphPart), Coverage tables.
#   https://learn.microsoft.com/en-us/typography/opentype/spec/math
#   https://learn.microsoft.com/en-us/typography/opentype/spec/chapter2
# - OpenType specification, tables 'head', 'hhea', 'maxp', 'hmtx' and the
#   table directory of font files and font collections.
# - OpenType specification, 'GSUB' table: FeatureList, LookupList, single
#   (type 1), alternate (type 3) and extension (type 7) substitutions; the
#   feature 'ssty' (script style) gives the glyphs drawn for scripts, its
#   alternate N for the script level N.
#   https://learn.microsoft.com/en-us/typography/opentype/spec/gsub
#   https://learn.microsoft.com/en-us/typography/opentype/spec/features_pt#tag-ssty
#   https://learn.microsoft.com/en-us/typography/opentype/spec/otff
#
# The tables are read byte by byte from these specifications. No code was
# taken from other implementations.

# --- binary reading -----------------------------------------------------------

u16 <- function(r, o) as.integer(r[o + 1]) * 256L + as.integer(r[o + 2])
i16 <- function(r, o) {
  v <- u16(r, o)
  if (v >= 32768L) v - 65536L else v
}
u32 <- function(r, o) sum(as.numeric(r[o + 1:4]) * c(16777216, 65536, 256, 1))
u16v <- function(r, o, n) {
  if (n == 0) {
    return(integer())
  }
  k <- seq_len(n) * 2L - 1L
  as.integer(r[o + k]) * 256L + as.integer(r[o + k + 1L])
}
sfnt_tag <- function(r, o) {
  x <- r[o + 1:4]
  if (any(x == as.raw(0))) "" else rawToChar(x)
}

# Glyphs of a Coverage table and their coverage index.
read_coverage <- function(r, o) {
  format <- u16(r, o)
  n <- u16(r, o + 2)
  if (n == 0) {
    return(list(g = integer(), i = integer()))
  }
  if (format == 1) {
    return(list(g = u16v(r, o + 4, n), i = seq_len(n) - 1L))
  }
  v <- u16v(r, o + 4, n * 3)
  s <- v[c(TRUE, FALSE, FALSE)]
  e <- v[c(FALSE, TRUE, FALSE)]
  si <- v[c(FALSE, FALSE, TRUE)]
  list(
    g = unlist(Map(seq, s, e)),
    i = unlist(Map(function(a, b, c) c + 0:(b - a), s, e, si))
  )
}

# MathValueRecord constants, in the order of the MathConstants table.
math_constant_names <- c(
  "mathLeading", "axisHeight", "accentBaseHeight", "flattenedAccentBaseHeight",
  "subscriptShiftDown", "subscriptTopMax", "subscriptBaselineDropMin",
  "superscriptShiftUp", "superscriptShiftUpCramped", "superscriptBottomMin",
  "superscriptBaselineDropMax", "subSuperscriptGapMin",
  "superscriptBottomMaxWithSubscript", "spaceAfterScript", "upperLimitGapMin",
  "upperLimitBaselineRiseMin", "lowerLimitGapMin", "lowerLimitBaselineDropMin",
  "stackTopShiftUp", "stackTopDisplayStyleShiftUp", "stackBottomShiftDown",
  "stackBottomDisplayStyleShiftDown", "stackGapMin", "stackDisplayStyleGapMin",
  "stretchStackTopShiftUp", "stretchStackBottomShiftDown",
  "stretchStackGapAboveMin", "stretchStackGapBelowMin",
  "fractionNumeratorShiftUp", "fractionNumeratorDisplayStyleShiftUp",
  "fractionDenominatorShiftDown", "fractionDenominatorDisplayStyleShiftDown",
  "fractionNumeratorGapMin", "fractionNumDisplayStyleGapMin",
  "fractionRuleThickness", "fractionDenominatorGapMin",
  "fractionDenomDisplayStyleGapMin", "skewedFractionHorizontalGap",
  "skewedFractionVerticalGap", "overbarVerticalGap", "overbarRuleThickness",
  "overbarExtraAscender", "underbarVerticalGap", "underbarRuleThickness",
  "underbarExtraDescender", "radicalVerticalGap",
  "radicalDisplayStyleVerticalGap", "radicalRuleThickness",
  "radicalExtraAscender", "radicalKernBeforeDegree", "radicalKernAfterDegree"
)

# Construction offsets of the vertical (or horizontal) variants.
read_variants_table <- function(r, mv, horizontal) {
  n_vert <- u16(r, mv + 6)
  if (horizontal) {
    cov <- read_coverage(r, mv + u16(r, mv + 4))
    offsets <- u16v(r, mv + 10 + 2 * n_vert, u16(r, mv + 8))
  } else {
    cov <- read_coverage(r, mv + u16(r, mv + 2))
    offsets <- u16v(r, mv + 10, n_vert)
  }
  list(cov = cov, offsets = offsets)
}

# Glyphs of the feature 'ssty' (script style) of a 'GSUB' table at offset
# `o`: two integer vectors, indexed by glyph index + 1, giving the glyph to
# use at script level 1 (`alt1`) and at script levels 2 and more (`alt2`), NA
# when the glyph has no script style. A single substitution gives the same
# glyph at every level; an alternate substitution gives its first alternate
# at level 1 and its second one, if any, at level 2.
read_ssty <- function(r, o, n_glyphs) {
  alt1 <- alt2 <- rep(NA_integer_, n_glyphs)
  if (is.null(o)) {
    return(list(alt1 = alt1, alt2 = alt2))
  }
  fl <- o + u16(r, o + 6)
  ll <- o + u16(r, o + 8)
  lookups <- integer()
  for (k in seq_len(u16(r, fl))) {
    rec <- fl + 2 + (k - 1) * 6
    if (sfnt_tag(r, rec) == "ssty") {
      ft <- fl + u16(r, rec + 4)
      lookups <- c(lookups, u16v(r, ft + 4, u16(r, ft + 2)))
    }
  }
  set <- function(g, a1, a2) {
    new <- g >= 0 & g < n_glyphs & is.na(alt1[g + 1])
    alt1[g[new] + 1] <<- a1[new]
    alt2[g[new] + 1] <<- a2[new]
  }
  for (li in unique(lookups)) {
    lk <- ll + u16(r, ll + 2 + 2 * li)
    type <- u16(r, lk)
    for (st in lk + u16v(r, lk + 6, u16(r, lk + 4))) {
      st_type <- type
      if (type == 7) {
        st_type <- u16(r, st + 2)
        st <- st + u32(r, st + 4)
      }
      format <- u16(r, st)
      cov <- read_coverage(r, st + u16(r, st + 2))
      if (st_type == 1 && format == 1) {
        a <- (cov$g + i16(r, st + 4)) %% 65536L
        set(cov$g, a, a)
      } else if (st_type == 1 && format == 2) {
        a <- u16v(r, st + 6, u16(r, st + 4))[cov$i + 1]
        set(cov$g, a, a)
      } else if (st_type == 3) {
        sets <- st + u16v(r, st + 6, u16(r, st + 4))[cov$i + 1]
        a1 <- vapply(sets, function(s) u16(r, s + 2), 0L)
        a2 <- vapply(sets, function(s) u16(r, s + 2 + 2 * (u16(r, s) > 1)), 0L)
        set(cov$g, a1, a2)
      }
    }
  }
  list(alt1 = alt1, alt2 = alt2)
}

# A math font: its file, constants and accessors to the glyph data.
# `font` is the path of a font file, or a family name found with
# 'systemfonts'.
read_math_font <- function(font) {
  if (file.exists(font)) {
    path <- font
    index <- 0L
    family <- basename(font)
  } else {
    m <- systemfonts::match_fonts(font)
    path <- m$path
    index <- m$index
    family <- font
    if (is.na(path) || !file.exists(path)) {
      stop("Font '", font, "' can not be found.", call. = FALSE)
    }
  }
  r <- readBin(path, "raw", file.size(path))
  off <- if (sfnt_tag(r, 0) == "ttcf") u32(r, 12 + 4 * index) else 0
  tb <- list()
  for (i in seq_len(u16(r, off + 4))) {
    p <- off + 12 + (i - 1) * 16
    tb[[sfnt_tag(r, p)]] <- u32(r, p + 8)
  }
  if (is.null(tb$MATH)) {
    stop(
      "Font '", family, "' (", basename(path), ") has no OpenType 'MATH' ",
      "table: use a math font such as 'STIX Two Math'.",
      call. = FALSE
    )
  }
  upem <- u16(r, tb$head + 18)
  n_metrics <- u16(r, tb$hhea + 34)
  n_glyphs <- u16(r, tb$maxp + 4)
  adv <- u16v(r, tb$hmtx, n_metrics * 2)[c(TRUE, FALSE)]
  if (n_glyphs > n_metrics) {
    adv <- c(adv, rep(adv[n_metrics], n_glyphs - n_metrics))
  }

  M <- tb$MATH
  mc <- M + u16(r, M + 4)
  gi <- M + u16(r, M + 6)
  mv <- M + u16(r, M + 8)

  values <- vapply(
    seq_along(math_constant_names),
    function(k) i16(r, mc + 8 + (k - 1) * 4),
    0
  )
  names(values) <- math_constant_names
  const <- c(
    scriptPercentScaleDown = i16(r, mc),
    scriptScriptPercentScaleDown = i16(r, mc + 2),
    delimitedSubFormulaMinHeight = u16(r, mc + 4),
    displayOperatorMinHeight = u16(r, mc + 6),
    values,
    radicalDegreeBottomRaisePercent = i16(r, mc + 8 + 51 * 4)
  )

  value_table <- function(o) {
    cov <- read_coverage(r, o + u16(r, o))
    val <- vapply(
      seq_len(u16(r, o + 2)),
      function(k) i16(r, o + 4 + (k - 1) * 4),
      0
    )
    function(g) {
      k <- match(g, cov$g)
      if (is.na(k)) NA_real_ else val[cov$i[k] + 1]
    }
  }
  italic <- value_table(gi + u16(r, gi))
  top_accent <- value_table(gi + u16(r, gi + 2))

  vert <- read_variants_table(r, mv, horizontal = FALSE)
  horiz <- read_variants_table(r, mv, horizontal = TRUE)
  construction <- function(tab, g) {
    k <- match(g, tab$cov$g)
    if (is.na(k)) NA else mv + tab$offsets[tab$cov$i[k] + 1]
  }
  variants <- function(tab, g) {
    co <- construction(tab, g)
    if (is.na(co)) {
      return(data.frame(gid = g, size = NA_real_))
    }
    n <- u16(r, co + 2)
    v <- u16v(r, co + 4, 2 * n)
    data.frame(gid = v[c(TRUE, FALSE)], size = v[c(FALSE, TRUE)])
  }

  ssty <- read_ssty(r, tb$GSUB, n_glyphs)

  cache <- new.env(parent = emptyenv())
  cache$ink <- list()
  cache$gid <- list()

  list(
    family = family,
    path = path,
    index = index,
    upem = upem,
    adv = adv,
    const = const,
    min_overlap = u16(r, mv),
    # boxes of the equations laid out with this font (see layout_equation())
    layouts = new.env(parent = emptyenv()),
    # glyph index of a character (0 when the font has no glyph for it)
    gid = function(ch) {
      key <- ch
      if (is.null(cache$gid[[key]])) {
        cache$gid[[key]] <- systemfonts::glyph_info(
          ch,
          path = path,
          index = index
        )$index
      }
      cache$gid[[key]]
    },
    italic = function(g) {
      v <- italic(g)
      if (is.na(v)) 0 else v
    },
    top_accent = top_accent,
    # glyph drawn for `g` at the script level `level` (feature 'ssty'): `g`
    # itself at level 0 or when the font has no script style for it
    ssty = function(g, level) {
      if (level < 1) {
        return(g)
      }
      a <- (if (level >= 2) ssty$alt2 else ssty$alt1)[g + 1]
      if (is.na(a)) g else a
    },
    variants = function(g) variants(vert, g),
    hvariants = function(g) variants(horiz, g),
    # glyph assembly: parts from bottom to top (vertical) or from left to
    # right (horizontal), NULL when none
    assembly = function(g, horizontal = FALSE) {
      co <- construction(if (horizontal) horiz else vert, g)
      if (is.na(co) || u16(r, co) == 0) {
        return(NULL)
      }
      a <- co + u16(r, co)
      n <- u16(r, a + 4)
      p <- matrix(u16v(r, a + 6, 5 * n), ncol = 5, byrow = TRUE)
      data.frame(
        gid = p[, 1],
        start = p[, 2],
        end = p[, 3],
        advance = p[, 4],
        extender = bitwAnd(p[, 5], 1L) == 1L
      )
    },
    # ink box of a glyph: xmin, xmax, ymin, ymax; zeros for empty glyphs
    ink = function(g) {
      key <- as.character(g)
      if (is.null(cache$ink[[key]])) {
        o <- systemfonts::glyph_outline(g, path = path, index = index, size = upem)
        cache$ink[[key]] <- if (nrow(o)) {
          c(min(o$x), max(o$x), min(o$y), max(o$y))
        } else {
          c(0, 0, 0, 0)
        }
      }
      cache$ink[[key]]
    }
  )
}

# Math fonts are read once per session.
math_font <- function(font) {
  key <- paste0("font_", font)
  if (is.null(equatags_cache[[key]])) {
    equatags_cache[[key]] <- read_math_font(font)
  }
  equatags_cache[[key]]
}
