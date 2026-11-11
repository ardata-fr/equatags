#' @export
#' @title Size of equations
#' @description Compute the size of equations written in LaTeX, as they are
#' displayed with an OpenType math font.
#'
#' The equations are converted to MathML with 'KaTeX' (the MathML used by
#' [transform_mathjax()]), then laid out with the rules of TeX and the
#' constants of the 'MATH' table of the font: spaces between operators,
#' fractions, scripts, limits of large operators, radicals, stretchy
#' delimiters, matrices, accents. Words and spaces have the widths stored in
#' the font file.
#'
#' The result does not depend on a graphic device or an application, only on
#' the font file. It is close to what 'Microsoft Word' displays with the same
#' font (a few percent on usual equations), see the section "Differences with
#' Word and browsers".
#' @param x a character vector of LaTeX math expressions.
#' @param font an OpenType math font, i.e. a font with a 'MATH' table: the path
#' of a font file, or a family name such as "STIX Two Math", "Latin Modern
#' Math", "Libertinus Math" or "Cambria Math", found with 'systemfonts' (fonts
#' registered with [systemfonts::register_font()] can be used). The default
#' is the file of 'STIX Two Math' shipped with 'gdtools'
#' ([gdtools::stixmath_file()]): the results are then the same on every
#' machine. Use `font = "STIX Two Math"` for the version installed on the
#' system, if any.
#' @param size font size in points.
#' @param display if TRUE, use the display style (large operators with limits
#' above and below, larger fractions), otherwise the inline style.
#' @return A data.frame with one row per equation and columns `width`,
#' `height` (above the baseline) and `depth` (below the baseline), in inches.
#' Equations using a construct that is not supported give `NA` values, with a
#' warning.
#' @section Differences with Word and browsers:
#' The layout follows the rules of TeX, as 'KaTeX' and LaTeX do. Compared
#' with 'Microsoft Word' displaying the equation in display style with the
#' same font, the median difference of width is about 1% on a set of usual
#' equations. The known differences are:
#'
#' - Word spaces binary operators and fractions a little less than TeX: about
#'   0.17 em less around each binary operator (2 points at 12 points, as in
#'   `a + b`), and no space of the null delimiters around fractions (0.12 em
#'   on each side). Equations with such operators or fractions are a few
#'   percent wider than in Word;
#' - some equations are given a little more space than in Word, which is
#'   harmless when the size is used to set the width of a column: the
#'   height of matrices in parentheses (up to 20%), the width of matrices in
#'   brackets (about 9%), the height of names of functions with an exponent,
#'   such as `\sin^2 x` (about 20%), the width of `\left( ... \right)` around
#'   a fraction (up to 15%);
#' - Word uses the font 'Cambria Math' unless the document sets another math
#'   font: the equations are then about 5% wider;
#' - browsers displaying the HTML output of 'KaTeX' use the fonts of 'KaTeX':
#'   the sizes differ by about 7%. The environment `array` is also 1 em
#'   wider in browsers (and in LaTeX), which add half the space between
#'   columns before its first column and after its last one; Word does not,
#'   and neither does this function.
#' @examples
#' equation_size(c("\\frac{a+b}{c}", "x^2 + \\sqrt{y_i}"))
#' equation_size("\\sum_{i=1}^{n} x_i", display = TRUE)
#' @seealso [equation_grob()] to draw an equation with 'grid'.
equation_size <- function(
  x,
  font = gdtools::stixmath_file(),
  size = 12,
  display = FALSE
) {
  check_equation_args(x, font, size, display)
  f <- math_font(font)
  k <- size / f$upem / 72
  sizes <- lapply(x, function(tex) {
    if (is.na(tex)) {
      return(c(NA_real_, NA_real_, NA_real_))
    }
    b <- tryCatch(layout_equation(tex, f, display = display), error = function(e) {
      warning("Equation '", tex, "' can not be measured: ", conditionMessage(e), call. = FALSE)
      NULL
    })
    if (is.null(b)) c(NA_real_, NA_real_, NA_real_) else c(b$w, b$h, b$d) * k
  })
  sizes <- do.call(rbind, sizes)
  data.frame(width = sizes[, 1], height = sizes[, 2], depth = sizes[, 3])
}

#' @export
#' @title Draw an equation with 'grid'
#' @description Create a grob drawing an equation written in LaTeX with an
#' OpenType math font. The equation is laid out as by [equation_size()]; the
#' glyphs are drawn with [grid::glyphGrob()], fraction bars and other rules
#' with rectangles, so the size of the drawing is the size computed.
#'
#' The graphic device must support the drawing of glyphs (R >= 4.3, see
#' `dev.capabilities()$glyphs`): 'ragg', 'svglite', `pdf()` and `cairo_pdf()`
#' do.
#'
#' The size of the grob is computed when it is created, from the font file
#' only: `widthDetails()`, `heightDetails()`, `ascentDetails()` and
#' `descentDetails()` return it in "bigpts" without measuring anything, and
#' no graphic device is needed until the grob is drawn. To get the size
#' without a device, use [equation_size()] (inches) or the numeric value of
#' these units: [grid::convertWidth()] opens a device when none is open.
#' @inheritParams equation_size
#' @param x a LaTeX math expression (a single string).
#' @param x0,y0 position of the equation, numeric values (in `default.units`)
#' or units.
#' @param hjust,vjust justification of the equation box relative to its
#' position (0 is left or bottom, 1 is right or top).
#' @param colour colour of the equation.
#' @param default.units units of `x0` and `y0` when they are numeric.
#' @param name,vp name and viewport of the grob.
#' @return A grob of class `equation_grob`.
#' @examples
#' if (getRversion() >= "4.3.0") {
#'   library(grid)
#'   grid.newpage()
#'   grid.draw(equation_grob("\\frac{a+b}{c} = \\sqrt{x^2 + y^2}", size = 24))
#' }
#' @seealso [equation_size()]
equation_grob <- function(
  x,
  font = gdtools::stixmath_file(),
  size = 12,
  display = FALSE,
  x0 = 0.5,
  y0 = 0.5,
  hjust = 0.5,
  vjust = 0.5,
  colour = "black",
  default.units = "npc",
  name = NULL,
  vp = NULL
) {
  if (getRversion() < "4.3.0") {
    stop("equation_grob() requires R >= 4.3.0 (grid::glyphGrob()).", call. = FALSE)
  }
  check_equation_args(x, font, size, display)
  if (length(x) != 1 || is.na(x)) {
    stop("`x` must be a single LaTeX expression.", call. = FALSE)
  }
  if (!grid::is.unit(x0)) x0 <- grid::unit(x0, default.units)
  if (!grid::is.unit(y0)) y0 <- grid::unit(y0, default.units)

  f <- math_font(font)
  b <- layout_equation(x, f, display = display)
  k <- size / f$upem # font units to big points (1/72 inch)
  w <- b$w * k
  h <- b$h * k
  d <- b$d * k
  # origin of the equation: left end of its baseline
  ox <- x0 - grid::unit(hjust * w, "bigpts")
  oy <- y0 - grid::unit(vjust * (h + d) - d, "bigpts")

  # colour of each item: its own (\color, \textcolor), or `colour`
  item_colour <- function(d) {
    if (!"col" %in% names(d)) {
      return(rep(colour, nrow(d)))
    }
    ifelse(is.na(d$col), colour, d$col)
  }

  children <- list()
  fl <- b$fills
  if (nrow(fl)) {
    # backgrounds (\colorbox) first
    children$fills <- grid::rectGrob(
      x = ox + grid::unit(fl$x * k, "bigpts"),
      y = oy + grid::unit(fl$y * k, "bigpts"),
      width = grid::unit(fl$w * k, "bigpts"),
      height = grid::unit(fl$h * k, "bigpts"),
      just = c(0, 0),
      gp = grid::gpar(fill = fl$col, col = NA),
      name = "fills"
    )
  }
  g <- b$glyphs
  if (nrow(g)) {
    # functions of R >= 4.3, found at run time
    gr_dev <- function(name) getExportedValue("grDevices", name)
    font_list <- gr_dev("glyphFontList")(
      gr_dev("glyphFont")(f$path, f$index, "", 400, "normal")
    )
    info <- gr_dev("glyphInfo")(
      id = g$gid,
      x = g$x * k,
      y = g$y * k,
      font = 1L,
      size = size * g$scale,
      fontList = font_list,
      width = w,
      height = h + d,
      hAnchor = gr_dev("glyphAnchor")(0, "left"),
      vAnchor = gr_dev("glyphAnchor")(0, "bottom"),
      col = item_colour(g)
    )
    children$glyphs <- getExportedValue("grid", "glyphGrob")(
      info,
      x = ox,
      y = oy,
      hjust = 0,
      vjust = 0,
      gp = grid::gpar(col = colour),
      name = "glyphs"
    )
  }
  r <- b$rules
  if (nrow(r)) {
    children$rules <- grid::rectGrob(
      x = ox + grid::unit(r$x * k, "bigpts"),
      y = oy + grid::unit(r$y * k, "bigpts"),
      width = grid::unit(r$w * k, "bigpts"),
      height = grid::unit(r$h * k, "bigpts"),
      just = c(0, 0),
      gp = grid::gpar(fill = item_colour(r), col = NA),
      name = "rules"
    )
  }
  l <- b$lines
  if (nrow(l)) {
    children$lines <- grid::segmentsGrob(
      x0 = ox + grid::unit(l$x0 * k, "bigpts"),
      y0 = oy + grid::unit(l$y0 * k, "bigpts"),
      x1 = ox + grid::unit(l$x1 * k, "bigpts"),
      y1 = oy + grid::unit(l$y1 * k, "bigpts"),
      # lwd is in 1/96 inch
      gp = grid::gpar(col = item_colour(l), lwd = l$thickness * k * 96 / 72, lineend = "butt"),
      name = "lines"
    )
  }
  grid::gTree(
    children = do.call(grid::gList, children),
    width = w,
    height = h,
    depth = d,
    x0 = x0,
    y0 = y0,
    hjust = hjust,
    vjust = vjust,
    tex = x,
    name = name,
    vp = vp,
    cl = "equation_grob"
  )
}

#' @export
#' @importFrom grid widthDetails
widthDetails.equation_grob <- function(x) {
  grid::unit(x$width, "bigpts")
}

#' @export
#' @importFrom grid heightDetails
heightDetails.equation_grob <- function(x) {
  grid::unit(x$height + x$depth, "bigpts")
}

#' @export
#' @importFrom grid ascentDetails
ascentDetails.equation_grob <- function(x) {
  grid::unit(x$height, "bigpts")
}

#' @export
#' @importFrom grid descentDetails
descentDetails.equation_grob <- function(x) {
  grid::unit(x$depth, "bigpts")
}

check_equation_args <- function(x, font, size, display) {
  if (!is.character(x)) {
    stop("`x` must be a character vector.", call. = FALSE)
  }
  if (!is.character(font) || length(font) != 1 || is.na(font)) {
    stop("`font` must be a font file or a font family name.", call. = FALSE)
  }
  if (!is.numeric(size) || length(size) != 1 || is.na(size) || size <= 0) {
    stop("`size` must be a positive number.", call. = FALSE)
  }
  if (!is.logical(display) || length(display) != 1 || is.na(display)) {
    stop("`display` must be TRUE or FALSE.", call. = FALSE)
  }
  invisible(NULL)
}
