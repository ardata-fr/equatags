# Constructs of KaTeX chosen to be hard to lay out (frames, stretched arrows,
# environments, spaces, sizes, fonts...), written for this corpus and
# compared with the HTML output of KaTeX in a browser. Each one
# is tested inline and in display mode. `inline` and `display` give the
# reason of an expected failure in this mode ("" if it works).
# Writes data-raw/corpus/sources/probes.rds.
# Run from the root of the package: Rscript data-raw/corpus/sources/probes.R
stopifnot("run from the root of the package" = file.exists("DESCRIPTION") && dir.exists("data-raw"))
unsupported <- "command not supported by KaTeX 0.16.9"
display_only <- "environment or \\tag of display mode only"
x <- list(
  list("cancel", r"(\cancel{x} \bcancel{y} \xcancel{z} \sout{abc})"),
  list("boxed", r"(\boxed{x^2})"),
  list("phase", r"(\phase{-78^\circ})"),
  list("angl", r"(\angl{n})"),
  list("xarrows", r"(\xrightarrow[under]{over} \xleftrightarrow{a} \xhookrightarrow{x})"),
  list("braces", r"(\overbrace{a+b}^{n} \underbrace{c+d}_{m})"),
  list("group accents", r"(\overgroup{AB} \utilde{AB} \overlinesegment{AB})"),
  list("aligned", r"(\begin{aligned} a &= b \\ &= c \end{aligned})"),
  list("cases", r"(\begin{cases} 1 & x>0 \\ 0 & \text{else}\end{cases})"),
  list("array rules", r"(\begin{array}{c|c} a & b \\ \hline c & d \end{array})"),
  list("CD", r"(\begin{CD} A @>a>> B \\ @VbVV @AAcA \\ C @= D \end{CD})", display_only),
  list("smallmatrix", r"(\begin{smallmatrix} a&b\\c&d\end{smallmatrix})"),
  list("alignat", r"(\begin{alignat}{2} a &= b & c &= d \end{alignat})", display_only),
  list("gathered", r"(\begin{gathered} a \\ b \end{gathered})"),
  list("equation", r"(\begin{equation} x \tag{1} \end{equation})", display_only),
  list("tag", r"(x \tag{A})", display_only),
  list("substack", r"(\substack{a \\ b} \sum_{\substack{i<n \\ j<m}} x)"),
  list("sideset", r"(\sideset{_a^b}{_c^d}\sum)", unsupported),
  list("prescripts", r"({}_aX_b^c)"),
  list("prescript", r"(\prescript{a}{b}{X})", unsupported),
  list("alphabets", r"(\mathcal{A}\mathfrak{B}\mathbb{C}\mathscr{D}\mathsf{E}\mathtt{F}\mathit{G}\boldsymbol{\alpha}\bm{\beta})"),
  list("text", r"(\text{é à ü 中文} \textit{it} \textbf{bf} \texttt{tt})"),
  list("operatorname", r"(\operatorname*{argmax}_x f \operatorname{sgn} x)"),
  list("left right middle", r"(\left. \frac{a}{b} \right|_{x=0} \left\langle x \middle| y \right\rangle)"),
  list("big delimiters", r"(\big( \Big[ \bigg\{ \Bigg\lfloor x \Bigg\rfloor)"),
  list("fractions", r"(\dfrac{a}{b} \tfrac{a}{b} \cfrac{1}{1+\cfrac{1}{x}} \genfrac(]{0pt}{2}{a}{b})"),
  list("binomials", r"(\binom{n}{k} \dbinom{n}{k} \tbinom{n}{k} {n \choose k} a \atop b {a \brace b} {a \brack b})"),
  list("overset", r"(\overset{!}{=} \underset{x}{\lim} \stackrel{def}{=})"),
  list("accents", r"(\vec{v} \widehat{abc} \widetilde{xyz} \overleftrightarrow{AB} \dot{x}\ddot{y} \mathring{a})"),
  list("spaces", r"(\kern1em a \mkern18mu b \hskip2em c \mskip3mu d \, \: \; \! \enspace \qquad)"),
  list("raisebox phantom", r"(\raisebox{0.5em}{x} \vphantom{A} \hphantom{B} \smash{y})"),
  list("rule", r"(\rule{1em}{0.5em} \rule[-0.2em]{2pt}{1em})"),
  list("colours", r"(\colorbox{teal}{$x$} \textcolor{#228B22}{y})"),
  list("rgb colour", r"(\color{rgb(10,20,30)} z)", "colour not supported by KaTeX 0.16.9"),
  list("sizes", r"(\Huge x \tiny y \scriptsize z \normalsize w)"),
  list("integrals", r"(\displaystyle\int\limits_0^1 \iint \oint \iiint \intop)"),
  list("limits", r"(\lim\limits_{x} \max\nolimits_y \sum\nolimits_k)"),
  list("negations", r"(\not= \neq \notin \nsubseteq \nless)"),
  list("math classes", r"(\mathrel{\#} \mathbin{\star} \mathop{\Pi} \mathpunct{,} \mathinner{x} \mathopen{(} \mathclose{)})"),
  list("links", r"(\href{https://a.b}{link} \url{https://a.b})"),
  list("html", r"(\htmlStyle{color:red}{x} \htmlClass{c}{y})"),
  list("char", r"(\char"263a \yen \pounds)"),
  list("euro", r"(\euro)", unsupported),
  list("unicode", r"(\unicode{x263A})", unsupported),
  list("ce", r"(\ce{H2O})", unsupported),
  list("pu", r"(\pu{123 kJ/mol})", unsupported),
  list("verb textsf", r"(\verb|a&b| \textsf{sf})"),
  list("emph", r"(\emph{em})", unsupported),
  list("delimiters", r"(\lparen x \rparen \llbracket x \rrbracket \lBrace x \rBrace)"),
  list("laps", r"(\mathllap{a} \mathrlap{b} \mathclap{c} \llap{d} \rlap{e})"),
  list("over", r"(a \over b)"),
  list("above", r"(a \above 2pt c)"),
  list("over above", r"(a \over b \above 2pt c)", "two generalized fractions in a group, invalid on purpose"),
  list("mod", r"(\pmod{p} \bmod \pod{q} \mod{r})"),
  list("lines arrows", r"(\underline{\overline{x}} \overleftarrow{AB} \underrightarrow{CD})"),
  list("Bmatrix*", r"(\begin{Bmatrix*}[r] -1 & 2 \\ 3 & -4\end{Bmatrix*})"),
  list("subarray", r"(\begin{subarray}{l} i \\ j \end{subarray})"),
  list("dots", r"(\left( \begin{matrix} a \\ \vdots \\ \ddots \end{matrix} \right))"),
  list("hdashline", r"(\begin{array}{:c:} a \\ \hdashline b \end{array})"),
  list("hdashline outside", r"(\hdashline)", "\\hdashline outside of an array, invalid on purpose"),
  list("fbox", r"(\fbox{abc} \boxed{\frac12})"),
  list("roots", r"(\sqrt[3]{\frac{a}{b}} \sqrt[n+1]{x})"),
  list("nary", r"(\int_a^b f(x)\,\mathrm{d}x \oiint \oiiint)")
)
reason <- vapply(x, function(p) if (length(p) > 2) p[[3]] else "", "")
out <- data.frame(
  ref = vapply(x, `[[`, "", 1),
  tex = vapply(x, `[[`, "", 2),
  inline = reason,
  # display_only constructs work in display mode
  display = ifelse(reason == display_only, "", reason),
  stringsAsFactors = FALSE
)
saveRDS(out, "data-raw/corpus/sources/probes.rds")
cat(nrow(out), "constructs\n")
