# Classic formulas, written for this corpus (formulas are facts, the LaTeX is
# ours); the reference is the page that gives the formula. Writes
# sources/classics.rds: data.frame(source, ref, category, display, tex).
# Run from data-raw/corpus: Rscript sources/classics.R
w <- function(page) paste0("https://en.wikipedia.org/wiki/", page)
x <- list(
  # statistics: distributions and estimators
  list("statistics", TRUE, w("Normal_distribution"), r"(f(x) = \frac{1}{\sigma\sqrt{2\pi}} \exp\left( -\frac{(x-\mu)^2}{2\sigma^2} \right))"),
  list("statistics", TRUE, w("Binomial_distribution"), r"(P(X = k) = \binom{n}{k} p^k (1-p)^{n-k})"),
  list("statistics", TRUE, w("Poisson_distribution"), r"(P(X = k) = \frac{\lambda^k e^{-\lambda}}{k!})"),
  list("statistics", TRUE, w("Mean"), r"(\bar{x} = \frac{1}{n} \sum_{i=1}^{n} x_i)"),
  list("statistics", TRUE, w("Variance"), r"(s^2 = \frac{1}{n-1} \sum_{i=1}^{n} \left( x_i - \bar{x} \right)^2)"),
  list("statistics", TRUE, w("Pearson_correlation_coefficient"), r"(r = \frac{\sum_{i}(x_i - \bar{x})(y_i - \bar{y})}{\sqrt{\sum_{i}(x_i - \bar{x})^2}\sqrt{\sum_{i}(y_i - \bar{y})^2}})"),
  list("statistics", TRUE, w("Student%27s_t-test"), r"(t = \frac{\bar{x}_1 - \bar{x}_2}{s_p \sqrt{\frac{1}{n_1} + \frac{1}{n_2}}})"),
  list("statistics", TRUE, w("Pearson%27s_chi-squared_test"), r"(\chi^2 = \sum_{i=1}^{k} \frac{(O_i - E_i)^2}{E_i})"),
  list("statistics", TRUE, w("F-test"), r"(F = \frac{\text{SS}_\text{between} / (k - 1)}{\text{SS}_\text{within} / (N - k)})"),
  list("statistics", TRUE, w("Confidence_interval"), r"(\bar{x} \pm z_{1-\alpha/2} \frac{\sigma}{\sqrt{n}})"),
  list("statistics", TRUE, w("Bayes%27_theorem"), r"(P(A \mid B) = \frac{P(B \mid A)\, P(A)}{P(B)})"),
  list("statistics", TRUE, w("Likelihood_function"), r"(\mathcal{L}(\theta \mid x) = \prod_{i=1}^{n} f(x_i \mid \theta))"),
  list("statistics", TRUE, w("Maximum_likelihood_estimation"), r"(\hat{\theta} = \operatorname*{arg\,max}_{\theta \in \Theta} \ell(\theta \mid x))"),
  list("statistics", TRUE, w("Central_limit_theorem"), r"(\sqrt{n}\left(\bar{X}_n - \mu\right) \xrightarrow{d} \mathcal{N}\left(0, \sigma^2\right))"),
  list("statistics", TRUE, w("Expected_value"), r"(\operatorname{E}[X] = \int_{-\infty}^{\infty} x f(x) \, dx)"),
  list("statistics", FALSE, w("Coefficient_of_determination"), r"(R^2 = 1 - \frac{SS_\text{res}}{SS_\text{tot}})"),
  list("statistics", FALSE, w("P-value"), r"(p < 0.001)"),
  list("statistics", FALSE, w("Standard_error"), r"(\sigma_{\bar{x}} = \sigma / \sqrt{n})"),
  # models
  list("models", TRUE, w("Ordinary_least_squares"), r"(\hat{\boldsymbol{\beta}} = \left( \mathbf{X}^\top \mathbf{X} \right)^{-1} \mathbf{X}^\top \mathbf{y})"),
  list("models", TRUE, w("Linear_regression"), r"(y_i = \beta_0 + \beta_1 x_{i1} + \cdots + \beta_p x_{ip} + \varepsilon_i, \quad \varepsilon_i \sim \mathcal{N}(0, \sigma^2))"),
  list("models", TRUE, w("Logistic_regression"), r"(\log\left( \frac{p}{1-p} \right) = \beta_0 + \beta_1 x)"),
  list("models", TRUE, w("Proportional_hazards_model"), r"(h(t \mid x) = h_0(t) \exp\left( \beta_1 x_1 + \cdots + \beta_p x_p \right))"),
  list("models", TRUE, w("Kaplan%E2%80%93Meier_estimator"), r"(\hat{S}(t) = \prod_{i:\, t_i \le t} \left( 1 - \frac{d_i}{n_i} \right))"),
  list("models", TRUE, w("Mixed_model"), r"(\mathbf{y} = X\boldsymbol{\beta} + Z\mathbf{u} + \boldsymbol{\epsilon})"),
  list("models", TRUE, w("Autoregressive_model"), r"(X_t = c + \sum_{i=1}^{p} \varphi_i X_{t-i} + \varepsilon_t)"),
  list("models", TRUE, w("Ridge_regression"), r"(\hat{\beta}_\text{ridge} = \operatorname*{arg\,min}_{\beta} \left\{ \lVert y - X\beta \rVert_2^2 + \lambda \lVert \beta \rVert_2^2 \right\})"),
  list("models", TRUE, w("Softmax_function"), r"(\sigma(\mathbf{z})_i = \frac{e^{z_i}}{\sum_{j=1}^{K} e^{z_j}})"),
  # mathematics
  list("calculus", TRUE, w("Taylor_series"), r"(f(x) = \sum_{n=0}^{\infty} \frac{f^{(n)}(a)}{n!} (x-a)^n)"),
  list("calculus", TRUE, w("Fourier_transform"), r"(\hat{f}(\xi) = \int_{-\infty}^{\infty} f(x)\, e^{-2\pi i x \xi} \, dx)"),
  list("calculus", TRUE, w("Gaussian_integral"), r"(\int_{-\infty}^{\infty} e^{-x^2} \, dx = \sqrt{\pi})"),
  list("calculus", TRUE, w("Limit_(mathematics)"), r"(\lim_{x \to 0} \frac{\sin x}{x} = 1)"),
  list("calculus", TRUE, w("Derivative"), r"(f'(x) = \lim_{h \to 0} \frac{f(x+h) - f(x)}{h})"),
  list("calculus", TRUE, w("Partial_derivative"), r"(\frac{\partial^2 f}{\partial x \, \partial y} = \frac{\partial^2 f}{\partial y \, \partial x})"),
  list("calculus", TRUE, w("Stokes%27_theorem"), r"(\oint_{\partial \Sigma} \mathbf{F} \cdot d\mathbf{r} = \iint_{\Sigma} (\nabla \times \mathbf{F}) \cdot d\mathbf{\Sigma})"),
  list("algebra", TRUE, w("Quadratic_formula"), r"(x = \frac{-b \pm \sqrt{b^2 - 4ac}}{2a})"),
  list("algebra", TRUE, w("Binomial_theorem"), r"((x+y)^n = \sum_{k=0}^{n} \binom{n}{k} x^{n-k} y^k)"),
  list("algebra", TRUE, w("Euler%27s_identity"), r"(e^{i\pi} + 1 = 0)"),
  list("algebra", TRUE, w("Golden_ratio"), r"(\varphi = 1 + \cfrac{1}{1 + \cfrac{1}{1 + \cfrac{1}{1 + \cdots}}})"),
  list("algebra", TRUE, w("Absolute_value"), r"(|x| = \begin{cases} x & \text{if } x \ge 0 \\ -x & \text{if } x < 0 \end{cases})"),
  list("algebra", TRUE, w("Riemann_zeta_function"), r"(\zeta(s) = \sum_{n=1}^{\infty} \frac{1}{n^s} = \prod_{p \text{ prime}} \frac{1}{1 - p^{-s}})"),
  list("linear algebra", TRUE, w("Determinant"), r"(\det\begin{pmatrix} a & b \\ c & d \end{pmatrix} = ad - bc)"),
  list("linear algebra", TRUE, w("Identity_matrix"), r"(I_3 = \begin{bmatrix} 1 & 0 & 0 \\ 0 & 1 & 0 \\ 0 & 0 & 1 \end{bmatrix})"),
  list("linear algebra", TRUE, w("Matrix_(mathematics)"), r"(A = \begin{pmatrix} a_{11} & a_{12} & \cdots & a_{1n} \\ a_{21} & a_{22} & \cdots & a_{2n} \\ \vdots & \vdots & \ddots & \vdots \\ a_{m1} & a_{m2} & \cdots & a_{mn} \end{pmatrix})"),
  list("linear algebra", TRUE, w("Covariance_matrix"), r"(\Sigma = \begin{pmatrix} \sigma_1^2 & \rho\sigma_1\sigma_2 \\ \rho\sigma_1\sigma_2 & \sigma_2^2 \end{pmatrix})"),
  list("linear algebra", TRUE, w("Eigenvalues_and_eigenvectors"), r"(A\mathbf{v} = \lambda \mathbf{v})"),
  list("linear algebra", TRUE, w("Norm_(mathematics)"), r"(\lVert \mathbf{x} \rVert_p = \left( \sum_{i=1}^{n} |x_i|^p \right)^{1/p})"),
  list("sets and logic", TRUE, w("Set-builder_notation"), r"(\{ x \in \mathbb{R} \mid x^2 < 2 \})"),
  list("sets and logic", TRUE, w("Natural_number"), r"(\mathbb{N} \subset \mathbb{Z} \subset \mathbb{Q} \subset \mathbb{R} \subset \mathbb{C})"),
  list("sets and logic", TRUE, w("Epsilon-delta_definition_of_limit"), r"(\forall \varepsilon > 0,\ \exists \delta > 0 : |x - c| < \delta \implies |f(x) - L| < \varepsilon)"),
  # sciences
  list("physics", TRUE, w("Mass%E2%80%93energy_equivalence"), r"(E = mc^2)"),
  list("physics", TRUE, w("Schr%C3%B6dinger_equation"), r"(i\hbar \frac{\partial}{\partial t} \Psi(\mathbf{r}, t) = \left[ -\frac{\hbar^2}{2m} \nabla^2 + V(\mathbf{r}, t) \right] \Psi(\mathbf{r}, t))"),
  list("physics", TRUE, w("Maxwell%27s_equations"), r"(\begin{aligned} \nabla \cdot \mathbf{E} &= \frac{\rho}{\varepsilon_0} \\ \nabla \cdot \mathbf{B} &= 0 \\ \nabla \times \mathbf{E} &= -\frac{\partial \mathbf{B}}{\partial t} \\ \nabla \times \mathbf{B} &= \mu_0 \left( \mathbf{J} + \varepsilon_0 \frac{\partial \mathbf{E}}{\partial t} \right) \end{aligned})"),
  list("physics", TRUE, w("Ideal_gas_law"), r"(pV = nRT)"),
  list("physics", FALSE, w("Speed_of_light"), r"(c = 299\,792\,458 \ \text{m}\,\text{s}^{-1})"),
  list("chemistry", TRUE, w("Chemical_equilibrium"), r"(\mathrm{N_2} + 3\,\mathrm{H_2} \rightleftharpoons 2\,\mathrm{NH_3})"),
  list("chemistry", TRUE, w("Henderson%E2%80%93Hasselbalch_equation"), r"(\mathrm{pH} = \mathrm{p}K_\mathrm{a} + \log_{10}\left( \frac{[\mathrm{A^-}]}{[\mathrm{HA}]} \right))"),
  list("biology", TRUE, w("Logistic_function"), r"(\frac{dN}{dt} = rN\left( 1 - \frac{N}{K} \right))"),
  list("biology", TRUE, w("Michaelis%E2%80%93Menten_kinetics"), r"(v = \frac{V_{\max} [S]}{K_\mathrm{M} + [S]})"),
  list("biology", TRUE, w("Hardy%E2%80%93Weinberg_principle"), r"(p^2 + 2pq + q^2 = 1)"),
  list("finance", TRUE, w("Net_present_value"), r"(\mathrm{NPV} = \sum_{t=0}^{N} \frac{R_t}{(1+i)^t})"),
  list("finance", TRUE, w("Compound_interest"), r"(A = P\left( 1 + \frac{r}{n} \right)^{nt})"),
  list("finance", TRUE, w("Black%E2%80%93Scholes_model"), r"(C = N(d_1) S_t - N(d_2) K e^{-r(T-t)}, \qquad d_{1,2} = \frac{\ln\frac{S_t}{K} + \left( r \pm \frac{\sigma^2}{2} \right)(T-t)}{\sigma\sqrt{T-t}})"),
  list("computer science", TRUE, w("Big_O_notation"), r"(T(n) = O(n \log n))"),
  list("computer science", TRUE, w("Entropy_(information_theory)"), r"(H(X) = -\sum_{x \in \mathcal{X}} p(x) \log_2 p(x))"),
  # in a table cell: inline, short, with units or text
  list("in tables", FALSE, w("Kilogram"), r"(9.81\ \mathrm{m\,s^{-2}})"),
  list("in tables", FALSE, w("Mean_squared_error"), r"(\mathrm{MSE} = \frac{1}{n}\sum_{i=1}^n (y_i - \hat{y}_i)^2)"),
  list("in tables", FALSE, w("Interquartile_range"), r"(\mathrm{IQR} = Q_3 - Q_1)"),
  list("in tables", FALSE, w("Odds_ratio"), r"(\mathrm{OR} = \frac{a/c}{b/d} = \frac{ad}{bc})"),
  list("in tables", FALSE, w("Degree_(angle)"), r"(45^\circ)"),
  list("in tables", FALSE, w("Approximation"), r"(\pi \approx 3.14159)"),
  list("in tables", FALSE, w("Plus%E2%80%93minus_sign"), r"(12.3 \pm 0.4)"),
  list("in tables", FALSE, w("Scientific_notation"), r"(6.022 \times 10^{23}\ \text{mol}^{-1})")
)
out <- data.frame(
  source = "classics",
  ref = vapply(x, `[[`, "", 3),
  category = vapply(x, `[[`, "", 1),
  display = vapply(x, `[[`, NA, 2),
  tex = vapply(x, `[[`, "", 4),
  stringsAsFactors = FALSE
)
saveRDS(out, "sources/classics.rds")
cat(nrow(out), "formulas\n")
