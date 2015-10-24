# For internal tests only; not for demo

x <- 1:10
y <- 1:10 ^ 3

res <- summary(lm(y ~ x, data.frame(x=x, y=y)))
intercept <- res$coefficients[1, 1]
slope <- res$coefficients[2, 1]
rsq <- res$r.squared

structure(c(intercept=intercept, slope=slope, rsq=rsq), class="fastlm")
