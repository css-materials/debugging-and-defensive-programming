library(tidyverse)
library(modelr)
library(broom)
library(gam)
College <- as_tibble(ISLR::College) %>%
mutate(Outstate = Outstate / 1000,
Room.Board = Room.Board / 1000,
PhD_log = log(PhD)) # rescale Outstate in thousands of dollars
crossv_kfold(College, k = 10) %>%
mutate(linear = map(train, ~ glm(Outstate ~ PhD,
data = .)),
log = map(train, ~ glm(Outstate ~ PhD_log, data = .)),
spline = map(train, ~ glm(Outstate ~ bs(PhD, df = 5), data = .))) %>% gather(type, model, linear:spline) %>%
mutate(mse = map2_dbl(model, test, mse)) %>% group_by(type) %>%
summarize(mse = mean(mse)) # k-fold cv of three model types
college_phd_spline <- gam(Outstate ~ bs(PhD, df = 5), data = College) # spline has the best model fit
college_phd_terms <- preplot(college_phd_spline, se = TRUE, rug = FALSE) # get first difference for age
# age plot
tibble(
x = college_phd_terms$`bs(PhD, df = 5)`$x,
y = college_phd_terms$`bs(PhD, df = 5)`$y,
se.fit = college_phd_terms$`bs(PhD, df = 5)`$se.y
)%>%mutate(y_low = y - 1.96 * se.fit, y_high = y + 1.96 * se.fit) %>%
ggplot(aes(x, y))+geom_line()+
geom_line(aes(y = y_low), linetype = 2)+
geom_line(aes(y = y_high), linetype = 2) +
labs(title = "Cubic spline of out-of-state tuition",
subtitle = "Knots = 2",
x = "Percent of faculty with PhDs", y = expression(f[1](PhD)))
