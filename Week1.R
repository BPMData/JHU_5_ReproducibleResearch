# Reproducible research is like writing the score for a song or symphony.


# Is this working with github?

# Is it working now?

# Now??

# Now??

# This origin has moved, please use...

# Okay, I think I actually have it now

install.packages("slidify")

av <- available.packages(filters=list())
av[av[, "Package"] == "slidify", ]

install.packages("devtools")
library(devtools)

library(devtools)
install_github('ramnathv/slidify')
install_github('ramnathv/slidifyLibraries')


data(airquality)

pairs(airquality)

plot(airquality[,1:6], main = "plot(airquality[,1:6])")

?labs

?plot

fit <- lm(Ozone ~ Wind + Solar.R + Temp, data = airquality)
fit
summary(fit)

plot(fit)


