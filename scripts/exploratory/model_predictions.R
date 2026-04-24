scale_seq <- seq(-2, 2, .005)

# PART 1
# hunting season and active kill in territory
# early winter
plogis(c(2.3158440 - 1.8916349 - 0.22172,
         2.3158440 - 1.8916349,
         2.3158440 - 0.22172,
         2.3158440))
# late winter
plogis(c(2.3158440 - 1.8916349 - 0.22172 - 0.1172071,
         2.3158440 - 1.8916349 - 0.1172071,
         2.3158440 - 0.22172 - 0.1172071,
         2.3158440 - 0.1172071))


# examining interaction effect study_period*snow
# red = Late winter
# green = Early winter
plot(x = scale_seq,
     y = plogis(2.3158440 - 1.8916349 + (0.687682 * scale_seq) - 0.554340),
     main = "Stdy period * snow",
     xlab = "snow",
     ylim = c(0, 1),
     col = 2, lwd = 0.1)
lines(x = scale_seq, 
      y = plogis(2.3158440 + (0.687682 * scale_seq) - 0.554340),
      col = 3, lwd = 2)
a <- 0.26064
b <- 0.08188
exp(a)
exp(a-1.96*b)
exp(a+1.96*b)
# PART 2
# hunting biomass and finding kill outside territory
# red = found a kill
plot(x = scale_seq,
     y = plogis(c(-0.50056 - 1.92891 - 0.04314 * scale_seq)),
     col = "red", ylim = c(0, 1))
lines(x = scale_seq,
      y = plogis(c(-0.50056 - 0.04314 * scale_seq)),
      col = "blue")


# hunting season and finding a kill outside territory
plogis(c(-0.50056 - 1.92891 + 1.03254, # kill, hunt
         -0.50056 - 1.92891, # kill, no hunt
         -0.50056 + 1.03254, # no kill, hunt 
         -0.50056)) # no kill, no hunt


# finding kill outside territory + distance from hunting
# red = found a kill
plot(x = scale_seq,
     y = plogis(c(-0.50056 -1.92891 -1.71212 * scale_seq)),
     col = "red", ylim = c(0, 1))
lines(x = scale_seq,
      y = plogis(c(-0.50056 -1.92891 + 0.29596 - 1.71212 * scale_seq)),
      col = "red", lty = 2)
lines(x = scale_seq,
      y = plogis(c(-0.50056 -1.92891 - 0.29596 - 1.71212 * scale_seq)),
      col = "red", lty = 2)
lines(x = scale_seq,
      y = plogis(c(-0.50056 - 1.71212 * scale_seq)),
      col = "blue")
lines(x = scale_seq,
      y = plogis(c(-0.50056 - (1.71212 + 0.43332) * scale_seq)),
      col = "blue", lty = 2)
lines(x = scale_seq,
      y = plogis(c(-0.50056 - (1.71212 - 0.43332) * scale_seq)),
      col = "blue", lty = 2)






















