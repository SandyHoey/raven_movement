scale_seq <- seq(-2, 2, .005)

# PART 1
# hunting season and active kill in territory
plot(x = 1:4, 
     y = plogis(c(3.464512 - 0.666247 - 0.137657,
                  3.464512 - 0.666247,
                  3.464512 - 0.137657,
                  3.464512)),
     main = "Active kill, Hunt season",
     ylim = c(0,1))


# examining interaction effect study_period*snow
# red = Late winter
# green = Early winter
plot(x = scale_seq,
     y = plogis(3.464512 - 0.666247 + (0.687682 * scale_seq) - 0.554340),
     main = "Stdy period * snow",
     xlab = "snow",
     ylim = c(0, 1),
     col = 2, lwd = 0.1)
lines(x = scale_seq, 
      y = plogis(3.464512 + (0.687682 * scale_seq) - 0.554340),
      col = 3, lwd = 2)



# PART 2
# hunting biomass and finding kill outside territory
# red = found a kill
plot(x = scale_seq,
     y = plogis(c(0.22332 - 2.21482 - 0.04314 * scale_seq)),
     col = "red", ylim = c(0, 1))
lines(x = scale_seq,
      y = plogis(c(0.22332 - 0.04314 * scale_seq)),
      col = "blue")


# hunting season and finding a kill outside territory
plot(x = 1:4, 
     y = plogis(c(0.22332 - 2.21482 + 0.82175, # kill, hunt
                  0.22332 - 2.21482, # kill, no hunt
                  0.22332 + 0.82175, # no kill, hunt 
                  0.22332)), # no kill, no hunt
     main = "Active kill, Hunt season",
     ylim = c(0,1))


# finding kill outside territory + distance from hunting
# red = found a kill
plot(x = scale_seq,
     y = plogis(c(0.22332 -2.21482 -1.71212 * scale_seq)),
     col = "red", ylim = c(0, 1))
lines(x = scale_seq,
      y = plogis(c(0.22332 -2.21482 + 0.29596 - 1.71212 * scale_seq)),
      col = "red", lty = 2)
lines(x = scale_seq,
      y = plogis(c(0.22332 -2.21482 - 0.29596 - 1.71212 * scale_seq)),
      col = "red", lty = 2)
lines(x = scale_seq,
      y = plogis(c(0.22332 - 1.71212 * scale_seq)),
      col = "blue")
lines(x = scale_seq,
      y = plogis(c(0.22332 - (1.71212 + 0.43332) * scale_seq)),
      col = "blue", lty = 2)
lines(x = scale_seq,
      y = plogis(c(0.22332 - (1.71212 - 0.43332) * scale_seq)),
      col = "blue", lty = 2)






















