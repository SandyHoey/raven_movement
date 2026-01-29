scale_seq <- seq(-2, 2, .005)

# PART 2
# hunting biomass and finding kill outside territory
# red = found a kill
plot(x = scale_seq,
     y = plogis(c(-0.16767 - 1.26290 - 0.04314 * scale_seq)),
     col = "red", ylim = c(0, 1))
lines(x = scale_seq,
      y = plogis(c(-0.16767 - 0.04314 * scale_seq)),
      col = "blue")


# hunting season and finding a kill outside territory
plot(x = 1:4, 
     y = plogis(c(-0.16767 - 1.26290 + 0.90412,
                  -0.16767 - 1.26290,
                  -0.16767 + 0.90412,
                  -0.16767)),
     main = "Active kill, Hunt season",
     ylim = c(0,1))


# finding kill outside territory + distance from hunting
# red = found a kill
plot(x = scale_seq,
     y = plogis(c(-0.16767 -1.26290 -1.71212 * scale_seq)),
     col = "red", ylim = c(0, 1))
lines(x = scale_seq,
      y = plogis(c(-0.16767 -1.26290 + 0.29596 - 1.71212 * scale_seq)),
      col = "red", lty = 2)
lines(x = scale_seq,
      y = plogis(c(-0.16767 -1.26290 - 0.29596 - 1.71212 * scale_seq)),
      col = "red", lty = 2)
lines(x = scale_seq,
      y = plogis(c(-0.16767 - 1.71212 * scale_seq)),
      col = "blue")
lines(x = scale_seq,
      y = plogis(c(-0.16767 - (1.71212 + 0.43332) * scale_seq)),
      col = "blue", lty = 2)
lines(x = scale_seq,
      y = plogis(c(-0.16767 - (1.71212 - 0.43332) * scale_seq)),
      col = "blue", lty = 2)






















