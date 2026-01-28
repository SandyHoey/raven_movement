scale_seq <- seq(-2, 2, .005)

# PART 2
# hunting biomass and finding kill outside territory
# red = found a kill
plot(x = scale_seq,
     y = plogis(c(0.65996 - 1.18325 + -0.03807 * scale_seq)),
     col = "red", ylim = c(0, 1))
lines(x = scale_seq,
      y = plogis(c(0.65996 + -0.03807 * scale_seq)),
      col = "blue")


# hunting season and finding a kill outside territory
plot(x = 1:4, 
     y = plogis(c(-0.10061 - 1.15507 + 0.83231,
                  -0.10061 - 1.15507,
                  -0.10061 + 0.83231,
                  -0.10061)),
     main = "Active kill, Hunt season")


# finding kill outside territory + distance from hunting
# red = found a kill
plot(x = scale_seq,
     y = plogis(c(-0.10061 - 1.5507 -1.70818 * scale_seq)),
     col = "red", ylim = c(0, 1))
lines(x = scale_seq,
      y = plogis(c(-0.10061 - 1.5507 + 0.27617 - (1.70818 + 0.43359) * scale_seq)),
      col = "red", lty = 2)
lines(x = scale_seq,
      y = plogis(c(-0.10061 - 1.5507 - 0.27617 - (1.70818 - 0.43359) * scale_seq)),
      col = "red", lty = 2)
lines(x = scale_seq,
      y = plogis(c(-0.10061 - 1.70818 * scale_seq)),
      col = "blue")
lines(x = scale_seq,
      y = plogis(c(-0.10061 - (1.70818 + 0.43359) * scale_seq)),
      col = "blue", lty = 2)
lines(x = scale_seq,
      y = plogis(c(-0.10061 - (1.70818 - 0.43359) * scale_seq)),
      col = "blue", lty = 2)






















