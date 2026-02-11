scale_seq <- seq(-2, 2, .005)

# PART 1
# hunting biomass and active kill in territory
# red = active kill
plot(x = scale_seq,
     y = plogis(c(3.18873 - 0.79651 - 0.05167 * scale_seq)),
     col = "red", ylim = c(0, 1))
lines(x = scale_seq,
      y = plogis(c(3.18873 - 0.05167 * scale_seq)),
      col = "blue")


# hunting season and active kill in territory
plot(x = 1:4, 
     y = plogis(c(3.18873 - 0.79651 - 0.21529,
                  3.18873 - 0.79651,
                  3.18873 - 0.21529,
                  3.18873)),
     main = "Active kill, Hunt season",
     ylim = c(0,1))


# active kill and study period
# red = found a kill
plot(x = 1:4,
     y = plogis(c(3.18873 - 0.79651,
                  3.18873 - 0.79651 - 0.4603,
                  3.18873,
                  3.18873 - 0.4603)),
     main = "Active kill, Study period",
     ylim = c(0, 1))
text(x = 1:4,
     y = 0.5,
     labels = c("early", "late", "early", "late"))
text(x = c(1.5, 3.5),
     y = 0.3,
     labels = c("Active Kill", "No Kill"))



# PART 2
# hunting biomass and finding kill outside territory
# red = found a kill
plot(x = scale_seq,
     y = plogis(c(0.58091 - 2.24366 - 0.04314 * scale_seq)),
     col = "red", ylim = c(0, 1))
lines(x = scale_seq,
      y = plogis(c(0.58091 - 0.04314 * scale_seq)),
      col = "blue")


# hunting season and finding a kill outside territory
plot(x = 1:4, 
     y = plogis(c(0.58091 - 2.24366 + 0.55666,
                  0.58091 - 2.24366,
                  0.58091 + 0.55666,
                  0.58091)),
     main = "Active kill, Hunt season",
     ylim = c(0,1))


# finding kill outside territory + distance from hunting
# red = found a kill
plot(x = scale_seq,
     y = plogis(c(0.58091 -2.24366 -1.71212 * scale_seq)),
     col = "red", ylim = c(0, 1))
lines(x = scale_seq,
      y = plogis(c(0.58091 -2.24366 + 0.29596 - 1.71212 * scale_seq)),
      col = "red", lty = 2)
lines(x = scale_seq,
      y = plogis(c(0.58091 -2.24366 - 0.29596 - 1.71212 * scale_seq)),
      col = "red", lty = 2)
lines(x = scale_seq,
      y = plogis(c(0.58091 - 1.71212 * scale_seq)),
      col = "blue")
lines(x = scale_seq,
      y = plogis(c(0.58091 - (1.71212 + 0.43332) * scale_seq)),
      col = "blue", lty = 2)
lines(x = scale_seq,
      y = plogis(c(0.58091 - (1.71212 - 0.43332) * scale_seq)),
      col = "blue", lty = 2)






















