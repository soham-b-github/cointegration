# --------------------------------------------
# Demonstration: Spurious Regression Trap
# --------------------------------------------
set.seed(42)
n <- 200

# Generate two independent random walks (I(1) processes)
rw1 <- cumsum(rnorm(n, 0, 1))   # Random walk 1
rw2 <- cumsum(rnorm(n, 0, 1))   # Random walk 2 (independent)

# Visualize both series
plot(rw1, type = "l", col = "blue", lwd = 2,
     ylim = range(c(rw1, rw2)),
     main = "Two Independent Random Walks (I(1))",
     ylab = "Value", xlab = "Time")
lines(rw2, col = "red", lwd = 2, lty = 2)
legend("topleft", legend = c("RW1", "RW2"),
       col = c("blue", "red"), lty = c(1, 2), lwd = 2, bty = "n")

# Run a spurious regression
model <- lm(rw1 ~ rw2)
summary(model)
