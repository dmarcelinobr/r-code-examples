library("ggplot2")

# Make plot object
p <- qplot(x, y, data=tmp, xlab="X-axis", ylab="Y-axis")
p <- p + stat_function(fun=f) + theme_pub()
p <- p + annotate("text", label = "Inverted U-shape/quadratic model", x = .5, y = .40, size = 6)
p + annotate("text", label = "Decreasing returns \n y = x + x + z", x = .5, y = .34, size = 6)


f1 <- function(x) .5 * x + .25
f2 <- function(x) -.1 * x + .25
f3 <- function(x) .25 * x + (x*x)
f4 <- function(x) .25 * x + .25 + (x*x)
f5 <- function(x) 0 + (1*x) - (1*x*x)
f6 <- function(x) 0 + (1*x) - (.5*x*x)

ggplot(data.frame(x=0:1), aes(x)) +
  stat_function(fun=f1) +
  ylab("% Votes") +
  xlab("% Money") +
  theme_pub()

ggplot(data.frame(x=c(0, 1)), aes(x)) +
  stat_function(fun=f2) +
  ylab("% Votes") +
  xlab("% Money") +
  theme_pub()

ggplot(data.frame(x=c(0, 1)), aes(x)) +
  stat_function(fun=f3) +
  ylab("% Votes") +
  xlab("% Money") +
  theme_pub()

ggplot(data.frame(x=c(0, 1)), aes(x)) +
  stat_function(fun=f4) +
  ylab("% Votes") +
  xlab("% Money") +
  theme_pub()

ggplot(data.frame(x=c(0, 1)), aes(x)) +
  stat_function(fun=f5) +
  ylab("% Votes") +
  xlab("% Money") +
  theme_pub()

ggplot(data.frame(x=c(0, 1)), aes(x)) +
  stat_function(fun=f6) +
  ylab("% Votes") +
  xlab("% Money") +
  theme_pub()

# use custom gradient (here jet colormap)
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
ggplot(melt(volcano), aes(x=X1, y=X2, fill=value)) + geom_tile() +
  scale_fill_gradientn(colours=jet.colors(7), legend_param=list(colorbar=T, colorbar_nbin=100))
