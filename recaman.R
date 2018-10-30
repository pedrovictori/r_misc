library(plotrix)

jump = 1
steps = 100
seq = 0

for (i in 1:steps) {
  current = seq[[length(seq)]]
  thisjump = jump
  
  if(current - jump >= 0 && ! is.element(current-jump, seq)){
    thisjump = jump * -1
  }
  
  seq = c(seq, current + thisjump)
  jump = jump + 1
}

png("recaman.png", width = 1200, height = 600, type = "cairo-png")
plot(1, type="n", xlab="", ylab="", xlim=c(0, max(seq)), ylim=c(-10, 10), asp = 1, axes=FALSE)
abline(h  = 0)
down = FALSE

for (i in 1:100) {
  center = (seq[[i]] + seq[[i+1]])/2
  radius = abs(seq[[i]] - seq[[i+1]])/2
  st = 0 + 180 * down
  end = 180 + 180 * down
  draw.arc(x = center, y = 0, radius = radius, deg1 = st, deg2 = end, col = "blue", n = .01)
  down = !down
}

dev.off()

