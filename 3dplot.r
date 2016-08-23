library(scatterplot3d)
dat <- read.csv("Screen.csv")
Fig <- scatterplot3d(dat$x,
              dat$y,
              dat$z, 
              main="Screen",
              color=dat$Color, pch=20, cex.symbols=0.7, # filled blue circles
              #type="h", lty.hplot=3,       # lines to the horizontal plane
              scale.y=.5, 
              xlab="VCR- Rel. Viab. ( Norm. to miR-NC)",
              ylab="VCR+ Rel. Viab. ( Norm. to miR-NC+VCR)",
              zlab="VCR+ Rel. Viab. ( Norm. to VCR-)",
              cex.lab=0.7, cex.axis = 0.7, box = 0)

