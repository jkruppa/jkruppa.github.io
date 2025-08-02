require("scatterplot3d")

par(mfrow = c(1, 2))

# Data, linear regression with two explanatory variables
wh <- iris$Species != "setosa"
x  <- iris$Sepal.Width[wh]
y  <- iris$Sepal.Length[wh]
z  <- iris$Petal.Width[wh]
df <- data.frame(x, y, z)
LM <- lm(y ~ x + z, df)

# scatterplot
s3d <- scatterplot3d(x, z, y, pch = 19, type = "p", color = "darkgrey",
                     grid = TRUE, box = FALSE,  
                     mar = c(2.5, 2.5, 2, 1.5), angle = 55,
                     xlab = "Kovariate 1", zlab = "Messwert Y", ylab = "Kovariate 2")

# regression plane
s3d$plane3d(LM, draw_polygon = TRUE, draw_lines = TRUE, 
            polygon_args = list(col = rgb(0.33725490196078434, 0.7058823529411765, 0.9137254901960784, .5)))

# overlay positive residuals
wh <- resid(LM) > 0
s3d$points3d(x[wh], z[wh], y[wh], pch = 19)


# scatterplot
s3d <- scatterplot3d(x, z, y, pch = 19, type = "p", color = "darkgrey",
                     grid = TRUE, box = FALSE,  
                     mar = c(2.5, 2.5, 2, 1.5), angle = 55,
                     xlab = "Kovariate 1", zlab = "Messwert Y", ylab = "Kovariate 2")

# compute locations of segments
orig     <- s3d$xyz.convert(x, z, y)
plane    <- s3d$xyz.convert(x, z, fitted(LM))
i.negpos <- 1 + (resid(LM) > 0) # which residuals are above the plane?

# draw residual distances to regression plane
segments(orig$x, orig$y, plane$x, plane$y, col = "#CC79A7", lty = c(2, 1)[i.negpos], 
         lwd = 1.5)

# draw the regression plane
s3d$plane3d(LM, draw_polygon = TRUE, draw_lines = TRUE, 
            polygon_args = list(col = rgb(0.8, 0.8, 0.8, 0.5)))

# redraw positive residuals and segments above the plane
wh <- resid(LM) > 0
segments(orig$x[wh], orig$y[wh], plane$x[wh], plane$y[wh], col = "#CC79A7", lty = 1, lwd = 1.5)
s3d$points3d(x[wh], z[wh], y[wh], pch = 19)

## flea model

require("scatterplot3d")

par(mfrow = c(1, 2))

# Data, linear regression with two explanatory variables
x  <- flea_model_tbl$weight
y  <- flea_model_tbl$jump_length
z  <- flea_model_tbl$count_leg
df <- data.frame(x, y, z)
LM <- lm(y ~ x + z, df)

# scatterplot
s3d <- scatterplot3d(x, z, y, pch = 19, type = "p", color = "darkgrey",
                     grid = TRUE, box = FALSE,  
                     mar = c(2.5, 2.5, 0, 1.5), angle = 55,
                     xlab = "Gewicht in [mg]", zlab = "Sprungweite in [cm]", 
                     ylab = "Anzahl Beinhaare")

# regression plane
s3d$plane3d(LM, draw_polygon = TRUE, draw_lines = TRUE, 
            polygon_args = list(col = rgb(0.33725490196078434, 0.7058823529411765, 0.9137254901960784, .5)))

# overlay positive residuals
wh <- resid(LM) > 0
s3d$points3d(x[wh], z[wh], y[wh], pch = 19)


# scatterplot
s3d <- scatterplot3d(x, z, y, pch = 19, type = "p", color = "darkgrey",
                     grid = TRUE, box = FALSE,  
                     mar = c(2.5, 2.5, 0, 1.5), angle = 55,
                     xlab = "Gewicht in [mg]", zlab = "Sprungweite in [cm]", 
                     ylab = "Anzahl Beinhaare")

# compute locations of segments
orig     <- s3d$xyz.convert(x, z, y)
plane    <- s3d$xyz.convert(x, z, fitted(LM))
i.negpos <- 1 + (resid(LM) > 0) # which residuals are above the plane?

# draw residual distances to regression plane
segments(orig$x, orig$y, plane$x, plane$y, col = "#CC79A7", lty = c(2, 1)[i.negpos], 
         lwd = 1.5)

# draw the regression plane
s3d$plane3d(LM, draw_polygon = TRUE, draw_lines = TRUE, 
            polygon_args = list(col = rgb(0.8, 0.8, 0.8, 0.5)))

# redraw positive residuals and segments above the plane
wh <- resid(LM) > 0
segments(orig$x[wh], orig$y[wh], plane$x[wh], plane$y[wh], col = "#CC79A7", lty = 1, lwd = 1.5)
s3d$points3d(x[wh], z[wh], y[wh], pch = 19)


