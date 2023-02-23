# TODO
#   - Push to GitHub to get this off my desktop!
#   - Write Readme

# Note: this example code isn't going to work by running the whole script, and
#   a little manual user input is required.  More details in the README.

# -----------------------------------------------
# Initialization
# -----------------------------------------------

library("raster") # for the "raster()" function
library("rgdal")  # used to make the "raster()" function work with
#   different standard image  types
library("rgl")    # for 3D exploratory scatter plot

# Set up image directory
image_dir <- "C:/Users/Lowell Moore/Desktop/Basic RGB clustering"
image_path <- paste(image_dir, "Stack (RGB).png", sep = "/")

# Function for drawing ternary plots
plot_ternary <- function(as, bs, cs){
  if(FALSE){ # debug
    n_points <- 100
    as <- abs(rnorm(n_points, mean = 2))
    bs <- abs(rnorm(n_points, mean = 4))
    cs <- abs(rnorm(n_points, mean = 8))
  }
  
  a_norm <- as/(as+bs+cs)
  b_norm <- bs/(as+bs+cs)
  c_norm <- cs/(as+bs+cs)
  
  plot_xs <- 1-(as/(as+cs))
  thetas <- acos(plot_xs/(1-a_norm))
  plot_ys <- (1-a_norm)*sin(thetas)
  
  plot(plot_xs, plot_ys, xlim = c(0, 1)
       #, ylim = c(0, sqrt(3))
       , cex = 0.7, pch = 16
       #, col = densCols(plot_xs, plot_ys, colramp = colorRampPalette(hcl.colors(12, palette = "Rocket")))
       , col = rgb(as/255, bs/255, cs/255, 0.3)
       , xlab = "Red/Blue"
       , ylab = "Green/(Red + Blue + Green)"
  )
  lines(c(0, 0.5, 1, 0), c(0, sqrt(3), 0, 0), lty = 2)
}

# Function to calculate cartesian coordinates used for ternary plots
calc_ternary <- function(as, bs, cs){
  if(FALSE){ # debug
    n_points <- 100
    as <- abs(rnorm(n_points, mean = 2))
    bs <- abs(rnorm(n_points, mean = 4))
    cs <- abs(rnorm(n_points, mean = 8))
  }
  
  a_norm <- as/(as+bs+cs)
  b_norm <- bs/(as+bs+cs)
  c_norm <- cs/(as+bs+cs)
  
  plot_xs <- 1-(as/(as+cs))
  thetas <- acos(plot_xs/(1-a_norm))
  plot_ys <- (1-a_norm)*sin(thetas)
  
  return(cbind.data.frame(plot_xs, plot_ys))
}

# Function to flag points occurring within a polygon
locate_poly <- function(x_points, y_points, n_poly){
  print(paste("Click", n_poly, "points to create polygon..."))
  points_select <- locator(n = n_poly)
  
  xs_line <- points_select$x
  ys_line <- points_select$y
  center_poly <- c(mean(xs_line), mean(ys_line))
  points(center_poly[1], center_poly[2], pch = 21, bg = "darkblue")
  
  xs_line <- c(xs_line, xs_line[1])
  ys_line <- c(ys_line, ys_line[1])
  lines(xs_line, ys_line, lty = 2, col = "red")
  
  ms_line <- numeric(0)
  bs_line <- numeric(0)
  flag <- 1:length(x_points)
  for(i in 1:(length(xs_line)-1)){
    # Debug : i <- 1
    ms_line[i] <- (ys_line[i+1]-ys_line[i])/(xs_line[i+1]-xs_line[i])
    bs_line[i] <- ys_line[i] - (ms_line[i]*xs_line[i])
    center_is_below <- center_poly[2] < (ms_line[i]*center_poly[1])+bs_line[i]
    if(center_is_below){
      flag_i <- which(y_points < (ms_line[i]*x_points)+bs_line[i])
    }
    if(!center_is_below){
      flag_i <- which(y_points > (ms_line[i]*x_points)+bs_line[i])
    }
    
    print(paste(
      "i =", i, "ms_line =", round(ms_line[i], 2), " -- bs_line =", round(bs_line[i], 2)
      , " -- center_is_below =", center_is_below
      , " -- flag = ", length(flag), "points"
      , " -- flag_i = ", length(flag_i), "points"
      , sep = " "))
    
    flag <- intersect(flag, flag_i)
  }
  
  return(flag)
}

# preview image of one image channel
eds_map <- raster(image_path, band = 1)
plot(eds_map, col = hcl.colors(100, palette = "Hawaii"))
# hcl.pals()


# -----------------------------------------------
# Retrieve flagged pixels over image area
# -----------------------------------------------

# Flagged pixels are identified across several RGB images

# Color channels
eds_R <- raster(image_path, band = 1)
eds_G <- raster(image_path, band = 2)
eds_B <- raster(image_path, band = 3)

xs <- as.numeric(eds_R[,])
ys <- as.numeric(eds_G[,])
zs <- as.numeric(eds_B[,])

if(FALSE){
  xs_norm <- xs/(xs+ys+zs)
  ys_norm <- ys/(xs+ys+zs)
  zs_norm <- zs/(xs+ys+zs)
}

# ----------------------------------------------------------
# Exploratory plots
# ----------------------------------------------------------

# Speed up plots by subsampling data
n_points <- length(xs)
flag <- sample(1:n_points, size = 0.5*n_points, replace = FALSE)

if(FALSE){
  plot(ys[flag], zs[flag], pch = 19, col = rgb(0, 0, 0, 0.01))
  
  plot(xs[flag], ys[flag], pch = 19, col = densCols(xs[flag], ys[flag]
                                                    , colramp = colorRampPalette(c("black", "white"))))
  
  plot3d(xs[flag], ys[flag], zs[flag], col = rgb(xs[flag]/255, ys[flag]/255, zs[flag]/255), alpha = 0.5
         , type = "p"
         #, lwd = 2
         , xlab = "Red", ylab = "Green", "Blue"
  )
  #hist(xs+ys+zs)
  
}


# ----------------------------------------------------------
# Plot 1: two scatter plots
# ----------------------------------------------------------

# Speed up plots by subsampling data
n_points <- length(xs)
flag <- sample(1:n_points, size = 1*n_points, replace = FALSE)

output_path <- paste(image_dir, "Stack (RGB) 2d scatter.bmp", sep = "/")
bmp(file = output_path
    , width = 1024, height = 512
    #, useDingbats = FALSE
    )
par(mfrow = c(1, 2))

# Note that the densCols function can be used to set a color scale corresponding
#   to the point density
plot(xs[flag], ys[flag], pch = 19, cex = 0.7
     #, col = densCols(xs[flag], ys[flag], colramp = colorRampPalette(hcl.colors(12, palette = "Rocket")))
     , col = rgb(xs[flag]/255, ys[flag]/255, zs[flag]/255)
     , xlab = "Red", ylab = "Green"
     )

plot(xs[flag], zs[flag], pch = 19, cex = 0.7
     #, col = densCols(xs[flag], zs[flag], colramp = colorRampPalette(hcl.colors(12, palette = "Rocket")))
     , col = rgb(xs[flag]/255, ys[flag]/255, zs[flag]/255)
     , xlab = "Red", ylab = "Blue"
     )

par(mfrow = c(1, 1))
dev.off()


# ----------------------------------------------------------
# Plot 2: histograms
# ----------------------------------------------------------

if(FALSE){ # These aren't particularly useful
  n_points <- length(xs)
  flag <- sample(1:n_points, size = 0.05*n_points, replace = FALSE)
  
  output_path <- paste(image_dir, "Stack (RGB) hist.pdf", sep = "/")
  pdf(file = output_path, width = 10, height = 4, useDingbats = FALSE)
  par(mfrow = c(1, 3))
  
  hist(xs_norm, xlab = "", main = "Red")
  hist(ys_norm, xlab = "", main = "Green")
  hist(zs_norm, xlab = "", main = "Blue")
  S
  par(mfrow = c(1, 1))
  dev.off()
}


# ----------------------------------------------------------
# Plot 3: ternary plot
# ----------------------------------------------------------

n_points <- length(xs)
flag <- sample(1:n_points, size = 1*n_points, replace = FALSE)

output_path <- paste(image_dir, "Stack (RGB) ternary.bmp", sep = "/")
bmp(file = output_path
    , width = 600, height = 600
    #, useDingbats = FALSE
)

plot_ternary(as = xs[flag], bs = ys[flag], cs = zs[flag])

dev.off()


# ----------------------------------------------------------
# clusters
# ----------------------------------------------------------

# Note: a little bit of manual user input is required in this section, where
#   clusters are identified using a ternary plot and the "locate" function, which
#   is called by the "locate_poly" function.

n_points <- length(xs)
flag <- sample(1:n_points, size = 0.1*n_points, replace = FALSE)
plot_ternary(as = xs[flag], bs = ys[flag], cs = zs[flag])

# Also note: a downsampled ternary plot is used to help draw the polygons,
#   but the full dataset is actually being referenced to the polygons.
ternary_coords <- calc_ternary(xs, ys, zs)

# Clusters are identified one-by-one and saved under a separate variable.
# Note: the locate function isn't very reliable, so click polygon vertices slowly
#   to make sure R doesn't crash and the polygon is registered correctly.
c1_green <- locate_poly(ternary_coords$plot_xs, ternary_coords$plot_ys, n_poly = 5)
c2_yellowgreen <- locate_poly(ternary_coords$plot_xs, ternary_coords$plot_ys, n_poly = 5)
c3_red <- locate_poly(ternary_coords$plot_xs, ternary_coords$plot_ys, n_poly = 5)
c4_lightblue <- locate_poly(ternary_coords$plot_xs, ternary_coords$plot_ys, n_poly = 5)
c4_darkblue <- locate_poly(ternary_coords$plot_xs, ternary_coords$plot_ys, n_poly = 5)

# This is used to hold the shape of the map area, and it can be plotted as a
#   visual aid if necessary
eds_map <- raster(image_path, band = 1)
plot(eds_map, col = hcl.colors(100, palette = "Rocket"))
# hcl.pals()

output_path <- paste(image_dir, "Stack (RGB) clusters.bmp", sep = "/")
bmp(file = output_path
    , width = round(1.2*ncol(eds_map)), height = round(1.2*nrow(eds_map))
    #, useDingbats = FALSE
)

# Note: some pixels are clustered in duplicate by overlapping polygons, and these
#   are overwritten according to the order in which cluster values are assigned
#   to the image
eds_map[] <- 0
eds_map[c1_green] <- 1
eds_map[c2_yellowgreen] <- 2
eds_map[c3_red] <- 3
eds_map[c4_lightblue] <- 4
eds_map[c4_darkblue] <- 5
plot(eds_map, col = hcl.colors(6, palette = "Hawaii"))

dev.off()

output_path <- paste(image_dir, "Stack (RGB) clusters.csv", sep = "/")
write.csv(file = output_path, x = as.matrix(eds_map)
          , row.names = FALSE, quote = FALSE)


# Test code for locate_poly() function, which may actually not work anymore?
if(FALSE){
  xs <- rnorm(100)
  ys <- rnorm(100)
  plot(xs, ys)
  
  flag_poly <- locate_poly(xs, ys, n_poly = 4)
  
  points(xs[flag_poly], ys[flag_poly], pch = 3, col = "red")
}