# Boundary Boxes
belt_crop <- . %>% 
  st_crop(xmin = -77.41678, xmax = -76.687215,
        ymin = 38.603916, ymax = 39.175237)

close_crop <- . %>% 
  st_crop(xmin = -77.398979, xmax = -76.896355,
          ymin = 38.722665, ymax = 39.012429)
