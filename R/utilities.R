

`%notin%` <- Negate(`%in%`)

# loads the data for 1 participant:

loadParticipantTaskData <- function(ID, task) {
  
  # both halves of the data should be in one file now:
  data <- read.table(file = sprintf('data/sub-%s/ses-%s/beh/sub-%s_%s.tsv',ID,task,ID,task), header = TRUE, sep = "\t", skip = 0)
  
  # remove lines we don't need:
  data <- data[which(data$abort == FALSE),]
  
  # that's all we want for now:
  return(data)
  
}


setupFigureFile <- function(target='inline',width=8,height=6,dpi=300,filename) {
  
  if (target == 'pdf') {
    pdf(file   = filename, 
        width  = width, 
        height = height)
  }
  if (target == 'svg') {
    svglite::svglite( filename = filename,
                      width = width,
                      height = height,
                      fix_text_size = FALSE) 
    # fix_text_size messes up figures on my machine... 
    # maybe it's better on yours?
  }
  if (target == 'png') {
    png( filename = filename,
         width = width*dpi,
         height = height*dpi,
         res = dpi
    )
  }
  if (target == 'tiff') {
    tiff( filename = filename,
          compression = 'lzw',
          width = width*dpi,
          height = height*dpi,
          res = dpi
    )
  }
}

# luminance of stimuli ----

printStimulusLuminance <- function() {
  
  # CLUT was linearized by PsychoPy,
  # so values are in linear space.
  
  # [ [  0., 135.44739,  2.4203537, np.nan, np.nan, np.nan  ],
  #   [  0.,  27.722954, 2.4203537, np.nan, np.nan, np.nan  ],
  #   [  0.,  97.999275, 2.4203537, np.nan, np.nan, np.nan  ],
  #   [  0.,   9.235623, 2.4203537, np.nan, np.nan, np.nan  ]  ],
  
  maxLums <- c(27.722954, 97.999275, 9.235623)
  
  background <-	(c(0.50000000,0.50000000,-1.00000000)  + 1) / 2
  red        <-	(c(0.50000000,-1.00000000,-1.00000000) + 1) / 2
  green      <- (c(-1.00000000,0.50000000,-1.00000000) + 1) / 2
  
  # luminances
  cat(sprintf('background: %0.3f\n',sum(background * maxLums)))
  cat(sprintf('red:        %0.3f\n',sum(red        * maxLums)))
  cat(sprintf('green:      %0.3f\n',sum(green      * maxLums)))
  
  Rmax = 27.722954
  Gmax = 97.999275
  Rw = Rmax / (Rmax+Gmax)
  Lw = Gmax / (Rmax+Gmax)
  col_binoc <- c()
  for (idx in c(1:3)) {
    col_binoc[idx] <- (Lw*red[idx]) + (Rw*green[idx])
  }
  
  cat(sprintf('binocular:  %0.3f (lum weighted)\n',sum(col_binoc * maxLums)))
  col_binoc <- c()
  for (idx in c(1:3)) {
    col_binoc[idx] <- (.5*red[idx]) + (.5*green[idx])
  }
  
  cat(sprintf('binocular:  %0.3f (50-50)\n',sum(col_binoc * maxLums)))
  cat(sprintf('red&green:  %0.3f\n',mean(c(sum(red        * maxLums),sum(green      * maxLums)))))
  
  
  
}