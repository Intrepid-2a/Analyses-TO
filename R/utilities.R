

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

