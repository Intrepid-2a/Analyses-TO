

getData <- function() {
  
  downloadOSFdata(repository='https://osf.io/qypv2/',
                  filelist=list('BIDS' = 
                                  c('task-area.zip',
                                    'task-curvature.zip',
                                    'task-distance.zip')),
                  folder='data',
                  overwrite=TRUE,
                  unzip=TRUE,
                  removezips=TRUE,
                  wait=40)
  
  
}



downloadOSFdata <- function(repository,filelist,folder,overwrite=TRUE,unzip=FALSE,removezips=FALSE,wait=40) {
  
  # get the 5-character repository name:
  slash_idx <- as.numeric(gregexpr(pattern ='/',repository)[[1]])
  if (rev(slash_idx)[1] == nchar(repository)) {
    repository <- substr(repository,1,rev(slash_idx)[1]-1)
    slash_idx <- as.numeric(gregexpr(pattern ='/',repository)[[1]])
    if (length(slash_idx)>0) {
      repository <- substr(repository,rev(slash_idx)[1]+1,nchar(repository))
    }
  }
  # connect to the repository:
  mainOSFnode <- osfr::osf_retrieve_node(repository)
  
  
  if (overwrite) {
    conflict <- 'overwrite'
  } else {
    conflict <- 'skip'
  }
  
  # loop through entries in filelist
  for (folderno in c(1:length(names(filelist)))) {
    
    foldername <- names(filelist)[folderno]
    
    # foldername needs to have a trailing back slash:
    if (substr(foldername,nchar(foldername),nchar(foldername)) != "\\" ) {
      foldername <- sprintf('%s\\',foldername)
    }
    
    # list files in the OSF folder:
    files <- osfr::osf_ls_files(mainOSFnode, path=foldername, n_max=500)
    
    cat('files on OSF:\n')
    print(files)
    
    filenames <- filelist[[names(filelist)[folderno]]]
    
    cat('files to download:\n')
    print(filenames)
    
    for (filename in filenames) {
      
      cat(sprintf('making sure we have: %s\n',filename))
      
      # find which line corresponds to the file:
      idx <- which(files$name == filename)
      
      # check that the file exists on OSF, and is unique:
      # if not: skip to next file
      if (length(idx) != 1) {
        next
      }
      
      # download the file:
      if (!file.exists(sprintf('data/%s',files$name[idx])) | overwrite) {
        osfr::osf_download(x = files[idx,], 
                           path = sprintf('%s', folder), 
                           conflicts = conflict)
      }
      
      if (filename != filenames[length(filenames)]) {
        Sys.sleep(wait)
      }
      
    }
    
  }
  
  if (unzip) {
    unzipZips(filelist=filelist,
              folder=folder,
              removezips=removezips)
  }
  
  
}


unzipZips <- function(filelist,folder,removezips=FALSE) {
  
  # loop through entries in filelist
  for (folderno in c(1:length(names(filelist)))) {
    
    foldername <- names(filelist)[folderno]
    
    # foldername needs to have a trailing back slash:
    if (substr(foldername,nchar(foldername),nchar(foldername)) != "\\" ) {
      foldername <- sprintf('%s\\',foldername)
    }
    
    filenames <- filelist[[names(filelist)[folderno]]]
    
    for (filename in filenames) {
      
      # check if it is a zip file:
      ext <- tools::file_ext(filename)
      if (ext == 'zip' & file.exists(sprintf('%s/%s',folder,filename))) {
        utils::unzip(zipfile=sprintf('%s/%s',folder,filename),
                     exdir=folder)
        if (removezips & file.exists(sprintf('%s/%s',folder,filename))) {
          file.remove(sprintf('%s/%s',folder,filename))
        }
        
      }
      
    }
    
  }
  
}

