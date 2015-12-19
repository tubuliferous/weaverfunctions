#' @docType package
#' @name bedanno
#' @title Useful functions.
#' @import dplyr
NULL
#' @import data.table
NULL
#' @import parallel


#######################################################
###################### Utilities ######################

#### Load packages and handle missing packages ####
#' @title activate_colorout
#' @description Syntax Highlighting
#' @details Install and activate terminal syntax Highlighting
#' @aliases activate_colorout
#' @author Ben Weaver
#' @export activate_colorout
#' @return NULL
activate_colorout <- function(){
  pkg = "colorout"
  if (pkg %in% rownames(installed.packages()) == FALSE){
    # Install package if not already installed
    download.file("http://www.lepem.ufc.br/jaa/colorout_1.1-0.tar.gz", destfile = "colorout_1.1-0.tar.gz")
    install.packages("colorout_1.1-0.tar.gz", type = "source", repos = NULL)
    # Enable newly installed package
    library(pkg,character.only=TRUE)
  }else{
  # Enable already installed package
  require(pkg,character.only=TRUE)
  }
  return(NULL)
}


#### Load package and handle missing package ####
#' @title install_missing_package
#' @description Install or load a package
#' @details Takes a package name as a character string and loads or installs then loads the named package
#' @aliases install_missing_package
#' @author Ben Weaver
#' @export install_missing_package
#' @param pkg A character
#' @return logical
install_missing_package <- function(pkg){
  if (!exists("biocLite")){
    source("http://bioconductor.org/biocLite.R")
  }
  chooseCRANmirror(ind=81)

  if (pkg %in% rownames(installed.packages()) == FALSE){
    # Install package if not already installed
    cran_package_list <- available.packages()[,1]
    if (pkg %in% cran_package_list){
      install.packages(pkg,repos='http://cran.us.r-project.org',dependencies=TRUE)
    }else{
      biocLite(pkg,suppressUpdates=FALSE,ask=FALSE)
    }
    # Enable newly installed package
    require(pkg,character.only=TRUE)
  }else{
  # Enable already installed package
  require(pkg,character.only=TRUE)
  }
}

#### Load packages and handle missing packages ####
#' @title activate_packages
#' @description Install and load multiple packages
#' @details Takes a vector of character package names and loads or installs then loads the named packages
#' @aliases activate_packages
#' @author Ben Weaver
#' @export activate_packages
#' @param pkg_vect A vector of character strings
#' @return logical
activate_packages <- function(...){
  sapply(list(...), install_missing_package)
}
#### Load packages and handle missing packages ####
#' @title get_directory_file_names
#' @description Get containted file paths from directory path
#' @details Takes directory path and returns contained file paths
#' @aliases get_directory_file_names
#' @author Ben Weaver
#' @export get_directory_file_names
#' @param directory A character vector of file name paths
#' @return character
get_directory_file_names <- function(directory){
  return(paste(directory, list.files(directory), sep=""))
}

#### Write to file with most commonly used options ####
#' @title qwrite
#' @description write.table() with common params
#' @details Write matrix-like object to file with most commonly used options
#' @aliases qwrite
#' @author Ben Weaver
#' @export qwrite
#' @param x A write.table() compatible object
#' @param file A character file path
#' @return NULL
qwrite <- function(x, file){
  write.table(x, file=file, quote=F, col.names=T, row.names=F, sep="\t")
  return(paste("Wrote file to", file))
}

#### Take samples of all files in a directory ####
#' @title qwritesamples
#' @description Get samples of files in directory
#' @details Write N samples of each file in directory (separate sample file for each file)
#' @aliases qwritesamples
#' @author Ben Weaver
#' @export qwritesamples
#' @param dir A character directory path
#' @param nsamples An integer number of rows to sample from each file
#' @param seed An integer seed for repeatable samples
#' @return NULL
qwritesamples <- function(dir,nsamples=10000,seed=NULL){
  file_paths <- get_directory_file_names(dir)
  sapply(file_paths,function(x){file_table<-data.table::fread(x);set.seed(seed);qwrite(file_table[sample(nrow(file_table),nsamples),],file=paste(x,".sample",sep=""))})
  return(NULL)
}

#### Chunk vector in N equal sizes and return list of sub vectors ####
#' @title chunk
#' @description Chunk vector in N equal sizes
#' @details Chunk vector in N equal sizes and return list of sub vectors. Taken from StackOverflow example
#' @aliases chunk
#' @author Ben Weaver
#' @export chunk
#' @param x A vector
#' @param n number of chunks
#' @return list
chunk <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE))

#### Split file into subset files by column name ####
#' @title split_file
#' @description Split file into subset files by column name
#' @details Split file into subset files by column name
#' @aliases split_file
#' @author Ben Weaver
#' @export split_file
#' @param file_path a character
#' @param split_col_name a character
#' @return NULL
split_file<-function(file_path,split_col_name){
  activate_packages("plyr")
  df<-data.frame(data.table::fread(file_path))
  split_list<-dlply(df,split_col_name,function(x){x})
  dir_path<-paste(file_path,"split",split_col_name,sep="_")
  dir.create(dir_path,recursive=TRUE)
  this_basename<-basename(file_path)
  write_split<-function(split){
    split_name<-split[[split_col_name]][1]
    new_file_path<-paste(dir_path,this_basename,sep="/")
    qwrite(split,file=paste(new_file_path,"split",split_name,sep="."))
  }
  lapply(split_list,write_split)
}

#' @title merge_files
#' @description Merge files with same header into subset file
#' @details Merge files with same header into subset file
#' @aliases merge_files
#' @author Ben Weaver
#' @export merge_files
#' @param file_paths a character
#' @return NULL
merge_files<-function(file_paths){
  activate_packages(c("data.table","parallel"))
  library("parallel")
  merged<-do.call(rbind,parallel::mclapply(file_paths,data.table::fread,mc.cores=4))
  out_file<-paste(dirname(file_paths[1]),"/merged.out",sep="")
  print(out_file)
  qwrite(merged,file=out_file)
}


#######################################################
##################### Statistics ######################

#' @title geom_mean
#' @description Geometric Mean
#' @details Performs geometric (product) mean on a vector
#' @aliases geom_mean
#' @author Ben Weaver
#' @export geom_mean
#' @param x A vector of numerics or integers
#' @return numeric

# Could have used: exp(mean(log(x)))
geom_mean <- function(x){prod(x)^(1/length(x))}

#######################################################
##################### Plotting ######################

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
#' @title multiplot
#' @description Multiple ggplot plots in single output
#' @details ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)  - cols:   Number of columns in layout   - layout: A matrix specifying the layout. If present, 'cols' is ignored. If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE), then plot 1 will go in the upper left, 2 will go in the upper right, and 3 will go all the way across the bottom.
#' @aliases multiplot
#' @author http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#' @export multiplot
#' @param ... A vector of plots
#' @param plotlist A list of ggplot objects
#' @param file Not sure about this
#' @param cols Integer number of columns in output
#' @param layout Matrix fine control of output
#' @return NULL
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#' @title gzfread
#' @description Import text from gzipped or gunzipped file to data.table
#' @aliases gzfread
#' @author http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#' @export gzfread
#' @param path a character
#' @return data.table
gzfread <- function(path, sep = "\t", out_dir = NULL){
  if(!stringr::str_detect(path, ".gz$")) {
    return(data.table::fread(path, sep = sep))
  }
  else{
    return(data.table::fread(paste0("zcat < ", "\"", path, "\"")))
  }
}
