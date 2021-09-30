setwd("~/R") #Setup working directory 
install.packages("bitops") #This has to be installed before RCurl
install.packages("RCurl")
install.packages("plyr")
library(bitops)  #Required bitops library before RCurl library
library(RCurl)
library(XML)
library(stringr)
library(plyr)

url <-"http://educationaldatamining.org/conferences/"
links <-getHTMLLinks(url)
links
filenames <-links[str_detect(links, ".pdf")]
filenames
##Create a list with str_extract_all
filename_list <-str_extract_all(filenames, "EDM.+pdf")
filename_list

##Create a function to download the files
##Using the base R function download.file is sufficient for standard situation

downloadPDF <- function(filename, baseurl, folder) {
  dir.create(folder, showWarnings = FALSE) #create a folder
  fileurl <- str_c(baseurl, filename)
  if (!file.exists(str_c(folder, "/", filename))) {
    download.file(fileurl, destfile = str_c(folder,"/", mode="wb", filename))
    Sys.sleep(5)
  }
}

l_ply(filename_list, downloadPDF,
      baseurl = "http://educationaldatamining.org/conferences/",
      folder = "EDM_Conf")
