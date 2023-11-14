#check if input directory is already there
input_present <- "input" %in% list.files()
if(!input_present){
  #download input directory
  zenodo_repos <- "10.5281/zenodo.10074865"
  zen4R::download_zenodo(doi = zenodo_repos,
                         timeout = 600)
  
  #unpack input directory
  untar("input.tar.gz")
  #rename to just input
  #file.rename("input_repos","input")
  #delete compressed version
  unlink("input.tar.gz")
}

