#script to download GLC2000 data
link <-  "https://forobs.jrc.ec.europa.eu/data/products/glc2000/glc2000_v1_1_Tiff.zip"

download.file(link,
              "input/GLC2000/GLC2000.zip",
              method = "wget")
unzip(zipfile = "input/GLC2000/GLC2000.zip",
      exdir = "input/GLC2000",
      junkpaths = TRUE)

