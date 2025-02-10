This repository holds the code needed to reproduce results for the manuscript "Lack of evidence for alternative stable states in boreal forest during the past 8 ka".
As the analyses and associated download are quite computation (and time) intensive we provide three options:
	1. Running a geographical subset.
		For this option the code will only run on a geographical subset. The input for this analysis option is provided in a Zenodo directory and will be downloaded automatically
		when running the main script for the subset analysis. The Zenodo directory has a size of 790 MB (downloading depends on your network speed but takes approximately 2 minutes).
		In order to run the subset analysis you only need to run the script "subset_analysis.R" located in the root of the directory.
		You can find results from the analyses in the output folder. All of the results will have the prefix "sub_". Needless to say, the results will differ significantly
		from the manuscript results due to the geographical subset.
		This depending on your machine this option will roughly take between 20 and 60 minutes to run.
	2. Running the entire analyses
		For this option the code will download all necessary climate and land cover data from different sources. The input directory provided in Zotero is also necessary for
		this analysis option. It includes the MODIS data, Pollen data and paths for the other data sources. The download for the complete original data will take roughly 1 hour.
		The analysis will take a long time due to data extraction and surrogate creation. Once the downloads are complete it will take approximately another hour.
		You can find the results from the analyses in the output folder. 
	3. Re-creating important figures
		The repository already includes analysis output in the output folder. If you don't want to run the entire analsyes you can have a look at the tables and recreate 
		figures 5 and 6 from the manuscript.

Technical specifications:
	- the code was written and run using R version 4.2.2 on Windows 10 x64
	- if problems occur while running the code please check your version

Data availability:
	- the input directory is automatically downloaded from Zenodo (https://zenodo.org/doi/10.5281/zenodo.10069577)
	  should this not work you can download it manually and untar the directory. It should be named "input" for the rest of the code to function.
	- data download (for the complete analysis) is automated using download paths and wget scripts. 
   	  Below is a list of stable doi, should the automatic downloading cease to work.
		- MPI
			-glac1d: https://doi.org/10.26050/WDCC/PMMXMCRTDGP132
			-ice6g: https://doi.org/10.26050/WDCC/PMMXMCRTDIP132
		-CRU v.4.07: https://doi.org/10.1038/s41597-020-0453-3
		-CHELSA-TraCE 21k: https://doi.org/10.5194/cp-2021-30
		-GLC2000:  https://forobs.jrc.ec.europa.eu/glc2000

		- Pollen-based tree cover is included completely in this repository. Original data can be downloaded from:
			https://doi.org/10.1594/PANGAEA.974798
		- MODIS44B v006 is no longer available through NASA. We included extracted point data in this repository. Currently a version is archieved on google earth engine 
			https://developers.google.com/earth-engine/datasets/catalog/MODIS_006_MOD44B


