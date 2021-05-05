
download_li7200_rs_comparison_data = function(sensorID){
  
  if(sensorID == "Li7200"){
    # Specify where to download the file locally
    data.folder.Li7200 = "~/eddy/neon-sensor-test/data/2021_Li7200_RS_Comparision/Li7200/"
  } else if(sensorID == "Li7200RS"){
    # Specify where to download the file locally
    data.folder.Li7200 = "~/eddy/neon-sensor-test/data/2021_Li7200_RS_Comparision/Li7200RS//"
  } else{
    message("Select A (dev) or B (int)")
    data.folder.Li7200 = "Nope"
  }
  
  if(data.folder.Li7200 != "Nope"){
    
    if(sensorID == "Li7200"){
      # Li7200 (reg)
      list.of.Li7200.files = c(
       
        paste0("https://dev-sae-files.test-s3.data.neonscience.org/ods/dataproducts/IP4/2021-04-22/HQTW/NEON.D10.HQTW.IP4.00200.001.ecte.2021-04-22.expanded.h5.gz"),
        paste0("https://dev-sae-files.test-s3.data.neonscience.org/ods/dataproducts/IP4/2021-04-23/HQTW/NEON.D10.HQTW.IP4.00200.001.ecte.2021-04-23.expanded.h5.gz"),
        paste0("https://dev-sae-files.test-s3.data.neonscience.org/ods/dataproducts/IP4/2021-04-24/HQTW/NEON.D10.HQTW.IP4.00200.001.ecte.2021-04-24.expanded.h5.gz"),
        paste0("https://dev-sae-files.test-s3.data.neonscience.org/ods/dataproducts/IP4/2021-04-25/HQTW/NEON.D10.HQTW.IP4.00200.001.ecte.2021-04-25.expanded.h5.gz"),
        paste0("https://dev-sae-files.test-s3.data.neonscience.org/ods/dataproducts/IP4/2021-04-26/HQTW/NEON.D10.HQTW.IP4.00200.001.ecte.2021-04-26.expanded.h5.gz"),
        paste0("https://dev-sae-files.test-s3.data.neonscience.org/ods/dataproducts/IP4/2021-04-27/HQTW/NEON.D10.HQTW.IP4.00200.001.ecte.2021-04-27.expanded.h5.gz"),
        paste0("https://dev-sae-files.test-s3.data.neonscience.org/ods/dataproducts/IP4/2021-04-28/HQTW/NEON.D10.HQTW.IP4.00200.001.ecte.2021-04-28.expanded.h5.gz"),
        paste0("https://dev-sae-files.test-s3.data.neonscience.org/ods/dataproducts/IP4/2021-04-29/HQTW/NEON.D10.HQTW.IP4.00200.001.ecte.2021-04-29.expanded.h5.gz"),
        paste0("https://dev-sae-files.test-s3.data.neonscience.org/ods/dataproducts/IP4/2021-04-30/HQTW/NEON.D10.HQTW.IP4.00200.001.ecte.2021-04-30.expanded.h5.gz"),
        paste0("https://dev-sae-files.test-s3.data.neonscience.org/ods/dataproducts/IP4/2021-05-01/HQTW/NEON.D10.HQTW.IP4.00200.001.ecte.2021-05-01.expanded.h5.gz")
        # paste0("https://dev-sae-files.test-s3.data.neonscience.org/ods/dataproducts/IP4/2021-05-02/HQTW/NEON.D10.HQTW.IP4.00200.001.ecte.2021-05-02.expanded.h5.gz"),
        # paste0("https://dev-sae-files.test-s3.data.neonscience.org/ods/dataproducts/IP4/2021-05-03/HQTW/NEON.D10.HQTW.IP4.00200.001.ecte.2021-05-03.expanded.h5.gz"),
        # paste0("https://dev-sae-files.test-s3.data.neonscience.org/ods/dataproducts/IP4/2021-05-04/HQTW/NEON.D10.HQTW.IP4.00200.001.ecte.2021-05-04.expanded.h5.gz")
      )
      
    }
    if(sensorID == "Li7200RS"){
      # Li7200 RS
      list.of.Li7200.files = c(
        paste0("https://int-sae-files.test-s3.data.neonscience.org/ods/dataproducts/IP4/2021-04-22/HQTW/NEON.D10.HQTW.IP4.00200.001.ecte.2021-04-22.expanded.h5.gz"),
        paste0("https://int-sae-files.test-s3.data.neonscience.org/ods/dataproducts/IP4/2021-04-23/HQTW/NEON.D10.HQTW.IP4.00200.001.ecte.2021-04-23.expanded.h5.gz"),
        paste0("https://int-sae-files.test-s3.data.neonscience.org/ods/dataproducts/IP4/2021-04-24/HQTW/NEON.D10.HQTW.IP4.00200.001.ecte.2021-04-24.expanded.h5.gz"),
        paste0("https://int-sae-files.test-s3.data.neonscience.org/ods/dataproducts/IP4/2021-04-25/HQTW/NEON.D10.HQTW.IP4.00200.001.ecte.2021-04-25.expanded.h5.gz"),
        paste0("https://int-sae-files.test-s3.data.neonscience.org/ods/dataproducts/IP4/2021-04-26/HQTW/NEON.D10.HQTW.IP4.00200.001.ecte.2021-04-26.expanded.h5.gz"),
        paste0("https://int-sae-files.test-s3.data.neonscience.org/ods/dataproducts/IP4/2021-04-27/HQTW/NEON.D10.HQTW.IP4.00200.001.ecte.2021-04-27.expanded.h5.gz"),
        paste0("https://int-sae-files.test-s3.data.neonscience.org/ods/dataproducts/IP4/2021-04-28/HQTW/NEON.D10.HQTW.IP4.00200.001.ecte.2021-04-28.expanded.h5.gz"),
        paste0("https://int-sae-files.test-s3.data.neonscience.org/ods/dataproducts/IP4/2021-04-29/HQTW/NEON.D10.HQTW.IP4.00200.001.ecte.2021-04-29.expanded.h5.gz"),
        paste0("https://int-sae-files.test-s3.data.neonscience.org/ods/dataproducts/IP4/2021-04-30/HQTW/NEON.D10.HQTW.IP4.00200.001.ecte.2021-04-30.expanded.h5.gz"),
        paste0("https://int-sae-files.test-s3.data.neonscience.org/ods/dataproducts/IP4/2021-05-01/HQTW/NEON.D10.HQTW.IP4.00200.001.ecte.2021-05-01.expanded.h5.gz")
        # paste0("https://int-sae-files.test-s3.data.neonscience.org/ods/dataproducts/IP4/2021-05-02/HQTW/NEON.D10.HQTW.IP4.00200.001.ecte.2021-05-02.expanded.h5.gz"),
        # paste0("https://int-sae-files.test-s3.data.neonscience.org/ods/dataproducts/IP4/2021-05-03/HQTW/NEON.D10.HQTW.IP4.00200.001.ecte.2021-05-03.expanded.h5.gz"),
        # paste0("https://int-sae-files.test-s3.data.neonscience.org/ods/dataproducts/IP4/2021-05-04/HQTW/NEON.D10.HQTW.IP4.00200.001.ecte.2021-05-04.expanded.h5.gz")
      )
      
    }
    
    
    for(file in list.of.Li7200.files){
      # Create the destination file location to tell R where to save the Zips
      file.dest.Li7200A = base::paste0(data.folder.Li7200, substr(file, 89, 999))
      
      utils::download.file(url = file, destfile = file.dest.Li7200A)
      
    }
    # Specify what files to unzip
    gzFile <- base::list.files(data.folder.Li7200, pattern = ".gz", full.names = TRUE)
    # Unzip all the files in the folder
    base::lapply(gzFile, R.utils::gunzip)
  }
}
download.test.files(sensorID = "Li7200")
download.test.files(sensorID = "Li7200RS")
