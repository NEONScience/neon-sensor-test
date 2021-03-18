# Practicing performing AMRS calculations from L0p data files
# The Objective here is to download the file from S3, read in the AMRS data, save that, then delete the file
download.amrs.test.data = function(s3filepath = ""){
  require(tidyr)
  require(dplyr)
  require(stringr)
  require(rhdf5)
  if(s3filepath != ""){
  
    # Create a temporary directory (Linux-Only) to store the data before storing in memory 
    temp_dir = "/tmp/fake_s3_data/"
    if(base::dir.exists(paths = temp_dir) == TRUE){
      # do nothing
    } else {
      # Create the tmp dir
      base::dir.create(path = temp_dir)
      # If the tmp dir is not created, stop...
      if(base::dir.exists(paths = temp_dir) == FALSE){
        stop(paste0(temp_dir, " failed to be created... Check permissions?"))
      }
    }
    
    # Create reduced file path from s3filename 
    localfilepath.dt = data.table::data.table(file = s3filepath) %>% 
      tidyr::separate(col = file, sep = "/", into = c("V1", "V2", "V3", "V4", "V5", "V6", "V7","V8","V9"))
    localfilepath = paste0(temp_dir, localfilepath.dt$V9[1])

    # Download file to tmp location
    utils::download.file(url = s3filepath, destfile = localfilepath)
    
    # Check files downloaded properly
    if(file.exists(localfilepath)){
      # Unzip the files
      message("Unzipping file")
      R.utils::gunzip(localfilepath)
      
      # Unzipped file name 
      localfile = gsub(localfilepath, pattern = ".gz", replacement = "")
      
      localfile.ls = rhdf5::h5ls(file = localfile, 
                                datasetinfo = FALSE)
      
      if(nrow(localfile.ls) > 0){
        
        localfile.check = localfile.ls %>% 
          dplyr::filter(stringr::str_detect(string = group, pattern = "dp0p/data") == TRUE & name == "amrs")
        
        if(nrow(localfile.check) == 1){
          
          h5.path = paste0(localfile.check$group[1], "/", localfile.check$name[1]) 
          
          data.in = rhdf5::h5read(file = localfile, name = h5.path)
          
          ml = names(data.in)
          
          data.out = data.in[[ml]]
          
          data.out.len = length(data.out)
          
          if(data.out.len == 14){
            message("Saving data!")
            
            # Set up S3 Write Creds
            write_key = readRDS("~/neon-sensor-test/write.RDS")
            Sys.setenv("AWS_ACCESS_KEY_ID"     = "sensor-test-writer",
                       "AWS_SECRET_ACCESS_KEY" =  write_key,
                       "AWS_S3_ENDPOINT"       = "neonscience.org",
                       "AWS_DEFAULT_REGION"    = "s3.data")
            
            if(grepl(s3filepath, pattern = "WREF") == TRUE){s3filename = "sensor/amrs/test/2021-02-09/amrs01.RDS"}
            if(grepl(s3filepath, pattern = "JORN") == TRUE){s3filename = "sensor/amrs/test/2021-02-09/amrs02.RDS"}
            if(grepl(s3filepath, pattern = "WOOD") == TRUE){s3filename = "sensor/amrs/test/2021-02-09/amrs03.RDS"}
            
            aws.s3::s3saveRDS(x = data.out, object = s3filename, bucket = "neon-sensor-test")
            
            # Delete old data
            file.remove(localfile)
            
          } else {
            stop("Unexpected number of columns, check data.out variable...")
          }
        } else {
          stop("AMRS L0p data not found")
        }
      } else {
        stop("File did not have any data...?")
      }
    } else {
      stop("Files did not download properly...")
    }
  } else {
    stop("Please specify an S3 file path")
  }
}

amrs_01.s3filepath = "https://neon-sae-files.s3.data.neonscience.org/ods/dataproducts/IP0/2021-02-09/WREF/NEON.D16.WREF.IP0.00200.001.ecte.2021-02-09.l0p.h5.gz"
amrs_02.s3filepath = "https://neon-sae-files.s3.data.neonscience.org/ods/dataproducts/IP0/2021-02-09/JORN/NEON.D14.JORN.IP0.00200.001.ecte.2021-02-09.l0p.h5.gz"
amrs_03.s3filepath = "https://neon-sae-files.s3.data.neonscience.org/ods/dataproducts/IP0/2021-02-09/WOOD/NEON.D09.WOOD.IP0.00200.001.ecte.2021-02-09.l0p.h5.gz"

download.amrs.test.data(s3filepath = amrs_01.s3filepath)
download.amrs.test.data(s3filepath = amrs_02.s3filepath)
download.amrs.test.data(s3filepath = amrs_03.s3filepath)