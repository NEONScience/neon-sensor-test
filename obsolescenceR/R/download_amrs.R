#' @title Extract AMRS data to S3
#' @description  This function takes an S3 bucket folder path, reads out the AMRS data, saves that, then deletes the file.
#' @param  s3filepath a character string that provides the directory path for the AMRS data stored in S3. The file path must end in the `h5.gz` format.
#' @examples
#' download.amrs.test.data(s3filepath = "https://neon-sae-files.s3.data.neonscience.org/ods/dataproducts/IP0/2021-02-09/WOOD/NEON.D09.WOOD.IP0.00200.001.ecte.2021-02-09.l0p.h5.gz")

download_amrs = function(s3filepath = ""){
  require(tidyr)
  require(dplyr)
  require(data.table)
  require(stringr)
  require(rhdf5)
  require(aws.s3)
  require(aws.signature)

  s3_bucket = "neon-sensor-test"

  if(s3filepath != ""){

    # Create a temporary directory (Linux-Only) to store the data before storing in memory
    temp_dir = "/tmp/fake_s3_data/"
    if(base::dir.exists(paths = temp_dir) == TRUE){
      # Delete any files lingering in that dir
      lapply(X = list.files("/tmp/fake_s3_data/", full.names = TRUE), file.remove)
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
      # Check that the h5 file has data
      if(nrow(localfile.ls) > 0){

        # Check that the file has the AMRS data we need
        localfile.check = localfile.ls %>%
          dplyr::filter(stringr::str_detect(string = group, pattern = "dp0p/data") == TRUE & name == "amrs")

        if(nrow(localfile.check) == 1){

          # Create a dyanmic file path that pulls just the AMRS data
          h5.path = paste0(localfile.check$group[1], "/", localfile.check$name[1])
          # Read the data in
          data.in = rhdf5::h5read(file = localfile, name = h5.path)
          # Delete the data right after it is read in
          lapply(X = list.files("/tmp/fake_s3_data/", full.names = TRUE), file.remove)

          # Figure what ml the AMRS is installed on
          ml = names(data.in)
          # Pull out the ML data
          data.out = data.in[[ml]]
          # Verify we have all 14 columns we have
          data.out.len = length(data.out)

          if(data.out.len == 14){
            message("Saving data!")

            # Set up S3 Write Creds
            write_key = readRDS("~/neon-sensor-test/write.RDS")
            Sys.setenv("AWS_ACCESS_KEY_ID"     = "sensor-test-writer",
                       "AWS_SECRET_ACCESS_KEY" =  write_key,
                       "AWS_S3_ENDPOINT"       = "neonscience.org",
                       "AWS_DEFAULT_REGION"    = "s3.data")

            # TODO This will have to be change when we move to the Preliminary Run data...
            if(grepl(s3filepath, pattern = "WREF") == TRUE){s3filename = "sensor/amrs/test/2021-02-09/amrs01.fst"}
            if(grepl(s3filepath, pattern = "JORN") == TRUE){s3filename = "sensor/amrs/test/2021-02-09/amrs02.fst"}
            if(grepl(s3filepath, pattern = "WOOD") == TRUE){s3filename = "sensor/amrs/test/2021-02-09/amrs03.fst"}

            # Create final data.table
            data.save = data.table::data.table(
              "accXaxs"     = data.out$accXaxs,
              "accXaxsDiff" = data.out$accXaxsDiff,
              "accYaxs"     = data.out$accYaxs,
              "accYaxsDiff" = data.out$accYaxsDiff,
              "accZaxs"     = data.out$accZaxs,
              "accZaxsDiff" = data.out$accZaxsDiff,
              "angXaxs"     = data.out$angXaxs,
              "angYaxs"     = data.out$angYaxs,
              "angZaxs"     = data.out$angZaxs,
              "avelXaxs"    = data.out$avelXaxs,
              "avelYaxs"    = data.out$avelYaxs,
              "avelZaxs"    = data.out$avelZaxs,
              "idx"         = data.out$idx,
              "time"        = data.out$time

              )


            # Format time
            data.save = data.save %>%
              dplyr::mutate(time = as.POSIXct(time,format = "%Y-%m-%dT%H:%M:%OS", tz = "UTC"))

            # fst files have better compression read about 1.5 times faster and save at the same size with the 100 compression
            # reading in a single column is twice as fast as reading in just 1 column, so if memory is limited this may be a good approach, but otherwise its smarter to just read in all the data
            aws.s3::s3write_using(x = data.save, FUN = fst::write.fst, compress = 100, object = s3filename, bucket = s3_bucket)

            object.exists = aws.s3::object_exists(object = s3filename, bucket = s3_bucket)

            if(object.exists == TRUE){message(paste0("Object: ", s3filename, " saved successfully"))}

            # Delete all data in the tmp folder
            lapply(X = list.files("/tmp/fake_s3_data/", full.names = TRUE), file.remove)

            if(length(list.files("/tmp/fake_s3_data/")) > 0){
              message("Deleting files again ... ")
              lapply(X = list.files("/tmp/fake_s3_data/", full.names = TRUE), file.remove)
            }

          } else {
            lapply(X = list.files("/tmp/fake_s3_data/", full.names = TRUE), file.remove)
            stop("Unexpected number of columns, check data.out variable...")
          }
        } else {
          lapply(X = list.files("/tmp/fake_s3_data/", full.names = TRUE), file.remove)
          stop("AMRS L0p data not found")
        }
      } else {
        lapply(X = list.files("/tmp/fake_s3_data/", full.names = TRUE), file.remove)
        stop("File did not have any data...?")
      }
    } else {
      lapply(X = list.files("/tmp/fake_s3_data/", full.names = TRUE), file.remove)
      stop("Files did not download properly...")
    }
  } else {
    lapply(X = list.files("/tmp/fake_s3_data/", full.names = TRUE), file.remove)
    stop("Please specify an S3 file path")
  }
}
