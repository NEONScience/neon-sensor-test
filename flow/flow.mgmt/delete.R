# Delete data
# Set up S3 Delete Creds
delete_key = readRDS("~/neon-sensor-test/delete.RDS")
Sys.setenv("AWS_ACCESS_KEY_ID"     = "kstyers",
           "AWS_SECRET_ACCESS_KEY" =  delete_key,
           "AWS_S3_ENDPOINT"       = "neonscience.org",
           "AWS_DEFAULT_REGION"    = "s3.data")