# Function to read vaccination data


read.vaccination.data <- function(data) {
  CountryCodes <- read.csv('Data/CountryCode.csv') %>% select(ReportingCountry,region)
  azure_api_key <- 'Nv6xafVAW903u9mO2gxiSIX0cPyNaGfI533IYOROynLSq9Lpq3pZ7O7GeBjLCzh40pnPei6p1TP1kmqlsSH+8w=='
  bl <- AzureStor::storage_endpoint("https://covvacdatafromtessy.blob.core.windows.net/", key = azure_api_key)
  cont <- AzureStor::storage_container(bl, "covvacdatafiles-weekly-latest")
  vaccine_dataset <- AzureStor::storage_read_csv(cont, "TESSy.csv")
  return(vaccine_dataset)
}
