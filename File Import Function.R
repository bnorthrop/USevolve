# FILE IMPORT FUNCTION FROM CLASS
## Modify & Document for this specific package

# import data set
import <- function(file){
  
  # if no file specified, prompt user
  if(missing(file)) {
    file <- file.choose()
  }
  
  # get file extension
  extension <- tools::file_ext(file)
  
  # import data set
  if (extension == "sas7bdat")
    df <- haven::read_sas(file)
  
  if (extension == "dta")
    df <- haven::read_stata(file)
  
  if (extension == "sav")
    df <- haven::read_spss(file)
    
  if (extension == "xlsx" | extension == "xls")
    df <- readxl::read_excel(file)
  
  if (extension %notin% c("sas7bdat", "dta", "sav", "xlsx", "xls"))
    df <- vroom::vroom(file)
  
  # return data frame
  return(df)
}

###################### OR ##########################

# import data set (3 dots allow us to specify options; e.g. which sheet we want from excel workbook)
import <- function(file, ...){
  
  # if no file specified, prompt user
  if(missing(file)) {
    file <- file.choose()
  }
  
  # get file extension (& make sure its lower case)
  extension <- tolower(tools::file_ext(file))
  
  # import data set
  df <- switch(extension,
               "sas7bdat" = haven::read_sas(file, ...),
               "dta" = haven::read_stata(file, ...),
               "sav" = haven:: read_spss(file, ...),
               "xlsx" = readxl::read_excel(file, ...),
               "xls" = readxl::read_excel(file, ...),
               vroom:vroom(file)
               )
  
  # return data frame
  return(as.data.frame(df))
}

#########################

# use (&test) the function
mydata <- import("email.dta")









