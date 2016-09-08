#' @title Convert ICD code(s)
#'
#' @author Daniel Lindholm
#'
#' @description Converting ICD codes between the ICD-9 and ICD-10 classifications is not entirely
#' trivial. There are about twice as many codes in ICD-10 compared to ICD-9. In addition,
#' several countries have constructed their own variants of the classification systems.
#' This function will try to convert ICD codes between the two definitions, with a possibility
#' to specify how flexible the translation should be. If a code is translated and yields multiple
#' ICD codes in the target definition, only the first code will be returned. 
#' The function also has multicore support, to reduce computing time when analyzing large datasets. Also, if an
#' ICD code is not found in the database on its 4-digit representation, the function will
#' also try to find a 3-digit code by keeping only the first three characters in the code.
#' It will work only with the 'clean' original versions of ICD, i.e. country-specific codes
#' have to be converted into a style that the function understands; e.g. "I259" for ICD10, "4149"
#' for ICD9. This function is in early development, and should be tested more before put to use.
#' 
#' @param ICDcode a vector containing the ICD codes that should be converted. The
#' code must not contain dots (which are common in some countries representation of the ICD10 system).
#' @param from What is the source classification: "icd10" or "icd9"? Defaults to "icd10".
#' @param to What is the destination classification: "icd10" or "icd9"? Defaults to "icd9".
#' @param flexibility Takes either of the following values: 0, 1, 2:
#'        \itemize{
#'            \item 0 means no flexibility at all, i.e. the content in ICD9 and ICD10 codes
#'            must match perfectly; this will probably induce quite many NAs.
#'            \item 1 means that the function accepts that either the ICD10 or the ICD9
#'            code more precisely describes the diagnosis.
#'            \item 2 corresponds to the most flexible conversion, where the function accepts
#'            that e.g. an ICD10 code is most properly described by several ICD9 codes, or vice versa.
#'        }
#'        Defaults to 1.
#' @param cores The number of processor cores to use. Multicore functionality is
#' available on all unix platforms (e.g. MacOSX, Linux), but not on Windows. Defaults to 1.
#' @return Outputs a character vector containing the converted ICD codes.
#' @examples
#' # Simple example of converting from ICD10 to ICD9:
#' convertICD("I259")
#'
#' # Run on several processor cores:
#' test_codes <- c("I10", "I259", "C820", "E669")
#' convertICD(test_codes, cores = 4)
#'
#' @export

convertICD <- function(ICDcode, from = "icd10", to = "icd9", flexibility = 1, cores = 1){
  # Load data.table
  library(data.table)
  #data(cICD)
  # Checks
  if (from != "icd10" & from != "icd9") stop("from should either be 'icd9' or 'icd10'!")
  if (to != "icd10" & to != "icd9") stop("to should either be 'icd9' or 'icd10'!")
  if (from == to) stop("from and to can't take on the same value!")
  if (!is.numeric(flexibility)) stop("flexibility should be numeric!")
  if (!is.element(flexibility, c(0,1,2))) stop("flexibility takes on one of the following only: 0, 1, 2")
  if(!is.numeric(cores)) stop("cores should be numeric!")
  if(cores > 1 & .Platform$OS.type != "unix"){
    warning("Multicore functionality only available in unix-like environments. Proceeding with only one core.")
    cores <- 1
  }
  if (from == "icd10") setkey(cICD, icd10)
  if (from == "icd9") setkey(cICD, icd9)
  # Parallel version
  if(cores > 1){
    library(parallel)
    result <- mclapply(ICDcode, function(x) .internal_convertICD(x, from = from, to = to, flexibility = flexibility), mc.cores = cores)
  }
  # Non-parallel version
  if(cores < 2){
    result <- lapply(ICDcode, function(x) .internal_convertICD(x, from = from, to = to, flexibility = flexibility))
  }
  return(unlist(result))
}
