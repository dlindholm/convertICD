
.get_ICD_code <- function(value, from, to){
  # value = the character string to look up
  # from = "icd9" or "icd10"
  # to = "icd9" or "icd10"
  # Requires that the key has been set in calling function!
  result <- cICD[value, get(to)][1]
  if (length(result) == 0) return(NA)
  else return(result)
}

.get_R_number <- function(value, from){
  # value = the character string to look up
  # from = "icd9" or "icd10"
  # Requires that the key has been set in calling function!
  result <- cICD[value, R_number][1]
  return(result)
}

.internal_convertICD <- function(ICDcode, from, to, flexibility = 1){
  ICDcode <- as.character(ICDcode)

  if (!is.element(ICDcode, cICD[[from]])) {
    #If the code is not found, try to take only the first 3 characters of the string,
    #to see if that is found. If not, return NA.
    if (nchar(ICDcode) > 3) ICDcode <- substr(ICDcode, 0, 3)
    if (!is.element(ICDcode, cICD[[from]])) return(NA)
  }

  result <- .get_ICD_code(ICDcode, from, to)
  R_number <- .get_R_number(ICDcode, from)

  if (R_number == 5) return(NA)

  if (flexibility == 0){
    if (R_number[1] == 1) return(result)
    else return(NA)
  }
  if (flexibility == 1){
    if (R_number[1] <= 3) return(result)
    else return(NA)
  }
  if (flexibility == 2){
    if (R_number[1] <= 4) return(result)
    else return(NA)
  }
}


