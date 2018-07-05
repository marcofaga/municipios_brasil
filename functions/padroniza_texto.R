#Padroniza os textos. Deixa sem acento, minúscula, remove espaços em branco
#remove non ascii caracteres

stringpat <- function(string) {
  
  if(class(string) != "character") {
    
    stop("Não é uma String")
    
  }
  
  string <- tolower(string)
  string <- stri_trans_general(string, "Latin-ASCII")
  string <- gsub("[^0-9A-Za-z///' ]", " ", string)
  string <- rm_white(string)
  
  return(string)
   
  
  
}