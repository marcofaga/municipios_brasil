#insere uma linha no index contendo nome do objeto e descrição.
insereindex <- function(obj,descr) {
  
  if("index" %in% ls(envir = globalenv())) {
  
  
  apagind <- c(obj,descr)
  insere <- rbind(index,apagind)

  insere$DUP <- duplicated(insere$obj,fromLast = T)

  insere <- insere[insere$DUP == F,]
  insere <- insere[,c(1,2)]
  insere <- insere[order(insere$obj),]
  rownames(insere) <- c(1:dim(insere)[1])
  return(insere)
  
  } else {
    
    datavazio <- data.frame(obj = obj, descr = descr,stringsAsFactors = F)
    
    
   
    
    
  }
  

  
}