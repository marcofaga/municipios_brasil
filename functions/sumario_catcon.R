sumario_catcon <- function(colA,colB,sub = "") {

  #sumario completo de cruzamento entre uma variável categórica e uma contínua.  
  
  if(class(colA) == "factor" & !class(colB) %in% c("factor","character")) {
    
    
    if(class(colA) == "factor") {
      valoresA <- levels(colA)
    } else {valoresA <- unique(colA) }
    
    num <- length(valoresA)
    
    lista <- c()
    
    x <- 1
    
    sub2 <- unique(sub)
    
    if(sub == "") {
      
      for(each in valoresA) {
        
        
        tamanho <- length(colB[colA == each])
        
        if(tamanho != 0) {
          soma1 <- round(sum(colB[colA == each]),1)
          media1 <- round(mean(colB[colA == each]),3)
          mediap <- round(mean(colB),3)
          min <- round(min(colB[colA == each]),3)
          max <- round(max(colB[colA == each]),3)
          desv <- round(sd(colB[colA == each]),3)
          zeta <- round((media1-mean(colB))/(desv/sqrt(tamanho)),3)
          pval <- round(pnorm(-abs(zeta)),3)
          estar <- ifelse(pval <= 0.001,"***",ifelse(pval <= 0.01,"**",ifelse(pval <= 0.05,"*","")))
          
        } else { 
          
          soma1 <- NA
          media1 <- NA
          min <- NA
          max <- NA
          desv <- NA
          zeta <- NA
          pval <- NA
          estar <- NA
          
        }
        
        
        lista[[x]] <-c(CATEGORIA = each,AMOSTRA = tamanho,SOMA = soma1,MEDIA = media1,MEDIA_P = mediap,MINIMO = min,MAXIMO = max,DESVP = desv,z = zeta,pvalor = pval,sig = estar)
        
        x <- x + 1
        
        
        
      }
      
      
    } else {
      
      
      for(each in valoresA) {
        
        
        for(each2 in sub2) {
          
          soma1 <- round(sum(colB[colA == each & sub == each2]),1)
          media1 <- round(mean(colB[colA == each  & sub == each2]),3)
          min <- round(min(colB[colA == each  & sub == each2]),3)
          max <- round(max(colB[colA == each  & sub == each2]),3)
          desv <- round(sd(colB[colA == each  & sub == each2]),3)
          numero <- length(colB[colA == each  & sub == each2])
          
          lista[[x]] <-c(CATEGORIA = each,CROSS = each2,AMOSTRA = numero, SOMA = soma1,MEDIA = media1,MINIMO = min,MAXIMO = max,DESVP = desv)
          
          x <- x + 1
          
        }
        
        
      }
      
      
      
      
      
      
    }
    
  } else {
    
    
    print("Uma tabela precisa ser continua e outra categorica")
    stop()
    
    
  }
  
  lista <- data.frame(lista)
  lista <- t(lista)
  lista <- data.frame(lista)
  
  if(sub != "") {
    
    
    lista <- lista[order(lista$Cross),]
    
  }
  
  rownames(lista) <- c(1:length(lista[,1]))
  num <- length(names(lista))-1
  lista[,2:num] <- apply(lista[,2:num],2,as.numeric)
  return(lista)
}
