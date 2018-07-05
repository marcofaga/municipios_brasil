latextabdic <- function(tabela, objeto = "") {
  
  #objeto funcao pode ser vazio (") ou "lastbold" que deixa a última linha em negrito e com uma linha acima.
  #Usado para linhas de totais.
  
  require(knitr)
  
  objname <- deparse(substitute(tabela))

  tabela <- as.data.frame(tabela)

  ncol <- names(tabela)

  ncol <- combine_words(ncol,sep = " & ",and = "")
  
  ncol <- paste(ncol,collapse = "")
  
  ndim <- dim(tabela)[1]

  apag <- c()

  dimensaocol <- dim(tabela)[2]
  
  pasta <- getwd()
  

  #construção do tex
  sink("temp.Rnw")
  
  cat("%Criado por Marco Antonio Faganello a partir do objeto ",objname,"em ",pasta,"\n")
  cat("\\begin{table}[h]","\n")
  cat("\\begin{center}","\n")
  cat("\\begin{tabular}{r|l|l}", "\n")
  cat("\\hline","\n")
  cat("\\hline","\n")
  
  for(ap1 in 1:nrow(tabela)) {
    
    linha <- paste(tabela[ap1, ])
    linha <- gsub("_", "\\_", linha, fixed = T)
    
    if(linha[5] == "NA") {
      
      row <- 3
      
    } else {
      
      row <- 4
      
    }
    
    cat("\\multirow{", row, "}{*}{", linha[1], "} & \\multirow{", row, "}{*}{", linha[2], "}", " & ", linha[3], "\\\\",   "\n", sep="")
    cat("&& ", linha[4], " \\\\", "\n")
    
    if(row == 4) {
    
      cat("&& ", linha[5], " \\\\", "\n")
    
    }
    
    cat("&& ", linha[6], " \\\\", "\n")
    cat("\\hline", "\n")
    
  }

  
  cat("\\end{center}","\n")
  cat("\\end{table}")
  
  sink()
  
  file.edit("temp.Rnw")

}