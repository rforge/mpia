fold_in <- function (docvecs, LSAspace, vectors.return=FALSE) {
   
   if (vectors.return==TRUE) {
      dqs = crossprod(t(crossprod(docvecs, LSAspace$tk)), solve(diag(LSAspace$sk)))
      class(dqs) = "docvector"
      return(dqs)
   } else {
      dqs = crossprod(t(crossprod(docvecs, LSAspace$tk)), solve(diag(LSAspace$sk)))
      dtm = crossprod(t(crossprod(t(LSAspace$tk), diag(LSAspace$sk))), t(dqs))
      
      rownames(dtm) = rownames(LSAspace$tk)
      colnames(dtm) = colnames(docvecs)
      environment(dtm) = new.env()
      class(dtm) = "textmatrix"
      return(dtm)
   }
   
} # function fold_in ()