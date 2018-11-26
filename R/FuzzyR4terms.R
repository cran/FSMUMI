#' @title Building a fuzzy inference system
#' @author Thi-Thu-Hong Phan, Andre Bigand, Emilie Poisson-Caillault
#' @description 
#' This function creates a fuzzy inference system that contains three input variables and three output variables.
#' Each variable is expressed by 4 linguistic terms as low, medium, medium_high and high.
#' A trapezoidal membership function is used to match input and output to a degree of membership.
#' @return a fis  object - a fuzzy  inference system. 
#' @import FuzzyR 


create.Myfis<-function(){
  fis= FuzzyR::newfis('test')
  #This version uses tramf. Before trimf is used
  
  fis= addvar(fis, 'input', 'cosine', c(0,1))
  fis= addvar(fis, 'input', 'jaccard', c(0,1))
  fis= addvar(fis, 'input', 'ed', c(0,1))
  fis= addvar(fis, 'output', 'w1', c(0,1))
  fis= addvar(fis, 'output', 'w2', c(0,1))
  fis= addvar(fis, 'output', 'w3', c(0,1))
  
  
  fis = addmf(fis, "input", 1, 'low','trapmf',c(-0.3,0,0.2,0.4))
  fis =addmf(fis, "input", 1, 'medium','trapmf',c(0.3,0.4,0.55,0.65))
  fis = addmf(fis, "input", 1, 'medium_high','trapmf',c(0.55,0.65,0.8,0.9))
  fis = addmf(fis, "input", 1, 'high','trapmf',c(0.8,0.9,1,1.1))
  
  
  fis = addmf(fis, "input", 2, 'low','trapmf',c(-0.3,0,0.2,0.4))
  fis = addmf(fis, "input", 2, 'medium','trapmf',c(0.3,0.4,0.55,0.65))
  fis = addmf(fis, "input", 2, 'medium_high','trapmf',c(0.55,0.65,0.8,0.9))
  fis = addmf(fis, "input", 2, 'high','trapmf',c(0.8,0.9,1,1.1))
  
  fis = addmf(fis, "input", 3, 'low','trapmf',c(-0.3,0,0.2,0.4))
  fis = addmf(fis, "input", 3, 'medium','trapmf',c(0.3,0.4,0.55,0.65))
  fis = addmf(fis, "input", 3, 'medium_high','trapmf',c(0.55,0.65,0.8,0.9))
  fis = addmf(fis, "input", 3, 'high','trapmf',c(0.8,0.9,1,1.1))
  
  
  fis = addmf(fis, "output", 1, 'low','trapmf',c(-0.3,0,0.2,0.4))
  fis = addmf(fis, "output", 1, 'medium','trapmf',c(0.3,0.4,0.55,0.65))
  fis = addmf(fis, "output", 1, 'medium_high','trapmf',c(0.55,0.65,0.8,0.9))
  fis = addmf(fis, "output", 1, 'high','trapmf',c(0.8,0.9,1,1.1))
  
  
  fis = addmf(fis, "output", 2, 'low','trapmf',c(-0.3,0,0.2,0.4))
  fis = addmf(fis, "output", 2, 'medium','trapmf',c(0.3,0.4,0.55,0.65))
  fis = addmf(fis, "output", 2, 'medium_high','trapmf',c(0.55,0.65,0.8,0.9))
  fis = addmf(fis, "output", 2, 'high','trapmf',c(0.8,0.9,1,1.1))
  
  
  fis = addmf(fis, "output", 3, 'low','trapmf',c(-0.3,0,0.2,0.4))
  fis = addmf(fis, "output", 3, 'medium','trapmf',c(0.3,0.4,0.55,0.65))
  fis = addmf(fis, "output", 3, 'medium_high','trapmf',c(0.55,0.65,0.8,0.9))
  fis = addmf(fis, "output", 3, 'high','trapmf',c(0.8,0.9,1,1.1))
  
  
  r1=c(1, 1, 1, 1, 1, 1, 1, 1)
  r2=c(1, 1, 2, 1, 1, 2, 1, 1)
  r3=c(1, 1, 3, 1, 1, 3, 1, 1)
  r4=c(1, 1, 4, 1, 1, 4, 1, 1)
  
  r5=c(1, 2, 1, 1, 2, 1, 1, 1)
  r6=c(1, 2, 2, 1, 2, 2, 1, 1)
  r7=c(1, 2, 3, 1, 2, 3, 1, 1) 
  r8=c(1, 2, 4, 1, 2, 4, 1, 1)
  
  r9=c(1, 3, 1, 1, 3, 1, 1, 1)
  r10=c(1, 3, 2, 1, 3, 2, 1, 1)
  r11=c(1, 3, 3, 1, 3, 3, 1, 1)
  r12=c(1, 3, 4, 1, 3, 4, 1, 1)
  
  r13=c(1, 4, 1, 1, 4, 1, 1, 1)
  r14=c(1, 4, 2, 1, 4, 2, 1, 1)
  r15=c(1, 4, 3, 1, 4, 3, 1, 1)
  r16=c(1, 4, 4, 1, 4, 4, 1, 1)
  
  
  r17=c(2, 1, 1, 2, 1, 1, 1, 1)
  r18=c(2, 1, 2, 2, 1, 2, 1, 1)
  r19=c(2, 1, 3, 2, 1, 3, 1, 1)
  r20=c(2, 1, 4, 2, 1, 4, 1, 1)
  
  r21=c(2, 2, 1, 2, 2, 1, 1, 1)
  r22=c(2, 2, 2, 2, 2, 2, 1, 1)
  r23=c(2, 2, 3, 2, 2, 3, 1, 1) 
  r24=c(2, 2, 4, 2, 2, 4, 1, 1)
  
  r25=c(2, 3, 1, 2, 3, 1, 1, 1)
  r26=c(2, 3, 2, 2, 3, 2, 1, 1)
  r27=c(2, 3, 3, 2, 3, 3, 1, 1)
  r28=c(2, 3, 4, 2, 3, 3, 1, 1)
  
  r29=c(2, 4, 1, 2, 4, 1, 1, 1)
  r30=c(2, 4, 2, 2, 4, 2, 1, 1)
  r31=c(2, 4, 3, 2, 4, 3, 1, 1)
  r32=c(2, 4, 4, 2, 4, 4, 1, 1)
  
  r33=c(3, 1, 1, 3, 1, 1, 1, 1)
  r34=c(3, 1, 2, 3, 1, 2, 1, 1)
  r35=c(3, 1, 3, 3, 1, 3, 1, 1)
  r36=c(4, 1, 4, 4, 1, 4, 1, 1)
  
  r37=c(3, 2, 1, 3, 2, 1, 1, 1)
  r38=c(3, 2, 2, 3, 2, 2, 1, 1)
  r39=c(3, 2, 3, 3, 2, 3, 1, 1)
  r40=c(3, 2, 4, 3, 2, 4, 1, 1)
   
  r41=c(3, 3, 1, 3, 3, 1, 1, 1)
  r42=c(3, 3, 2, 3, 3, 2, 1, 1)
  r43=c(3, 3, 3, 3, 3, 3, 1, 1)
  r44=c(3, 3, 4, 3, 3, 4, 1, 1)
  
  r45=c(3, 4, 1, 3, 4, 1, 1, 1)
  r46=c(3, 4, 2, 3, 4, 2, 1, 1)
  r47=c(3, 4, 3, 3, 4, 3, 1, 1)
  r48=c(3, 4, 4, 3, 4, 4, 1, 1)
  
  r49=c(4, 1, 1, 4, 1, 1, 1, 1)
  r50=c(4, 1, 2, 4, 1, 2, 1, 1)
  r51=c(4, 1, 3, 4, 1, 3, 1, 1)
  r52=c(4, 1, 4, 4, 1, 4, 1, 1)
  
  r53=c(4, 2, 1, 4, 2, 1, 1, 1)
  r54=c(4, 2, 2, 4, 2, 2, 1, 1)
  r55=c(4, 2, 3, 4, 2, 3, 1, 1)
  r56=c(4, 2, 4, 4, 2, 4, 1, 1)
  
  r57=c(4, 3, 1, 4, 3, 1, 1, 1)
  r58=c(4, 3, 2, 4, 3, 2, 1, 1)
  r59=c(4, 3, 3, 4, 3, 3, 1, 1)
  r60=c(4, 3, 4, 4, 3, 4, 1, 1)
  
  r61=c(4, 4, 1, 4, 4, 1, 1, 1)
  r62=c(4, 4, 2, 4, 4, 2, 1, 1)
  r63=c(4, 4, 3, 4, 4, 3, 1, 1)
  r64=c(4, 4, 4, 4, 4, 4, 1, 1)
  
  rules=rbind(r1,r2,r3,r4,r5,r6,r7,r8,r9,r10,r11,r12,r13,r14,r15,r16,r17,r18,r19,r20,r21,r22,r23,r24,r25,r26,r27, r28, r29, r30, r31, r32,
  r33, r34,r35, r36, r37, r38, r39, r40, r41, r42, r43, r44,
  r45, r46, r47, r48, r49, r50, r51, r52, r53, r54,
  r55, r56, r57, r58, r59, r60, r61, r62, r63, r64)
  fis= addrule(fis, rules)
  return(fis)
}




