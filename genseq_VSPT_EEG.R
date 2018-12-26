###################################################################### 
# Generate new pseudorandom sequence for VSPT EEG
# v2c written by Enoch 20181219
# Press "source" to run the whole program!
# The generated sequence is stored in the dataframe called "output"
# The "output" is then recoded nicely to "output.nice"
######################################################################

#custom function to check whether there is n or less consecutive duplicates in vector x
checkseq = function(x, n){
  stopifnot(length(x)>=n)
  stopifnot(n>=1)
  
  good = TRUE
  for (i in 1:(length(x)-n)){
    temp = TRUE
    for(j in 1:n){
      temp = temp & (x[i]==x[i+j])
    }
    if (temp)
      good = FALSE
  }
  return(good)
}

#custom function for generating a shortest sequence
#x is the vector to be randomized
genseq = function (x){
  #create sequence for each condition
  num = length(x)
  a=sample(x,num,FALSE) 
  b=sample(x,num,FALSE)
  c=sample(x,num,FALSE)
  d=sample(x,num,FALSE)
  #convert four sequences to one
  k = vector("numeric")
  for (i in 1:num) {
    k = append(k,a[i])
    k = append(k,b[i])
    k = append(k,c[i])
    k = append(k,d[i])
  }
  return(k)
}

# custom function for counting the consecutive repetitions of a vector 
checkrep = function (x){
  c = vector("numeric")
  c = append(c,0)
  p=0
  for(i in 2:length(x)){
    if(x[i]==x[i-1]){
      p = p+1
      c = append(c,p)
    }
    else{
      c = append(c,0)
      p=0
    }
    
  }
  return(c)
}

# function to generate the matrix to be shuffled
setseq = function(n){
  a = c(TRUE,TRUE,FALSE,FALSE)
  b = c(TRUE,FALSE,TRUE,FALSE)
  c = !xor(a,b)
  m = matrix(rep(c(a,b,c),n),nrow = 8,ncol=3)
  return(m)
}

# -------------------- settings -----------------
# per: W = True, S = False
# stim: L = True, R = False
# resp: L = True, R = False
makemagic = function(haha, returntype){
m = setseq(8)
n = nrow(m)

minlen = 8
len=32  #controls how long the total sequence is & must be a multiple of minlen!
per_consec = 4 #max allowed same consecutive answers
stim_consec=4
ans_consec=4 

# generate long sequence by fragments---------------
finished = FALSE
c = 1
while(!finished){
  
  print(c)
  c=c+1
  pre = matrix(NA, nrow = 0, ncol = 3)
  s = matrix(NA, nrow = 0, ncol = 3)
  
  for (i in 1:(len/minlen)){
    temp = m[sample(n),]
    gap = rbind(pre,temp)
    
    #print(paste("generating fragment",i,"-----------"))
    count = 1
    done = FALSE
    while(done==FALSE){
      count = count+1
      #print(paste("fragment:",i,"cycle:",count))
      temp = m[sample(n),]
      gap = rbind(pre,temp)
      done = checkseq(gap[,1],per_consec)&&checkseq(gap[,2],stim_consec)&&checkseq(gap[,3],ans_consec)
    }
    s=rbind(s,temp)
    pre=temp
  }
  
  # recoding
  output = data.frame(per = ifelse(s[,1],"W","S")
                      , stim=ifelse(s[,2], "L","R")
                      , ans=ifelse(s[,3], "L","R"))
  
  # checking
  output$countper = checkrep(output$per)
  output$countstim = checkrep(output$stim)
  output$countans = checkrep(output$ans)
  output$persw = ifelse(output$countper==0,"Sh","Rp")
  output$anssw = ifelse(output$countans==0,"Sh","Rp")
  output$persw[1]="X"
  output$anssw[1]="X"
  
  x = table(output$persw)
  y = table(output$anssw)
  
  if (abs(x[1]-x[2])==1 && abs(y[1]-y[2])==1){
    z = table(paste0(output$per,output$persw),paste0(output$ans,output$anssw))
    z = z[c("SRp","SSh","WRp","WSh"),c("LRp","LSh","RRp","RSh")]
    
    if(length(which(z==0))==0 && length(which(z==3))==0 && length(which(z==4))==0 && length(which(z==5))==0 && length(which(z==6))==0){
      finished = TRUE
    }
    
  }
  
  
}# end of while loop

# making a nicer output after things are done
output.nice = data.frame(cond = paste0(output$per,output$persw),
                         stim = output$stim,
                         ans = output$ans,
                         ans_rep = paste0(output$ans,output$anssw))

# code the first item in output.nice
j = which(z==1,arr.ind=TRUE)
output.nice$cond[1]=rownames(z)[j[1]]
output.nice$ans_rep[1]=colnames(z)[j[2]]

# return different output depending on need
if (returntype=="count"){
  # save data to working directory
  save(output,output.nice,file = paste0(haha,output.nice$cond[1],output.nice$ans_rep[1],".RData"))
  return(c)
}
else if (returntype=="firstrow"){
  save(output,output.nice,file = "found.RData")
  return(paste0(output.nice$cond[1],output.nice$ans_rep[1]))
}
  

} # end of function

# # making sequences with for loop
# runs = vector("numeric")
# for (y in 41:50 ){
#   runs= append(runs,makemagic(y,"count"))
#   
# }

# find specific seq
found = FALSE
while(!found){
  x = makemagic(0,"firstrow")
  print(paste("------",x,"------"))
  found = ("WShLSh"==x)
}


# if something is messed up then run again
#print("oop we need to run again ......................")
#source("genseq_VSPT_EEG.R")


# double checking ----------------
#table(paste0(output$per,output$persw),paste0(output$ans,output$anssw))
#table(paste0(output$per,output$stim,output$ans))
#table(output$per,paste0(output$ans,output$countans))
#table(output.nice$cond,output.nice$ans_rep)