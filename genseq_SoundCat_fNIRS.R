# Generate sequence for sound categorization task in fNIRS neurofeedback
# written by Enoch 20181219

stiminfo = read.csv("stim_info.csv")


# check all stim types are present in a specified window
checkwindow = function(x, n){
  stopifnot(length(x)>=n)
  stopifnot(n>=4)
  
  good = TRUE
  for (i in 1:(length(x)-n)){
    k = x[i:(i+n-1)]
    good = (good && length(unique(k))==4)
  }
  return(good)
}

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

# ---------------------------- generate sequence ---------------------
x = rep(1:4,2)
finished = FALSE
c = 1
while (!finished) {
  print(c)
  c = c + 1
  pre = vector("numeric")
  s = vector("numeric")
  
  for(i in 1:5){
    temp = sample(x)
    gap = append(pre,temp)
    done = FALSE
    
    while(!done){
      temp = sample(x)
      gap = append(pre,temp)
      done = checkseq(gap,2)
    }
    
    s = append(s,temp)
    pre = temp
  }
  
  finished = checkwindow(s,6)
  
}

# ------------------------- recode seq and map to stim ----------------
library(dplyr)
one = stiminfo %>% filter(category==1)
two = stiminfo %>% filter(category==2)
three = stiminfo %>% filter(category==3)
four = stiminfo %>% filter(category==4)

t1 = sample(1:10)
t2 = sample(1:10)
t3 = sample(1:10)
t4 = sample(1:10)

output = data.frame(cat = s) %>% group_by(cat) %>% mutate(cnt = row_number())
output = output %>% mutate(sound = case_when(cat == 1 ~ one$sound[t1][cnt],
                                            cat == 2 ~ two$sound[t2][cnt],
                                            cat == 3 ~ three$sound[t3][cnt],
                                            cat == 4 ~ four$sound[t4][cnt]))

output = output %>% mutate(temp = case_when(cat == 1 ~ one$temp[t1][cnt],
                                            cat == 2 ~ two$temp[t2][cnt],
                                            cat == 3 ~ three$temp[t3][cnt],
                                            cat == 4 ~ four$temp[t4][cnt]))

output = output %>% mutate(spec = case_when(cat == 1 ~ one$spec[t1][cnt],
                                            cat == 2 ~ two$spec[t2][cnt],
                                            cat == 3 ~ three$spec[t3][cnt],
                                            cat == 4 ~ four$spec[t4][cnt]))


write.csv(output,'seq3.csv',row.names=FALSE, quote = FALSE)

