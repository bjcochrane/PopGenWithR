ggcolhist <-function (data,tail=2){
  dat <-data.frame(nums=data)
  if(tail==1) 
    qt <- c(0,quantile(dat$nums,c(.95)))
  if(tail==2)
    qt <-quantile(dat$nums,c(.025,.975))
  
  bt=diff(range(dat$nums))/30
  hs <- ggplot(dat,aes(nums)) +
    geom_histogram(binwidth=bt,fill="red",color="black") +
    geom_histogram(data=subset(dat,nums>qt[1]&nums<qt[2]),binwidth=bt,colour="black", fill="white") 
  return(hs)
}
