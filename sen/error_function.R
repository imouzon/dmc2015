# error function 
# 1 means ignore the basket value for a second; 2 means the true error defined

err1 = function(raw_d, pre_d){
  a1 = (raw_d$c1Used - pre_d$c1Prediction)^2
  b1 = sum(raw_d$c1Used)/length(raw_d$c1Used)
  e1 = sum(a1)/b1/b1
  a2 = (raw_d$c2Used - pre_d$c2Prediction)^2
  b2 = sum(raw_d$c2Used)/length(raw_d$c2Used)
  e2 = sum(a2)/b2/b2
  a3 = (raw_d$c3Used - pre_d$c3Prediction)^2
  b3 = sum(raw_d$c3Used)/length(raw_d$c3Used)
  e3 = sum(a3)/b3/b3
  err1 = e1 + e2 + e3
  }
  

err1 = function(raw_d, pre_d){
  a1 = (raw_d$c1Used - pre_d$c1Prediction)^2
  b1 = sum(raw_d$c1Used)/length(raw_d$c1Used)
  e1 = sum(a1)/b1/b1
  a2 = (raw_d$c2Used - pre_d$c2Prediction)^2
  b2 = sum(raw_d$c2Used)/length(raw_d$c2Used)
  e2 = sum(a2)/b2/b2
  a3 = (raw_d$c3Used - pre_d$c3Prediction)^2
  b3 = sum(raw_d$c3Used)/length(raw_d$c3Used)
  e3 = sum(a3)/b3/b3
  a4 = (raw_d$basketValue - pre_d$basketValuePre)^2
  b4 = sum(raw_d$basketValue)/length(raw_d$basketValue)
  e4 = sum(a4)/b4/b4
  
  
  err2 = e1 + e2 + e3 +e4
}

