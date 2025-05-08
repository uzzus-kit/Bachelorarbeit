interval_score=function(targets, alpha, quantil_l,quantil_r){
  sharpness=quantil_r-quantil_l
  IS=((pmin(pmax(0,quantil_l-targets),max_value))
      +pmin(pmax(0,targets-quantil_r),max_value)*2/alpha)
  total=sharpness+IS
  return(sharpness,IS,total)
}
