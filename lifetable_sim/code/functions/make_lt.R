make_lt <- function(ages, last_age, mx){
  lt <- tibble(x = ages, n = rep(1, length(ages)), Mx = mx)
  lt <- lt %>% 
    mutate(
      ax = case_when(
        x==0 ~ 0.07 + 1.7*Mx,
        #x==110 ~ 1/Mx, # this was causing issues with zeroes
        TRUE ~ n/2
      ),
      qx = n * Mx / (1 + (n - ax)* Mx),
      px = 1 - qx,
      lx = lag(cumprod(px), default = 1),
      dx = lx - lead(lx, default = 0),
      Lx = n * lead(lx, default = 0) + (ax* dx),
      Tx = rev(cumsum(rev(Lx))),
      ex = Tx / lx
    ) 
  return(lt)
}