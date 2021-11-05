## extract file
df <- read.csv("treasurynotes12311993.csv")

## convert maturity to standard date format
df <- transform(df, "Maturity" = as.Date(as.character(df[,1]), "%Y%m%d"))

## calculate ttm = years to maturity 
df$ttm <- (as.numeric(df[,1])-as.numeric(as.Date("1993-12-31")))/365

##determine coupon.no = number of coupon payments
df$coupon.no <- floor((df[,5]-0.01)*2)+1

##set parameters and ensm function to calculate discount factor Z(0,T)
p <- c(0.0687, -0.0422, -0.2399, 0.2116, 0.9652, 0.8825)
z.ensm <- function(ttm) {
  ifelse(ttm==0, 0, exp(-(p[1] + (p[2]+p[3])*(1-exp(-ttm/p[5]))/(ttm/p[5]) - p[3]*exp(-ttm/p[5]) + p[4]*( (1-exp(-ttm/p[6]))/(ttm/p[6]) - exp(-ttm/p[6])))*ttm))
}

#set a function to calculate pv of coupon payments
coupon.price <- function(ttm1, c1, n) {
  
  result <- 0
  for(k in n) {
    l <- ttm1 - 0.5*(k-1)
    result <- result + (c1/2)*z.ensm(l)
    if(l < 0.5) {
      break
    }
  }
  return(result)
}

#create output = price table

price.list <- matrix(0, 224,5, dimnames = list(1:224, c("coupon.pv", "par.pv", "price.model", "price.gross", "price.diff")))

for (i in 1:224) {
  price.list[i,1] <- coupon.price(df[i,5], df[i,2], c(1:df[i,6]))
  price.list[i,2] <- 100*z.ensm(df[i,5])
  price.list[i,3] <- price.list[i,1]+price.list[i,2]
  price.list[i,4] <- df[i,3] + (1 - (df[i,5]*2 - floor(df[i,5]*2)))*df[i,2]/2
  price.list[i,5] <- price.list[i,4] - price.list[i,3]
}
