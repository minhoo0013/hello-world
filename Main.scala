import scala.math._

object Main extends App {


def fectorial(x:Int):Int = {
    var num: Int = 1
    if(x < 2)
      num =1
    else
      for(i <-1 to x){
        num *= i
      }
    return num

}
//Error function(approximate)
def erf(x:Double): Double = {
  val t = 1/(1+0.5*abs(x))
  var tau = t*exp(-pow(x,2)-1.26551223 + 1.0000002368*t + 0.37409196*t*t + 0.09678418*pow(t,3) - 0.18628806*pow(t,4) + 0.27886807*pow(t,5) - 1.13520398*pow(t,6) + 1.48851587*pow(t,7) - 0.82215233*pow(t,8) + 0.17087277*pow(t,9))
  
  if(x>=0)
    return 1-tau
  else
    return tau-1  
}

//distribution
def normCdf(x:Double):Double = {
    
    return (1+erf(x/sqrt(2)))/2
}
def normPdf(x:Double):Double = {
    return exp(-pow(x,2))/sqrt(2*Pi)
}


val CALL:Int = 1
val PUT:Int = -1


//market information
class MarketInf(underPrice:Double,rate:Double,div:Double,vol:Double){
  var underPrice_ = underPrice
  var rate_ = rate
  var div_ = div
  var vol_ = vol
}

case class EOption(strike:Double,t:Double,opType:Int,bid:Double,ask:Double,market:MarketInf){
  var s_ = market.underPrice_
  var r = market.rate_
  var d_ = market.div_
  var sigma_ = market.vol_
  var d1_ = ( log(1/(strike/s_)) + (r-d_ + pow(sigma_,2)/2)*t ) / ( sigma_ * sqrt(t) )
  var d2_ = d1_ - sigma_ * sqrt(t)


  def price(): Double = {
      return opType*(s_ * normCdf(opType*d1_)*exp(-t*d_) - strike*exp(-t*r)*normCdf(opType*d2_))
  }
  def vega(): Double = {
      return normPdf(d1_)*sqrt(t)*s_
  }
  def calculateImpliedVol(mkPrice:Double,init:Double = 0.1,tol:Double = 0.001):Double = {
  var imVol = init
  var diff: Double = 1
  var e:Double = 1
  //optim using newton method
  while(e>tol){
    d1_ = ( log(1/(strike/s_)) + (r-d_ + pow(imVol,2)/2)*t ) / ( imVol * sqrt(t) )
    d2_ = d1_ - imVol * sqrt(t)
    diff = price() - mkPrice
    e = abs(diff)
    imVol = imVol - diff/vega()
  }
    
  return imVol

}
    
  def bidVol():Double = calculateImpliedVol(bid)
  def askvol():Double = calculateImpliedVol(ask)

}
val market1 = new MarketInf(300,rate = 0.03,div = 0.02,0.3)

val calls = List(EOption(295,0.5,CALL,4.18,4.2,market1),EOption(297.5,0.5,CALL,1.85,1.87,market1),EOption(300,0.5,CALL,0.38,0.39,market1))//,EOption(302.5,0.5,CALL,0.03,0.04,market1))

val puts = List(EOption(295,0.5,PUT,0.03,0.04,market1),EOption(297.5,0.5,PUT,0.19,0.2,market1),EOption(300,0.5,PUT,1.22,1.24,market1),EOption(302.5,0.5,PUT,3.36,3.38,market1))

var result = calls.map(x => x.bidVol())
result.foreach{x => println(x)}
}
