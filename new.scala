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

//option features
case class EOption(strike:Double,t:Double,opType:Int,bid:Double,ask:Double,market:MarketInf){
  var s_ = market.underPrice_
  var r = market.rate_
  var d_ = market.div_
  var sigma_ = market.vol_
  var d1_ = ( log(1/(strike/s_)) + (r-d_ + pow(sigma_,2)/2)*t ) / ( sigma_ * sqrt(t) )
  var d2_ = d1_ - sigma_ * sqrt(t)
  var strike_ = strike
  def price(): Double = {
      return opType*(s_ * normCdf(opType*d1_)*exp(-t*d_) - strike*exp(-t*r)*normCdf(opType*d2_))
  }
  def vega(): Double = {
      return normPdf(d1_)*sqrt(t)*s_
  }

  def findImVol(mkPrice:Double,init:Double = 0.2,tol:Double = 0.0001):Double = {
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
  def bidVol:Double = findImVol(bid)
  def askVol:Double = findImVol(ask)

}

val market1 = new MarketInf(300,rate = 0.03,div = 0.02,0.3)

//result

val calls = List(EOption(295,0.02,CALL,6.07,6.12,market1),EOption(297.5,0.02,PUT,4.49,4.45,market1),EOption(300,0.02,PUT,3.18,3.20,market1),EOption(302.5,0.02,CALL,2.2,2.21,market1))

val puts = List(EOption(295,0.02,PUT,2.14,2.15,market1),EOption(297.5,0.02,PUT,3.04,3.05,market1),EOption(300,0.02,PUT,4.23,4.25,market1),EOption(302.5,0.02,PUT,5.72,5.76,market1))

calls.foreach{call => println(call.strike_ + " calls imVol = " + call.bidVol + " | " + call.askVol)}
puts.foreach{put => println(put.strike_ + " puts bid ask = " + put.bidVol + " | " + put.askVol)}

}
