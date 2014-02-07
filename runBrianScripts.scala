import scala.sys.process._

def run(Ree: Double) = Process("python clusteredNetwork.py -rEE " + Ree.toString).run().exitValue()

/*val inter = 0.01
val rep = 10.0
for (i <- 0.0 until inter by inter/rep)
{
  println("Start iter" + i.toString)
  ((1.0+i-inter+inter/rep) until (4.0+i-inter+inter/rep) by inter).par.map(rEE => run(rEE))
}*/


(2.5 until 4.0 by 0.015).par.map(rEE => run(rEE))
