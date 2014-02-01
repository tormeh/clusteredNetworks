import scala.sys.process._
val rEEs = 1.0 until 4.0 by 0.2

def run(Ree: Double) = Process("python clusteredNetwork.py -rEE " + Ree.toString).run()

rEEs.par.map(rEE => run(rEE))

