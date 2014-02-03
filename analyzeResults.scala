import java.io._
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.mutable.ParArray

val directoryStr = System.getProperty("user.home") + "/Documents/MNSprojectResults/"
val directory = new java.io.File(directoryStr)
val files = directory.listFiles

//this next one is really ugly, so let's get some type checking so we know what we're dealing with.
val results = files.par.map(file => readFromFile(file.toString())): ParArray[(ParArray[(Double, Double)], ParArray[(Double, Double)])]

def readFromFile(fileStr: String): (ParArray[(Double, Double)], ParArray[(Double, Double)]) = 
{
  val file = Source.fromFile(fileStr).getLines.toArray
  val excitatorySpikeStrs = file(0).slice(1, file(0).length-1).split("\\)")
  val inhibitorySpikeStrs = file(1).slice(1, file(1).length-1).split("\\)")
  
  def getSpike(s: String): (Double, Double) =
  {
    var timeUnit = "rsthsrtxjdytk7utirbsrh"
    var mult = 0.0
    if (s.contains(" ms")) {timeUnit = " ms"; mult = 1.0/1000}
    if (s.contains(" s")) {timeUnit = " s"; mult = 1.0}
    val shrinked = s.slice(s.indexOf("(")+1, s.indexOf(timeUnit)-1)
    val neuron = shrinked.split(", ")(0).toDouble
    val time = shrinked.split(", ")(1).toDouble * mult
    (neuron, time)
  }
  
  val excitatorySpikes = excitatorySpikeStrs.par.map(spikeStr => getSpike(spikeStr))
  val inhibitorySpikes = inhibitorySpikeStrs.par.map(spikeStr => getSpike(spikeStr))
  
  (excitatorySpikes, inhibitorySpikes)
}

val fanofactors = results.map(spiketrains => averageFanoFactor(spiketrains, 0.1, 0.1, 5000))
println(fanofactors)

def fanofactorsForSingleSpikeTrain(spiketrain: ParArray[(Double, Double)], timeChunk: Double, duration: Double, numOfNeurons: Int): List[Double] =
{
  var factors = ArrayBuffer[Double]()
  var neuronspikes = ArrayBuffer[ArrayBuffer[Double]]()
  for (i <- 0 until numOfNeurons)
  {
    neuronspikes += ArrayBuffer[Double]()
  }
  for (spike <- spiketrain)
  {
    neuronspikes((spike._1).toInt) += (spike._2).toDouble
  }
  /*for (neuron <- neuronspikes)
  {
    factors.append(fanofactorOfNeuron(neuron, timeChunk, duration))
  }*/
  factors = neuronspikes.map(neuron => fanofactorOfNeuron(neuron.toList, timeChunk, duration))
  return factors.toList
}

def fanofactorOfNeuron(spikeTimesForNeuron: List[Double], timeChunkSize: Double, duration: Double): Double =
{
  val numOfChunks = ((1000*duration).toInt/(1000*timeChunkSize)).toInt
  var numOfSpikesInChunk = Array.fill[Int](numOfChunks)(0)
  
  for (spiketime <- spikeTimesForNeuron)
  {
    val chunk = (spiketime/timeChunkSize).toInt
    numOfSpikesInChunk(chunk) = numOfSpikesInChunk(chunk) + 1
  }
  val mean = numOfSpikesInChunk.sum.toDouble/numOfSpikesInChunk.length
  var preVariance = 0.0
  for (numOfSpikes <- numOfSpikesInChunk)
  {
    preVariance = preVariance + math.pow((numOfSpikes-mean), 2)
  }
  val variance = preVariance/numOfSpikesInChunk.length
  var factorForThisNeuron = 0.0
  if (math.pow((mean - 0.0), 2) > 0.01)
  {
    factorForThisNeuron = variance/mean
  }
  return factorForThisNeuron
}

def fanofactors(spiketrains: (ParArray[(Double, Double)], ParArray[(Double, Double)]), timeChunk: Double, duration: Double, numOfNeurons: Int): List[Double] =
{
  val factors = fanofactorsForSingleSpikeTrain(spiketrains._1, timeChunk, duration, numOfNeurons) ++ fanofactorsForSingleSpikeTrain(spiketrains._2, timeChunk, duration, numOfNeurons)
  return factors.toList
}

def averageFanoFactor(spiketrains: (ParArray[(Double, Double)], ParArray[(Double, Double)]), timeChunk: Double, duration: Double, numOfNeurons: Int): Double =
{
  val fanoFactorForNeurons = fanofactors(spiketrains, timeChunk, duration, numOfNeurons)
  return fanoFactorForNeurons.sum/fanoFactorForNeurons.length
}