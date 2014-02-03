import java.io._
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.mutable.ParArray

val directoryStr = System.getProperty("user.home") + "/Documents/MNSprojectResults/"
val directory = new java.io.File(directoryStr)
val files = directory.listFiles

//this next one is really ugly, so let's get some type checking so we know what we're dealing with.
val results = files.par.map(file => readFromFile(file.toString())).toList: List[(List[(Double, Double)], List[(Double, Double)], Double, Double, Int)]


def readFromFile(fileStr: String): (List[(Double, Double)], List[(Double, Double)], Double, Double, Int) = 
{
  val localFileName = fileStr.split("/").last
  val filepropertiesStrs = localFileName.split("_")
  val rEE = filepropertiesStrs(1).split(":")(1).toDouble
  val duration = filepropertiesStrs(2).split(":")(1).toDouble
  val numOfNeurons = filepropertiesStrs(3).split(":")(1).toInt
  
  
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
  
  val excitatorySpikes = excitatorySpikeStrs.par.map(spikeStr => getSpike(spikeStr)).toList
  val inhibitorySpikes = inhibitorySpikeStrs.par.map(spikeStr => getSpike(spikeStr)).toList
  
  (excitatorySpikes, inhibitorySpikes, rEE, duration, numOfNeurons)
}

val timeChunk = 0.1
val fanofactors = results.map(res => averageFanoFactor((res._1, res._2), timeChunk, res._4, res._5))
println(fanofactors)

def fanofactorsForSingleSpikeTrain(spiketrain: List[(Double, Double)], timeChunk: Double, duration: Double, numOfNeurons: Int): List[Double] =
{
  var neuronspikes = Array.fill[ArrayBuffer[Double]](numOfNeurons)(ArrayBuffer[Double]())
  for (spike <- spiketrain)
  {
    neuronspikes((spike._1).toInt) += (spike._2).toDouble
  }
  val factors = neuronspikes.par.map(neuron => fanofactorOfNeuron(neuron.toList, timeChunk, duration)).toList
  return factors.toList
}

def fanofactorOfNeuron(spikeTimesForNeuron: List[Double], timeChunkSize: Double, duration: Double): Double =
{
  val numOfChunks = (duration.toDouble/timeChunkSize.toDouble).toInt
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

def fanofactors(spiketrains: (List[(Double, Double)], List[(Double, Double)]), timeChunk: Double, duration: Double, numOfNeurons: Int): List[Double] =
{
  val factors = fanofactorsForSingleSpikeTrain(spiketrains._1, timeChunk, duration, numOfNeurons) ++ fanofactorsForSingleSpikeTrain(spiketrains._2, timeChunk, duration, numOfNeurons)
  return factors.toList
}

def averageFanoFactor(spiketrains: (List[(Double, Double)], List[(Double, Double)]), timeChunk: Double, duration: Double, numOfNeurons: Int): Double =
{
  val fanoFactorForNeurons = fanofactors(spiketrains, timeChunk, duration, numOfNeurons)
  return fanoFactorForNeurons.sum/fanoFactorForNeurons.length
}