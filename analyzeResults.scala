import java.io._
import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.parallel.mutable.ParArray

val sdirectoryStr = System.getProperty("user.home") + "/Documents/MNSprojectResults/s/"
val sdirectory = new java.io.File(sdirectoryStr)
val sfiles = sdirectory.listFiles

val bdirectoryStr = System.getProperty("user.home") + "/Documents/MNSprojectResults/b/"
val bdirectory = new java.io.File(bdirectoryStr)
val bfiles = bdirectory.listFiles

//this next one is really ugly, so let's get some type checking so we know what we're dealing with.
val sresults = sfiles.par.map(file => sReadFromFile(file.toString())).toList: List[(List[(Int, Double)], List[(Int, Double)], Double, Double, Int)]
val bresults = bfiles.par.map(file => bReadFromFile(file.toString())).toList: List[(List[(Int, Double)], List[(Int, Double)], Double, Double, Int)]

val results = sresults ++ bresults 

def sReadFromFile(fileStr: String): (List[(Int, Double)], List[(Int, Double)], Double, Double, Int) = 
{
  val localFileName = fileStr.split("/").last
  val filepropertiesStrs = localFileName.split("_")
  val rEE = filepropertiesStrs(1).split(":")(1).toDouble
  val duration = filepropertiesStrs(2).split(":")(1).toDouble
  val numOfNeurons = filepropertiesStrs(3).split(":")(1).toInt
  
  
  val file = Source.fromFile(fileStr).getLines.toArray
  val excitatorySpikeStrs = file(0).slice(1, file(0).length-1).split("\\)")
  val inhibitorySpikeStrs = file(1).slice(1, file(1).length-1).split("\\)")
  
  def getSpike(s: String): (Int, Double) =
  {
    var timeUnit = "rsthsrtxjdytk7utirbsrh"
    var mult = 0.0
    if (s.contains(" ms")) {timeUnit = " ms"; mult = 1.0/1000}
    if (s.contains(" s")) {timeUnit = " s"; mult = 1.0}
    val shrinked = s.slice(s.indexOf("(")+1, s.indexOf(timeUnit)-1)
    val neuron = shrinked.split(", ")(0).toInt
    val time = shrinked.split(", ")(1).toDouble * mult
    (neuron, time)
  }
  
  val excitatorySpikes = excitatorySpikeStrs.par.map(spikeStr => getSpike(spikeStr)).toList
  val inhibitorySpikes = inhibitorySpikeStrs.par.map(spikeStr => getSpike(spikeStr)).toList
  
  (excitatorySpikes, inhibitorySpikes, rEE, duration, numOfNeurons)
}

def bReadFromFile(fileStr: String): (List[(Int, Double)], List[(Int, Double)], Double, Double, Int) = 
{
  val localFileName = fileStr.split("/").last
  val filepropertiesStrs = localFileName.split("_")
  val rEE = filepropertiesStrs(1).split(":")(1).toDouble
  val duration = filepropertiesStrs(2).split(":")(1).toDouble
  val N_e = filepropertiesStrs(3).split(":")(1).toInt
  val N_i = filepropertiesStrs(4).split(":")(1).toInt
  val len = filepropertiesStrs(5).split(":")(1).toInt
  
  
  val file = new File(fileStr)
  val fstream = new FileInputStream(file)
  val bstream = new BufferedInputStream(fstream)
  val dstream = new DataInputStream(bstream)
  
  var spi = ArrayBuffer[(Int, Double)]()
  
  var newint = 0
  var newdouble = 0.0
  for (i <- 0 until len)
  {
    newint = dstream.readInt
    newdouble = dstream.readDouble
    spi += ((newint, newdouble))
  }
  val spikes = spi.toList: List[(Int, Double)]
  
  var excitatorySpikesB = ArrayBuffer[(Int, Double)]()
  var inhibitorySpikesB = ArrayBuffer[(Int, Double)]()
  
  for (spike <- spikes)
  {
    if (spike._1 < 4000)
    {
      excitatorySpikesB += ((spike._1, spike._2))
    }
    else
    {
      inhibitorySpikesB += ((spike._1-4000, spike._2))
    }
  }
  
  val excitatorySpikes = excitatorySpikesB.toList
  val inhibitorySpikes = inhibitorySpikesB.toList
  
  (excitatorySpikes, inhibitorySpikes, rEE, duration, N_e+N_i)
}

val timeChunk = 0.1

val fanofactors = results.par.map(res => averageFanoFactor((res._1, res._2), res._3, timeChunk, res._4, res._5)).toList
val sortedFanofactors = fanofactors.sortWith((x, y) => x._1 < y._1)
println(sortedFanofactors)

/*val res = results.last
val fanofactors = averageFanoFactor((res._1, res._2), res._3, timeChunk, res._4, res._5)*/

def fanofactorsForSingleSpikeTrain(spiketrain: List[(Int, Double)], timeChunk: Double, duration: Double, numOfNeurons: Int): List[Double] =
{
  var neuronspikes = Array.fill[ArrayBuffer[Double]](numOfNeurons)(ArrayBuffer[Double]())
  for (spike <- spiketrain)
  {
    neuronspikes(spike._1) += (spike._2).toDouble
  }
  /*for (i <- 0 until neuronspikes.length)
  {
    println(i.toString + neuronspikes(i).toString)
  }*/
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

def fanofactors(spiketrains: (List[(Int, Double)], List[(Int, Double)]), timeChunk: Double, duration: Double, numOfNeurons: Int): List[Double] =
{
  val factors = fanofactorsForSingleSpikeTrain(spiketrains._1, timeChunk, duration, numOfNeurons/5*4) ++ fanofactorsForSingleSpikeTrain(spiketrains._2, timeChunk, duration, numOfNeurons/5*1)
  /*println("")
  println(factors.toString)
  println("")*/
  return factors.toList
}

def averageFanoFactor(spiketrains: (List[(Int, Double)], List[(Int, Double)]), rEE: Double, timeChunk: Double, duration: Double, numOfNeurons: Int): (Double, Double) =
{
  val fanoFactorForNeurons = fanofactors(spiketrains, timeChunk, duration, numOfNeurons)
  val averageFanofactor = fanoFactorForNeurons.sum/fanoFactorForNeurons.length
  return (rEE, averageFanofactor)
}