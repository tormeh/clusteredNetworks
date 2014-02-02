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
    var firstNumberStr = ""
    var secondNumberStr = ""
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



/*def fanofactorsForSingleMonitor(spike_mon, timeChunk, duration, numOfNeurons):
    factors = []
    neuronspikes = []
    for i in range(numOfNeurons):
        neuronspikes.append([])
    for spike in spike_mon.spikes:
        neuronspikes[int(spike[0])].append(float(spike[1]))
    for neuron in neuronspikes:
        factors.append(fanofactorOfNeuron(neuron, timeChunk, duration))
    return factors

def fanofactorOfNeuron(spikeTimesForNeuron, timeChunkSize, duration):
    numOfSpikesInChunk = [0] * (int(1000*duration)/int(1000*timeChunkSize))
    #print(len(numOfSpikesInChunk))
    for spiketime in spikeTimesForNeuron:
        chunk = int((spiketime)/timeChunkSize)
        numOfSpikesInChunk[chunk] = numOfSpikesInChunk[chunk] + 1
    mean = float(sum(numOfSpikesInChunk))/len(numOfSpikesInChunk)
    preVariance = 0
    for numOfSpikes in numOfSpikesInChunk:
        preVariance = preVariance + (numOfSpikes-mean)**2
    variance = preVariance/len(numOfSpikesInChunk)
    if (mean - 0.0)**2 > 0.01:
        factorForThisNeuron = variance/mean
    else:
        factorForThisNeuron = 0
    return factorForThisNeuron

def fanofactors(spike_mons, timeChunk, duration, numOfNeurons):
    factors = []
    for spike_mon in spike_mons:
        factors.extend(fanofactorsForSingleMonitor(spike_mon, timeChunk, duration, numOfNeurons))
    return factors

def averageFanoFactor(spike_mons, timeChunk, duration, numOfNeurons):
    fanoFactorForNeurons = fanofactors(spike_mons, timeChunk, duration, numOfNeurons)
    return float(sum(fanoFactorForNeurons)/len(fanoFactorForNeurons))*/