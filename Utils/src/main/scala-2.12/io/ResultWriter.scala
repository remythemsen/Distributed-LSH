package io

import java.io.{BufferedWriter, File, FileWriter}

class ResultWriter(outPath:String, testType:String, legend:String = "") {
  val time = new java.text.SimpleDateFormat("MM-dd_HH.mm").format(new java.util.Date)
  // Create file
  val file = new File(outPath+"/"+testType+"-"+time+".txt")

  // Write Initial information to the file.
  val bw = new BufferedWriter(new FileWriter(file))
  bw.write(getSystemInfo)
  if(!legend.equals("")) bw.write(legend+"\n")
  bw.close()

  def writeResult(line:String) : Unit = {
    val bw = new BufferedWriter(new FileWriter(file,true))
    bw.append(line+"\n")
    bw.close()
  }

  def getSystemInfo : String = {
    val sb = new StringBuilder
    sb.append("OS: ")
    sb.append(System.getProperty("os.name")+", ")
    sb.append(System.getProperty("os.version")+", ")
    sb.append(System.getProperty("os.arch")+"\n")
    sb.append("JVM: ")
    sb.append(System.getProperty("java.vendor")+", ")
    sb.append(System.getProperty("java.version")+"\n")
    sb.append("CPU: ")
    sb.append(System.getenv("PROCESSOR_IDENTIFIER")+", ")
    sb.append("Cores: "+Runtime.getRuntime.availableProcessors()+"\n")
    sb.append("\n")
    sb.toString
  }
}
