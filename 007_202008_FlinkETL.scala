package sample

import org.apache.flink.api.scala._
import org.apache.flink.streaming.api.scala.StreamExecutionEnvironment
import org.apache.flink.streaming.api.windowing.time.Time

object sample {

  def main(args:Array[String]):Unit = {
    println("hello")

    // 流处理word count
    val env = StreamExecutionEnvironment.getExecutionEnvironment
    val textStream = env.socketTextStream("localhost",9000,'\n')

    val windowWordCount = textStream
                          .flatMap(line => line.split("\\s"))
                          .map(word => (word,1))
                          .keyBy(0)
                          .timeWindow(Time.seconds(5))
                          .sum(1)

    windowWordCount.print().setParallelism(1)
    env.execute("Socket Window WordCount")
  }
}
