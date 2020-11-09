package MavenTest.FirstMaven

/* 
 * env：eclipse 2020-03, scala v2.11.11, spark v2.4.4, maven 3.6.3, java 1.8, win 7 64bit passed
 * SparkMissHandle.scala
 */

import java.io._
import java.io.FileSystem
import java.io.PrintWriter
import scala.reflect.io.Directory
import java.util
import java.io.FileWriter
import org.apache.commons.lang3.StringUtils
import scala.io._
import scala.util.{Try, Success, Failure}
import scala.util.control.Breaks.{break, breakable}
import org.apache.log4j.{Level, Logger}
import org.apache.spark.sql.types._
import org.apache.spark.sql.{Row, Encoder, Encoders, SaveMode, SparkSession}
import org.apache.spark.ml.recommendation.ALS

object SimpleApp {  
  def fun1 (args: Array[String]):Boolean = {  
    println("你好，开始处理缺失值，请在这之前把所有的文件转成csv并放到当前jar文件所在文件夹的original文件夹中！！！！")
    
    // formal code of miss handle
        
    
    Logger.getLogger("org").setLevel(Level.ERROR) 
    val spark:SparkSession = SparkSession.builder
                             .appName("Test1")
                             .master("local[1]")
                             .getOrCreate 
    
    var inputpath:String = "original"
    var outputpath:String = "corrected"
    var logfilepath:String = "logfile"
    
    var folder_input = new File(inputpath)
    if (!(folder_input.isDirectory)){ // http://www.cnblogs.com/noteless/p/9609837.html
        println("检测到没有original文件夹，请在当前文件夹内新建名为original的文件夹，并把数据放在里面!!!")
        return false
    } 
    var files_input = folder_input.listFiles().filter(_.isFile())
    var total_file_number = files_input.toIterator.length
    if ( total_file_number == 0){
        println("检测到original文件夹下没有数据文件！！！！！")
        return false
    } 
    var current_file_number = 0 
      
    var folder_output = new File(outputpath)
    if (!(folder_output.isDirectory)){
        folder_output.mkdir()    
    }
    var files_output = folder_output.listFiles().filter(_.isFile())
    var folder_output_empty = files_output.toIterator.length
    
    var folder_log = new File(logfilepath)
    if(!(folder_log.isDirectory)){
        folder_log.mkdir()    
    }
      
    for (alpha <- files_input.toIterator){
        // 查看input中文件是否在output中
        println(alpha.getName)
        var existed = true
        var flag = true 
        var file_original = alpha.getName.r  // 文件名中不能出现括号
        
        if ( folder_output_empty > 0){
            for ( beta <- files_output.toIterator if flag){
                var file_corrected = beta.getName
                existed = file_original.findAllIn(file_corrected).mkString.equals(alpha.getName)
                if (existed){
                    println(alpha.getName)
                    println(beta.getName)
                    total_file_number = total_file_number - 1
                    flag = false
                }
            }   
        } else {
            existed = false
        }
        
        if ( existed == false ){ 
            total_file_number = total_file_number - 1
//            println("出现没有处理过的文件了")
            
            // 正式开始分析
            current_file_number = current_file_number + 1
            println("现在处理第 " + current_file_number + " 个文件； 还剩余文件数：" + total_file_number.toString + " 个文件")
            println("总共有文件:" + files_input.toIterator.length.toString)
            println("当前文件为：" + alpha.toString)
      
            // file i/o
            var file = new File(alpha.toString) 
            var bytes = new Array[Byte](file.length().toInt) 
            var in = new BufferedInputStream(new FileInputStream(file)) 
            in.read(bytes) 
            in.close()
            var data = Source.fromBytes(bytes).mkString 
        
            // 对data的string进行分解
            val valid_col = 6
            val valid_col_end = 23
            var lines = data.split("\r").map(_.trim)
            var data_header = lines(0).split(",").map(_.trim)
            
            if (data_header(6) != "Unnamed: 6") { // 特殊代码，见Line 249
            
            var not_blank_end = 0
            flag = true
            for (i <- 0 until lines.length - 1 if flag){
                var cols = lines(i).split(",").map(_.trim)
                // println(lines(i))
                if (StringUtils.isEmpty(cols(valid_col)) || StringUtils.isBlank(cols(valid_col)) || cols(valid_col) == "None"){
                    not_blank_end = i 
                    flag = false
                } else {
                    not_blank_end = 0
                }
            }
            var data_SparkCorrected = Array.ofDim[String](not_blank_end, valid_col_end - valid_col)
            for (i <- valid_col until valid_col_end){
                data_SparkCorrected(0)(i - valid_col) = data_header(i)
            }
            val data_toSpark_header = Array[String](data_header(valid_col), "variables", "values")
            var data_toSpark = new util.ArrayList[Row]()
            for (i <- 0 until not_blank_end){
                breakable{
                    var cols = lines(i).split(",").map(_.trim)
                    if ( cols(valid_col) == data_header(valid_col) ) {
                        // println("碰到表头，略过此行")
                        break 
                    } else {
                        var j = valid_col + 1
                        var row:Int = i
                        var column:Int = 0
                        while (j < valid_col_end){
                           var NaN_1 = Try(cols(j).toDouble) // 目前错误：0值，NaN，中文或异常字符
                           var NaN_2 = NaN_1.isSuccess
                           column = j - valid_col
                           if ( NaN_2 ) {
                             if (cols(j).toDouble != 0){ 
                                 data_toSpark.add( Row( row.toString, column.toString, cols(j) ))
                             } else {
                                 cols(j) = "NaN"
                                 data_toSpark.add( Row( row.toString, column.toString, cols(j) ))
                             }
                           } else {
                             cols(j) = "NaN" 
                             data_toSpark.add( Row( row.toString, column.toString, cols(j) ))
                           }
                           j = j + 1
                        }
                        j = valid_col
                        while (j < valid_col_end){
                            data_SparkCorrected(i)(j - valid_col) = cols(j)
                            j = j + 1
                        }
                    }
                }
            }
    
            // print for test
//            println(data_toSpark.getClass)
//            println(data_SparkCorrected.getClass)
//            println(data_toSpark.size)
//            println(data_SparkCorrected(1)(0))
    
            val schema = StructType(List(
                         StructField(data_toSpark_header(0), StringType, true),
                         StructField(data_toSpark_header(1), StringType, true),
                         StructField(data_toSpark_header(2), StringType, true)
                         ))                
            import spark.implicits._
            var data_inSpark = spark.createDataFrame(data_toSpark, schema).toDF() 
            val data_inSpark_final = data_inSpark.select(
                                     data_inSpark.col(data_toSpark_header(0)).cast(IntegerType).as(data_toSpark_header(0)), 
                                     data_inSpark.col(data_toSpark_header(1)).cast(IntegerType).as(data_toSpark_header(1)),
                                     data_inSpark.col(data_toSpark_header(2)).cast(DoubleType).as(data_toSpark_header(2))
                                     ).toDF
    
            // CF train
            val data_inSpark_final_NoNaN = data_inSpark_final.na.drop() 
            val als = new ALS()
//                          .setRank(2)
                          .setMaxIter(10)
                          .setRegParam(1)
                          .setUserCol(data_toSpark_header(0))
                          .setItemCol(data_toSpark_header(1))
                          .setRatingCol(data_toSpark_header(2))
            val model = als.fit(data_inSpark_final_NoNaN) 

            // CF predict
            var data_predicted = model.transform(data_inSpark_final)
            data_predicted = data_predicted.orderBy(data_toSpark_header(0))
//            println("hello2")
    
            // correct to data_SparkCorrected
            var i = 1
            var j = 1
            var file_outputpath = outputpath.concat("/missSolved_")
            var file_logfilepath = logfilepath.concat("/missSolved_")
            file_outputpath = file_outputpath.concat(alpha.getName)
            file_logfilepath = file_logfilepath.concat(alpha.getName)
            var fw = new FileWriter(file_outputpath)
            var logfile = new FileWriter(file_logfilepath.concat("_logfile.txt")) // 每个数据文件的修改日志
    
            fw.write(data_header(valid_col).concat(",")) // time stamp
            while (j < data_SparkCorrected(0).length){
                if ( j == data_SparkCorrected(0).length - 1){
                    fw.write(data_SparkCorrected(0)(j).concat("\n"))
                } else {
                    fw.write(data_SparkCorrected(0)(j).concat(","))
                }
                j = j + 1 // loop control
            }
        
            j = 1 // 重置

            data_predicted = data_predicted.select(
                                            data_predicted.col(data_toSpark_header(0)).cast(StringType).as(data_toSpark_header(0)), 
                                            data_predicted.col(data_toSpark_header(1)).cast(StringType).as(data_toSpark_header(1)), 
                                            data_predicted.col("prediction").cast(StringType).as("prediction")
                                            ).toDF
//            println("到这里了")
            while (i < data_SparkCorrected.length){
                fw.write(data_SparkCorrected(i)(0) + ",") // time stamp
                while (j < data_SparkCorrected(i).length){
                    if ( data_SparkCorrected(i)(j) == "NaN" ){
                        var data_key = data_predicted.filter(
                                                      data_predicted(data_toSpark_header(0)).equalTo(i.toString)
                                                      &&
                                                      data_predicted(data_toSpark_header(1)).equalTo(j.toString)
                                                      ).collect // spark.sql.Row
                        data_SparkCorrected(i)(j) = data_key(0)(2).toString
                        if ( data_SparkCorrected(i)(j).toDouble <= 0 ){
                            data_SparkCorrected(i)(j) = 0.1.toString
                        }
                        println("在第 " + current_file_number + " 个文件中发现NaN, 修正中...；文件名为：　" + alpha.toString)
                        logfile.write("在第 " + current_file_number + " 个文件中发现NaN, 修正中...；\n文件名为：　" + alpha.toString + "\n")
                        println(data_SparkCorrected(i)(0))
                        logfile.write("时间戳：" + data_SparkCorrected(i)(0) + "\n")
                        println(data_SparkCorrected(0)(j))
                        logfile.write("变量名：" + data_SparkCorrected(0)(j) + "\n")
                        println(data_SparkCorrected(i)(j))
                        logfile.write("补充的值：" + data_SparkCorrected(i)(j) + "\n")      
                }
                    if ( j == data_SparkCorrected(i).length - 1 ){
                        data_SparkCorrected(i)(j) = data_SparkCorrected(i)(j).concat("\n")
                    } else {
                        data_SparkCorrected(i)(j) = data_SparkCorrected(i)(j).concat(",")
                    }
                    fw.write(data_SparkCorrected(i)(j))
                    j = j + 1 // inner loop control
                }
                j = 1
                i = i + 1 // outer loop control
            }
        
            // close file writer
            fw.close()
            logfile.write("本文件分析结束,文件名为：" + alpha.toString + "\n")
            logfile.close()
    
           } else {   // if header 出现 Unnamed: 6  // 特殊代码,见Line 91
             println("这个文件没有HRV值:" + alpha.getName.toString + "\n" + "\n" + "\n" + "\n" + "\n" + "\n" + "\n")
             var file_logfilepath = logfilepath.concat("/noHRVvalue_")
             file_logfilepath = file_logfilepath.concat(alpha.getName.toString)
             var logfile = new FileWriter(file_logfilepath.concat("_error_logfile.txt")) // 每个数据文件的修改日志
             logfile.write("本文件分析结束,结果错误,文件名为：" + alpha.toString + "\n")
             logfile.close()
             }
            
        } // if 判断结束处，即更新未处理过的文件结束处    
            
   } // 读取输入文件夹的结束处   
        
   spark.stop()

   // end of file
    
    println("！！！缺失值处理结束！！！")
    return true
  }
}

