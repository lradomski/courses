package timeusage

import java.io.{BufferedWriter, File, FileWriter}
import java.nio.file.{Path, Paths}
import java.util.Scanner

import org.apache.spark.sql._
import org.apache.spark.sql.types._


/** Main class */
object TimeUsage
{

  import org.apache.spark.sql.SparkSession
  import org.apache.spark.sql.functions._

  val spark: SparkSession =
    SparkSession
      .builder()
      .appName("Time Usage")
      .config("spark.master", "local[*]")
      .getOrCreate()

  // For implicit conversions like converting RDDs to DataFrames
  import spark.implicits._

  def lrTrruncate() =
  {
    val s = new Scanner(new File(fsPath("/timeusage/atussum.csv")) )
    val out = new File("/Users/luke/git/courses/spark/timeusage/src/main/resources/timeusage/test.csv")
    val bw = new BufferedWriter(new FileWriter(out))
    for (i <- 1 to 100) bw.write(s.next() + "\n")
    bw.close()

  }

  def lrtest() = println("Test!")
  def lrGetRdd() = spark.sparkContext.textFile(fsPath("/timeusage/test.csv"))

  /** Main function */
  def main(args: Array[String]): Unit =
  {
    //lrTrruncate()
    timeUsageByLifePeriod()
  }

  def timeUsageByLifePeriod(): Unit =
  {
    val (columns, initDf) = read("/timeusage/atussum.csv")
    val (primaryNeedsColumns, workColumns, otherColumns) = classifiedColumns(columns)
    val summaryDf = timeUsageSummary(primaryNeedsColumns, workColumns, otherColumns, initDf)

//    summaryDf.cache()
//
//    var r1: Array[TimeUsageRow] = null
//    var c1: Array[String] = null
//
//    var r2: Array[TimeUsageRow] = null
//    var c2: Array[String] = null
//
//    var r3: Array[TimeUsageRow] = null
//    var c3: Array[String] = null

    {
      val finalDf = timeUsageGrouped(summaryDf)
//      r1 = timeUsageSummaryTyped(finalDf).collect()
//      c1 = finalDf.columns
      finalDf.show()
    }

//    {
//      val finalDf = timeUsageGroupedSql(summaryDf)
//      r2 = timeUsageSummaryTyped(finalDf).collect()
//      c2 = finalDf.columns
//      finalDf.show()
//    }
//
//    {
//      val finalDf = timeUsageGroupedTyped(timeUsageSummaryTyped(summaryDf))
//      c3 = finalDf.columns
//      r3 = finalDf.collect()
//      finalDf.show()
//    }
//
//    assert(r1.length == r2.length)
//    assert(r3.length == r2.length)
//
//    r1.zipWithIndex.foreach( vi =>
//    {
//      val (v,i) = (vi._1, vi._2)
//      assert(v == r2(i), i + "(2): " + v + "==" + r2(i))
//      assert(v == r3(i), i + "(3): " + v + "==" + r3(i))
//    })
//
//    c1.zipWithIndex.foreach( vi =>
//    {
//      val (v,i) = (vi._1, vi._2)
//      assert(v == c2(i), i + "(2): " + v + "==" + c2(i))
//      assert(v == c3(i), i + "(3): " + v + "==" + c3(i))
//    })
  }


  /** @return The read DataFrame along with its column names. */
  def read(resource: String): (List[String], DataFrame) =
  {
    val rdd = spark.sparkContext.textFile(fsPath(resource))

    val headerColumns = rdd.first().split(",").to[List]
    // Compute the schema based on the first line of the CSV file
    val schema = dfSchema(headerColumns)

    val data =
      rdd
        .mapPartitionsWithIndex((i, it) => if (i == 0) it.drop(1) else it) // skip the header line
        .map(_.split(",").to[List])
        .map(row)

    val dataFrame =
      spark.createDataFrame(data, schema)

    (headerColumns, dataFrame)
  }

  /** @return The filesystem path of the given resource */
  def fsPath(resource: String): String =
  Paths.get(getClass.getResource(resource).toURI).toString

  /** @return The schema of the DataFrame, assuming that the first given column has type String and all the others
    *         have type Double. None of the fields are nullable.
    * @param columnNames Column names of the DataFrame
    */
  def dfSchema(columnNames: List[String]): StructType =
  {
    StructType(
      new StructField(columnNames.head, DataTypes.StringType, nullable = false)
        :: columnNames.tail.map(new StructField(_, DataTypes.DoubleType, nullable = false))
    )
  }


  /** @return An RDD Row compatible with the schema produced by `dfSchema`
    * @param line Raw fields
    */
  def row(line: List[String]): Row = Row.fromSeq(line.head :: line.tail.map(_.toDouble))


  /** @return The initial data frame columns partitioned in three groups: primary needs (sleeping, eating, etc.),
    *         work and other (leisure activities)
    * @see https://www.kaggle.com/bls/american-time-use-survey
    *
    *      The dataset contains the daily time (in minutes) people spent in various activities. For instance, the column
    *      “t010101” contains the time spent sleeping, the column “t110101” contains the time spent eating and drinking, etc.
    *
    *      This method groups related columns together:
    * 1. “primary needs” activities (sleeping, eating, etc.). These are the columns starting with “t01”, “t03”, “t11”,
    *      “t1801” and “t1803”.
    * 2. working activities. These are the columns starting with “t05” and “t1805”.
    * 3. other activities (leisure). These are the columns starting with “t02”, “t04”, “t06”, “t07”, “t08”, “t09”,
    *      “t10”, “t12”, “t13”, “t14”, “t15”, “t16” and “t18” (those which are not part of the previous groups only).
    */
  def classifiedColumns(columnNames: List[String]): (List[Column], List[Column], List[Column]) =
  {
    import spark.implicits._
    case class Columns(primary: List[Column], working: List[Column], other: List[Column])

    val primary = List("t01", "t03", "t11", "t1801", "t1803")
    val working = List("t05", "t1805")
    val other = List("t02", "t04", "t06", "t07", "t08", "t09", "t10", "t12", "t13", "t14", "t15", "t16", "t18")

    val cols = columnNames.foldLeft(Columns(List(), List(), List()))(
      (cols, s) =>
        if (primary.find(s.startsWith(_)).isDefined) Columns(col(s) :: cols.primary, cols.working, cols.other)
        else if (working.find(s.startsWith(_)).isDefined) Columns(cols.primary, col(s) :: cols.working, cols.other)
        else if (other.find(s.startsWith(_)).isDefined) Columns(cols.primary, cols.working, col(s) :: cols.other)
        else cols
    )

    (cols.primary, cols.working, cols.other)
  }

  /** @return a projection of the initial DataFrame such that all columns containing hours spent on primary needs
    *         are summed together in a single column (and same for work and leisure). The “teage” column is also
    *         projected to three values: "young", "active", "elder".
    * @param primaryNeedsColumns List of columns containing time spent on “primary needs”
    * @param workColumns         List of columns containing time spent working
    * @param otherColumns        List of columns containing time spent doing other activities
    * @param df                  DataFrame whose schema matches the given column lists
    *
    *                            This methods builds an intermediate DataFrame that sums up all the columns of each group of activity into
    *                            a single column.
    *
    *                            The resulting DataFrame should have the following columns:
    * - working: value computed from the “telfs” column of the given DataFrame:
    *   - "working" if 1 <= telfs < 3
    *   - "not working" otherwise
    * - sex: value computed from the “tesex” column of the given DataFrame:
    *   - "male" if tesex = 1, "female" otherwise
    * - age: value computed from the “teage” column of the given DataFrame:
    *   - "young" if 15 <= teage <= 22,
    *   - "active" if 23 <= teage <= 55,
    *   - "elder" otherwise
    * - primaryNeeds: sum of all the `primaryNeedsColumns`, in hours
    * - work: sum of all the `workColumns`, in hours
    * - other: sum of all the `otherColumns`, in hours
    *
    *                            Finally, the resulting DataFrame should exclude people that are not employable (ie telfs = 5).
    *
    *                            Note that the initial DataFrame contains time in ''minutes''. You have to convert it into ''hours''.
    */
  def timeUsageSummary(
                        primaryNeedsColumns: List[Column],
                        workColumns: List[Column],
                        otherColumns: List[Column],
                        df: DataFrame
                      ): DataFrame =
  {


    /*
when(people("gender") === "male", 0)
     .when(people("gender") === "female", 1)
     .otherwise(2)
     */
    val workingStatusProjection: Column = when((df("telfs") >= 1).and(df("telfs") < 3), "working").otherwise("not working").alias("working")
    val sexProjection: Column = when(df("tesex").equalTo(1), "male").otherwise("female").alias("sex")
    val ageProjection: Column =
      when((df("teage") >= 15).and(df("teage") <= 22), "young")
        .when((df("teage") >= 23).and(df("teage") <= 55), "active")
        .otherwise("elder").alias("age")

    val primaryNeedsProjection: Column = (primaryNeedsColumns.reduce(_ + _) / 60).alias("primaryNeeds")
    val workProjection: Column = (workColumns.reduce(_ + _) / 60).alias("work")
    val otherProjection: Column = (otherColumns.reduce(_ + _) / 60).alias("other")
    df
      .select(workingStatusProjection, sexProjection, ageProjection, primaryNeedsProjection, workProjection, otherProjection)
      .where($"telfs" <= 4) // Discard people who are not in labor force
  }

  /** @return the average daily time (in hours) spent in primary needs, working or leisure, grouped by the different
    *         ages of life (young, active or elder), sex and working status.
    * @param summed DataFrame returned by `timeUsageSumByClass`
    *
    *               The resulting DataFrame should have the following columns:
    * - working: the “working” column of the `summed` DataFrame,
    * - sex: the “sex” column of the `summed` DataFrame,
    * - age: the “age” column of the `summed` DataFrame,
    * - primaryNeeds: the average value of the “primaryNeeds” columns of all the people that have the same working
    *               status, sex and age, rounded with a scale of 1 (using the `round` function),
    * - work: the average value of the “work” columns of all the people that have the same working status, sex
    *               and age, rounded with a scale of 1 (using the `round` function),
    * - other: the average value of the “other” columns all the people that have the same working status, sex and
    *               age, rounded with a scale of 1 (using the `round` function).
    *
    *               Finally, the resulting DataFrame should be sorted by working status, sex and age.
    */
  def timeUsageGrouped(summed: DataFrame): DataFrame =
  {
    summed
      .select($"working", $"sex", $"age", $"primaryNeeds", $"work", $"other") // )
      .groupBy($"working", $"sex", $"age")
//      .agg(round(avg($"primaryNeeds"), 1), round(avg($"work"), 1), round(avg($"other"), 1))
      .agg(round(avg($"primaryNeeds"), 1).alias("primaryNeeds"), round(avg($"work"), 1).alias("work"), round(avg($"other"), 1).alias("other"))
      .orderBy($"working", $"sex", $"age")
  }

  /**
    * @return Same as `timeUsageGrouped`, but using a plain SQL query instead
    * @param summed DataFrame returned by `timeUsageSumByClass`
    */
  def timeUsageGroupedSql(summed: DataFrame): DataFrame =
  {
    val viewName = s"summed"
    summed.createOrReplaceTempView(viewName)
    spark.sql(timeUsageGroupedSqlQuery(viewName))
  }

  /** @return SQL query equivalent to the transformation implemented in `timeUsageGrouped`
    * @param viewName Name of the SQL view to use
    */
  def timeUsageGroupedSqlQuery(viewName: String): String =
  "SELECT " +
    "working, sex, age, " + //primaryNeeds, work, other " +
    "ROUND(AVG(primaryNeeds), 1) as primaryNeeds, ROUND(AVG(work), 1) as work, ROUND(AVG(other), 1) as other " +
    "FROM summed " +
    "GROUP BY working, sex, age " +
    "ORDER BY  working, sex, age "

  /**
    * @return A `Dataset[TimeUsageRow]` from the “untyped” `DataFrame`
    * @param timeUsageSummaryDf `DataFrame` returned by the `timeUsageSummary` method
    *
    *                           Hint: you should use the `getAs` method of `Row` to look up columns and
    *                           cast them at the same time.
    */
  /*
  working: String,
  sex: String,
  age: String,
  primaryNeeds: Double,
  work: Double,
  other: Double
  */
  def timeUsageSummaryTyped(timeUsageSummaryDf: DataFrame): Dataset[TimeUsageRow] =
  timeUsageSummaryDf.map(r => TimeUsageRow(
    working = r.getAs[String]("working"),
    sex = r.getAs[String]("sex"),
    age = r.getAs[String]("age"),
    primaryNeeds = r.getAs[Double]("primaryNeeds"),
    work = r.getAs[Double]("work"),
    other = r.getAs[Double]("other")
  ))

  /**
    * @return Same as `timeUsageGrouped`, but using the typed API when possible
    * @param summed Dataset returned by the `timeUsageSummaryTyped` method
    *
    *               Note that, though they have the same type (`Dataset[TimeUsageRow]`), the input
    *               dataset contains one element per respondent, whereas the resulting dataset
    *               contains one element per group (whose time spent on each activity kind has
    *               been aggregated).
    *
    *               Hint: you should use the `groupByKey` and `typed.avg` methods.
    */
  def timeUsageGroupedTyped(summed: Dataset[TimeUsageRow]): Dataset[TimeUsageRow] =
  {
    summed.groupByKey(r => (r.working, r.sex, r.age))
      .agg(round(avg($"primaryNeeds"), 1).as[Double], round(avg($"work"), 1).as[Double], round(avg($"other"),1).as[Double])
      .orderBy($"key")
      .map(
        kpwo =>
          {
            val (working, sex, age, pn, w, o) = (kpwo._1._1, kpwo._1._2, kpwo._1._3, kpwo._2, kpwo._3, kpwo._4)
            TimeUsageRow(working = working, sex = sex, age = age, primaryNeeds = pn, work = w, other = o)
          }
      )
  }

  /**
    * Models a row of the summarized data set
    *
    * @param working      Working status (either "working" or "not working")
    * @param sex          Sex (either "male" or "female")
    * @param age          Age (either "young", "active" or "elder")
    * @param primaryNeeds Number of daily hours spent on primary needs
    * @param work         Number of daily hours spent on work
    * @param other        Number of daily hours spent on other activities
    */
  case class TimeUsageRow(working: String,
                          sex: String,
                          age: String,
                          primaryNeeds: Double,
                          work: Double,
                          other: Double)

}