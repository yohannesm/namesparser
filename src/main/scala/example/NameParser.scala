package example

import scala.collection.immutable.ListMap
import scala.collection.mutable
import scala.util.Try

case class FullName(firstName: String, lastName: String)
case class State(firstNameIndexes: Array[Int], lastNameIndexes: Array[Int])

object NameParser extends App {
  val NumberUniqueNames = 25

  def generateModifiedNames(
      set: collection.mutable.Set[FullName]): Set[FullName] = {
    val indexedSet = set.toIndexedSeq
    val arrFirstNameAndIndex =
      (for (i <- indexedSet.indices) yield indexedSet(i).firstName -> i).toArray
    val arrLastNameAndIndex =
      (for (i <- indexedSet.indices) yield indexedSet(i).lastName -> i).toArray
    //most naive algorithm , just shift all the names by one
    var resultSet = Set[FullName]()
    for (i <- 0 until NumberUniqueNames - 1) {
      resultSet += FullName(arrFirstNameAndIndex(i)._1,
                            arrLastNameAndIndex(i + 1)._1)
    }
    // the last name need to be added is the 0th last name and nth first name
    resultSet += FullName(arrFirstNameAndIndex(NumberUniqueNames - 1)._1,
                          arrLastNameAndIndex(0)._1)
    resultSet
  }

  def InsertOrUpdateMap(map: collection.mutable.Map[String, Int],
                        key: String,
                        value: Int): Unit = {
    if (map.contains(key)) {
      val oldValue = map.getOrElse(key, 0)
      val newValue = oldValue + value
      map.update(key, newValue)
    } else {
      map += (key -> value)
    }
  }

  def InsertOrUpdateFullNameMap(map: collection.mutable.Map[FullName, Int],
                                key: FullName,
                                value: Int): Unit = {
    if (map.contains(key)) {
      val oldValue = map.getOrElse(key, 0)
      val newValue = oldValue + value
      map.update(key, newValue)
    } else {
      map += (key -> value)
    }
  }

  def InsertOrUpdateFullNameSet(
      lastNameMap: collection.mutable.Map[String, Int],
      firstNameMap: collection.mutable.Map[String, Int],
      fullNameSet: collection.mutable.Set[FullName],
      fullName: FullName): Unit = {
    if (!lastNameMap.contains(fullName.lastName) && !firstNameMap.contains(
          fullName.firstName) && fullNameSet.size < NumberUniqueNames) {
      fullNameSet += fullName
    }
  }

  def ParseAndPopulateMaps(str: String): Unit = {
    //assuming the 0th index will always be last name and 1st index is first name
    val splittedString = str.split("[,--]").map(_.trim)
    val lastNameStr = splittedString(0)
    val firstNameStr = splittedString(1)
    val fullName = FullName(firstNameStr, lastNameStr)
    InsertOrUpdateFullNameSet(LastnameMap, FirstnameMap, FullnameSet, fullName)
    InsertOrUpdateMap(LastnameMap, lastNameStr, 1)
    InsertOrUpdateMap(FirstnameMap, firstNameStr, 1)
    InsertOrUpdateFullNameMap(FullnameMap, fullName, 1)

//    splittedString.foreach { s =>
//      print(s + " ")
//      println("")
//    }
  }

  var LastnameMap = collection.mutable.Map[String, Int]()
  var FirstnameMap = collection.mutable.Map[String, Int]()
  var FullnameMap = collection.mutable.LinkedHashMap[FullName, Int]()
  var FullnameSet = collection.mutable.Set[FullName]()

  val sourceFile = scala.io.Source.fromFile("coding-test-data.txt")
  val lines =
    Try(sourceFile.getLines.toStream).toOption.getOrElse(Stream[String]())
  println(lines.size)
  //for(i <- lines.indices) yield println(lines(i))

  for { i <- lines.indices if i % 2 == 0 } yield ParseAndPopulateMaps(lines(i))
  sourceFile.close()

  println("lastName maps are : ")
  LastnameMap.foreach {
    case (k, v) => println("last name = " + k + " count =" + v)
  }
  println("firstName maps are : ")
  FirstnameMap.foreach {
    case (k, v) => println("first name = " + k + " count =" + v)
  }
  println("FullName maps are : ")
  FullnameMap.foreach {
    case (k, v) => println("full name = " + k + " count =" + v)
  }

  val tenMostPopularLastName =
    ListMap(LastnameMap.toSeq.sortWith(_._2 > _._2): _*)
      .take(10)
      .toArray
  val tenMostPopularFirstName =
    ListMap(FirstnameMap.toSeq.sortWith(_._2 > _._2): _*)
      .take(10)
      .toArray

  println("ten most popular lastNames are : ")
  tenMostPopularLastName.foreach {
    case (k, v) => println("name = " + k + " with count = " + v)
  }
  println("ten most popular firstNames are : ")
  tenMostPopularFirstName.foreach {
    case (k, v) => println("name = " + k + " with count = " + v)
  }

  val setModifiedNames = generateModifiedNames(FullnameSet)
  println("25 modified names are: ")
  setModifiedNames.foreach(x => println("Full name = " + x))

}

trait Greeting {
  lazy val greeting: String = "hello"
}
