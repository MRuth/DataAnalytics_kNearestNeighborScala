import scala.io.Source
import scala.collection.mutable

/**
  * Created by Montana Ruth on 9/30/2016.
  */

object Runner extends App {
  val in = Source.fromFile("Files/iris.data").getLines().map{s =>
    val split = s.split(",")
    Flower(split(0).toDouble,split(1).toDouble,split(2).toDouble,split(3).toDouble,split(4))
  }.zipWithIndex.toList

  val training = in.filter(_._2 % 3 != 2).map(_._1)
  val validation = in.filter(_._2 % 3 == 2).map(_._1)
  val possibleKValues = (2 to 30).filter(_%2==1).toList

  val map: mutable.HashMap[Flower,List[(Double,String)]] = new mutable.HashMap()
  val accuracies = possibleKValues.map{k =>
    val outcomes =
    validation.map{flower =>
      map.getOrElseUpdate(flower, training.map{t =>
        (t.distanceTo(flower),t.species)
      }.sortBy(_._1)).take(k).groupBy(_._2).maxBy(_._2.size)._1 == flower.species
    }
    (k,(outcomes.count(_ == true)/outcomes.length.toDouble)*100)
  }.sortBy(_._2).reverse

  accuracies.foreach(t => println(f"K-Value: ${t._1}\tAccuracy: ${t._2}%.2f"))


}

case class Flower(sepalLength: Double, sepalWidth: Double, petalLength: Double,
                  petalWidth: Double, species: String) {

  def distanceTo(other: Flower): Double =
    Math.sqrt(Math.pow((other.sepalLength-sepalLength),2) + Math.pow((other.sepalWidth-sepalWidth),2)+
      Math.pow((other.petalLength-petalLength),2) + Math.pow((other.petalWidth-petalWidth),2))
}