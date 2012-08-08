
package s7.sensation

import scala.collection.mutable.HashMap

import java.net.URL

// Maybe use "Title" instead of "Name" for Song to keep nomenclature consistent?
object Song {
  def apply(id: InputId)(implicit apiKey: EchoNestKey): Song =
    new Song(None, id)

  def apply(name: InputName, id: InputId)(implicit apiKey: EchoNestKey): Song =
    new Song(Some(name), id)
}

class Song (name: Option[InputName], id: InputId)(implicit apiKey: EchoNestKey)
extends Query {
  val base = "song/"
  val data = HashMap.empty[QueryParam, Any]
  name.foreach((x: InputName) => data(Name) = x.str)
  data(Id) = id.str

  // The best way, or just override a HashMap interface with QueryParam keys?
  def hotttnesss: Double = data.getOrElseUpdate(
    Hotttnesss,runQuery(Hotttnesss)).asInstanceOf[Double]
}
