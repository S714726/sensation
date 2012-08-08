
package s7.sensation

import scala.collection.mutable.HashMap

object Artist {
  def apply(name: InputName)(implicit apiKey: EchoNestKey): Artist = 
    new Artist(Some(name), None)

  // remember Rosetta stuff
  def apply(id: InputId)(implicit apiKey: EchoNestKey): Artist =
    new Artist(None, Some(id))

  def apply(name: InputName, id: InputId)(implicit apiKey: EchoNestKey): Artist =
    new Artist(Some(name), Some(id))
}

class Artist (name: Option[InputName], id: Option[InputId])(implicit apiKey: EchoNestKey)
extends Query {
  val base = "artist/"

  // Need accessor methods for basically every property in data, including "Name"
  val data = HashMap.empty[QueryParam, Any]
  name.foreach((x: InputName) => data(Name) = x.str)
  id.foreach((x: InputId) => data(Id) = x.str)

  // need to handle songs by ID too
  // also songs(optional start, optional end) 
  def songs: Seq[Song] = data.getOrElseUpdate(Songs, runQuery(Songs))
    .asInstanceOf[Seq[Song]]
}
