package s7.sensation.taste

import s7.sensation._
import s7.sensation.song.Song
import scala.xml.Elem

object TasteProfile {
  // TODO Implement ArtistProfile

  def apply(seed: Song, name: String)(implicit apiKey: EchoNestKey): SongProfile =
    new SongProfile (new Query {
      val base = "catalog/"

      def generateQuery(p: QueryParameter): (String, RequestMethod, Seq[(String, String)]) =
        ("create", PostRequest, List("type" -> "song", "name" -> name))

      def processQuery (p: QueryParameter, elem: Elem): String = {
        Console.println(elem)
        (elem \ "id") text
      }
    }.runQuery(NoParameters).asInstanceOf[String])
}

// Taste profiles do have "name"s associated with them, but I'm not storing them
abstract class TasteProfile (val id: String)(implicit apiKey: EchoNestKey) {
  // TODO Implement compatibility with Artist
  //   both need to have some kind of similar apply(Id) method
  type Item = Song

// use scala.util.parsing.json.{JSONObject, JSONArray} to insert/format stuff
// for update:
//   new JSONObject(Map[String, Any])
//   new JSONArray(List[Any])
//   JSONObject.toString()

  // Don't know if we have to add new items manually for update to work
  //   TODO ask Ingrid
//  def feedback(i: Item, f: Feedback) {
    // feedback updates item with feedback similar to playlist
    // need to abstract out Feedback class ... maybe to just an
    // s7.sensation.core along with the common query stuff
//  }

  def delete {
    new Query {
      val base = "catalog/"

      def generateQuery(p: QueryParameter): (String, RequestMethod, Seq[(String, String)]) =
        ("delete", PostRequest, List("id" -> id))

      def processQuery (p: QueryParameter, elem: Elem): Any = {
        Console.println(elem)
      }
    }.runQuery(NoParameters)
  }
}

// profiles can be artist or song, but there's not much difference because
//   we're passing IDs around; the difference is more on the end user side
// can't pass a "skip artist" to an artist profile and vice versa
//   That's why there have to be two separate classes instantiated by the end user
/*
class ArtistProfile() extends TasteProfile {
}
*/

class SongProfile(id: String)(implicit apiKey: EchoNestKey)
extends TasteProfile(id)(apiKey) {
}
