package s7.sensation.taste

import s7.sensation._
import s7.sensation.song.{Id, Song}

import scala.util.parsing.json.{JSONArray, JSONObject}
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

  def update(i: Item, f: Feedback): String = {
    new Query {
      val base = "catalog/"

      def generateQuery(p: QueryParameter): (String, RequestMethod, Seq[(String, String)]) = {
        val data = new JSONArray(List(
          new JSONObject(Map("action" -> (f match {
            case SkipSong => "skip"
            case PlaySong => "play"
            case _ => "update"
          }), "item" -> new JSONObject(
            Map("song_id" -> i(Id), "item_id" -> i(Id)) ++ (f match {
              case FavoriteSong => Map("favorite" -> true)
              case BanSong => Map("ban" -> true)
              case _ => Map()
            })))))).toString()
        ("update", PostRequest, List("id" -> id, "data" -> data))
      }

      def processQuery (p: QueryParameter, elem: Elem): String =
        (elem \ "ticket") text
    }.runQuery(NoParameters).asInstanceOf[String]
  }

  def read: Option[String] = read(List())
  def read(i: Item): Option[String] = read(List("item_id" -> i(Id)))

  // read returns JSON output inside the XML for some reason; the Scala JSON
  //   parser is complete crap so for now return raw JSON
  // At least return None if the output is empty for now
  def read(args: List[(String, String)]): Option[String] = new Query {
    val base = "catalog/"

    def generateQuery(p: QueryParameter): (String, RequestMethod, Seq[(String, String)]) =
      ("read", GetRequest, List("id" -> id) ++ args)

    def processQuery (p: QueryParameter, elem: Elem): Option[String] = {
      val result = (elem \\ "items" text)
      result match {
        case "()" => None
        case _ => Some(result)
      }
    }
  }.runQuery(NoParameters).asInstanceOf[Option[String]]

  def status(ticket: String): Boolean = new Query {
    val base = "catalog/"

    def generateQuery(p: QueryParameter): (String, RequestMethod, Seq[(String, String)]) =
      ("status", GetRequest, List("ticket" -> ticket))

    def processQuery (p: QueryParameter, elem: Elem): Boolean =
      if ((elem \ "ticket_status" text) == "complete") true else false
  }.runQuery(NoParameters).asInstanceOf[Boolean]

  def delete {
    new Query {
      val base = "catalog/"

      def generateQuery(p: QueryParameter): (String, RequestMethod, Seq[(String, String)]) =
        ("delete", PostRequest, List("id" -> id))

      def processQuery (p: QueryParameter, elem: Elem): Any = { }
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
