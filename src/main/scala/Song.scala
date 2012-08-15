package s7.sensation.song

import scala.collection.mutable.HashMap
import scala.xml.{Elem, Node, NodeSeq}

import s7.sensation._
import s7.sensation.artist
import s7.sensation.playlist.PlaylistSeed

sealed abstract class Parameter extends QueryParameter
case object Title extends Parameter
case object Id extends Parameter
case object Artist extends Parameter
case object Hotttnesss extends Parameter

object Search {
  sealed abstract class SearchParameter extends QueryParameter

  // Don't confuse these with Song and Artist objects; Search is used to
  //   find/create Artists and Songs from an unknown starting point
  case object Title extends SearchParameter
  case object Artist extends SearchParameter
  case object Combined extends SearchParameter
}

object Song {
  def apply(elems: (Parameter, Any)*)(implicit apiKey: EchoNestKey): Song =
    new Song(HashMap(elems:_*))

  def apply(elems: Node)(implicit apiKey: EchoNestKey): Song = {
    var songParams = Seq.empty[(Parameter, Any)]
    var artistParams = Seq.empty[(artist.Parameter, Any)]

    elems.nonEmptyChildren.foreach {
      (elem) => elem match {
        case <title>{v}</title> => songParams +:= (Title -> v.text)
        case <id>{v}</id> => songParams +:= (Id -> v.text)
        case <artist_name>{v}</artist_name> => artistParams +:= (artist.Name -> v.text)
        case <artist_id>{v}</artist_id> => artistParams +:= (artist.Id -> v.text)
      }
    }

    // This does not work as expected
    if (artistParams.size > 0)
      songParams +:= (Artist -> artist.Artist(artistParams:_*))

    apply(songParams:_*)
  }

  def search(elems: (Search.SearchParameter, Any)*)
  (implicit apiKey: EchoNestKey): Seq[Song] = new Query {
    val base = "song/"

    def generateQuery(p: QueryParameter)(implicit apiKey: EchoNestKey): String = {
      generateQuery(p, "search?" + (elems.map{(kv) =>
        (kv._1 match {
          case Search.Title => "title="
          case Search.Artist => "artist="
          case Search.Combined => "combined="
        }) + kv._2.asInstanceOf[String].replaceAll(" ", "%20")}.mkString("&")))(apiKey)
    }

    def processQuery(p: QueryParameter, elem: Elem): Any =
      (elem \ "songs" \\ "song") map ((x) => Song(x))
  }.runQuery(NoParameters).asInstanceOf[Seq[song.Song]]
}

class Song (val data: HashMap[Parameter, Any])(implicit apiKey: EchoNestKey)
extends Query with PlaylistSeed {
  val base = "song/"

  def apply(t: Title.type): String =
    data.getOrElseUpdate(Title, runQuery(Title).asInstanceOf[String])
    .asInstanceOf[String]

  def apply(i: Id.type): String =
    data.getOrElseUpdate(Id, runQuery(Id).asInstanceOf[String])
    .asInstanceOf[String]

  def apply(h: Hotttnesss.type): Double =
     data.getOrElseUpdate(Hotttnesss, runQuery(Hotttnesss).asInstanceOf[String].toDouble)
    .asInstanceOf[Double]

  def generateQuery(p: QueryParameter)(implicit apiKey: EchoNestKey): String =
    generateQuery(p, ((p match {
      case Title => "profile?id=" + apply(Id)
      case Id => ""
      case Hotttnesss => "profile?id=" + data(Id) + "&bucket=song_hotttnesss"
      })))(apiKey)

  def processQuery(p: QueryParameter, elem: Elem): Any = p match {
    case Title => elem \ "songs" \\ "song" \\ "title" text
    case Id => ""
    case Hotttnesss => elem \ "songs" \\ "song" \\ "song_hotttnesss" text
  }
}
