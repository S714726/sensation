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
case object NoParameter extends Parameter

object Search {
  sealed abstract class SearchParameter extends QueryParameter

  // Don't confuse these with Song and Artist objects; Search is used to
  //   find/create Artists and Songs from an unknown starting point
  case class Title(v: String) extends SearchParameter
  case class Artist(v: String) extends SearchParameter
  case class Combined(v: String) extends SearchParameter
  case class Style(v: Seq[String]) extends SearchParameter
}

object Song {
  def apply(elems: (Parameter, Any)*)(implicit apiKey: EchoNestKey): Song =
    new Song(HashMap(elems:_*))

  def fromXML(elems: Node): Seq[(Parameter, Any)] = elems.nonEmptyChildren.map {
    (elem) => elem match {
      case <title>{v}</title> => Title -> v.text
      case <id>{v}</id> => Id -> v.text
      case _ => NoParameter -> null
    }
  }.filterNot(_._1 == NoParameter)

  def search(elems: Search.SearchParameter*)
  (implicit apiKey: EchoNestKey): Seq[Song] = new Query {
    val base = "song/"

    def generateQuery(p: QueryParameter)(implicit apiKey: EchoNestKey): String = {
      generateQuery(p, "search?" + (elems.map {
        (elem) => elem match {
          case Search.Title(v) => "title=" + v
          case Search.Artist(v) => "artist=" + v
          case Search.Combined(v) => "combined=" + v
          case Search.Style(v) => v.map((x) => "style=" + x).mkString("&")
        }
      }.mkString("&")))(apiKey)
    }

    def processQuery(p: QueryParameter, elem: Elem): Any =
      (elem \ "songs" \\ "song") map ((x) => Song(fromXML(x):_*))
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

  def apply(i: Artist.type): artist.Artist = data.getOrElseUpdate(Artist, runQuery(Artist).asInstanceOf[artist.Artist])
    .asInstanceOf[artist.Artist]

  def apply(h: Hotttnesss.type): Double =
     data.getOrElseUpdate(Hotttnesss, runQuery(Hotttnesss).asInstanceOf[String].toDouble)
    .asInstanceOf[Double]

  def generateQuery(p: QueryParameter)(implicit apiKey: EchoNestKey): String =
    generateQuery(p, ((p match {
      case Title => "profile?id=" + apply(Id)
      case Id => ""
      case Artist => "profile?id=" + data(Id)
      case Hotttnesss => "profile?id=" + data(Id) + "&bucket=song_hotttnesss"
      })))(apiKey)

  def processQuery(p: QueryParameter, elem: Elem): Any = p match {
    case Title => elem \ "songs" \\ "song" \\ "title" text
    case Id => ""
    // there exists a way to process this
    case Artist => {
      val node = elem \ "songs" \\ "song"
      artist.Artist(artist.Name -> (node \\ "artist_name" text),
                    artist.Id -> (node \\ "artist_id" text))
    }
    case Hotttnesss => elem \ "songs" \\ "song" \\ "song_hotttnesss" text
  }
}
