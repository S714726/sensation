package s7.sensation.song

import scala.collection.mutable.HashMap
import scala.xml.{Elem, Node}

import s7.sensation._
import s7.sensation.playlist.PlaylistSeed

sealed abstract class Parameter extends QueryParameter
case object Title extends Parameter
case object Id extends Parameter
case object Hotttnesss extends Parameter

object Song {
  def apply(id: String)(implicit apiKey: EchoNestKey): Song =
    new Song(id)

  // Perhaps a constructor map of sorts, as a lot of convenient information is
  //   often passed in along with the id
  def apply(id: String, title: String)(implicit apiKey: EchoNestKey): Song = {
    val song: Song = new Song(id)
    song.data(Title) = title
    song
  }
}

class Song (id: String)(implicit apiKey: EchoNestKey)
extends Query with PlaylistSeed {
  val base = "song/"
  val data = HashMap.empty[Parameter, Any]
  data(Id) = id

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
