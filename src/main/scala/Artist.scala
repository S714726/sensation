package s7.sensation.artist

import scala.collection.mutable.HashMap
import scala.xml.{Elem, Node}

import s7.sensation._
import s7.sensation.playlist.PlaylistSeed
import s7.sensation.song.Song

sealed abstract class Identifier
case class ArtistName(value: String) extends Identifier
case class ArtistId(value: String) extends Identifier

sealed abstract class Parameter extends QueryParameter
case object Name extends Parameter
case object Id extends Parameter
case object Songs extends Parameter

object Artist {
  def apply(ident: Identifier)(implicit apiKey: EchoNestKey): Artist = 
    new Artist(ident)

  // search method would go here, has a load of options
}

class Artist (val ident: Identifier)(implicit apiKey: EchoNestKey)
extends Query with PlaylistSeed {
  val base = "artist/"
  val data = HashMap.empty[Parameter, Any]

  ident match {
    case ArtistName(s) => data(Name) = s
    case ArtistId(s) => data(Id) = s
  }

  def apply(n: Name.type): String =
    data.getOrElseUpdate(Name, runQuery(Name).asInstanceOf[String])
    .asInstanceOf[String]

  def apply(i: Id.type): String =
    data.getOrElseUpdate(Id, runQuery(Id).asInstanceOf[String])
    .asInstanceOf[String]

  def getIdentifier: Identifier = if (data.contains(Name)) ArtistName(apply(Name))
                                  else ArtistId(apply(Id))

  def apply(s: Songs.type): Seq[Song] =
    data.getOrElseUpdate(Songs, runQuery(Songs).asInstanceOf[Seq[Song]])
   .asInstanceOf[Seq[Song]]

  def generateQuery(p: QueryParameter)(implicit apiKey: EchoNestKey): String =
    generateQuery(p, (p match {
      case Name => "profile?id=" + apply(Id)
      case Id => "profile?name=" + apply(Name)
      case Songs => "songs?" + (getIdentifier match {
        case ArtistName(n) => "name=" + n.replaceAll(" ", "%20")
        case ArtistId(i) => "id=" + i
      })
    }))(apiKey)

  def processQuery(p: QueryParameter, elem: Elem): Any = p match {
    case Name => elem \ "artist" \\ "name" text
    case Songs => elem \ "songs" \\ "song" map {
      (n: Node) => Song((n \ "id") text, (n \ "title") text)}
  }
}
