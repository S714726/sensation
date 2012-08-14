package s7.sensation.artist

import scala.collection.mutable.HashMap
import scala.xml.{Elem, Node}

import s7.sensation._
import s7.sensation.playlist.PlaylistSeed
import s7.sensation.song.Song

sealed abstract class Parameter extends QueryParameter
case object Name extends Parameter
case object Id extends Parameter
case object Songs extends Parameter

object Artist {
  def apply(elems: (Parameter, Any)*)(implicit apiKey: EchoNestKey): Artist =
    new Artist(HashMap(elems:_*))

  def apply(elems: Node)(implicit apiKey: EchoNestKey): Artist =
    apply(elems.nonEmptyChildren.map{(elem) => elem match {
      case <name>{v}</name> => Name -> v.text
      case <id>{v}</id> => Id -> v.text
    }}:_*)

  // search method would go here, has a load of options
}

class Artist (val data: HashMap[Parameter, Any])(implicit apiKey: EchoNestKey)
extends Query with PlaylistSeed {
  val base = "artist/"

  // Is there any way to programmatically write these apply methods?
  //   Switching to Enums? Macros(ugh)?
  //   Perhaps clump up parameters with common return types, e.g. String
  def apply(n: Name.type): String =
    data.getOrElseUpdate(Name, runQuery(Name).asInstanceOf[String])
    .asInstanceOf[String]

  def apply(i: Id.type): String =
    data.getOrElseUpdate(Id, runQuery(Id).asInstanceOf[String])
    .asInstanceOf[String]

  def apply(s: Songs.type): Seq[Song] =
    data.getOrElseUpdate(Songs, runQuery(Songs).asInstanceOf[Seq[Song]])
   .asInstanceOf[Seq[Song]]

  def generateQuery(p: QueryParameter)(implicit apiKey: EchoNestKey): String =
    generateQuery(p, (p match {
      case Name => "profile?id=" + apply(Id)
      case Id => "profile?name=" + apply(Name)
      case Songs => "songs?" + (data.get(Name) match {
        case Some(n) => "name=" + n.asInstanceOf[String].replaceAll(" ", "%20")
        case None => "id=" + data.getOrElse(Id, "").asInstanceOf[String]
      })}))(apiKey)

  def processQuery(p: QueryParameter, elem: Elem): Any = p match {
    case Name => elem \ "artist" \\ "name" text
    case Songs => (elem \ "songs" \\ "song") map ((x) => Song(x))
  }
}
