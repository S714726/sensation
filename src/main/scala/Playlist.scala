package s7.sensation.playlist

import scala.xml.{Elem, Node}

import s7.sensation.artist
import s7.sensation.song
import s7.sensation._

trait PlaylistSeed

trait CreateQuery extends Query {
  def playseeds: Seq[PlaylistSeed]

  def generateQuery(p: QueryParameter)(implicit apiKey: EchoNestKey): String =
    generateQuery(p, playseeds.take(5).map {
      (s: PlaylistSeed) => s match {
        case a: artist.Artist => "artist" + (a.data.get(artist.Name) match {
          case Some(n) => "=" + n.asInstanceOf[String].replaceAll(" ", "%20")
          case None => "_id=" + a.data.getOrElse(artist.Id, "").asInstanceOf[String]
        })
        case s: song.Song => "song_id=" + s(song.Id)
      }}.mkString("&"))
}

object Static {
  // just basic for now, tons of parameters in the api static call
  //   also TrackId with Rosetta ?
  def apply(seeds: Seq[PlaylistSeed])(implicit apiKey: EchoNestKey): Seq[song.Song] = {
    new CreateQuery {
      val base = "playlist/basic?"
      val playseeds = seeds

      def processQuery(p: QueryParameter, elem: Elem): Any =
        (elem \ "songs" \\ "song") map ((x) => song.Song(x))
    }.runQuery(NoParameters).asInstanceOf[Seq[song.Song]]
  }
}

object Dynamic {
  def apply(seeds: Seq[PlaylistSeed])(implicit apiKey: EchoNestKey): Dynamic =
    new Dynamic(new CreateQuery {
      val base = "playlist/dynamic/create?"
      val playseeds = seeds
      def processQuery(p: QueryParameter, elem: Elem): String = (elem \ "session_id") text
    }.runQuery(NoParameters).asInstanceOf[String])
}

class Dynamic (val session_id: String)(implicit apiKey: EchoNestKey) {
  def next: Seq[song.Song] = new Query {
    val base = "playlist/dynamic/next?"
    def generateQuery(p: QueryParameter)(implicit apiKey: EchoNestKey): String =
      generateQuery(p, "session_id=" + session_id)

    def processQuery(p: QueryParameter, elem: Elem): Any =
      (elem \ "songs" \\ "song") map ((x) => song.Song(x))
  }.runQuery(NoParameters).asInstanceOf[Seq[song.Song]]

  def delete {
    new Query {
      val base = "playlist/dynamic/delete?"
      def generateQuery(p: QueryParameter)(implicit apiKey: EchoNestKey): String =
        generateQuery(p, "session_id=" + session_id)

      def processQuery(p: QueryParameter, elem: Elem): Any = { }
    }.runQuery(NoParameters)
  }
}
