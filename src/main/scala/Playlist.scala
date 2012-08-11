package s7.sensation.playlist

import scala.xml.{Elem, Node}

import s7.sensation.artist
import s7.sensation.song
import s7.sensation._

// 3 kinds: basic, static, dynamic
//   basic & static return the same results: Seq[Song],
//   resembling artists
// Dynamic playlists need opening & closing (auto or manual),
//   have functions for retrieving one at a time, steering, etc.

trait PlaylistSeed
case object Parameter extends QueryParameter

object Static {
  // just basic for now, tons of parameters in the api static call
  //   also TrackId with Rosetta ?
  def apply(seeds: Seq[PlaylistSeed])(implicit apiKey: EchoNestKey): Seq[song.Song] = {
    new Query {
      val base = "playlist/"
      def generateQuery(p: QueryParameter)(implicit apiKey: EchoNestKey): String =
        generateQuery(p, "basic?" + seeds.take(5).map {
          (s: PlaylistSeed) => s match {
            case a: artist.Artist => "artist" + (a.getIdentifier match {
              case artist.ArtistName(n) => "=" + n.replaceAll(" ", "%20")
              case artist.ArtistId(i) => "_id=" + i
            })
            case s: song.Song => "song_id=" + s(song.Id)
          }}.mkString("&"))
      def processQuery(p: QueryParameter, elem: Elem): Any =
        elem \ "songs" \\ "song" map {
          (n: Node) => song.Song((n \ "id") text, (n \ "title") text)
        }
    }.runQuery(Parameter).asInstanceOf[Seq[song.Song]]
  }
}

/*
object Dynamic {
}
*/
