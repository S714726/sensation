package s7.sensation.playlist

import scala.xml.{Elem, Node}

import s7.sensation.artist
import s7.sensation.song
import s7.sensation._

trait PlaylistSeed

// There are a few more options, but these are simple
sealed abstract class Feedback
case object BanArtist extends Feedback
case object FavoriteArtist extends Feedback
case object BanSong extends Feedback
case object SkipSong extends Feedback
case object FavoriteSong extends Feedback

trait CreateQuery extends Query {
  def playSeeds: Seq[PlaylistSeed]

  def generateQuery(p: QueryParameter)(implicit apiKey: EchoNestKey): String =
    generateQuery(p, playSeeds.take(5).map {
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
      val playSeeds = seeds

      def processQuery(p: QueryParameter, elem: Elem): Any =
        (elem \ "songs" \\ "song") map ((x) => song.Song(x))
    }.runQuery(NoParameters).asInstanceOf[Seq[song.Song]]
  }
}

object Dynamic {
  def apply(seeds: Seq[PlaylistSeed])(implicit apiKey: EchoNestKey): Dynamic =
    new Dynamic(new CreateQuery {
      val base = "playlist/dynamic/create?"
      val playSeeds = seeds
      def processQuery(p: QueryParameter, elem: Elem): String = (elem \ "session_id") text
    }.runQuery(NoParameters).asInstanceOf[String])
}

class Dynamic (val session_id: String)(implicit apiKey: EchoNestKey) {
  abstract class DynamicQuery (val command: String) extends Query {
    val base = "playlist/dynamic/"
    def generateQuery(p: QueryParameter)(implicit apiKey: EchoNestKey): String =
      generateQuery(p, command + "session_id=" + session_id)
  }

  // Next can return multiple songs, but it's simpler to just return one and
  //   its corresponding feedback callback
  // So, find the next song and return it along with a function used to rate it
  def next: (song.Song, (Feedback) => Unit) = {
    val s = new DynamicQuery("next?results=1&") {
      def processQuery(p: QueryParameter, elem: Elem): Any =
        song.Song((elem \ "songs" \\ "song") head)
    }.runQuery(NoParameters).asInstanceOf[song.Song]

    (s, (f: Feedback) => new DynamicQuery(
      "feedback?" + (f match {
        case BanArtist => "ban_artist"
        case FavoriteArtist => "favorite_artist"
        case BanSong => "ban_song"
        case SkipSong => "skip_song"
        case FavoriteSong => "favorite_song"
      }) + "=" + s(song.Id) + "&") {
        def processQuery(p: QueryParameter, elem: Elem): Any = { }
      }.runQuery(NoParameters))
  }

  // remember that lookahead is also found in pandora, etc.
  // def lookahead: Seq[song.Song] = new DynamicQuery("next?results=&lookahead=...") {

  def restart(seeds: Seq[PlaylistSeed]) {
    new CreateQuery {
      val base = "playlist/dynamic/restart?session_id=" + session_id + "&"
      val playSeeds = seeds
      def processQuery(p: QueryParameter, elem: Elem): Any = { }
    }.runQuery(NoParameters)
  }

  def delete {
    new DynamicQuery("delete?") {
      def processQuery(p: QueryParameter, elem: Elem): Any = { }
    }.runQuery(NoParameters)
  }
}
