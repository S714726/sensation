
package s7.sensation

import scala.collection.mutable.HashMap
import scala.xml

import java.net.URL

//     Notes:
// Java API does everything by fetching individual bucket information
// Python API only makes requests when it needs to
//   Uses dicts to store stuff

case class EchoNestKey(key: String)

abstract class Query ()(implicit val apiKey: EchoNestKey) {
  val root = "http://developer.echonest.com/api/v4/"
  val opts = "&format=xml"
  def base: String

  // Use Any instead of pimped types because the boilerplate involved
  //   introduces the potential for more errors than type safety prevents
  def data: HashMap[QueryParam, Any]

  def runQuery(p: QueryParam): Any = {
  // temporarily keep track of the number of queries we send out
    Console.println("--------------------------that's one query")
//    try {
      val elem = xml.XML.load(
        new URL(root + base +
                (p match {
                  case Songs => "songs?name=" + data(Name)
                  case Hotttnesss => "profile?id=" + data(Id) + "&bucket=song_hotttnesss"
                  // needs to be exhaustive...maybe queries aren't necessary for some
                }) + "&api_key=" + apiKey.key + opts))
      p match {
        case Songs => elem \ "songs" \\ "song" map {
          (n: xml.Node) => Song(InputName((n \ "title") text), InputId((n \ "id") text))}
        case Hotttnesss => (elem \ "songs" \\ "song" \\ "song_hotttnesss" text) toDouble
        // case _ => needs to be exhaustive
      }
//    } catch (ex: Exception) {
      /* Catch a few exceptions:
       HTTP 400 errors I've recieved so far:
       too many requests per time period
       bad API key
       */
//    }
  }
}

// Match input strings and assign results to QueryParam
//   naming problem: can't have these objects along with "Artist" and "Song"
sealed abstract class QueryParam
case object Name extends QueryParam
case object Id extends QueryParam
case object Artists extends QueryParam // call this "Artists" for now, probably bad
case object Songs extends QueryParam
case object Hotttnesss extends QueryParam

case class InputName(str: String)
case class InputId(str: String)

