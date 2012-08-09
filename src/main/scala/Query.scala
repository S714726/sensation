package s7.sensation

import scala.collection.mutable.HashMap
import scala.xml.{Elem, Node, XML}

import java.net.URL

case class EchoNestKey(key: String)

trait Query {
  val root = "http://developer.echonest.com/api/v4/"
  val opts = "&format=xml"
  def base: String

  // Use Any instead of pimped types because the boilerplate involved
  //   introduces the potential for more errors than type safety prevents
  def data: HashMap[QueryParam, Any]

  def runQuery(p: QueryParam)(implicit apiKey: EchoNestKey): Any =
    processQuery(p, fetchQuery(p)(apiKey))

  def fetchQuery(p: QueryParam)(implicit apiKey: EchoNestKey): Elem = {
    // temporarily keep track of the number of queries we send out
    Console.println("--------------------------that's one query")
    //    try {
    XML.load(new URL
             (root + base +
              (p match {
                case Songs => "songs?name=" + data(Name).asInstanceOf[String]
                .replaceAll(" ", "%20")
                case Hotttnesss => "profile?id=" + data(Id) + "&bucket=song_hotttnesss"
              }) + "&api_key=" + apiKey.key + opts))
    //    } catch (ex: Exception) {
      /* A few exceptions to be aware of or catch/throw myself:
Code	Value
-1	Unknown Error
0	Success
1	Missing/ Invalid API Key
2	This API key is not allowed to call this method
3	Rate Limit Exceeded
4	Missing Parameter
5	Invalid Parameter

Should handle these in processQuery, to test them
       */
    //    }
  }

  def processQuery (p: QueryParam, elem: Elem)(implicit apiKey: EchoNestKey): Any = {
    p match {
      case Songs => elem \ "songs" \\ "song" map {
        (n: Node) => Song(InputName((n \ "title") text), InputId((n \ "id") text))}
      case Hotttnesss => (elem \ "songs" \\ "song" \\ "song_hotttnesss" text) toDouble
      // case _ => needs to be exhaustive
    }
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
