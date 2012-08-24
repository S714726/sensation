package s7.sensation

import scala.collection.mutable.HashMap
import scala.xml.{Elem, Node, XML}

import java.net.{HttpURLConnection, URL, URLEncoder}

case class EchoNestKey(key: String)

abstract class QueryParameter
case object NoParameters extends QueryParameter

sealed abstract class RequestMethod
case object GetRequest extends RequestMethod
case object PostRequest extends RequestMethod

// There are a few more options, but these are simple
sealed abstract class Feedback
case object BanArtist extends Feedback
case object FavoriteArtist extends Feedback
case object BanSong extends Feedback
case object FavoriteSong extends Feedback
case object SkipSong extends Feedback
case object PlaySong extends Feedback

trait Query {
  val root = "http://developer.echonest.com/api/v4/"
  def base: String
  val opts = "&format=xml"

  def runQuery(p: QueryParameter)(implicit apiKey: EchoNestKey): Any = {
    val query = generateQuery(p)
    processQuery(p, fetchQuery(query._1, query._2, query._3)(apiKey))
  }

  def generateQuery(p: QueryParameter): (String, RequestMethod, Seq[(String, String)])

  def fetchQuery(url: String, method: RequestMethod, arguments: Seq[(String, String)])
    (implicit apiKey: EchoNestKey): Elem = {
    val args = arguments.map {
      (x) => x._1 + "=" + URLEncoder.encode(x._2, "UTF-8")
    }.mkString("&") + opts + "&api_key=" + apiKey.key
    val connection = (new URL(root + base + url + "?" + (method match {
      case GetRequest => args
      case PostRequest => ""
    }))) .openConnection .asInstanceOf[HttpURLConnection]
    connection.setRequestProperty("Accept-Charset", "UTF-8")

    method match {
      case PostRequest => {
        connection.setRequestMethod("POST")
        connection.setDoOutput(true)
        connection.setRequestProperty(
          "Content-Type", "application/x-www-form-urlencoded;charset=UTF-8")
        val outstr = connection.getOutputStream
        outstr.write(args.getBytes("UTF-8"))
        outstr.close
      }
      case GetRequest =>
    }
    val instr = connection.getInputStream
    val result = XML.load(instr)
    instr.close
    result
  }

  def processQuery (p: QueryParameter, elem: Elem): Any
// Put exceptions in Query.processQuery, use super call
/* A few exceptions to be aware of or catch/throw myself:
Code	Value
-1	Unknown Error
0	Success
1	Missing/ Invalid API Key
2	This API key is not allowed to call this method
3	Rate Limit Exceeded
4	Missing Parameter
5	Invalid Parameter
*/
}
