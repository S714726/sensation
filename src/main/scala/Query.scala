package s7.sensation

import scala.collection.mutable.HashMap
import scala.xml.{Elem, Node, XML}

import java.net.URL

case class EchoNestKey(key: String)

abstract class QueryParameter
case object NoParameters extends QueryParameter

trait Query {
  val root = "http://developer.echonest.com/api/v4/"
  def base: String
  val opts = "&format=xml"

  def runQuery(p: QueryParameter)(implicit apiKey: EchoNestKey): Any =
    processQuery(p, fetchQuery(generateQuery(p)(apiKey)))

  def generateQuery(p: QueryParameter)(implicit apiKey: EchoNestKey): String

  def generateQuery(p: QueryParameter, interior: String)
    (implicit apiKey: EchoNestKey): String =
      root + base + interior + "&api_key=" + apiKey.key + opts

  def fetchQuery(q: String): Elem = XML.load(new URL(q))

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
