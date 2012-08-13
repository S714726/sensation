import scala.collection.mutable.HashMap
import scala.xml.{Elem, XML}
import org.scalatest.FunSpec
import s7.sensation._
import s7.sensation.artist.{Artist, Name, Songs}
import s7.sensation.song.{Hotttnesss, Song, Title, Id}

object ApiKey {
  implicit val key = EchoNestKey("Placeholder")
}
import ApiKey._

trait ValidResponseQuery extends Query {
  override def runQuery(p: QueryParameter)(implicit apiKey: EchoNestKey): Any = 
    processQuery(p, XML.loadString(p match {
      case Songs => """<response><status><version>4.2</version><code>0</code><message>Success</message></status><start>0</start><total>21</total><songs><song><id>SOXRZLV1338A5D520C</id><title>I Love You</title></song><song><id>SOEGJJQ1338A5D520E</id><title>Sanctuary</title></song><song><id>SOICSBG1377417E93B</id><title>Treading Water - DC Breaks Remix</title></song><song><id>SODLCUO132C061C49C</id><title>Too Close (Distance Remix)</title></song><song><id>SOQODTN1338A5D520B</id><title>Hands Are Clever</title></song><song><id>SOETQPT1338A5D520D</id><title>Whispering</title></song><song><id>SOXAJIB136F315BDB9</id><title>Up All Night (Nadastrom Remix)</title></song><song><id>SOCTCLY137564877D8</id><title>Up All Night (Nadastrom Remix)</title></song><song><id>SOUNATS13773F99DAB</id><title>Up All Night (SBTRKT VIP Remix)</title></song><song><id>SOOJING1338A5D5207</id><title>When Doves Cry</title></song><song><id>SOQMZGY133A92335F6</id><title>Up All Night (SBTRKT Remix)</title></song><song><id>SOPUVFH1369E64A7DC</id><title>Up All Night</title></song><song><id>SOYJLXH136A42C44AE</id><title>Up All Night</title></song><song><id>SORXXCY137405DC472</id><title>Humming Bird</title></song><song><id>SOJHSAM13907520299</id><title>Too Close</title></song></songs></response>"""
      case Hotttnesss => """<response><status><version>4.2</version><code>0</code><message>Success</message></status><songs><song><song_hotttnesss>0.709238</song_hotttnesss><artist_id>ARAH1Y21187B9AFC50</artist_id><id>SOGIBAI1316771B9D6</id><artist_name>Coolio</artist_name><title>Gangsta's Paradise</title></song></songs></response>"""
    }))
}

class EchoNestSpec extends FunSpec {
  var artist: Artist = _
  var song: Song = _

  describe("Artist") {
    artist = new Artist(HashMap(Name -> "Placeholder")) with ValidResponseQuery
    it("should correctly generate queries") {
      val query = artist.generateQuery(Songs)
      assert(query.slice(0, 37) == """http://developer.echonest.com/api/v4/""")
      val split = query.split(Array('/', '?', '&', '='))
      assert(split(5) == "artist" && split(6) == "songs")
      assert(split.contains("api_key"))
      assert(split.contains("name"))
      val format = split.indexOf("format")
      assert(format > -1 && format < split.length - 1 && 
             split(format+1) == "xml")
    }
    it("should parse & store query results") {
      assert(artist(Songs).head(Title) == "I Love You")
    }
  }

  describe("Song") {
    song = new Song(HashMap(Id -> "Placeholder")) with ValidResponseQuery
    it("should correctly generate queries") {
      val query = song.generateQuery(Hotttnesss)
      assert(query.slice(0, 37) == """http://developer.echonest.com/api/v4/""")
      val split = query.split(Array('/', '?', '&', '='))
      assert(split(5) == "song" && split(6) == "profile")
      assert(split.contains("api_key"))
      val bucket = split.indexOf("bucket")
      assert(bucket > -1 && bucket < split.length - 1 && 
             split(bucket+1) == "song_hotttnesss")
      val format = split.indexOf("format")
      assert(format > -1 && format < split.length - 1 && 
             split(format+1) == "xml")
    }
    it("should parse & store query results") {
      assert(song(Hotttnesss) == 0.709238)
    }
  }
}
