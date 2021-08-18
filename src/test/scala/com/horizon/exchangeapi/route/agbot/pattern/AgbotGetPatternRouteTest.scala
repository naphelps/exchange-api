package com.horizon.exchangeapi.route.agbot.pattern

import com.horizon.exchangeapi.tables.{AgbotPatternRow, AgbotPatternsTQ, AgbotRow, AgbotsTQ, OrgRow, OrgsTQ, ResourceChangesTQ, UserRow, UsersTQ}
import com.horizon.exchangeapi.{ApiTime, ApiUtils, GetAgbotPatternsResponse, HttpCode, Role, TestDBConnection}
import org.json4s.jackson.JsonMethods.parse
import org.json4s.{DefaultFormats, Formats}
import org.json4s.native.Serialization.write
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatest.funsuite.AnyFunSuite
import scalaj.http.{Http, HttpResponse}
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.Await
import scala.concurrent.duration.{Duration, DurationInt}


class AgbotGetPatternRouteTest extends AnyFunSuite with BeforeAndAfterAll with BeforeAndAfterEach {
  private val ACCEPT: (String, String) = ("Content-Type", "application/json")
  // private val AGBOTAUTH: (String, String) = ("Authorization", "Basic " + ApiUtils.encode("AgbotGetPatternRouteTest/a1" + ":" + "a1tok"))
  private val CONTENT: (String, String) = ACCEPT
  private val ROOTAUTH: (String, String) = ("Authorization", "Basic " + ApiUtils.encode(Role.superUser + ":" + sys.env.getOrElse("EXCHANGE_ROOTPW", "")))
  private val URL: String = sys.env.getOrElse("EXCHANGE_URL_ROOT", "http://localhost:8080") + "/v1/orgs/" + "AgbotGetPatternRouteTest"
  // private val USERAUTH: (String, String) = ("Authorization", "Basic " + ApiUtils.encode("AgbotGetPatternRouteTest/u1" + ":" + "u1pw"))
  private val DBCONNECTION: TestDBConnection = new TestDBConnection
  
  private val AWAITDURATION: Duration = 15.seconds
  
  // Resources we minimally and statically need for all test cases.
  private val TESTAGBOTS: Seq[AgbotRow] =
    Seq(AgbotRow(id            = "AgbotGetPatternRouteTest/a0",
                 lastHeartbeat = ApiTime.nowUTC,
                 msgEndPoint   = "",
                 name          = "name1",
                 orgid         = "AgbotGetPatternRouteTest",
                 owner         = "AgbotGetPatternRouteTest/u1",
                 publicKey     = "",
                 token         = ""))
  private val TESTORGANIZATION: OrgRow =
    OrgRow(description        = "",
           heartbeatIntervals = "",
           label              = "",
           lastUpdated        = ApiTime.nowUTC,
           limits             = "",
           orgId              = "AgbotGetPatternRouteTest",
           orgType            = "",
           tags               = None)
  private val TESTUSERS: Seq[UserRow] =
    Seq(UserRow(admin       = false,
                email       = "",
                hashedPw    = "",
                hubAdmin    = false,
                lastUpdated = ApiTime.nowUTC,
                orgid       = "AgbotGetPatternRouteTest",
                updatedBy   = "",
                username    = "AgbotGetPatternRouteTest/u1"),
        UserRow(admin       = false,
                email       = "",
                hashedPw    = "",
                hubAdmin    = false,
                lastUpdated = ApiTime.nowUTC,
                orgid       = "AgbotGetPatternRouteTest",
                updatedBy   = "",
                username    = "AgbotGetPatternRouteTest/u2"),
        UserRow(admin       = false,
                email       = "",
                hashedPw    = "",
                hubAdmin    = false,
                lastUpdated = ApiTime.nowUTC,
                orgid       = "AgbotGetPatternRouteTest",
                updatedBy   = "",
                username    = "AgbotGetPatternRouteTest/v"))
  
  implicit private val formats: Formats = DefaultFormats.withLong
  
  // Begin building testing harness.
  override def beforeAll(): Unit = {
    Await.ready(DBCONNECTION.getDb.run((OrgsTQ.rows += TESTORGANIZATION) andThen
                                       (UsersTQ.rows ++= TESTUSERS) andThen
                                       (AgbotsTQ.rows ++= TESTAGBOTS)), AWAITDURATION)
  }
  
  // Teardown testing harness and cleanup.
  override def afterAll(): Unit = {
    Await.ready(DBCONNECTION.getDb.run(ResourceChangesTQ.rows.filter(_.orgId startsWith "AgbotGetPatternRouteTest").delete andThen
                                       OrgsTQ.rows.filter(_.orgid startsWith "AgbotGetPatternRouteTest").delete), AWAITDURATION)
    
    DBCONNECTION.getDb.close()
  }
  
  // Agreement bots that are dynamically needed, specific to the test case.
  def fixtureAgbotPatterns(testCode: Seq[AgbotPatternRow] => Any, testData: Seq[AgbotPatternRow]): Any = {
    try {
      Await.result(DBCONNECTION.getDb.run(AgbotPatternsTQ.rows ++= testData), AWAITDURATION)
      testCode(testData)
    }
    finally
      Await.result(DBCONNECTION.getDb.run(AgbotPatternsTQ.rows.filter(_.patId inSet testData.map(_.patId)).delete), AWAITDURATION)
  }
  
  test("GET /orgs/" + "AgbotGetPatternRouteTest" + "/agbots/a0/patterns/nonexistentpattern -- 404 Not Found") {
    val response: HttpResponse[String] = Http(URL + "/agbots/a0/patterns/nonexistentpattern").headers(CONTENT).headers(ACCEPT).headers(ROOTAUTH).asString
    
    info("code: " + response.code)
    info("body: " + response.body)
    
    assert(response.code === HttpCode.NOT_FOUND.intValue)
    
    val responseBody: GetAgbotPatternsResponse = parse(response.body).extract[GetAgbotPatternsResponse]
    assert(responseBody.patterns.size === 0)
  }
  
  test("GET /orgs/" + "AgbotGetPatternRouteTest" + "/agbots/a0/patterns/AgbotGetPatternRouteTest-p0 -- Default") {
    val TESTAGBOTPATTERNS: Seq[AgbotPatternRow] =
      Seq(AgbotPatternRow(agbotId      = "AgbotGetPatternRouteTest/a0",
                          lastUpdated  = "",
                          nodeOrgid    = "",
                          patId        = "AgbotGetPatternRouteTest-p0",
                          pattern      = "",
                          patternOrgid = "AgbotGetPatternRouteTest"),
          AgbotPatternRow(agbotId      = "AgbotGetPatternRouteTest/a0",
                          lastUpdated  = "",
                          nodeOrgid    = "",
                          patId        = "AgbotGetPatternRouteTest-p1",
                          pattern      = "",
                          patternOrgid = "AgbotGetPatternRouteTest"))
    
    fixtureAgbotPatterns(
      _ => {
        val response: HttpResponse[String] = Http(URL + "/agbots/a0/patterns/AgbotGetPatternRouteTest-p0").headers(CONTENT).headers(ACCEPT).headers(ROOTAUTH).asString
        
        info("code: " + response.code)
        info("body: " + response.body)
        
        assert(response.code === HttpCode.OK.intValue)
        
        val responseBody: GetAgbotPatternsResponse = parse(response.body).extract[GetAgbotPatternsResponse]
        assert(responseBody.patterns.size === 1)
        assert(responseBody.patterns.contains("AgbotGetPatternRouteTest-p0") === true)
        
        assert(responseBody.patterns("AgbotGetPatternRouteTest-p0").lastUpdated  === TESTAGBOTPATTERNS.head.lastUpdated)
        assert(responseBody.patterns("AgbotGetPatternRouteTest-p0").nodeOrgid    === TESTAGBOTPATTERNS.head.nodeOrgid)
        assert(responseBody.patterns("AgbotGetPatternRouteTest-p0").pattern      === TESTAGBOTPATTERNS.head.pattern)
        assert(responseBody.patterns("AgbotGetPatternRouteTest-p0").patternOrgid === TESTAGBOTPATTERNS.head.patternOrgid)
      }, TESTAGBOTPATTERNS)
  }
}
