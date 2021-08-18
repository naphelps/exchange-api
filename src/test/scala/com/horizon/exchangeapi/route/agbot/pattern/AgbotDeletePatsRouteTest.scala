package com.horizon.exchangeapi.route.agbot.pattern

import com.horizon.exchangeapi.tables.{AgbotPatternRow, AgbotPatternsTQ, AgbotRow, AgbotsTQ, OrgRow, OrgsTQ, ResourceChangeRow, ResourceChangesTQ, UserRow, UsersTQ}
import com.horizon.exchangeapi.{ApiTime, ApiUtils, GetAgbotPatternsResponse, HttpCode, Role, TestDBConnection}
import org.json4s.jackson.JsonMethods.parse
import org.json4s.{DefaultFormats, Formats}
import org.json4s.native.Serialization.write
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatest.funsuite.AnyFunSuite
import scalaj.http.{Http, HttpResponse}
import slick.jdbc.PostgresProfile.api._

import java.time.{ZoneId, ZonedDateTime}
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, DurationInt}


class AgbotDeletePatsRouteTest extends AnyFunSuite with BeforeAndAfterAll with BeforeAndAfterEach {
  private val ACCEPT: (String, String) = ("Content-Type", "application/json")
  // private val AGBOTAUTH: (String, String) = ("Authorization", "Basic " + ApiUtils.encode("AgbotDeletePatsRouteTest/a1" + ":" + "a1tok"))
  private val CONTENT: (String, String) = ACCEPT
  private val ROOTAUTH: (String, String) = ("Authorization", "Basic " + ApiUtils.encode(Role.superUser + ":" + sys.env.getOrElse("EXCHANGE_ROOTPW", "")))
  private val URL: String = sys.env.getOrElse("EXCHANGE_URL_ROOT", "http://localhost:8080") + "/v1/orgs/" + "AgbotDeletePatsRouteTest"
  // private val USERAUTH: (String, String) = ("Authorization", "Basic " + ApiUtils.encode("AgbotDeletePatsRouteTest/u1" + ":" + "u1pw"))
  private val DBCONNECTION: TestDBConnection = new TestDBConnection
  
  private val AWAITDURATION: Duration = 15.seconds
  
  // Resources we minimally and statically need for all test cases.
  private val TESTAGBOTS: Seq[AgbotRow] =
    Seq(AgbotRow(id            = "AgbotDeletePatsRouteTest/a0",
                 lastHeartbeat = ApiTime.nowUTC,
                 msgEndPoint   = "",
                 name          = "name1",
                 orgid         = "AgbotDeletePatsRouteTest",
                 owner         = "AgbotDeletePatsRouteTest/u1",
                 publicKey     = "",
                 token         = ""))
  private val TESTORGANIZATION: OrgRow =
    OrgRow(description        = "",
           heartbeatIntervals = "",
           label              = "",
           lastUpdated        = ApiTime.nowUTC,
           limits             = "",
           orgId              = "AgbotDeletePatsRouteTest",
           orgType            = "",
           tags               = None)
  private val TESTUSERS: Seq[UserRow] =
    Seq(UserRow(admin       = false,
                email       = "",
                hashedPw    = "",
                hubAdmin    = false,
                lastUpdated = ApiTime.nowUTC,
                orgid       = "AgbotDeletePatsRouteTest",
                updatedBy   = "",
                username    = "AgbotDeletePatsRouteTest/u1"),
        UserRow(admin       = false,
                email       = "",
                hashedPw    = "",
                hubAdmin    = false,
                lastUpdated = ApiTime.nowUTC,
                orgid       = "AgbotDeletePatsRouteTest",
                updatedBy   = "",
                username    = "AgbotDeletePatsRouteTest/u2"),
        UserRow(admin       = false,
                email       = "",
                hashedPw    = "",
                hubAdmin    = false,
                lastUpdated = ApiTime.nowUTC,
                orgid       = "AgbotDeletePatsRouteTest",
                updatedBy   = "",
                username    = "AgbotDeletePatsRouteTest/v"))
  
  implicit private val formats: Formats = DefaultFormats.withLong
  
  // Begin building testing harness.
  override def beforeAll(): Unit = {
    Await.ready(DBCONNECTION.getDb.run((OrgsTQ.rows += TESTORGANIZATION) andThen
                                       (UsersTQ.rows ++= TESTUSERS) andThen
                                       (AgbotsTQ.rows ++= TESTAGBOTS)), AWAITDURATION)
  }
  
  // Teardown testing harness and cleanup.
  override def afterAll(): Unit = {
    Await.ready(DBCONNECTION.getDb.run(ResourceChangesTQ.rows.filter(_.orgId startsWith "AgbotDeletePatsRouteTest").delete andThen
                                       OrgsTQ.rows.filter(_.orgid startsWith "AgbotDeletePatsRouteTest").delete), AWAITDURATION)
    
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
  
  test("Delete /orgs/" + "AgbotDeletePatsRouteTest" + "/agbots/a0/patterns -- 404 Not Found - No patterns") {
    val response: HttpResponse[String] = Http(URL + "/agbots/a0/patterns").method("delete").headers(CONTENT).headers(ACCEPT).headers(ROOTAUTH).asString
    
    info("code: " + response.code)
    info("body: " + response.body)
    
    assert(response.code === HttpCode.NOT_FOUND.intValue)
  }
  
  test("GET /orgs/" + "AgbotDeletePatsRouteTest" + "/agbots/a0/patterns -- Default") {
    val TESTAGBOTPATTERNS: Seq[AgbotPatternRow] =
      Seq(AgbotPatternRow(agbotId      = "AgbotDeletePatsRouteTest/a0",
                          lastUpdated  = ApiTime.nowUTC,
                          nodeOrgid    = "",
                          patId        = "AgbotDeletePatsRouteTest/p0",
                          pattern      = "",
                          patternOrgid = "AgbotDeletePatsRouteTest"),
          AgbotPatternRow(agbotId      = "AgbotDeletePatsRouteTest/a0",
                          lastUpdated  = ApiTime.nowUTC,
                          nodeOrgid    = "",
                          patId        = "AgbotDeletePatsRouteTest/p1",
                          pattern      = "",
                          patternOrgid = "AgbotDeletePatsRouteTest"))
    
    fixtureAgbotPatterns(
      _ => {
        val response: HttpResponse[String] = Http(URL + "/agbots/a0/patterns").method("delete").headers(CONTENT).headers(ACCEPT).headers(ROOTAUTH).asString
        
        info("code: " + response.code)
        info("body: " + response.body)
        
        assert(response.code === HttpCode.DELETED.intValue)
        
        val agbotPatterns: Seq[AgbotPatternRow] = Await.result(DBCONNECTION.getDb.run(AgbotPatternsTQ.rows.filter(_.agbotId === "AgbotDeletePatsRouteTest/a0").result), AWAITDURATION)
        assert(agbotPatterns.size === 0)
        
        val resourceChange: Seq[ResourceChangeRow] = Await.result(DBCONNECTION.getDb.run(ResourceChangesTQ.rows.filter(_.id === "a0").filter(_.orgId === "AgbotDeletePatsRouteTest").result), AWAITDURATION)
        assert(resourceChange.size === 1)
  
        assert(TESTAGBOTPATTERNS.head.agbotId      === (resourceChange.head.orgId + "/" + resourceChange.head.id))
        assert(0 <= resourceChange.head.changeId)
        assert(resourceChange.head.category        === "agbot")
        assert(TESTAGBOTPATTERNS.head.lastUpdated   <  ApiTime.fixFormatting(ZonedDateTime.ofInstant(resourceChange.head.lastUpdated.toInstant, ZoneId.of("UTC")).toString))
        assert(resourceChange.head.operation       === "deleted")
        assert(TESTAGBOTPATTERNS.head.patternOrgid === resourceChange.head.orgId)
        assert(resourceChange.head.public          === "false")
        assert(resourceChange.head.resource        === "agbotpatterns")
  
        Await.ready(DBCONNECTION.getDb.run(ResourceChangesTQ.rows.filter(_.id === "a0").filter(_.orgId === "AgbotDeletePatsRouteTest").delete), AWAITDURATION)
      }, TESTAGBOTPATTERNS)
  }
}
