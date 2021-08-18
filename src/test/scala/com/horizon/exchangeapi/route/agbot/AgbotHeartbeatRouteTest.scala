package com.horizon.exchangeapi.route.agbot

import com.horizon.exchangeapi.{ApiTime, ApiUtils, HttpCode, PutAgbotsRequest, Role, TestDBConnection}
import com.horizon.exchangeapi.tables.{AgbotRow, AgbotsTQ, OrgRow, OrgsTQ, ResourceChangeRow, ResourceChangesTQ, UserRow, UsersTQ}
import org.json4s.{DefaultFormats, Formats}
import org.json4s.jackson.JsonMethods.parse
import org.json4s.native.Serialization.write
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatest.funsuite.AnyFunSuite
import scalaj.http.{Http, HttpResponse}
import slick.jdbc.PostgresProfile.api._

import java.time.{ZoneId, ZonedDateTime}
import javax.ws.rs.DELETE
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, DurationInt}

class AgbotHeartbeatRouteTest extends AnyFunSuite with BeforeAndAfterAll with BeforeAndAfterEach {
  private val ACCEPT: (String, String) = ("Content-Type", "application/json")
  // private val AGBOTAUTH: (String, String) = ("Authorization", "Basic " + ApiUtils.encode("AgbotHeartbeatRouteTest/a1" + ":" + "a1tok"))
  private val CONTENT: (String, String) = ACCEPT
  private val ROOTAUTH: (String, String) = ("Authorization", "Basic " + ApiUtils.encode(Role.superUser + ":" + sys.env.getOrElse("EXCHANGE_ROOTPW", "")))
  private val URL: String = sys.env.getOrElse("EXCHANGE_URL_ROOT", "http://localhost:8080") + "/v1/orgs/" + "AgbotHeartbeatRouteTest"
  // private val USERAUTH: (String, String) = ("Authorization", "Basic " + ApiUtils.encode("AgbotHeartbeatRouteTest/u1" + ":" + "u1pw"))
  private val DBCONNECTION: TestDBConnection = new TestDBConnection
  
  private val AWAITDURATION: Duration = 15.seconds
  
  // Resources we minimally and statically need for all test cases.
  private val TESTAGBOTS: AgbotRow =
    AgbotRow(id            = "AgbotHeartbeatRouteTest/a0",
      lastHeartbeat = ApiTime.nowUTC,
      msgEndPoint   = "",
      name          = "",
      orgid         = "AgbotHeartbeatRouteTest",
      owner         = "root/root",
      publicKey     = "",
      token         = "")
  private val TESTORGANIZATION: OrgRow =
    OrgRow(description        = "",
      heartbeatIntervals = "",
      label              = "",
      lastUpdated        = ApiTime.nowUTC,
      limits             = "",
      orgId              = "AgbotHeartbeatRouteTest",
      orgType            = "",
      tags               = None)
  private val TESTUSERS: Seq[UserRow] =
    Seq(UserRow(admin       = false,
      email       = "",
      hashedPw    = "",
      hubAdmin    = false,
      lastUpdated = ApiTime.nowUTC,
      orgid       = "AgbotHeartbeatRouteTest",
      updatedBy   = "",
      username    = "AgbotHeartbeatRouteTest/u1"),
      UserRow(admin       = false,
        email       = "",
        hashedPw    = "",
        hubAdmin    = false,
        lastUpdated = ApiTime.nowUTC,
        orgid       = "AgbotHeartbeatRouteTest",
        updatedBy   = "",
        username    = "AgbotHeartbeatRouteTest/u2"),
      UserRow(admin       = false,
        email       = "",
        hashedPw    = "",
        hubAdmin    = false,
        lastUpdated = ApiTime.nowUTC,
        orgid       = "AgbotHeartbeatRouteTest",
        updatedBy   = "",
        username    = "AgbotHeartbeatRouteTest/v"))
  
  implicit private val formats: Formats = DefaultFormats.withLong
  
  // Begin building testing harness.
  override def beforeAll(): Unit = {
    Await.ready(DBCONNECTION.getDb.run((OrgsTQ.rows += TESTORGANIZATION) andThen
                                       (AgbotsTQ.rows += TESTAGBOTS) andThen
                                       (UsersTQ.rows ++= TESTUSERS)), AWAITDURATION)
  }
  
  // Teardown testing harness and cleanup.
  override def afterAll(): Unit = {
    Await.ready(DBCONNECTION.getDb.run(ResourceChangesTQ.rows.filter(_.orgId startsWith "AgbotHeartbeatRouteTest").delete andThen
                                       OrgsTQ.rows.filter(_.orgid startsWith "AgbotHeartbeatRouteTest").delete), AWAITDURATION)
    
    DBCONNECTION.getDb.close()
  }
  
  // Agreement bots that are dynamically needed, specific to the test case.
  def fixtureAgbots(testCode: Seq[AgbotRow] => Any, testData: Seq[AgbotRow]): Any = {
    try {
      Await.result(DBCONNECTION.getDb.run(AgbotsTQ.rows ++= testData), AWAITDURATION)
      testCode(testData)
    }
    finally
      Await.result(DBCONNECTION.getDb.run(AgbotsTQ.rows.filter(_.id inSet testData.map(_.id)).delete), AWAITDURATION)
  }
  
  test("POST /orgs/" + "AgbotHeartbeatRouteTest" + "/agbots/nonexistentagbot/heartbeat -- 404 Not Found") {
    val response: HttpResponse[String] = Http(URL + "/agbots/nonexistentagbot/heartbeat").method("post").headers(ACCEPT).headers(ROOTAUTH).asString
    info("code: " + response.code)
    info("body: " + response.body)
    assert(response.code === HttpCode.NOT_FOUND.intValue)
  }
  
  test("POST /orgs/" + "AgbotHeartbeatRouteTest" + "/agbots/a0/heartbeat -- Default Heartbeat") {
    val response: HttpResponse[String] = Http(URL + "/agbots/a0/heartbeat").method("post").headers(ACCEPT).headers(ROOTAUTH).asString
    info("code: " + response.code)
    info("body: " + response.body)
    assert(response.code === HttpCode.POST_OK.intValue)
    
    val agbot: Seq[AgbotRow] = Await.result(DBCONNECTION.getDb.run(AgbotsTQ.rows.filter(_.id === "AgbotHeartbeatRouteTest/a0").result), AWAITDURATION)
    assert(agbot.size === 1)
  
    assert(TESTAGBOTS.id            === agbot.head.id)
    assert(TESTAGBOTS.lastHeartbeat  <  agbot.head.lastHeartbeat)
    assert(TESTAGBOTS.msgEndPoint   === agbot.head.msgEndPoint)
    assert(TESTAGBOTS.name          === agbot.head.name)
    assert(TESTAGBOTS.orgid         === agbot.head.orgid)
    assert(TESTAGBOTS.owner         === agbot.head.owner)
    assert(TESTAGBOTS.publicKey     === agbot.head.publicKey)
    assert(TESTAGBOTS.token         === agbot.head.token)
  }
}
