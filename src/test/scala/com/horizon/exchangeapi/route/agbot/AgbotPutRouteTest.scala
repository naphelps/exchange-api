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
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, DurationInt}

class AgbotPutRouteTest extends AnyFunSuite with BeforeAndAfterAll with BeforeAndAfterEach {
  private val ACCEPT: (String, String) = ("Content-Type", "application/json")
  // private val AGBOTAUTH: (String, String) = ("Authorization", "Basic " + ApiUtils.encode("AgbotPutRouteTest/a1" + ":" + "a1tok"))
  private val CONTENT: (String, String) = ACCEPT
  private val ROOTAUTH: (String, String) = ("Authorization", "Basic " + ApiUtils.encode(Role.superUser + ":" + sys.env.getOrElse("EXCHANGE_ROOTPW", "")))
  private val URL: String = sys.env.getOrElse("EXCHANGE_URL_ROOT", "http://localhost:8080") + "/v1/orgs/" + "AgbotPutRouteTest"
  // private val USERAUTH: (String, String) = ("Authorization", "Basic " + ApiUtils.encode("AgbotPutRouteTest/u1" + ":" + "u1pw"))
  private val DBCONNECTION: TestDBConnection = new TestDBConnection
  
  private val AWAITDURATION: Duration = 15.seconds
  
  // Resources we minimally and statically need for all test cases.
  private val TESTORGANIZATION: OrgRow =
    OrgRow(description        = "",
           heartbeatIntervals = "",
           label              = "",
           lastUpdated        = ApiTime.nowUTC,
           limits             = "",
           orgId              = "AgbotPutRouteTest",
           orgType            = "",
           tags               = None)
  private val TESTUSERS: Seq[UserRow] =
    Seq(UserRow(admin       = false,
                email       = "",
                hashedPw    = "",
                hubAdmin    = false,
                lastUpdated = ApiTime.nowUTC,
                orgid       = "AgbotPutRouteTest",
                updatedBy   = "",
                username    = "AgbotPutRouteTest/u1"),
        UserRow(admin       = false,
                email       = "",
                hashedPw    = "",
                hubAdmin    = false,
                lastUpdated = ApiTime.nowUTC,
                orgid       = "AgbotPutRouteTest",
                updatedBy   = "",
                username    = "AgbotPutRouteTest/u2"),
        UserRow(admin       = false,
                email       = "",
                hashedPw    = "",
                hubAdmin    = false,
                lastUpdated = ApiTime.nowUTC,
                orgid       = "AgbotPutRouteTest",
                updatedBy   = "",
                username    = "AgbotPutRouteTest/v"))
  
  implicit private val formats: Formats = DefaultFormats.withLong
  
  // Begin building testing harness.
  override def beforeAll(): Unit = {
    Await.ready(DBCONNECTION.getDb.run((OrgsTQ.rows += TESTORGANIZATION) andThen
                                       (UsersTQ.rows ++= TESTUSERS)), AWAITDURATION)
  }
  
  // Teardown testing harness and cleanup.
  override def afterAll(): Unit = {
    Await.ready(DBCONNECTION.getDb.run(ResourceChangesTQ.rows.filter(_.orgId startsWith "AgbotPutRouteTest").delete andThen
                                       OrgsTQ.rows.filter(_.orgid startsWith "AgbotPutRouteTest").delete), AWAITDURATION)
    
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
  
  test("PUT /orgs/" + "AgbotPutRouteTest" + "/agbots/a0 -- 400 Bad input - Bad Request Body") {
    val BADREQUESTBODY: String =
      """{
        |  "somekey": "somevalue"
        |}""".stripMargin
    
    val response: HttpResponse[String] = Http(URL + "/agbots/a0").put(write(BADREQUESTBODY)).headers(CONTENT).headers(ACCEPT).headers(ROOTAUTH).asString
    info("code: " + response.code)
    info("body: " + response.body)
    
    assert(response.code === HttpCode.BAD_INPUT.intValue)
  }
  
  test("PUT /orgs/" + "AgbotPutRouteTest" + "/agbots/a1 -- 400 Bad input - Empty Token") {
    val TESTAGBOT: PutAgbotsRequest =
      PutAgbotsRequest(msgEndPoint = None,
                       name        = "",
                       publicKey   = "",
                       token       = "")
    
    val response: HttpResponse[String] = Http(URL + "/agbots/a1").put(write(TESTAGBOT)).headers(CONTENT).headers(ACCEPT).headers(ROOTAUTH).asString
    info("code: " + response.code)
    info("body: " + response.body)
    
    assert(response.code === HttpCode.BAD_INPUT.intValue)
  }
  
  test("PUT /orgs/" + "AgbotPutRouteTest" + "/agbots/a2 -- Default Insert") {
    val TESTAGBOT: PutAgbotsRequest =
      PutAgbotsRequest(msgEndPoint = None,
                       name        = "name",
                       publicKey   = "publickey",
                       token       = "token")
    
    val response: HttpResponse[String] = Http(URL + "/agbots/a2").put(write(TESTAGBOT)).headers(CONTENT).headers(ACCEPT).headers(ROOTAUTH).asString
    info("code: " + response.code)
    info("body: " + response.body)
    assert(response.code === HttpCode.PUT_OK.intValue)
    
    val agbot: Seq[AgbotRow] = Await.result(DBCONNECTION.getDb.run(AgbotsTQ.rows.filter(_.id === "AgbotPutRouteTest/a2").result), AWAITDURATION)
    assert(agbot.size === 1)
    
    assert(agbot.head.id === "AgbotPutRouteTest/a2")
    assert(agbot.head.lastHeartbeat.nonEmpty)
    assert(agbot.head.msgEndPoint === TESTAGBOT.msgEndPoint.getOrElse(""))
    assert(agbot.head.name === TESTAGBOT.name)
    assert(agbot.head.orgid === "AgbotPutRouteTest")
    assert(agbot.head.owner === "root/root")
    assert(agbot.head.publicKey === TESTAGBOT.publicKey)
    assert(agbot.head.token.nonEmpty)
    
    val resourceChange: Seq[ResourceChangeRow] = Await.result(DBCONNECTION.getDb.run(ResourceChangesTQ.rows.filter(_.id === "a2").filter(_.orgId === "AgbotPutRouteTest").result), AWAITDURATION)
    assert(resourceChange.size === 1)
    
    assert(resourceChange.head.id        === "a2")
    assert(0 <= resourceChange.head.changeId)
    assert(resourceChange.head.category  === "agbot")
    assert(resourceChange.head.lastUpdated.toString.nonEmpty)
    assert(resourceChange.head.operation === "created/modified")
    assert(resourceChange.head.orgId     === "AgbotPutRouteTest")
    assert(resourceChange.head.public    === "false")
    assert(resourceChange.head.resource  === "agbot")
    
    Await.ready(DBCONNECTION.getDb.run(AgbotsTQ.rows.filter(_.id === agbot.head.id).delete andThen
                                       ResourceChangesTQ.rows.filter(_.changeId === resourceChange.head.changeId).delete), AWAITDURATION)
  }
  
  test("PUT /orgs/" + "AgbotPutRouteTest" + "/agbots/a3 -- Default Update") {
    val AGBOTUPDATE: PutAgbotsRequest =
      PutAgbotsRequest(msgEndPoint = Option("msgEndPoint"),
        name        = "name",
        publicKey   = "publickey",
        token       = "token")
    val TESTAGBOT: Seq[AgbotRow] =
      Seq(AgbotRow(id            = "AgbotPutRouteTest/a3",
                   lastHeartbeat = ApiTime.nowUTC,
                   msgEndPoint   = "",
                   name          = "",
                   orgid         = "AgbotPutRouteTest",
                   owner         = "root/root",
                   publicKey     = "",
                   token         = ""))
    
    fixtureAgbots(
      _ => {
        val response: HttpResponse[String] = Http(URL + "/agbots/a3").put(write(AGBOTUPDATE)).headers(CONTENT).headers(ACCEPT).headers(ROOTAUTH).asString
        info("code: " + response.code)
        info("body: " + response.body)
        assert(response.code === HttpCode.PUT_OK.intValue)
  
        val agbot: Seq[AgbotRow] = Await.result(DBCONNECTION.getDb.run(AgbotsTQ.rows.filter(_.id === "AgbotPutRouteTest/a3").result), AWAITDURATION)
        assert(agbot.size === 1)
  
        assert(TESTAGBOT.head.id            === agbot.head.id)
        assert(TESTAGBOT.head.lastHeartbeat  <  agbot.head.lastHeartbeat)
        assert(AGBOTUPDATE.msgEndPoint.get  === agbot.head.msgEndPoint)
        assert(AGBOTUPDATE.name             === agbot.head.name)
        assert(TESTAGBOT.head.orgid         === agbot.head.orgid)
        assert(TESTAGBOT.head.owner         === agbot.head.owner)
        assert(AGBOTUPDATE.publicKey        === agbot.head.publicKey)
        assert(TESTAGBOT.head.token         !== agbot.head.token)
  
        val resourceChange: Seq[ResourceChangeRow] = Await.result(DBCONNECTION.getDb.run(ResourceChangesTQ.rows.filter(_.id === "a3").filter(_.orgId === "AgbotPutRouteTest").result), AWAITDURATION)
        assert(resourceChange.size === 1)
  
        assert(TESTAGBOT.head.id             === (resourceChange.head.orgId + "/" + resourceChange.head.id))
        assert(0 <= resourceChange.head.changeId)
        assert(resourceChange.head.category  === "agbot")
        assert(TESTAGBOT.head.lastHeartbeat   <  ApiTime.fixFormatting(ZonedDateTime.ofInstant(resourceChange.head.lastUpdated.toInstant, ZoneId.of("UTC")).toString))
        assert(resourceChange.head.operation === "created/modified")
        assert(TESTAGBOT.head.orgid          === resourceChange.head.orgId)
        assert(resourceChange.head.public    === "false")
        assert(resourceChange.head.resource  === "agbot")
  
        Await.ready(DBCONNECTION.getDb.run(ResourceChangesTQ.rows.filter(_.id === "a3").filter(_.orgId === "AgbotPutRouteTest").delete), AWAITDURATION)
      }, TESTAGBOT)
  }
}
