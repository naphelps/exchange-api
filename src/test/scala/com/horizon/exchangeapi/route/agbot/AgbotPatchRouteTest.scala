package com.horizon.exchangeapi.route.agbot

import com.horizon.exchangeapi.{ApiTime, ApiUtils, HttpCode, PatchAgbotsRequest, PutAgbotsRequest, Role, TestDBConnection}
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

class AgbotPatchRouteTest extends AnyFunSuite with BeforeAndAfterAll with BeforeAndAfterEach {
  private val ACCEPT: (String, String) = ("Content-Type", "application/json")
  // private val AGBOTAUTH: (String, String) = ("Authorization", "Basic " + ApiUtils.encode("AgbotPatchRouteTest/a1" + ":" + "a1tok"))
  private val CONTENT: (String, String) = ACCEPT
  private val ROOTAUTH: (String, String) = ("Authorization", "Basic " + ApiUtils.encode(Role.superUser + ":" + sys.env.getOrElse("EXCHANGE_ROOTPW", "")))
  private val URL: String = sys.env.getOrElse("EXCHANGE_URL_ROOT", "http://localhost:8080") + "/v1/orgs/" + "AgbotPatchRouteTest"
  // private val USERAUTH: (String, String) = ("Authorization", "Basic " + ApiUtils.encode("AgbotPatchRouteTest/u1" + ":" + "u1pw"))
  private val DBCONNECTION: TestDBConnection = new TestDBConnection
  
  private val AWAITDURATION: Duration = 15.seconds
  
  // Resources we minimally and statically need for all test cases.
  private val TESTORGANIZATION: OrgRow =
    OrgRow(description        = "",
      heartbeatIntervals = "",
      label              = "",
      lastUpdated        = ApiTime.nowUTC,
      limits             = "",
      orgId              = "AgbotPatchRouteTest",
      orgType            = "",
      tags               = None)
  private val TESTUSERS: Seq[UserRow] =
    Seq(UserRow(admin       = false,
      email       = "",
      hashedPw    = "",
      hubAdmin    = false,
      lastUpdated = ApiTime.nowUTC,
      orgid       = "AgbotPatchRouteTest",
      updatedBy   = "",
      username    = "AgbotPatchRouteTest/u1"),
      UserRow(admin       = false,
        email       = "",
        hashedPw    = "",
        hubAdmin    = false,
        lastUpdated = ApiTime.nowUTC,
        orgid       = "AgbotPatchRouteTest",
        updatedBy   = "",
        username    = "AgbotPatchRouteTest/u2"),
      UserRow(admin       = false,
        email       = "",
        hashedPw    = "",
        hubAdmin    = false,
        lastUpdated = ApiTime.nowUTC,
        orgid       = "AgbotPatchRouteTest",
        updatedBy   = "",
        username    = "AgbotPatchRouteTest/v"))
  
  implicit private val formats: Formats = DefaultFormats.withLong
  
  // Begin building testing harness.
  override def beforeAll(): Unit = {
    Await.ready(DBCONNECTION.getDb.run((OrgsTQ.rows += TESTORGANIZATION) andThen
                                       (UsersTQ.rows ++= TESTUSERS)), AWAITDURATION)
  }
  
  // Teardown testing harness and cleanup.
  override def afterAll(): Unit = {
    Await.ready(DBCONNECTION.getDb.run(ResourceChangesTQ.rows.filter(_.orgId startsWith "AgbotPatchRouteTest").delete andThen
                                       OrgsTQ.rows.filter(_.orgid startsWith "AgbotPatchRouteTest").delete), AWAITDURATION)
    
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
  
  test("PATCH /orgs/" + "AgbotPatchRouteTest" + "/agbots/a0 -- 400 Bad input - Bad Request Body") {
    val BADREQUESTBODY: String =
      """{
        |  "somekey": "somevalue"
        |}""".stripMargin
    
    val response: HttpResponse[String] = Http(URL + "/agbots/a0").postData(write(BADREQUESTBODY)).method("patch").headers(CONTENT).headers(ACCEPT).headers(ROOTAUTH).asString
    info("code: " + response.code)
    info("body: " + response.body)
    
    assert(response.code === HttpCode.BAD_INPUT.intValue)
  }
  
  ignore("PATCH /orgs/" + "AgbotPatchRouteTest" + "/agbots/a0 -- 400 Bad input - Too Many Attributes") {
    val AGBOTUPDATE: PatchAgbotsRequest =
      PatchAgbotsRequest(msgEndPoint = Option("msgEndPoint"),
                         name        = Option("name"),
                         publicKey   = Option("publicKey"),
                         token       = Option("token"))
    val TESTAGBOT: Seq[AgbotRow] =
      Seq(AgbotRow(id            = "AgbotPatchRouteTest/a0",
                   lastHeartbeat = ApiTime.nowUTC,
                   msgEndPoint   = "",
                   name          = "",
                   orgid         = "AgbotPatchRouteTest",
                   owner         = "root/root",
                   publicKey     = "",
                   token         = ""))
    
    fixtureAgbots(
      _ => {
        val response: HttpResponse[String] = Http(URL + "/agbots/a0").postData(write(AGBOTUPDATE)).method("patch").headers(CONTENT).headers(ACCEPT).headers(ROOTAUTH).asString
        info("code: " + response.code)
        info("body: " + response.body)
        assert(response.code === HttpCode.PUT_OK.intValue)
        
        val agbot: Seq[AgbotRow] = Await.result(DBCONNECTION.getDb.run(AgbotsTQ.rows.filter(_.id === "AgbotPatchRouteTest/a0").result), AWAITDURATION)
        assert(agbot.size === 1)
        
        info(agbot.head.id)
        info(agbot.head.lastHeartbeat)
        info(agbot.head.msgEndPoint)
        info(agbot.head.name)
        info(agbot.head.orgid)
        info(agbot.head.owner)
        info(agbot.head.publicKey)
        info(agbot.head.token)
        
        
        assert(TESTAGBOT.head.id            === agbot.head.id)
        assert(TESTAGBOT.head.lastHeartbeat  <  agbot.head.lastHeartbeat)
        assert(AGBOTUPDATE.msgEndPoint  === agbot.head.msgEndPoint)
        assert(TESTAGBOT.head.name          === agbot.head.name)
        assert(TESTAGBOT.head.orgid         === agbot.head.orgid)
        assert(TESTAGBOT.head.owner         === agbot.head.owner)
        assert(TESTAGBOT.head.publicKey     === agbot.head.publicKey)
        assert(TESTAGBOT.head.token         === agbot.head.token)
        
        val resourceChange: Seq[ResourceChangeRow] = Await.result(DBCONNECTION.getDb.run(ResourceChangesTQ.rows.filter(_.id === "a0").filter(_.orgId === "AgbotPatchRouteTest").result), AWAITDURATION)
        assert(resourceChange.size === 1)
        
        assert(TESTAGBOT.head.id             === (resourceChange.head.orgId + "/" + resourceChange.head.id))
        assert(0 <= resourceChange.head.changeId)
        assert(resourceChange.head.category  === "agbot")
        assert(TESTAGBOT.head.lastHeartbeat   <  ApiTime.fixFormatting(ZonedDateTime.ofInstant(resourceChange.head.lastUpdated.toInstant, ZoneId.of("UTC")).toString))
        assert(resourceChange.head.operation === "modified")
        assert(TESTAGBOT.head.orgid          === resourceChange.head.orgId)
        assert(resourceChange.head.public    === "false")
        assert(resourceChange.head.resource  === "agbot")
      }, TESTAGBOT)
  }
  
  test("PATCH /orgs/" + "AgbotPatchRouteTest" + "/agbots/a1 -- Patch msgEndPoint") {
    val AGBOTUPDATE: PatchAgbotsRequest =
      PatchAgbotsRequest(msgEndPoint = Option("msgEndPoint"),
                         name        = None,
                         publicKey   = None,
                         token       = None)
    val TESTAGBOT: Seq[AgbotRow] =
      Seq(AgbotRow(id            = "AgbotPatchRouteTest/a1",
                   lastHeartbeat = ApiTime.nowUTC,
                   msgEndPoint   = "",
                   name          = "",
                   orgid         = "AgbotPatchRouteTest",
                   owner         = "root/root",
                   publicKey     = "",
                   token         = ""))
    
    fixtureAgbots(
      _ => {
        val response: HttpResponse[String] = Http(URL + "/agbots/a1").postData(write(AGBOTUPDATE)).method("patch").headers(CONTENT).headers(ACCEPT).headers(ROOTAUTH).asString
        info("code: " + response.code)
        info("body: " + response.body)
        assert(response.code === HttpCode.PUT_OK.intValue)
        
        val agbot: Seq[AgbotRow] = Await.result(DBCONNECTION.getDb.run(AgbotsTQ.rows.filter(_.id === "AgbotPatchRouteTest/a1").result), AWAITDURATION)
        assert(agbot.size === 1)
        
        assert(TESTAGBOT.head.id            === agbot.head.id)
        assert(TESTAGBOT.head.lastHeartbeat  <  agbot.head.lastHeartbeat)
        assert(AGBOTUPDATE.msgEndPoint.get  === agbot.head.msgEndPoint)
        assert(TESTAGBOT.head.name          === agbot.head.name)
        assert(TESTAGBOT.head.orgid         === agbot.head.orgid)
        assert(TESTAGBOT.head.owner         === agbot.head.owner)
        assert(TESTAGBOT.head.publicKey     === agbot.head.publicKey)
        assert(TESTAGBOT.head.token         === agbot.head.token)
        
        val resourceChange: Seq[ResourceChangeRow] = Await.result(DBCONNECTION.getDb.run(ResourceChangesTQ.rows.filter(_.id === "a1").filter(_.orgId === "AgbotPatchRouteTest").result), AWAITDURATION)
        assert(resourceChange.size === 1)
        
        assert(TESTAGBOT.head.id             === (resourceChange.head.orgId + "/" + resourceChange.head.id))
        assert(0 <= resourceChange.head.changeId)
        assert(resourceChange.head.category  === "agbot")
        assert(TESTAGBOT.head.lastHeartbeat   <  ApiTime.fixFormatting(ZonedDateTime.ofInstant(resourceChange.head.lastUpdated.toInstant, ZoneId.of("UTC")).toString))
        assert(resourceChange.head.operation === "modified")
        assert(TESTAGBOT.head.orgid          === resourceChange.head.orgId)
        assert(resourceChange.head.public    === "false")
        assert(resourceChange.head.resource  === "agbot")
      }, TESTAGBOT)
  }
  
  test("PATCH /orgs/" + "AgbotPatchRouteTest" + "/agbots/a2 -- Patch name") {
    val AGBOTUPDATE: PatchAgbotsRequest =
      PatchAgbotsRequest(msgEndPoint = None,
                         name        = Option("name"),
                         publicKey   = None,
                         token       = None)
    val TESTAGBOT: Seq[AgbotRow] =
      Seq(AgbotRow(id            = "AgbotPatchRouteTest/a2",
                   lastHeartbeat = ApiTime.nowUTC,
                   msgEndPoint   = "",
                   name          = "",
                   orgid         = "AgbotPatchRouteTest",
                   owner         = "root/root",
                   publicKey     = "",
                   token         = ""))
    
    fixtureAgbots(
      _ => {
        val response: HttpResponse[String] = Http(URL + "/agbots/a2").postData(write(AGBOTUPDATE)).method("patch").headers(CONTENT).headers(ACCEPT).headers(ROOTAUTH).asString
        info("code: " + response.code)
        info("body: " + response.body)
        assert(response.code === HttpCode.PUT_OK.intValue)
        
        val agbot: Seq[AgbotRow] = Await.result(DBCONNECTION.getDb.run(AgbotsTQ.rows.filter(_.id === "AgbotPatchRouteTest/a2").result), AWAITDURATION)
        assert(agbot.size === 1)
        
        assert(TESTAGBOT.head.id            === agbot.head.id)
        assert(TESTAGBOT.head.lastHeartbeat  <  agbot.head.lastHeartbeat)
        assert(TESTAGBOT.head.msgEndPoint   === agbot.head.msgEndPoint)
        assert(AGBOTUPDATE.name.get         === agbot.head.name)
        assert(TESTAGBOT.head.orgid         === agbot.head.orgid)
        assert(TESTAGBOT.head.owner         === agbot.head.owner)
        assert(TESTAGBOT.head.publicKey     === agbot.head.publicKey)
        assert(TESTAGBOT.head.token         === agbot.head.token)
        
        val resourceChange: Seq[ResourceChangeRow] = Await.result(DBCONNECTION.getDb.run(ResourceChangesTQ.rows.filter(_.id === "a2").filter(_.orgId === "AgbotPatchRouteTest").result), AWAITDURATION)
        assert(resourceChange.size === 1)
        
        assert(TESTAGBOT.head.id             === (resourceChange.head.orgId + "/" + resourceChange.head.id))
        assert(0 <= resourceChange.head.changeId)
        assert(resourceChange.head.category  === "agbot")
        assert(TESTAGBOT.head.lastHeartbeat   <  ApiTime.fixFormatting(ZonedDateTime.ofInstant(resourceChange.head.lastUpdated.toInstant, ZoneId.of("UTC")).toString))
        assert(resourceChange.head.operation === "modified")
        assert(TESTAGBOT.head.orgid          === resourceChange.head.orgId)
        assert(resourceChange.head.public    === "false")
        assert(resourceChange.head.resource  === "agbot")
      }, TESTAGBOT)
  }
  
  test("PATCH /orgs/" + "AgbotPatchRouteTest" + "/agbots/a3 -- Patch publicKey") {
    val AGBOTUPDATE: PatchAgbotsRequest =
      PatchAgbotsRequest(msgEndPoint = None,
                         name        = None,
                         publicKey   = Option("publicKey"),
                         token       = None)
    val TESTAGBOT: Seq[AgbotRow] =
      Seq(AgbotRow(id            = "AgbotPatchRouteTest/a3",
                   lastHeartbeat = ApiTime.nowUTC,
                   msgEndPoint   = "",
                   name          = "",
                   orgid         = "AgbotPatchRouteTest",
                   owner         = "root/root",
                   publicKey     = "",
                   token         = ""))
    
    fixtureAgbots(
      _ => {
        val response: HttpResponse[String] = Http(URL + "/agbots/a3").postData(write(AGBOTUPDATE)).method("patch").headers(CONTENT).headers(ACCEPT).headers(ROOTAUTH).asString
        info("code: " + response.code)
        info("body: " + response.body)
        assert(response.code === HttpCode.PUT_OK.intValue)
        
        val agbot: Seq[AgbotRow] = Await.result(DBCONNECTION.getDb.run(AgbotsTQ.rows.filter(_.id === "AgbotPatchRouteTest/a3").result), AWAITDURATION)
        assert(agbot.size === 1)
        
        assert(TESTAGBOT.head.id            === agbot.head.id)
        assert(TESTAGBOT.head.lastHeartbeat  <  agbot.head.lastHeartbeat)
        assert(TESTAGBOT.head.msgEndPoint   === agbot.head.msgEndPoint)
        assert(TESTAGBOT.head.name          === agbot.head.name)
        assert(TESTAGBOT.head.orgid         === agbot.head.orgid)
        assert(TESTAGBOT.head.owner         === agbot.head.owner)
        assert(AGBOTUPDATE.publicKey.get    === agbot.head.publicKey)
        assert(TESTAGBOT.head.token         === agbot.head.token)
        
        val resourceChange: Seq[ResourceChangeRow] = Await.result(DBCONNECTION.getDb.run(ResourceChangesTQ.rows.filter(_.id === "a3").filter(_.orgId === "AgbotPatchRouteTest").result), AWAITDURATION)
        assert(resourceChange.size === 1)
        
        assert(TESTAGBOT.head.id             === (resourceChange.head.orgId + "/" + resourceChange.head.id))
        assert(0 <= resourceChange.head.changeId)
        assert(resourceChange.head.category  === "agbot")
        assert(TESTAGBOT.head.lastHeartbeat   <  ApiTime.fixFormatting(ZonedDateTime.ofInstant(resourceChange.head.lastUpdated.toInstant, ZoneId.of("UTC")).toString))
        assert(resourceChange.head.operation === "modified")
        assert(TESTAGBOT.head.orgid          === resourceChange.head.orgId)
        assert(resourceChange.head.public    === "false")
        assert(resourceChange.head.resource  === "agbot")
      }, TESTAGBOT)
  }
  
  test("PATCH /orgs/" + "AgbotPatchRouteTest" + "/agbots/a4 -- Patch publicKey") {
    val AGBOTUPDATE: PatchAgbotsRequest =
      PatchAgbotsRequest(msgEndPoint = None,
        name        = None,
        publicKey   = None,
        token       = Option("token"))
    val TESTAGBOT: Seq[AgbotRow] =
      Seq(AgbotRow(id            = "AgbotPatchRouteTest/a4",
        lastHeartbeat = ApiTime.nowUTC,
        msgEndPoint   = "",
        name          = "",
        orgid         = "AgbotPatchRouteTest",
        owner         = "root/root",
        publicKey     = "",
        token         = ""))
    
    fixtureAgbots(
      _ => {
        val response: HttpResponse[String] = Http(URL + "/agbots/a4").postData(write(AGBOTUPDATE)).method("patch").headers(CONTENT).headers(ACCEPT).headers(ROOTAUTH).asString
        info("code: " + response.code)
        info("body: " + response.body)
        assert(response.code === HttpCode.PUT_OK.intValue)
        
        val agbot: Seq[AgbotRow] = Await.result(DBCONNECTION.getDb.run(AgbotsTQ.rows.filter(_.id === "AgbotPatchRouteTest/a4").result), AWAITDURATION)
        assert(agbot.size === 1)
        
        assert(TESTAGBOT.head.id            === agbot.head.id)
        assert(TESTAGBOT.head.lastHeartbeat  <  agbot.head.lastHeartbeat)
        assert(TESTAGBOT.head.msgEndPoint   === agbot.head.msgEndPoint)
        assert(TESTAGBOT.head.name          === agbot.head.name)
        assert(TESTAGBOT.head.orgid         === agbot.head.orgid)
        assert(TESTAGBOT.head.owner         === agbot.head.owner)
        assert(TESTAGBOT.head.publicKey     === agbot.head.publicKey)
        assert(TESTAGBOT.head.token         !== agbot.head.token)
        
        val resourceChange: Seq[ResourceChangeRow] = Await.result(DBCONNECTION.getDb.run(ResourceChangesTQ.rows.filter(_.id === "a4").filter(_.orgId === "AgbotPatchRouteTest").result), AWAITDURATION)
        assert(resourceChange.size === 1)
        
        assert(TESTAGBOT.head.id             === (resourceChange.head.orgId + "/" + resourceChange.head.id))
        assert(0 <= resourceChange.head.changeId)
        assert(resourceChange.head.category  === "agbot")
        assert(TESTAGBOT.head.lastHeartbeat   <  ApiTime.fixFormatting(ZonedDateTime.ofInstant(resourceChange.head.lastUpdated.toInstant, ZoneId.of("UTC")).toString))
        assert(resourceChange.head.operation === "modified")
        assert(TESTAGBOT.head.orgid          === resourceChange.head.orgId)
        assert(resourceChange.head.public    === "false")
        assert(resourceChange.head.resource  === "agbot")
      }, TESTAGBOT)
  }
}
