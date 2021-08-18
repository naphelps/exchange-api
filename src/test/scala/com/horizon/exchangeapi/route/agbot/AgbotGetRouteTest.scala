package com.horizon.exchangeapi.route.agbot

import com.horizon.exchangeapi.{ApiTime, ApiUtils, GetAgbotsResponse, HttpCode, Role, TestDBConnection}
import com.horizon.exchangeapi.tables.{AgbotRow, AgbotsTQ, OrgRow, OrgsTQ, ResourceChangesTQ, ServicesTQ, UserRow, UsersTQ}
import org.json4s.{DefaultFormats, Formats}
import org.json4s.jackson.JsonMethods.parse
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatest.funsuite.AnyFunSuite
import scalaj.http.{Http, HttpResponse}
import slick.jdbc.PostgresProfile.api._

import scala.collection.Searching.Found
import scala.concurrent.Await
import scala.concurrent.duration.{Duration, DurationInt}

class AgbotGetRouteTest extends AnyFunSuite with BeforeAndAfterAll with BeforeAndAfterEach {
  private val ACCEPT: (String, String) = ("Content-Type", "application/json")
  // private val AGBOTAUTH: (String, String) = ("Authorization", "Basic " + ApiUtils.encode("AgbotGetRouteTest/a1" + ":" + "a1tok"))
  private val CONTENT: (String, String) = ACCEPT
  private val ROOTAUTH: (String, String) = ("Authorization", "Basic " + ApiUtils.encode(Role.superUser + ":" + sys.env.getOrElse("EXCHANGE_ROOTPW", "")))
  private val URL: String = sys.env.getOrElse("EXCHANGE_URL_ROOT", "http://localhost:8080") + "/v1/orgs/" + "AgbotGetRouteTest"
  // private val USERAUTH: (String, String) = ("Authorization", "Basic " + ApiUtils.encode("AgbotGetRouteTest/u1" + ":" + "u1pw"))
  private val DBCONNECTION: TestDBConnection = new TestDBConnection
  
  private val AWAITDURATION: Duration = 15.seconds
  
  // Resources we minimally and statically need for all test cases.
  private val TESTAGBOTS: Seq[AgbotRow] =
    Seq(AgbotRow(id            = "AgbotGetRouteTest/a1",
      lastHeartbeat = ApiTime.nowUTC,
      msgEndPoint   = "",
      name          = "name1",
      orgid         = "AgbotGetRouteTest",
      owner         = "AgbotGetRouteTest/u1",
      publicKey     = "",
      token         = ""),
      AgbotRow(id            = "AgbotGetRouteTest/a2",
        lastHeartbeat = ApiTime.nowUTC,
        msgEndPoint   = "",
        name          = "name2",
        orgid         = "AgbotGetRouteTest",
        owner         = "AgbotGetRouteTest/u2",
        publicKey     = "",
        token         = ""),
      AgbotRow(id            = "AgbotGetRouteTest/b",
        lastHeartbeat = ApiTime.nowUTC,
        msgEndPoint   = "",
        name          = "",
        orgid         = "AgbotGetRouteTest",
        owner         = "AgbotGetRouteTest/v",
        publicKey     = "",
        token         = ""))
  private val TESTORGANIZATION: OrgRow =
    OrgRow(description        = "",
      heartbeatIntervals = "",
      label              = "",
      lastUpdated        = ApiTime.nowUTC,
      limits             = "",
      orgId              = "AgbotGetRouteTest",
      orgType            = "",
      tags               = None)
  private val TESTUSERS: Seq[UserRow] =
    Seq(UserRow(admin       = false,
      email       = "",
      hashedPw    = "",
      hubAdmin    = false,
      lastUpdated = ApiTime.nowUTC,
      orgid       = "AgbotGetRouteTest",
      updatedBy   = "",
      username    = "AgbotGetRouteTest/u1"),
      UserRow(admin       = false,
        email       = "",
        hashedPw    = "",
        hubAdmin    = false,
        lastUpdated = ApiTime.nowUTC,
        orgid       = "AgbotGetRouteTest",
        updatedBy   = "",
        username    = "AgbotGetRouteTest/u2"),
      UserRow(admin       = false,
        email       = "",
        hashedPw    = "",
        hubAdmin    = false,
        lastUpdated = ApiTime.nowUTC,
        orgid       = "AgbotGetRouteTest",
        updatedBy   = "",
        username    = "AgbotGetRouteTest/v"))
  
  implicit private val formats: Formats = DefaultFormats.withLong
  
  // Begin building testing harness.
  override def beforeAll(): Unit = {
    Await.ready(DBCONNECTION.getDb.run((OrgsTQ.rows += TESTORGANIZATION) andThen
                                       (UsersTQ.rows ++= TESTUSERS)), AWAITDURATION)
  }
  
  // Teardown testing harness and cleanup.
  override def afterAll(): Unit = {
    Await.ready(DBCONNECTION.getDb.run(ResourceChangesTQ.rows.filter(_.orgId startsWith "AgbotGetRouteTest").delete andThen
                                       OrgsTQ.rows.filter(_.orgid startsWith "AgbotGetRouteTest").delete), AWAITDURATION)
    
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
  
  test("GET /orgs/" + "AgbotGetRouteTest" + "/agbot/" + "nonexistentagbot -- 404 Not Found - Agbot Does Not Exist") {
    val response: HttpResponse[String] = Http(URL + "/agbot/" + "nonexistentagbot").headers(CONTENT).headers(ACCEPT).headers(ROOTAUTH).asString
    
    info("code: " + response.code)
    info("body: " + response.body)
    
    assert(response.code === HttpCode.NOT_FOUND.intValue)
  }
  
  test("GET /orgs/" + "AgbotGetRouteTest" + "/agbots/" + "AgbotGetRouteTest/a3 -- 400 Invalid Input - Agbot Attribute Does Not Exist") {
    val TESTAGBOT: Seq[AgbotRow] =
      Seq(AgbotRow(id            = "AgbotGetRouteTest/a3",
                   lastHeartbeat = ApiTime.nowUTC,
                   msgEndPoint   = "",
                   name          = "",
                   orgid         = "AgbotGetRouteTest",
                   owner         = "AgbotGetRouteTest/u1",
                   publicKey     = "",
                   token         = ""))
    
    fixtureAgbots(
      _ => {
        val response: HttpResponse[String] = Http(URL + "/agbots/" + "a3").param("attribute", "nonexistentattribute").headers(CONTENT).headers(ACCEPT).headers(ROOTAUTH).asString
  
        info("code: " + response.code)
        info("body: " + response.body)
  
        assert(response.code === HttpCode.BAD_INPUT.intValue)
      }, TESTAGBOT)
  }
  
  ignore("GET /orgs/" + "AgbotGetRouteTest" + "/agbots/a3 -- Default") {
    val TESTAGBOT: Seq[AgbotRow] =
      Seq(AgbotRow(id            = "AgbotGetRouteTest/a3",
        lastHeartbeat = ApiTime.nowUTC,
        msgEndPoint   = "msgEndPoint",
        name          = "name",
        orgid         = "AgbotGetRouteTest",
        owner         = "AgbotGetRouteTest/u1",
        publicKey     = "publicKey",
        token         = "token"))
    
    fixtureAgbots(
      _ => {
        val response: HttpResponse[String] = Http(URL + "/agbots/a3").headers(CONTENT).headers(ACCEPT).headers(ROOTAUTH).asString
        
        info("code: " + response.code)
        info("body: " + response.body)
        
        assert(response.code === HttpCode.OK.intValue)
        
        val responseBody: GetAgbotsResponse = parse(response.body).extract[GetAgbotsResponse]
        assert(responseBody.agbots.size === 1)
        assert(responseBody.agbots.contains("AgbotGetRouteTest/a3") === true)
        
        assert(responseBody.agbots("AgbotGetRouteTest/a3").lastHeartbeat === TESTAGBOT.head.lastHeartbeat)
        assert(responseBody.agbots("AgbotGetRouteTest/a3").msgEndPoint   === TESTAGBOT.head.msgEndPoint)
        assert(responseBody.agbots("AgbotGetRouteTest/a3").name          === TESTAGBOT.head.name)
        assert(responseBody.agbots("AgbotGetRouteTest/a3").owner         === TESTAGBOT.head.owner)
        assert(responseBody.agbots("AgbotGetRouteTest/a3").publicKey     === TESTAGBOT.head.publicKey)
        assert(responseBody.agbots("AgbotGetRouteTest/a3").token         === TESTAGBOT.head.token)
      }, TESTAGBOT)
  }
}
