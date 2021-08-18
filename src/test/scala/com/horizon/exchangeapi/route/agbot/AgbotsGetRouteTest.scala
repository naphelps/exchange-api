package com.horizon.exchangeapi.route.agbot

import com.horizon.exchangeapi.tables.{AgbotRow, AgbotsTQ, OrgRow, OrgsTQ, ResourceChangesTQ, UserRow, UsersTQ}
import com.horizon.exchangeapi.{ApiTime, ApiUtils, GetAgbotsResponse, HttpCode, Role, TestDBConnection}
import org.json4s.jackson.JsonMethods.parse
import org.json4s.{DefaultFormats, Formats}
import org.json4s.native.Serialization.write
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach}
import org.scalatest.funsuite.AnyFunSuite
import scalaj.http.{Http, HttpResponse}
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.Await
import scala.concurrent.duration.{Duration, DurationInt}


class AgbotsGetRouteTest extends AnyFunSuite with BeforeAndAfterAll with BeforeAndAfterEach {
  private val ACCEPT: (String, String) = ("Content-Type", "application/json")
  // private val AGBOTAUTH: (String, String) = ("Authorization", "Basic " + ApiUtils.encode("AgbotsGetRouteTest/a1" + ":" + "a1tok"))
  private val CONTENT: (String, String) = ACCEPT
  private val ROOTAUTH: (String, String) = ("Authorization", "Basic " + ApiUtils.encode(Role.superUser + ":" + sys.env.getOrElse("EXCHANGE_ROOTPW", "")))
  private val URL: String = sys.env.getOrElse("EXCHANGE_URL_ROOT", "http://localhost:8080") + "/v1/orgs/" + "AgbotsGetRouteTest"
  // private val USERAUTH: (String, String) = ("Authorization", "Basic " + ApiUtils.encode("AgbotsGetRouteTest/u1" + ":" + "u1pw"))
  private val DBCONNECTION: TestDBConnection = new TestDBConnection
  
  private val AWAITDURATION: Duration = 15.seconds
  
  // Resources we minimally and statically need for all test cases.
  private val TESTAGBOTS: Seq[AgbotRow] =
    Seq(AgbotRow(id            = "AgbotsGetRouteTest/a1",
                 lastHeartbeat = ApiTime.nowUTC,
                 msgEndPoint   = "",
                 name          = "name1",
                 orgid         = "AgbotsGetRouteTest",
                 owner         = "AgbotsGetRouteTest/u1",
                 publicKey     = "",
                 token         = ""),
        AgbotRow(id            = "AgbotsGetRouteTest/a2",
                 lastHeartbeat = ApiTime.nowUTC,
                 msgEndPoint   = "",
                 name          = "name2",
                 orgid         = "AgbotsGetRouteTest",
                 owner         = "AgbotsGetRouteTest/u2",
                 publicKey     = "",
                 token         = ""),
        AgbotRow(id            = "AgbotsGetRouteTest/b",
                 lastHeartbeat = ApiTime.nowUTC,
                 msgEndPoint   = "",
                 name          = "",
                 orgid         = "AgbotsGetRouteTest",
                 owner         = "AgbotsGetRouteTest/v",
                 publicKey     = "",
                 token         = ""))
  private val TESTORGANIZATION: OrgRow =
    OrgRow(description        = "",
           heartbeatIntervals = "",
           label              = "",
           lastUpdated        = ApiTime.nowUTC,
           limits             = "",
           orgId              = "AgbotsGetRouteTest",
           orgType            = "",
           tags               = None)
  private val TESTUSERS: Seq[UserRow] =
    Seq(UserRow(admin       = false,
                email       = "",
                hashedPw    = "",
                hubAdmin    = false,
                lastUpdated = ApiTime.nowUTC,
                orgid       = "AgbotsGetRouteTest",
                updatedBy   = "",
                username    = "AgbotsGetRouteTest/u1"),
        UserRow(admin       = false,
                email       = "",
                hashedPw    = "",
                hubAdmin    = false,
                lastUpdated = ApiTime.nowUTC,
                orgid       = "AgbotsGetRouteTest",
                updatedBy   = "",
                username    = "AgbotsGetRouteTest/u2"),
        UserRow(admin       = false,
                email       = "",
                hashedPw    = "",
                hubAdmin    = false,
                lastUpdated = ApiTime.nowUTC,
                orgid       = "AgbotsGetRouteTest",
                updatedBy   = "",
                username    = "AgbotsGetRouteTest/v"))
  
  implicit private val formats: Formats = DefaultFormats.withLong
  
  // Begin building testing harness.
  override def beforeAll(): Unit = {
    Await.ready(DBCONNECTION.getDb.run((OrgsTQ.rows += TESTORGANIZATION) andThen
                                       (UsersTQ.rows ++= TESTUSERS)), AWAITDURATION)
  }
  
  // Teardown testing harness and cleanup.
  override def afterAll(): Unit = {
    Await.ready(DBCONNECTION.getDb.run(ResourceChangesTQ.rows.filter(_.orgId startsWith "AgbotsGetRouteTest").delete andThen
                                       OrgsTQ.rows.filter(_.orgid startsWith "AgbotsGetRouteTest").delete), AWAITDURATION)
    
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
  
  test("GET /orgs/" + "AgbotsGetRouteTest" + "/agbots -- 404 Not Found - No Agbots") {
    val response: HttpResponse[String] = Http(URL + "/agbots").headers(CONTENT).headers(ACCEPT).headers(ROOTAUTH).asString
    
    info("code: " + response.code)
    info("body: " + response.body)
    
    assert(response.code === HttpCode.NOT_FOUND.intValue)
    
    val responseBody: GetAgbotsResponse = parse(response.body).extract[GetAgbotsResponse]
    assert(responseBody.agbots.size === 0)
  }
  
  test("GET /orgs/" + "AgbotsGetRouteTest" + "/agbots -- Default") {
    val TESTAGBOT: Seq[AgbotRow] =
      Seq(AgbotRow(id            = "AgbotsGetRouteTest/a3",
               lastHeartbeat = ApiTime.nowUTC,
               msgEndPoint   = "msgEndPoint",
               name          = "name",
               orgid         = "AgbotsGetRouteTest",
               owner         = "AgbotsGetRouteTest/u1",
               publicKey     = "publicKey",
               token         = "token"))
    
    fixtureAgbots(
      _ => {
        val response: HttpResponse[String] = Http(URL + "/agbots").headers(CONTENT).headers(ACCEPT).headers(ROOTAUTH).asString
  
        info("code: " + response.code)
        info("body: " + response.body)
  
        assert(response.code === HttpCode.OK.intValue)
  
        val responseBody: GetAgbotsResponse = parse(response.body).extract[GetAgbotsResponse]
        assert(responseBody.agbots.size === 1)
        assert(responseBody.agbots.contains("AgbotsGetRouteTest/a3") === true)
        
        assert(responseBody.agbots("AgbotsGetRouteTest/a3").lastHeartbeat === TESTAGBOT.head.lastHeartbeat)
        assert(responseBody.agbots("AgbotsGetRouteTest/a3").msgEndPoint   === TESTAGBOT.head.msgEndPoint)
        assert(responseBody.agbots("AgbotsGetRouteTest/a3").name          === TESTAGBOT.head.name)
        assert(responseBody.agbots("AgbotsGetRouteTest/a3").owner         === TESTAGBOT.head.owner)
        assert(responseBody.agbots("AgbotsGetRouteTest/a3").publicKey     === TESTAGBOT.head.publicKey)
        assert(responseBody.agbots("AgbotsGetRouteTest/a3").token         === TESTAGBOT.head.token)
      }, TESTAGBOT)
  }
  
  test("GET /orgs/" + "AgbotsGetRouteTest" + "/agbots -- Filter id") {
    fixtureAgbots(
      _ => {
        val response: HttpResponse[String] = Http(URL + "/agbots").param("idfilter", "AgbotsGetRouteTest/a1").headers(CONTENT).headers(ACCEPT).headers(ROOTAUTH).asString
        
        info("code: " + response.code)
        info("body: " + response.body)
        
        assert(response.code === HttpCode.OK.intValue)
        
        val responseBody: GetAgbotsResponse = parse(response.body).extract[GetAgbotsResponse]
        assert(responseBody.agbots.size === 1)
        assert(responseBody.agbots.contains("AgbotsGetRouteTest/a1") === true)
      }, TESTAGBOTS)
  }
  
  test("GET /orgs/" + "AgbotsGetRouteTest" + "/agbots -- Filter like id") {
    fixtureAgbots(
      _ => {
        val response: HttpResponse[String] = Http(URL + "/agbots").param("idfilter", "AgbotsGetRouteTest/a%").headers(CONTENT).headers(ACCEPT).headers(ROOTAUTH).asString
        
        info("code: " + response.code)
        info("body: " + response.body)
        
        assert(response.code === HttpCode.OK.intValue)
        
        val responseBody: GetAgbotsResponse = parse(response.body).extract[GetAgbotsResponse]
        assert(responseBody.agbots.size === 2)
        assert(responseBody.agbots.contains("AgbotsGetRouteTest/a1") === true)
        assert(responseBody.agbots.contains("AgbotsGetRouteTest/a2") === true)
      }, TESTAGBOTS)
  }
  
  test("GET /orgs/" + "AgbotsGetRouteTest" + "/agbots -- Filter name") {
    fixtureAgbots(
      _ => {
        val response: HttpResponse[String] = Http(URL + "/agbots").param("name", "name1").headers(CONTENT).headers(ACCEPT).headers(ROOTAUTH).asString
        
        info("code: " + response.code)
        info("body: " + response.body)
        
        assert(response.code === HttpCode.OK.intValue)
        
        val responseBody: GetAgbotsResponse = parse(response.body).extract[GetAgbotsResponse]
        assert(responseBody.agbots.size === 1)
        assert(responseBody.agbots.contains("AgbotsGetRouteTest/a1") === true)
      }, TESTAGBOTS)
  }
  
  test("GET /orgs/" + "AgbotsGetRouteTest" + "/agbots -- Filter like name") {
    fixtureAgbots(
      _ => {
        val response: HttpResponse[String] = Http(URL + "/agbots").param("name", "name%").headers(CONTENT).headers(ACCEPT).headers(ROOTAUTH).asString
        
        info("code: " + response.code)
        info("body: " + response.body)
        
        assert(response.code === HttpCode.OK.intValue)
        
        val responseBody: GetAgbotsResponse = parse(response.body).extract[GetAgbotsResponse]
        assert(responseBody.agbots.size === 2)
        assert(responseBody.agbots.contains("AgbotsGetRouteTest/a1") === true)
        assert(responseBody.agbots.contains("AgbotsGetRouteTest/a2") === true)
      }, TESTAGBOTS)
  }
  
  test("GET /orgs/" + "AgbotsGetRouteTest" + "/agbots -- Filter owner") {
    fixtureAgbots(
      _ => {
        val response: HttpResponse[String] = Http(URL + "/agbots").param("owner", "AgbotsGetRouteTest/u1").headers(CONTENT).headers(ACCEPT).headers(ROOTAUTH).asString
        
        info("code: " + response.code)
        info("body: " + response.body)
        
        assert(response.code === HttpCode.OK.intValue)
        
        val responseBody: GetAgbotsResponse = parse(response.body).extract[GetAgbotsResponse]
        assert(responseBody.agbots.size === 1)
        assert(responseBody.agbots.contains("AgbotsGetRouteTest/a1") === true)
      }, TESTAGBOTS)
  }
  
  test("GET /orgs/" + "AgbotsGetRouteTest" + "/agbots -- Filter like owner") {
    fixtureAgbots(
      _ => {
        val response: HttpResponse[String] = Http(URL + "/agbots").param("owner", "AgbotsGetRouteTest/u%").headers(CONTENT).headers(ACCEPT).headers(ROOTAUTH).asString
        
        info("code: " + response.code)
        info("body: " + response.body)
        
        assert(response.code === HttpCode.OK.intValue)
        
        val responseBody: GetAgbotsResponse = parse(response.body).extract[GetAgbotsResponse]
        assert(responseBody.agbots.size === 2)
        assert(responseBody.agbots.contains("AgbotsGetRouteTest/a1") === true)
        assert(responseBody.agbots.contains("AgbotsGetRouteTest/a2") === true)
      }, TESTAGBOTS)
  }
}
