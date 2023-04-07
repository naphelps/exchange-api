package org.openhorizon.exchangeapi.route.organization

import org.openhorizon.exchangeapi.table.{AgbotRow, AgbotsTQ, NodeErrorRow, NodeErrorTQ, NodeRow, NodesTQ, OrgRow, OrgsTQ, ResourceChangesTQ, UserRow, UsersTQ}
import org.openhorizon.exchangeapi.{ApiTime, ApiUtils, HttpCode, Password, Role, TestDBConnection}
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods
import org.openhorizon.exchangeapi.table.{NodeErrorRow, NodeErrorTQ, NodeRow, NodesTQ}
import org.openhorizon.exchangeapi.{Password, Role, TestDBConnection}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite
import scalaj.http.{Http, HttpResponse}

import scala.concurrent.Await
import scala.concurrent.duration.{Duration, DurationInt}
import slick.jdbc.PostgresProfile.api._

class TestGetAllNodeErrorsRoute extends AnyFunSuite with BeforeAndAfterAll {

  private val ACCEPT = ("Accept","application/json")
  private val AWAITDURATION: Duration = 15.seconds
  private val DBCONNECTION: TestDBConnection = new TestDBConnection
  private val URL = sys.env.getOrElse("EXCHANGE_URL_ROOT", "http://localhost:8080") + "/v1/orgs/"
  private val ROUTE = "/search/nodes/error/all"

  private implicit val formats = DefaultFormats

  private val HUBADMINPASSWORD = "adminpassword"
  private val ORG1USERPASSWORD = "org1userpassword"
  private val ORG1ADMINPASSWORD = "org1adminpassword"
  private val ORG2USERPASSWORD = "org2userpassword"
  private val ORG2ADMINPASSWORD = "org2adminpassword"
  private val NODE1TOKEN = "node1token"
  private val NODE2TOKEN = "node2token"
  private val AGBOT1TOKEN = "agbot1token"
  private val AGBOT2TOKEN = "agbot2token"

  private val TESTORGS: Seq[OrgRow] =
    Seq(
      OrgRow(
        heartbeatIntervals = "",
        description        = "Test Organization 1",
        label              = "testGetAllNodeErrors",
        lastUpdated        = ApiTime.nowUTC,
        limits             = "",
        orgId              = "testGetAllNodeErrorsRoute1",
        orgType            = "",
        tags               = None),
      OrgRow(
        heartbeatIntervals = "",
        description        = "Test Organization 2",
        label              = "testGetAllNodeErrors",
        lastUpdated        = ApiTime.nowUTC,
        limits             = "",
        orgId              = "testGetAllNodeErrorsRoute2",
        orgType            = "",
        tags               = None
      ))

  private val TESTUSERS: Seq[UserRow] =
    Seq(
      UserRow(
        username    = "root/TestGetAllNodeErrorsRouteHubAdmin",
        orgid       = "root",
        hashedPw    = Password.hash(HUBADMINPASSWORD),
        admin       = false,
        hubAdmin    = true,
        email       = "TestGetAllNodeErrorsRouteHubAdmin@ibm.com",
        lastUpdated = ApiTime.nowUTC,
        updatedBy   = "root"
      ),
      UserRow(
        username    = TESTORGS(0).orgId + "/org1user",
        orgid       = TESTORGS(0).orgId,
        hashedPw    = Password.hash(ORG1USERPASSWORD),
        admin       = false,
        hubAdmin    = false,
        email       = "org1user@ibm.com",
        lastUpdated = ApiTime.nowUTC,
        updatedBy   = "root"
      ),
      UserRow(
        username    = TESTORGS(0).orgId + "/org1admin",
        orgid       = TESTORGS(0).orgId,
        hashedPw    = Password.hash(ORG1ADMINPASSWORD),
        admin       = true,
        hubAdmin    = false,
        email       = "org1admin@ibm.com",
        lastUpdated = ApiTime.nowUTC,
        updatedBy   = "root"
      ),
      UserRow(
        username    = TESTORGS(1).orgId + "/org2user",
        orgid       = TESTORGS(1).orgId,
        hashedPw    = Password.hash(ORG2USERPASSWORD),
        admin       = false,
        hubAdmin    = false,
        email       = "org2user@ibm.com",
        lastUpdated = ApiTime.nowUTC,
        updatedBy   = "root"
      ),
      UserRow(
        username    = TESTORGS(1).orgId + "/org2admin",
        orgid       = TESTORGS(1).orgId,
        hashedPw    = Password.hash(ORG2ADMINPASSWORD),
        admin       = true,
        hubAdmin    = false,
        email       = "org2uadmin@ibm.com",
        lastUpdated = ApiTime.nowUTC,
        updatedBy   = "root"
      ))

  private val TESTNODES: Seq[NodeRow] =
    Seq(
      NodeRow(
        arch               = "",
        id                 = TESTORGS(0).orgId + "/node1",
        heartbeatIntervals = "",
        lastHeartbeat      = None,
        lastUpdated        = ApiTime.nowUTC,
        msgEndPoint        = "",
        name               = "",
        nodeType           = "",
        orgid              = TESTORGS(0).orgId,
        owner              = TESTUSERS(1).username, //org 1 user
        pattern            = "",
        publicKey          = "",
        regServices        = "",
        softwareVersions   = "",
        token              = Password.hash(NODE1TOKEN),
        userInput          = ""),
      NodeRow(
        arch               = "",
        id                 = TESTORGS(1).orgId + "/node2",
        heartbeatIntervals = "",
        lastHeartbeat      = None,
        lastUpdated        = ApiTime.nowUTC,
        msgEndPoint        = "",
        name               = "",
        nodeType           = "",
        orgid              = TESTORGS(1).orgId,
        owner              = TESTUSERS(3).username, //org 2 user
        pattern            = "",
        publicKey          = "",
        regServices        = "",
        softwareVersions   = "",
        token              = Password.hash(NODE2TOKEN),
        userInput          = ""),
      NodeRow(
        arch               = "",
        id                 = TESTORGS(0).orgId + "/usererrornode",
        heartbeatIntervals = "",
        lastHeartbeat      = None,
        lastUpdated        = ApiTime.nowUTC,
        msgEndPoint        = "",
        name               = "",
        nodeType           = "",
        orgid              = TESTORGS(0).orgId,
        owner              = TESTUSERS(1).username, //org 1 user
        pattern            = "",
        publicKey          = "",
        regServices        = "",
        softwareVersions   = "",
        token              = "",
        userInput          = ""),
      NodeRow(
        arch               = "",
        id                 = TESTORGS(0).orgId + "/admingoodnode",
        heartbeatIntervals = "",
        lastHeartbeat      = None,
        lastUpdated        = ApiTime.nowUTC,
        msgEndPoint        = "",
        name               = "",
        nodeType           = "",
        orgid              = TESTORGS(0).orgId,
        owner              = TESTUSERS(2).username, //org 1 admin
        pattern            = "",
        publicKey          = "",
        regServices        = "",
        softwareVersions   = "",
        token              = "",
        userInput          = ""),
      NodeRow(
        arch               = "",
        id                 = TESTORGS(0).orgId + "/adminerrornode",
        heartbeatIntervals = "",
        lastHeartbeat      = None,
        lastUpdated        = ApiTime.nowUTC,
        msgEndPoint        = "",
        name               = "",
        nodeType           = "",
        orgid              = TESTORGS(0).orgId,
        owner              = TESTUSERS(2).username, //org 1 admin
        pattern            = "",
        publicKey          = "",
        regServices        = "",
        softwareVersions   = "",
        token              = "",
        userInput          = ""))

  private val TESTAGBOTS: Seq[AgbotRow] =
    Seq(AgbotRow(
      id            = TESTORGS(0).orgId + "/agbot1",
      orgid         = TESTORGS(0).orgId,
      token         = Password.hash(AGBOT1TOKEN),
      name          = "",
      owner         = TESTUSERS(1).username, //org 1 user
      msgEndPoint   = "",
      lastHeartbeat = ApiTime.nowUTC,
      publicKey     = ""
    ),
      AgbotRow(
        id            = TESTORGS(1).orgId + "/agbot2",
        orgid         = TESTORGS(1).orgId,
        token         = Password.hash(AGBOT2TOKEN),
        name          = "",
        owner         = TESTUSERS(3).username, //org 2 user
        msgEndPoint   = "",
        lastHeartbeat = ApiTime.nowUTC,
        publicKey     = ""
      ))

  private val ROOTAUTH = ("Authorization","Basic " + ApiUtils.encode(Role.superUser + ":" + sys.env.getOrElse("EXCHANGE_ROOTPW", "")))
  private val HUBADMINAUTH = ("Authorization", "Basic " + ApiUtils.encode(TESTUSERS(0).username + ":" + HUBADMINPASSWORD))
  private val ORG1USERAUTH = ("Authorization", "Basic " + ApiUtils.encode(TESTUSERS(1).username + ":" + ORG1USERPASSWORD))
  private val ORG1ADMINAUTH = ("Authorization", "Basic " + ApiUtils.encode(TESTUSERS(2).username + ":" + ORG1ADMINPASSWORD))
  private val ORG2USERAUTH = ("Authorization", "Basic " + ApiUtils.encode(TESTUSERS(3).username + ":" + ORG2USERPASSWORD))
  private val ORG2ADMINAUTH = ("Authorization", "Basic " + ApiUtils.encode(TESTUSERS(4).username + ":" + ORG2ADMINPASSWORD))
  private val NODE1AUTH = ("Authorization", "Basic " + ApiUtils.encode(TESTNODES(0).id + ":" + NODE1TOKEN))
  private val NODE2AUTH = ("Authorization", "Basic " + ApiUtils.encode(TESTNODES(1).id + ":" + NODE2TOKEN))
  private val AGBOT1AUTH = ("Authorization", "Basic " + ApiUtils.encode(TESTAGBOTS(0).id + ":" + AGBOT1TOKEN))
  private val AGBOT2AUTH = ("Authorization", "Basic " + ApiUtils.encode(TESTAGBOTS(1).id + ":" + AGBOT2TOKEN))

  private val TESTNODEERRORS: Seq[NodeErrorRow] =
    Seq(
      NodeErrorRow(
        nodeId = TESTNODES(2).id,
        errors = "error message 1",
        lastUpdated = ApiTime.nowUTC
      ),
      NodeErrorRow(
        nodeId = TESTNODES(4).id,
        errors = "error message 2",
        lastUpdated = ApiTime.nowUTC
      ))

  override def beforeAll(): Unit = {
    Await.ready(DBCONNECTION.getDb.run(
      (OrgsTQ ++= TESTORGS) andThen
        (UsersTQ ++= TESTUSERS) andThen
        (AgbotsTQ ++= TESTAGBOTS) andThen
        (NodesTQ ++= TESTNODES) andThen
        (NodeErrorTQ ++= TESTNODEERRORS)), AWAITDURATION
    )
  }

  override def afterAll(): Unit = {
    Await.ready(DBCONNECTION.getDb.run(ResourceChangesTQ.filter(_.orgId startsWith "testGetAllNodeErrorsRoute").delete andThen
      OrgsTQ.filter(_.orgid startsWith "testGetAllNodeErrorsRoute").delete andThen
      UsersTQ.filter(_.username startsWith "root/TestGetAllNodeErrorsRouteHubAdmin").delete), AWAITDURATION)
    DBCONNECTION.getDb.close()
  }

  def assertErrorsEqual(error1: NodeErrorsResp, error2: NodeErrorRow): Unit = {
    assert(error1.nodeId === error2.nodeId)
    assert(error1.error === error2.errors)
    assert(error1.lastUpdated === error2.lastUpdated)
  }

  //this succeeds and returns an empty list for a nonexistent org -- would expect a 404 not found instead, but whatever
  test("GET /orgs/doesNotExist" + ROUTE + " -- success, empty list") {
    val response: HttpResponse[String] = Http(URL + "doesNotExist" + ROUTE).headers(ACCEPT).headers(ROOTAUTH).asString
    info("Code: " + response.code)
    info("Body: " + response.body)
    assert(response.code === HttpCode.OK.intValue)
    val errorList: Seq[NodeErrorsResp] = JsonMethods.parse(response.body).extract[AllNodeErrorsInOrgResp].nodeErrors.toList
    assert(errorList.isEmpty)
  }

  test("GET /orgs/" + TESTORGS(0).orgId + ROUTE + " -- as root -- success, returns all error nodes") {
    val response: HttpResponse[String] = Http(URL + TESTORGS(0).orgId + ROUTE).headers(ACCEPT).headers(ROOTAUTH).asString
    info("Code: " + response.code)
    info("Body: " + response.body)
    assert(response.code === HttpCode.OK.intValue)
    val errorMap: Map[String, NodeErrorsResp] = JsonMethods.parse(response.body).extract[AllNodeErrorsInOrgResp].nodeErrors.map(a => a.nodeId -> a).toMap
    assert(errorMap.size === 2)
    for (nodeError <- TESTNODEERRORS) {
      assert(errorMap.contains(nodeError.nodeId))
      assertErrorsEqual(errorMap(nodeError.nodeId), nodeError)
    }
  }

  test("GET /orgs/" + TESTORGS(0).orgId + ROUTE + " -- as node in org -- 403 access denied") {
    val response: HttpResponse[String] = Http(URL + TESTORGS(0).orgId + ROUTE).headers(ACCEPT).headers(NODE1AUTH).asString
    info("Code: " + response.code)
    info("Body: " + response.body)
    assert(response.code === HttpCode.ACCESS_DENIED.intValue)
    val errors: AllNodeErrorsInOrgResp = JsonMethods.parse(response.body).extract[AllNodeErrorsInOrgResp]
    assert(errors.nodeErrors.isEmpty)
  }

  test("GET /orgs/" + TESTORGS(0).orgId + ROUTE + " -- as node in other org -- 403 access denied") {
    val response: HttpResponse[String] = Http(URL + TESTORGS(0).orgId + ROUTE).headers(ACCEPT).headers(NODE2AUTH).asString
    info("Code: " + response.code)
    info("Body: " + response.body)
    assert(response.code === HttpCode.ACCESS_DENIED.intValue)
    val errors: AllNodeErrorsInOrgResp = JsonMethods.parse(response.body).extract[AllNodeErrorsInOrgResp]
    assert(errors.nodeErrors.isEmpty)
  }

  test("GET /orgs/" + TESTORGS(0).orgId + ROUTE + " -- as agbot in org -- success, returns all error nodes") {
    val response: HttpResponse[String] = Http(URL + TESTORGS(0).orgId + ROUTE).headers(ACCEPT).headers(AGBOT1AUTH).asString
    info("Code: " + response.code)
    info("Body: " + response.body)
    assert(response.code === HttpCode.OK.intValue)
    val errorMap: Map[String, NodeErrorsResp] = JsonMethods.parse(response.body).extract[AllNodeErrorsInOrgResp].nodeErrors.map(a => a.nodeId -> a).toMap
    assert(errorMap.size === 2)
    for (nodeError <- TESTNODEERRORS) {
      assert(errorMap.contains(nodeError.nodeId))
      assertErrorsEqual(errorMap(nodeError.nodeId), nodeError)
    }
  }

  test("GET /orgs/" + TESTORGS(0).orgId + ROUTE + " -- as agbot in other org -- 403 access denied") {
    val response: HttpResponse[String] = Http(URL + TESTORGS(0).orgId + ROUTE).headers(ACCEPT).headers(AGBOT2AUTH).asString
    info("Code: " + response.code)
    info("Body: " + response.body)
    assert(response.code === HttpCode.ACCESS_DENIED.intValue)
    val errors: AllNodeErrorsInOrgResp = JsonMethods.parse(response.body).extract[AllNodeErrorsInOrgResp]
    assert(errors.nodeErrors.isEmpty)
  }

  test("GET /orgs/" + TESTORGS(0).orgId + ROUTE + " -- as hub admin -- 403 access denied") {
    val response: HttpResponse[String] = Http(URL + TESTORGS(0).orgId + ROUTE).headers(ACCEPT).headers(HUBADMINAUTH).asString
    info("Code: " + response.code)
    info("Body: " + response.body)
    assert(response.code === HttpCode.ACCESS_DENIED.intValue)
    val errors: AllNodeErrorsInOrgResp = JsonMethods.parse(response.body).extract[AllNodeErrorsInOrgResp]
    assert(errors.nodeErrors.isEmpty)
  }

  test("GET /orgs/" + TESTORGS(0).orgId + ROUTE + " -- as admin in org -- success, returns all error nodes") {
    val response: HttpResponse[String] = Http(URL + TESTORGS(0).orgId + ROUTE).headers(ACCEPT).headers(ORG1ADMINAUTH).asString
    info("Code: " + response.code)
    info("Body: " + response.body)
    assert(response.code === HttpCode.OK.intValue)
    val errorMap: Map[String, NodeErrorsResp] = JsonMethods.parse(response.body).extract[AllNodeErrorsInOrgResp].nodeErrors.map(a => a.nodeId -> a).toMap
    assert(errorMap.size === 2)
    for (nodeError <- TESTNODEERRORS) {
      assert(errorMap.contains(nodeError.nodeId))
      assertErrorsEqual(errorMap(nodeError.nodeId), nodeError)
    }
  }

  test("GET /orgs/" + TESTORGS(0).orgId + ROUTE + " -- as user in org -- success, returns only error nodes owned by user") {
    val response: HttpResponse[String] = Http(URL + TESTORGS(0).orgId + ROUTE).headers(ACCEPT).headers(ORG1USERAUTH).asString
    info("Code: " + response.code)
    info("Body: " + response.body)
    assert(response.code === HttpCode.OK.intValue)
    val errorList: Seq[NodeErrorsResp] = JsonMethods.parse(response.body).extract[AllNodeErrorsInOrgResp].nodeErrors.toList
    assert(errorList.length === 1)
    assertErrorsEqual(errorList.head, TESTNODEERRORS(0))
  }

  test("GET /orgs/" + TESTORGS(0).orgId + ROUTE + " -- as admin in other org -- 403 access denied") {
    val response: HttpResponse[String] = Http(URL + TESTORGS(0).orgId + ROUTE).headers(ACCEPT).headers(ORG2ADMINAUTH).asString
    info("Code: " + response.code)
    info("Body: " + response.body)
    assert(response.code === HttpCode.ACCESS_DENIED.intValue)
    val errors: AllNodeErrorsInOrgResp = JsonMethods.parse(response.body).extract[AllNodeErrorsInOrgResp]
    assert(errors.nodeErrors.isEmpty)
  }

  test("GET /orgs/" + TESTORGS(0).orgId + ROUTE + " -- as user in other org -- 403 access denied") {
    val response: HttpResponse[String] = Http(URL + TESTORGS(0).orgId + ROUTE).headers(ACCEPT).headers(ORG2USERAUTH).asString
    info("Code: " + response.code)
    info("Body: " + response.body)
    assert(response.code === HttpCode.ACCESS_DENIED.intValue)
    val errors: AllNodeErrorsInOrgResp = JsonMethods.parse(response.body).extract[AllNodeErrorsInOrgResp]
    assert(errors.nodeErrors.isEmpty)
  }

  test("GET /orgs/" + TESTORGS(1).orgId + ROUTE + " -- as root -- success, returns empty list (no error nodes)") {
    val response: HttpResponse[String] = Http(URL + TESTORGS(1).orgId + ROUTE).headers(ACCEPT).headers(ROOTAUTH).asString
    info("Code: " + response.code)
    info("Body: " + response.body)
    assert(response.code === HttpCode.OK.intValue)
    val errors: AllNodeErrorsInOrgResp = JsonMethods.parse(response.body).extract[AllNodeErrorsInOrgResp]
    assert(errors.nodeErrors.isEmpty)
  }

}
