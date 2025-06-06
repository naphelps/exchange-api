package org.openhorizon.exchangeapi.route.apikey
import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods
import org.openhorizon.exchangeapi.auth.{Identity, Role}
import org.openhorizon.exchangeapi.table.apikey.{ApiKeyMetadata, ApiKeyRow, ApiKeysTQ}
import org.openhorizon.exchangeapi.table.organization.{OrgRow, OrgsTQ}
import org.openhorizon.exchangeapi.table.user.{UserRow, UsersTQ}
import org.openhorizon.exchangeapi.utility.{ApiUtils, Configuration, DatabaseConnection, ExchMsg}
import org.openhorizon.exchangeapi.table.resourcechange.ResourceChangesTQ
import org.openhorizon.exchangeapi.route.user.GetUsersResponse
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite
import scalaj.http.{Http, HttpResponse}
import scala.concurrent.Await
import org.apache.pekko.actor.ActorSystem
import org.apache.pekko.event.LoggingAdapter
import org.apache.pekko.http.scaladsl.model.{StatusCode, StatusCodes}
import scala.concurrent.duration.{Duration, DurationInt}
import slick.jdbc
import slick.jdbc.PostgresProfile.api._
import org.openhorizon.exchangeapi.auth.{Password, Role}
import scala.concurrent.ExecutionContext.Implicits.global
import _root_.org.openhorizon.exchangeapi.utility.{HttpCode,ApiTime}
import java.sql.Timestamp
import java.util.UUID

class TestDeleteApiKeyRoute extends AnyFunSuite with BeforeAndAfterAll {
  private val ACCEPT = ("Accept","application/json")
  private val AWAITDURATION: Duration = 15.seconds
  private val DBCONNECTION: jdbc.PostgresProfile.api.Database = DatabaseConnection.getDatabase
  private val URL = sys.env.getOrElse("EXCHANGE_URL_ROOT", "http://localhost:8080") + "/v1/orgs/"
  private val ROUTE = "/apikeys/"
  private val PASSWORD = "password"
  private implicit val formats: DefaultFormats.type = DefaultFormats
  
    private val TESTORGS = Seq(
      OrgRow(
      description = "",
      heartbeatIntervals = "",
      label = "",
      lastUpdated = "",
      limits = "",
      orgId = "testDeleteApiKeyRouteOrg0",
      orgType = "",
      tags = None
    ),
      OrgRow(
      description = "",
      heartbeatIntervals = "",
      label = "",
      lastUpdated = "",
      limits = "",
      orgId = "testDeleteApiKeyRouteOrg1",
      orgType = "",
      tags = None
    )
  )

  
   private val TESTUSERS = Seq(
    UserRow(
    createdAt = ApiTime.nowUTCTimestamp,
    email = Some("admin0@example.com"),
    identityProvider = "Open Horizon",
    isHubAdmin = false,
    isOrgAdmin = true,
    modifiedAt = ApiTime.nowUTCTimestamp,
    modified_by = None,
    organization = "testDeleteApiKeyRouteOrg0",
    password = Some(Password.hash(PASSWORD)),
    user = UUID.randomUUID(),
    username = "testDeleteApiKeyRouteAdmin0"
  ),
    UserRow(
    createdAt = ApiTime.nowUTCTimestamp,
    email = Some("user0@example.com"),
    identityProvider = "Open Horizon",
    isHubAdmin = false,
    isOrgAdmin = false,
    modifiedAt = ApiTime.nowUTCTimestamp,
    modified_by = None,
    organization = "testDeleteApiKeyRouteOrg0",
    password = Some(Password.hash(PASSWORD)),
    user = UUID.randomUUID(),
    username = "testDeleteApiKeyRouteUser0"
  ),
    UserRow(
    createdAt = ApiTime.nowUTCTimestamp,
    email = Some("user1@example.com"),
    identityProvider = "Open Horizon",
    isHubAdmin = false,
    isOrgAdmin = false,
    modifiedAt = ApiTime.nowUTCTimestamp,
    modified_by = None,
    organization = "testDeleteApiKeyRouteOrg1",
    password = Some(Password.hash(PASSWORD)),
    user = UUID.randomUUID(),
    username = "testDeleteApiKeyRouteUser1"
  )
)

  private val TESTAPIKEYS = Seq(
    ApiKeyRow(
      orgid = "testDeleteApiKeyRouteOrg0",
      id = UUID.randomUUID(),
      user = TESTUSERS(0).user,
      description = "Test API Key 1",
      hashedKey = "hash1",
      createdAt = ApiTime.nowUTCTimestamp,
      createdBy = TESTUSERS(0).user,
      modifiedAt = ApiTime.nowUTCTimestamp,
      modifiedBy = TESTUSERS(0).user
    ),
    ApiKeyRow(
      orgid = "testDeleteApiKeyRouteOrg0",
      id = UUID.randomUUID(),
      user = TESTUSERS(1).user,
      description = "Test API Key 2",
      hashedKey = "hash2",
      createdAt = ApiTime.nowUTCTimestamp,
      createdBy = TESTUSERS(1).user,
      modifiedAt = ApiTime.nowUTCTimestamp,
      modifiedBy = TESTUSERS(1).user
    ),
    ApiKeyRow(
      orgid = "testDeleteApiKeyRouteOrg0",
      id = UUID.randomUUID(),
      user = TESTUSERS(1).user,
      description = "Test API Key 3",
      hashedKey = "hash3",
      createdAt = ApiTime.nowUTCTimestamp,
      createdBy = TESTUSERS(1).user,
      modifiedAt = ApiTime.nowUTCTimestamp,
      modifiedBy = TESTUSERS(1).user
    ),
    ApiKeyRow(
      orgid = "testDeleteApiKeyRouteOrg1",
      id = UUID.randomUUID(),
      user = TESTUSERS(2).user,
      description = "Test API Key 4",
      hashedKey = "hash4",
      createdAt = ApiTime.nowUTCTimestamp,
      createdBy = TESTUSERS(2).user,
      modifiedAt = ApiTime.nowUTCTimestamp,
      modifiedBy = TESTUSERS(2).user
    ),
  )

  private val ORGADMINAUTH = ("Authorization", "Basic " + ApiUtils.encode("testDeleteApiKeyRouteOrg0/testDeleteApiKeyRouteAdmin0:password"))
  private val USERAUTH = ("Authorization", "Basic " + ApiUtils.encode("testDeleteApiKeyRouteOrg0/testDeleteApiKeyRouteUser0:password"))


  override def beforeAll(): Unit = {
    val insertAction = DBIO.seq(
      OrgsTQ ++= TESTORGS,
      UsersTQ ++= TESTUSERS,
      ApiKeysTQ ++= TESTAPIKEYS)
      Await.result(DBCONNECTION.run(insertAction.transactionally), AWAITDURATION)
  }

  override def afterAll(): Unit = {
    Await.ready(DBCONNECTION.run(
      ResourceChangesTQ.filter(_.orgId startsWith "testPostApiKeyRouteOrg").delete
    ), AWAITDURATION)
    Await.ready(DBCONNECTION.run(
      ApiKeysTQ.filter(_.orgid startsWith "testDeleteApiKeyRouteOrg").delete
    ), AWAITDURATION)
    Await.ready(DBCONNECTION.run(
      UsersTQ.filter(_.username startsWith "testDeleteApiKeyRoute").delete 
    ), AWAITDURATION)
    Await.ready(DBCONNECTION.run(
      OrgsTQ.filter(_.orgid startsWith "testDeleteApiKeyRouteOrg").delete
    ), AWAITDURATION)
  }

  // DELETE /v1/orgs/{orgid}/users/{username}/apikeys/{keyid} 
  // User deletes their own API key
  test("DELETE /orgs/" + TESTORGS(0).orgId + "/users/" + TESTUSERS(1).username + ROUTE + TESTAPIKEYS(1).id + " -- user deletes own apikey") {
    val response = Http(
      URL + TESTORGS(0).orgId + "/users/" + TESTUSERS(1).username + ROUTE + TESTAPIKEYS(1).id
    ).headers(ACCEPT).headers(USERAUTH).method("DELETE").asString

    info("Code: " + response.code)
    info("Body: " + response.body)
    assert(response.code === HttpCode.DELETED.intValue)

    val checkResponse = Http(
      URL + TESTORGS(0).orgId + "/users/" +
        TESTUSERS(1).username + ROUTE + TESTAPIKEYS(1).id
    ).headers(ACCEPT).headers(USERAUTH).asString

    info("Check Code: " + checkResponse.code)
    info("Check Body: " + checkResponse.body)
    assert(checkResponse.code === HttpCode.NOT_FOUND.intValue)
  }

  // Org admin deletes an API key belonging to a user in their org
  test("DELETE /orgs/" + TESTORGS(0).orgId + "/users/" + TESTUSERS(1).username + ROUTE + TESTAPIKEYS(2).id + " -- org admin deletes apikey of the user in this org") {
    val response = Http(
      URL + TESTORGS(0).orgId + "/users/" + TESTUSERS(1).username + ROUTE + TESTAPIKEYS(2).id
    ).headers(ACCEPT).headers(ORGADMINAUTH).method("DELETE").asString

    info("Code: " + response.code)
    info("Body: " + response.body)
    assert(response.code === HttpCode.DELETED.intValue)

    val checkResponse = Http(
      URL + TESTORGS(0).orgId + "/users/" +
        TESTUSERS(1).username + ROUTE + TESTAPIKEYS(2).id
    ).headers(ACCEPT).headers(ORGADMINAUTH).asString

    info("Check Code: " + checkResponse.code)
    info("Check Body: " + checkResponse.body)
    assert(checkResponse.code === HttpCode.NOT_FOUND.intValue)
  }

  // Deleting a non-existent API key should return 404
  test("DELETE /orgs/" + TESTORGS(0).orgId + "/users/" + TESTUSERS(1).username + ROUTE + "nonexistentkey -- should return 404") {
    val response = Http(
      URL + TESTORGS(0).orgId + "/users/" + TESTUSERS(1).username + ROUTE + UUID.randomUUID()
    ).headers(ACCEPT).headers(USERAUTH).method("DELETE").asString

    info("Code: " + response.code)
    info("Body: " + response.body)
    assert(response.code === HttpCode.NOT_FOUND.intValue)
  }

  // User tries to delete another user's API key in the same org
  test("DELETE /orgs/" + TESTORGS(0).orgId + "/users/" + TESTUSERS(0).username + ROUTE + TESTAPIKEYS(0).id + " -- user tries to delete apikey of another user") {
    val response = Http(
      URL + TESTORGS(0).orgId + "/users/" + TESTUSERS(0).username + ROUTE + TESTAPIKEYS(0).id
    ).headers(ACCEPT).headers(USERAUTH).method("DELETE").asString

    info("Code: " + response.code)
    info("Body: " + response.body)
    assert(response.code === HttpCode.ACCESS_DENIED.intValue)
  }

  // Org admin tries to delete an API key from a different org (should fail)
  test("DELETE /orgs/" + TESTORGS(1).orgId + "/users/" + TESTUSERS(2).username + ROUTE + TESTAPIKEYS(2).id + " -- org admin tries to delete key from another org") {
    val response = Http(
      URL + TESTORGS(1).orgId + "/users/" + TESTUSERS(2).username + ROUTE + TESTAPIKEYS(2).id
    ).headers(ACCEPT).headers(ORGADMINAUTH).method("DELETE").asString

    info("Code: " + response.code)
    info("Body: " + response.body)
    assert(response.code === HttpCode.ACCESS_DENIED.intValue)
  }
  
}