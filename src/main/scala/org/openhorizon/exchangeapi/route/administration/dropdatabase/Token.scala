package org.openhorizon.exchangeapi.route.administration.dropdatabase

import akka.actor.ActorSystem
import akka.event.LoggingAdapter
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import de.heikoseeberger.akkahttpjackson.JacksonSupport
import io.swagger.v3.oas.annotations.media.{Content, Schema}
import io.swagger.v3.oas.annotations.{Operation, responses}
import jakarta.ws.rs.{GET, Path}
import org.openhorizon.exchangeapi.route.administration.AdminDropdbTokenResponse
import org.openhorizon.exchangeapi.{Access, AuthenticationSupport, HttpCode, Role, TAction}
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.ExecutionContext

@Path("/v1/admin/dropdb/token")
@io.swagger.v3.oas.annotations.tags.Tag(name = "administration")
trait Token extends JacksonSupport with AuthenticationSupport {
  // Will pick up these values when it is mixed in wDirectives.ith ExchangeApiApp
  def db: Database
  def system: ActorSystem
  def logger: LoggingAdapter
  implicit def executionContext: ExecutionContext
  
  
  // =========== GET /admin/dropdb/token ===============================
  @GET
  @Operation(summary = "Gets a 1-time token for deleting the DB",
             description = """Returns a timed token that can be given to POST /admin/dropdb. The token is good for 10 minutes. Since dropping the DB tables deletes all of their data, this is a way of confirming you really want to do it. This can only be run as root.""",
             responses =
               Array(new responses.ApiResponse(responseCode = "200", description = "response body",
                                               content =
                                                 Array(new Content(mediaType = "application/json",
                                                                   schema = new Schema(implementation = classOf[AdminDropdbTokenResponse])))),
                     new responses.ApiResponse(responseCode = "401", description = "invalid credentials"),
                     new responses.ApiResponse(responseCode = "403", description = "access denied")))
  def getToken: Route =
    get {
      logger.debug("Doing GET /admin/dropdb/token")
      complete({
        (HttpCode.OK, AdminDropdbTokenResponse(createToken(Role.superUser)))
      }) // end of complete
    }
  
  
  val token: Route =
    path("admin" / "dropdb" / "token") {
      get {
        exchAuth(TAction(), Access.ADMIN) {
          _ =>
            getToken
        }
      }
    }
}
