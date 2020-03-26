package $package$.routes

import cats.effect.Sync
import cats.implicits._
import com.typesafe.scalalogging.StrictLogging
import org.http4s._
import org.http4s.dsl.Http4sDsl
import org.latestbit.slack.morphism.client.SlackApiClientT
import org.latestbit.slack.morphism.client.reqresp.chat.SlackApiPostEventReply
import org.latestbit.slack.morphism.common.SlackResponseTypes

import $package$.config.AppConfig
import $package$.db.SlackTokensDb
import $package$.templates.SlackSampleMessageReplyTemplateExample

class SlackCommandEventsRoutes[F[_] : Sync](
    slackApiClient: SlackApiClientT[F],
    implicit val tokensDb: SlackTokensDb[F],
    implicit val config: AppConfig
) extends StrictLogging
    with SlackEventsMiddleware {

  def routes(): HttpRoutes[F] = {
    val dsl = new Http4sDsl[F] {}
    import dsl._

    HttpRoutes.of[F] {
      case req @ POST -> Root / "command" => {
        slackSignedRoutes[F]( req ) {
          req.decode[UrlForm] { form =>
            ( form.getFirst( "text" ), form.getFirst( "response_url" ) ) match {
              case ( Some( text ), Some( responseUrl ) ) => {

                val commandReply = new SlackSampleMessageReplyTemplateExample(
                  text
                )

                slackApiClient.chat
                  .postEventReply(
                    response_url = responseUrl,
                    SlackApiPostEventReply(
                      text = commandReply.renderPlainText(),
                      blocks = commandReply.renderBlocks(),
                      response_type = Some( SlackResponseTypes.EPHEMERAL )
                    )
                  )
                  .flatMap { resp =>
                    resp.leftMap( err => logger.error( err.getMessage() ) )
                    Ok()
                  }
              }
              case _ => {
                BadRequest()
              }
            }
          }
        }
      }
    }
  }

}
