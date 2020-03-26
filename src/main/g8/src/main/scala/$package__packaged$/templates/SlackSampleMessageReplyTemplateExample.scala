package $package$.templates

import java.time.Instant

import org.latestbit.slack.morphism.client.templating._
import org.latestbit.slack.morphism.messages.SlackBlock

class SlackSampleMessageReplyTemplateExample( replyToMessage: String ) extends SlackMessageTemplate {

  override def renderPlainText(): String =
    s"I've just received from you some text:"

  override def renderBlocks(): Option[List[SlackBlock]] =
    blocks(
      sectionBlock(
        text = md"I've just received from you some text:\n\${formatSlackQuoteText( replyToMessage )}"
      ),
      dividerBlock(),
      contextBlock(
        blockElements(
          md"I'm glad that you still remember me",
          md"Current time is: \${formatDate( Instant.now(), SlackTextFormatters.SlackDateTimeFormats.DATE_LONG_PRETTY )}"
        )
      ),
      actionsBlock(
        blockElements(
          button( text = pt"Simple", action_id = "simple-message-button" )
        )
      )
    )

}
