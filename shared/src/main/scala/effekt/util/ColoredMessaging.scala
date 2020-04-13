package effekt
package util

import org.bitbucket.inkytonik.kiama.util.Messaging.Messages
import org.bitbucket.inkytonik.kiama.util.{ Message, Messaging, Positions, Severities }

class ColoredMessaging(positions: Positions) extends Messaging(positions) {

  import Severities._

  def severityToWord(severity: Severity): String =
    severity match {
      case Error       => s"${Console.RED}error${Console.RESET}"
      case Warning     => s"${Console.YELLOW}warning${Console.RESET}"
      case Information => s"${Console.WHITE}info${Console.RESET}"
      case Hint        => "hint"
    }

  override def formatMessage(message: Message): String =
    start(message) match {
      case Some(pos) =>
        val severity = severityToWord(message.severity)
        val context = util.Highlight(pos.optContext.getOrElse(""))
        s"[$severity] ${pos.format} ${homogenizePath(message.label)}\n$context\n"
      case None =>
        val severity = severityToWord(message.severity)
        s"[$severity] ${homogenizePath(message.label)}\n"
    }

  /**
   * To allow uniform testing on all platforms, we homogenize the paths to Unix-style.
   *
   * This way the negative tests look the same on Windows and Linux
   */
  private def homogenizePath(label: String): String =
    label.replace('\\', '/')

  // Filter out duplicates
  // TODO this is a hack and should be solved in typer, where the messages are generated by unification
  override def formatMessages(messages: Messages): String =
    messages.sorted.map(formatMessage).distinct.mkString("")

}
