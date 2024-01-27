package rockthejvm.websockets

sealed trait OutputMessage

case class Register(
    user: Option[User],
    msg: String = """|Register your username with the following command:
                     |/name <username>""".stripMargin
) extends OutputMessage

case class ParsingError(user: Option[User], msg: String) extends OutputMessage

case class SuccessfulRegistration(
    user: User,
    msg: String = ""
) extends OutputMessage
object SuccessfulRegistration {
  def apply(user: User) = {
    user match {
      case User(name) =>
        new SuccessfulRegistration(user, s"$name entered the chat")
    }
  }
}

case class UnsupportedCommand(
    user: Option[User],
    msg: String = "Unsupported command"
) extends OutputMessage

case object KeepAlive extends OutputMessage

case object DiscardMessage extends OutputMessage

case class SendToUser(user: User, msg: String) extends OutputMessage {
  def forUser(targetUser: User): Boolean = targetUser == user
}

case class ChatMsg(from: User, to: User, msg: String) extends OutputMessage {
  def forUser(targetUser: User): Boolean = targetUser == to
}
