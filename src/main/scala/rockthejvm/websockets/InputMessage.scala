package rockthejvm.websockets

import cats.data.Validated
import cats.effect.kernel.Ref
import cats.Monad
import cats.syntax.all.*
import cats.parse.Parser
import cats.parse.Parser.char
import cats.parse.Rfc5234.{alpha, sp, wsp}
import cats.Applicative
import cats.data.Validated.Valid
import cats.data.Validated.Invalid

trait InputMessage[F[_]] {
  def defaultRoom: Validated[String, Room]
  def parse(
      userRef: Ref[F, Option[User]],
      text: String
  ): F[List[OutputMessage]]
}

case class TextCommand(left: String, right: Option[String])

object InputMessage {
  def make[F[_]: Monad](
      protocol: Protocol[F]
  ): InputMessage[F] = {
    new InputMessage[F] {
      override def defaultRoom: Validated[String, Room] = {
        Room("Default")
      }

      override def parse(
          userRef: Ref[F, Option[User]],
          text: String
      ): F[List[OutputMessage]] = {
        text.trim match {
          case "" => List(DiscardMessage).pure[F]
          case txt =>
            userRef.get.flatMap { u =>
              u.fold {
                defaultRoom match {
                  case Valid(r) =>
                    processText4UnReg(txt, protocol, userRef, r)
                  case Invalid(e) => List(ParsingError(u, e)).pure[F]
                }
              } { user => procesText4Reg(user, txt, protocol) }
            }
        }
      }
    }
  }

  private def commandParser: Parser[TextCommand] = {
    val leftSide = (char('/').string ~ alpha.rep.string).string
    val rightSide: Parser[(Unit, String)] = sp ~ alpha.rep.string
    ((leftSide ~ rightSide.?) <* wsp.rep.?).map((left, right) =>
      TextCommand(left, right.map((_, s) => s))
    )
  }

  private def parseToTextCommand(
      value: String
  ): Either[Parser.Error, TextCommand] = {
    commandParser.parseAll(value)
  }

  private def processText4UnReg[F[_]: Monad](
      text: String,
      protocol: Protocol[F],
      userRef: Ref[F, Option[User]],
      room: Room
  ): F[List[OutputMessage]] = {
    if (text.charAt(0) == '/') {
      parseToTextCommand(text).fold(
        _ =>
          List(
            ParsingError(
              None,
              "Characters after '/' must be between A-Z or a-z"
            )
          ).pure[F],
        v =>
          v match {
            case TextCommand("/name", Some(n)) =>
              protocol.isUsernameInUse(n).flatMap { b =>
                if (b) {
                  List(ParsingError(None, "User name already in use")).pure[F]
                } else {
                  protocol.register(n).flatMap {
                    case SuccessfulRegistration(u, _) =>
                      for {
                        _ <- userRef.update(_ => Some(u))
                        om <- protocol.enterRoom(u, room)
                      } yield {
                        List(
                          SendToUser(
                            u,
                            "/help shows all available commands"
                          )
                        ) ++ om
                      }
                    case parsingerror @ ParsingError(_, _) =>
                      List(parsingerror).pure[F]
                    case _ => List.empty[OutputMessage].pure[F]
                  }
                }
              }
            case _ =>
              List(UnsupportedCommand(None)).pure[F]
          }
      )
    } else {
      List(Register(None)).pure[F]
    }
  }

  private def procesText4Reg[F[_]: Applicative](
      user: User,
      text: String,
      protocol: Protocol[F]
  ): F[List[OutputMessage]] = {
    if (text.charAt(0) == '/') {
      parseToTextCommand(text).fold(
        _ =>
          List(
            ParsingError(
              None,
              "Characters after '/' must be between A-Z or a-z"
            )
          ).pure[F],
        v =>
          v match {
            case TextCommand("/name", Some(n)) =>
              List(ParsingError(Some(user), "You can't register again")).pure[F]
            case TextCommand("/room", Some(r)) =>
              Room(r) match {
                case Valid(room) =>
                  protocol.enterRoom(user, room)
                case Invalid(e) =>
                  List(ParsingError(Some(user), e)).pure[F]
              }
            case TextCommand("/help", None) =>
              protocol.help(user).map(List(_))
            case TextCommand("/rooms", None) =>
              protocol.listRooms(user)
            case TextCommand("/members", None) =>
              protocol.listMembers(user)
            case _ => List(UnsupportedCommand(Some(user))).pure[F]
          }
      )
    } else {
      protocol.chat(user, text)
    }
  }
}
