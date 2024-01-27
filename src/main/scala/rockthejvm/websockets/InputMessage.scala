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
  ): F[Seq[OutputMessage]]
}

case class TextCommand(left: String, right: Option[String])

object InputMessage {
  def make[F[_]: Monad](
      command: Command[F]
  ): InputMessage[F] = {
    new InputMessage[F] {
      override def defaultRoom: Validated[String, Room] = {
        Room("Default")
      }

      override def parse(
          userRef: Ref[F, Option[User]],
          text: String
      ): F[Seq[OutputMessage]] = {
        text.trim match {
          case "" => Seq(DiscardMessage).pure[F]
          case txt @ _ =>
            userRef.get.flatMap { u =>
              u.fold {
                defaultRoom match {
                  case Valid(r) =>
                    processText4UnReg(txt, command, userRef, r)
                  case Invalid(e) => Seq(ParsingError(u, e)).pure[F]
                }
              } { user => procesText4Reg(user, txt, command) }
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
      cmd: Command[F],
      userRef: Ref[F, Option[User]],
      room: Room
  ): F[Seq[OutputMessage]] = {
    if (text.charAt(0) == '/') {
      parseToTextCommand(text).fold(
        _ =>
          Seq(
            ParsingError(
              None,
              "Characters after '/' must be between A-Z or a-z"
            )
          ).pure[F],
        v =>
          v match {
            case TextCommand("/name", Some(n)) =>
              cmd.isUsernameInUse(n).flatMap { b =>
                if (b) {
                  Seq(ParsingError(None, "User name already in use")).pure[F]
                } else {
                  cmd.register(n).flatMap {
                    case SuccessfulRegistration(u, _) =>
                      for {
                        _ <- userRef.update(_ => Some(u))
                        om <- cmd.enterRoom(u, room)
                      } yield {
                        Seq(
                          SendToUser(
                            u,
                            "/help shows all available commands"
                          )
                        ) ++ om
                      }
                    case parsingerror @ ParsingError(_, _) =>
                      Seq(parsingerror).pure[F]
                    case _ => Seq.empty[OutputMessage].pure[F]
                  }
                }
              }
            case _ =>
              Seq(UnsupportedCommand(None)).pure[F]
          }
      )
    } else {
      Seq(Register(None)).pure[F]
    }
  }

  private def procesText4Reg[F[_]: Applicative](
      user: User,
      text: String,
      cmd: Command[F]
  ): F[Seq[OutputMessage]] = {
    if (text.charAt(0) == '/') {
      parseToTextCommand(text).fold(
        _ =>
          Seq(
            ParsingError(
              None,
              "Characters after '/' must be between A-Z or a-z"
            )
          ).pure[F],
        v =>
          v match {
            case TextCommand("/name", Some(n)) =>
              Seq(ParsingError(Some(user), "You can't register again")).pure[F]
            case TextCommand("/room", Some(r)) =>
              Room(r) match {
                case Valid(room) =>
                  cmd.enterRoom(user, room)
                case Invalid(e) =>
                  Seq(ParsingError(Some(user), e)).pure[F]
              }
            case TextCommand("/help", None) =>
              cmd.help(user)
            case TextCommand("/rooms", None) =>
              cmd.listRooms(user)
            case TextCommand("/members", None) =>
              cmd.listMembers(user)
            case _ => Seq(UnsupportedCommand(Some(user))).pure[F]
          }
      )
    } else {
      cmd.chat(user, text)
    }
  }
}
