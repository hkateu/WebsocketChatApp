package rockthejvm.websockets

import cats.Monad
import cats.data.Validated.Valid
import cats.data.Validated.Invalid
import cats.effect.kernel.Ref
import cats.syntax.all.*
import cats.Applicative

trait Command[F[_]] {
  def register(name: String): F[OutputMessage]
  def isUsernameInUse(name: String): F[Boolean]
  def enterRoom(user: User, room: Room): F[Seq[OutputMessage]]
  def chat(user: User, text: String): F[Seq[OutputMessage]]
  def help(user: User): F[Seq[OutputMessage]]
  def listRooms(user: User): F[Seq[OutputMessage]]
  def listMembers(user: User): F[Seq[OutputMessage]]
  def disconnect(userRef: Ref[F, Option[User]]): F[Seq[OutputMessage]]
}

object Command {
  def make[F[_]: Monad](chatState: Ref[F, ChatState]): Command[F] =
    new Command[F] {
      override def register(name: String): F[OutputMessage] =
        User(name) match
          case Valid(u) =>
            SuccessfulRegistration(u).pure[F]
          case Invalid(e) =>
            ParsingError(None, e.toString).pure[F]

      override def isUsernameInUse(name: String): F[Boolean] = 
        chatState.get.map{cs => 
          cs.userRooms.keySet.exists(_.name == name)
        }
      override def enterRoom(user: User, room: Room): F[Seq[OutputMessage]] =
        chatState.get.flatMap { cs =>
          cs.userRooms.get(user) match
            case Some(r) =>
              if (r == room) {
                Seq(
                  SendToUser(user, s"You are already in the ${room.room} room")
                ).pure[F]
              } else {
                val leaveMessages = removeFromCurrentRoom(chatState, user)
                val enterMessages = addToRoom(chatState, user, room)
                for {
                  leave <- leaveMessages
                  enter <- enterMessages
                } yield leave ++ enter
              }
            case None =>
              addToRoom(chatState, user, room)
        }

      override def chat(user: User, text: String): F[Seq[OutputMessage]] =
        for
          cs <- chatState.get
          output <- cs.userRooms.get(user) match
            case Some(room) =>
              broadcastMessage(cs, room, ChatMsg(user, user, text))
            case None =>
              Seq(SendToUser(user, "You are not currently in a room")).pure[F]
        yield output

      override def help(user: User): F[Seq[OutputMessage]] =
        val text = """Commands:
            | /help             - Show this text
            | /room             - Change to default/entry room
            | /room <room name> - Change to specified room
            | /rooms            - List all rooms
            | /members          - List members in current room
        """.stripMargin
        Seq(SendToUser(user, text)).pure[F]

      override def listRooms(user: User): F[Seq[OutputMessage]] =
        chatState.get.map { cs =>
          val roomList =
            cs.roomMembers.keys
              .map(_.room)
              .toList
              .sorted
              .mkString("Rooms:\n\t", "\n\t", "")
          Seq(SendToUser(user, roomList))
        }

      override def listMembers(user: User): F[Seq[OutputMessage]] =
        chatState.get.map { cs =>
          val memberList =
            cs.userRooms.get(user) match
              case Some(room) =>
                cs.roomMembers
                  .getOrElse(room, Set())
                  .map(_.name)
                  .toList
                  .sorted
                  .mkString("Room Members:\n\t", "\n\t", "")
              case None => "You are not currently in a room"
          Seq(SendToUser(user, memberList))
        }
      override def disconnect(
          userRef: Ref[F, Option[User]]
      ): F[Seq[OutputMessage]] =
        userRef.get.flatMap {
          case Some(user) => removeFromCurrentRoom(chatState, user)
          case None       => Seq.empty[OutputMessage].pure[F]
        }
    }

  private def addToRoom[F[_]: Monad](
      stateRef: Ref[F, ChatState],
      user: User,
      room: Room
  ): F[Seq[OutputMessage]] = {
    stateRef
      .updateAndGet { cs =>
        val updatedMemberList = cs.roomMembers.getOrElse(room, Set()) + user
        ChatState(
          cs.userRooms + (user -> room),
          cs.roomMembers + (room -> updatedMemberList)
        )
      }
      .flatMap(
        broadcastMessage(
          _,
          room,
          SendToUser(user, s"${user.name} has joined the ${room.room} room")
        )
      )
  }

  private def broadcastMessage[F[_]: Applicative](
      cs: ChatState,
      room: Room,
      om: OutputMessage
  ): F[Seq[OutputMessage]] =
    cs.roomMembers
      .getOrElse(room, Set.empty[User])
      .map(u =>
        om match
          case SendToUser(user, msg)  => SendToUser(u, msg)
          case ChatMsg(from, to, msg) => ChatMsg(from, u, msg)
          case _                      => DiscardMessage
      )
      .toSeq
      .pure[F]

  private def removeFromCurrentRoom[F[_]: Monad](
      stateRef: Ref[F, ChatState],
      user: User
  ): F[Seq[OutputMessage]] =
    stateRef.get.flatMap { cs =>
      cs.userRooms.get(user) match
        case Some(room) =>
          val updateMembers = cs.roomMembers.getOrElse(room, Set()) - user
          stateRef
            .update(ccs =>
              ChatState(
                ccs.userRooms - user,
                if (updateMembers.isEmpty) {
                  ccs.roomMembers - room
                } else {
                  ccs.roomMembers + (room -> updateMembers)
                }
              )
            )
            .flatMap(_ =>
              broadcastMessage(
                cs,
                room,
                SendToUser(user, s"${user.name} has left the ${room.room} room")
              )
            )
        case None =>
          Seq.empty[OutputMessage].pure[F]
    }
}
