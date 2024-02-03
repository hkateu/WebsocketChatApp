package rockthejvm.websockets

import fs2.io.file.Files
import cats.effect.Temporal
import org.http4s.server.websocket.WebSocketBuilder2
import cats.effect.std.Queue
import cats.effect.kernel.Ref
import fs2.concurrent.Topic
import org.http4s.{HttpApp, HttpRoutes}
import org.http4s.dsl.Http4sDsl
import org.http4s.StaticFile
import cats.syntax.all.*
import fs2.Stream
import org.http4s.websocket.WebSocketFrame
import fs2.Pipe
import scala.concurrent.duration.*
import io.circe.generic.auto.*
import io.circe.syntax.*
import org.http4s.MediaType
import org.http4s.headers.`Content-Type`

class Routes[F[_]: Files: Temporal] extends Http4sDsl[F] {
  def service (
      wsb: WebSocketBuilder2[F],
      q: Queue[F, OutputMessage],
      t: Topic[F, OutputMessage],
      im: InputMessage[F],
      protocol: Protocol[F],
      cs: Ref[F, ChatState]
  ): HttpApp[F] = {
    HttpRoutes.of[F] {
      case request @ GET -> Root / "chat.html" =>
        StaticFile
          .fromPath(
            fs2.io.file.Path("src/main/resources/chat.html"),
            Some(request)
          )
          .getOrElseF(NotFound())

      case GET -> Root / "metrics" =>
        def currentState: F[String] = {
          cs.get.map { cState =>
            s"""
                |<!Doctype html>
                |<title>Chat Server State</title>
                |<body>
                |<pre>Users: ${cState.userRooms.keys.size}</pre>
                |<pre>Rooms: ${cState.roomMembers.keys.size}</pre>
                |<pre>Overview: 
                |${cState.roomMembers.keys.toList
                .map(room =>
                  cState.roomMembers
                    .getOrElse(room, Set())
                    .map(_.name)
                    .toList
                    .sorted
                    .mkString(s"${room.room} Room Members:\n\t", "\n\t", "")
                )
                .mkString("Rooms:\n\t", "\n\t", "")}
                |</pre>
                |</body>
                |</html>
            """.stripMargin
          }
        }

        currentState.flatMap { currState =>
          Ok(currState, `Content-Type`(MediaType.text.html))
        }

      case GET -> Root / "ws" =>
        for {
          uRef <- Ref.of[F, Option[User]](None)
          uQueue <- Queue.unbounded[F, OutputMessage]
          ws <- wsb.build(
            send(t, uQueue, uRef),
            receive(protocol, im, uRef, q, uQueue)
          )
        } yield ws
    }
  }.orNotFound

  private def handleWebSocketStream (
      wsf: Stream[F, WebSocketFrame],
      im: InputMessage[F],
      protocol: Protocol[F],
      uRef: Ref[F, Option[User]]
  ): Stream[F, OutputMessage] = {
    for {
      sf <- wsf
      om <- Stream.evalSeq(
        sf match {
          case WebSocketFrame.Text(text, _) =>
            im.parse(uRef, text)
          case WebSocketFrame.Close(_) =>
            protocol.disconnect(uRef)
        }
      )
    } yield om
  }

  private def receive (
      protocol: Protocol[F],
      im: InputMessage[F],
      uRef: Ref[F, Option[User]],
      q: Queue[F, OutputMessage],
      uQueue: Queue[F, OutputMessage]
  ): Pipe[F, WebSocketFrame, Unit] = { wsf =>
    handleWebSocketStream(wsf, im, protocol, uRef)
      .evalMap { m =>
        uRef.get.flatMap {
          case Some(_) =>
            q.offer(m)
          case None =>
            uQueue.offer(m)
        }
      }
      .concurrently {
        Stream
          .awakeEvery(30.seconds)
          .map(_ => KeepAlive)
          .foreach(uQueue.offer)
      }
  }

  private def filterMsg (
      msg: OutputMessage,
      userRef: Ref[F, Option[User]]
  ): F[Boolean] = {
    msg match {
      case DiscardMessage => false.pure[F]
      case sendtouser @ SendToUser(_, _) =>
        userRef.get.map { _.fold(false)(u => sendtouser.forUser(u)) }
      case chatmsg @ ChatMsg(_, _, _) =>
        userRef.get.map { _.fold(false)(u => chatmsg.forUser(u)) }
      case _ => true.pure[F]
    }
  }

  private def processMsg(msg: OutputMessage): WebSocketFrame = {
    msg match {
      case KeepAlive => WebSocketFrame.Ping()
      case msg @ _   => WebSocketFrame.Text(msg.asJson.noSpaces)
    }
  }

  private def send (
      t: Topic[F, OutputMessage],
      uQueue: Queue[F, OutputMessage],
      uRef: Ref[F, Option[User]]
  ): Stream[F, WebSocketFrame] = {
    def uStream =
      Stream
        .fromQueueUnterminated(uQueue)
        .filter {
          case DiscardMessage => false
          case _              => true
        }
        .map(processMsg)

    def mainStream =
      t.subscribe(maxQueued = 1000)
        .evalFilter(filterMsg(_, uRef))
        .map(processMsg)

    Stream(uStream, mainStream).parJoinUnbounded
  }

}
