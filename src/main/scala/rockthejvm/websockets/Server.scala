package rockthejvm.websockets

import cats.effect.kernel.Async
import fs2.io.file.Files
import fs2.io.net.Network
import cats.effect.std.Queue
import fs2.concurrent.Topic
import cats.effect.kernel.Ref
import com.comcast.ip4s.*
import cats.syntax.all.*
import org.http4s.ember.server.EmberServerBuilder

object Server {
  def server[F[_]: Async: Files: Network](
      q: Queue[F, OutputMessage],
      t: Topic[F, OutputMessage],
      im: InputMessage[F],
      cmd: Command[F],
      cs: Ref[F, ChatState]
  ): F[Unit] = {
    val host = host"0.0.0.0"
    val port = port"8080"
    EmberServerBuilder
      .default[F]
      .withHost(host)
      .withPort(port)
      .withHttpWebSocketApp(wsb => new Routes().service(wsb, q, t, im, cmd, cs))
      .build
      .useForever
      .void
  }
}
