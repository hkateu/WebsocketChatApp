package rockthejvm.websockets

import weaver.*
import org.http4s.client.Client
import cats.effect.IO
import org.http4s.Request
import org.http4s.Method
import org.http4s.server.websocket.WebSocketBuilder2
import cats.effect.std.Queue
import fs2.concurrent.Topic
import cats.effect.kernel.Ref
import org.http4s.syntax.all.*
import org.http4s.HttpApp
import cats.effect.kernel.Resource
import org.http4s.Response
import org.http4s.Status

object RoutesSpec extends SimpleIOSuite {
  val service: IO[HttpApp[IO]] =
    for {
      wsb <- WebSocketBuilder2[IO]
      q <- Queue.unbounded[IO, OutputMessage]
      t <- Topic[IO, OutputMessage]
      cs <- Ref.of[IO, ChatState](ChatState(Map.empty, Map.empty))
      protocol <- IO(Protocol.make[IO](cs))
      im <- IO(InputMessage.make[IO](protocol))
    } yield new Routes().service(wsb, q, t, im, protocol, cs)

  val httpClient = service.map(Client.fromHttpApp(_))

  def sendRequest(request: Request[IO]): IO[Resource[IO, Response[IO]]] =
    httpClient.map(_.run(request))

  test("/chat.html should return 200 Ok") {
    val req = Request[IO](
      method = Method.GET,
      uri = uri"http://localhost:8080/chat.html"
    )
    sendRequest(req).flatMap { resource =>
      resource.use { res => IO(expect.eql(res.status, Status.Ok)) }
    }
  }

  test("/metrics should return 200 Ok") {
    val req = Request[IO](
      method = Method.GET,
      uri = uri"http://localhost:8080/metrics"
    )
    sendRequest(req).flatMap { resource =>
      resource.use { res => IO(expect.all(res.status == Status.Ok)) }
    }
  }

  test("/metrics should return String") {
    val req = Request[IO](
      method = Method.GET,
      uri = uri"http://localhost:8080/metrics"
    )
    httpClient
      .flatMap(client => client.expect[String](req))
      .map(s => expect.eql(true, s.isInstanceOf[String]))
  }
}
