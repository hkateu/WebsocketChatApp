package rockthejvm.websockets

import munit.CatsEffectSuite
import cats.effect.IO
import Routes.service
import org.http4s.Request
import org.http4s.Method
import org.http4s.implicits.uri
import org.http4s.Status
import org.http4s.server.websocket.WebSocketBuilder2
// import org.http4s.Response
// import org.http4s.client.websocket.{WSClient, WSConnection}

class HelloWorldSpec extends CatsEffectSuite:
  test("/chat.html returns status code 200") {
    val chatPageStatus = WebSocketBuilder2[IO].flatMap{wsb => 
      service[IO](wsb)
        .run(Request(method = Method.GET, uri = uri"/chat.html"))
        .map(_.status)  
    }
    assertIO(chatPageStatus, Status.Ok)
  }

  // test("/ws test"){
  //   val connect = WSConnection
  //  WSClient[IO](false)()
  //   // println(retHelloWorld.flatMap(_.as[String]))
  //   assertIO(retHelloWorld.flatMap(_.as[String]), "{\"message\":\"Hello, world\"}")
  // }

  // test("HelloWorld returns hello world message") {
  //   assertIO(retHelloWorld.flatMap(_.as[String]), "{\"message\":\"Hello, world\"}")
  // }

  // private[this] val retHelloWorld: IO[Response[IO]] =
  //   val getHW = Request[IO](Method.GET, uri"/ws")
  //   val chatPageStatus = WebSocketBuilder2[IO].flatMap(service[IO](_).run(getHW))
  //   chatPageStatus
