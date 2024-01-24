package rockthejvm.websockets

import munit.CatsEffectSuite
import cats.effect.IO
import Routes.service
import org.http4s.Request
import org.http4s.Method
import org.http4s.implicits.uri
import org.http4s.Status
import org.http4s.server.websocket.WebSocketBuilder2

class HelloWorldSpec extends CatsEffectSuite:
  ???
  // test("/chat.html returns status code 200") {
  //   val chatPageStatus = WebSocketBuilder2[IO].flatMap{wsb => 
  //     service[IO](wsb)
  //       .run(Request(method = Method.GET, uri = uri"/chat.html"))
  //       .map(_.status)  
  //   }
  //   assertIO(chatPageStatus, Status.Ok)
  // }
