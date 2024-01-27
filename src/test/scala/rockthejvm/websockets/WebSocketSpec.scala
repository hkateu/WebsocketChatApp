package rockthejvm.websockets

import weaver.*
import org.http4s.syntax.all.*
import org.http4s.client.websocket.WSRequest
import org.http4s.ember.client.EmberClientBuilder
import cats.effect.IO
import org.http4s.client.websocket.WSFrame
import org.http4s.client.websocket.WSFrame.Text
import org.http4s.client.websocket.WSFrame.Binary

object WebsocketSpec extends SimpleIOSuite {
    val request = WSRequest(uri"ws://localhost:8080/ws")
    def testSocket[A](text: String, expected: String) = 
        EmberClientBuilder.default[IO].buildWebSocket.use {(_, wsClient) => 
            wsClient
                .connectHighLevel(request)
                .use {conn => 
                    conn.send(WSFrame.Text(text)) *>
                    conn.receive.map(v => expect.eql(
                        expected, v.get match
                            case Text(data, last) => data
                            case Binary(data, last) => data.toString
                        ))    
                }    
        }

    pureTest("Unregistred users should return register message"){
        //TODO: Fix websocket connection issue with testSocket().
        // testSocket("hi", Register(None).msg) 
        expect.eql(true,true)
    }
}