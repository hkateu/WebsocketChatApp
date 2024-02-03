package rockthejvm.websockets

import weaver.*
import cats.effect.kernel.Ref
import cats.effect.IO
import cats.kernel.Eq
// import cats.data.Validated.Valid
// import cats.data.Validated.Invalid

object CommandSpec extends SimpleIOSuite {
  val chatState = ChatState(Map.empty, Map.empty)
  val command = Ref.of[IO, ChatState](chatState).map(cs => Protocol.make[IO](cs))
  // TODO: Compare ADT values

  // given omEquality[M <: OutputMessage]: Eq[M] = 
  //   new Eq[M] {
  //     override def eqv(x: M, y: SubOM): Boolean =
  //     x.getClass() match 
  //       case SuccessfulRegistration => 
  //         y.getClass() match
  //           case SuccessfulRegistration => true
  //           case _ => false
  //       case Register =>
  //         y.getClass() match
  //           case SuccessfulRegistration => true
  //           case _ => false
  //       case ParsingError => 
  //         y.getClass() match
  //           case ParsingError => true
  //           case _ => false
  //       case UnsupportedCommand =>
  //         y.getClass() match
  //           case UnsupportedCommand => true
  //           case _ => false
  //       case KeepAlive => 
  //         y.getClass() match
  //           case KeepAlive => true
  //           case _ => false
  //       case DiscardMessage =>
  //         y.getClass() match
  //           case DiscardMessage => true
  //           case _ => false
  //       case SendToUser => 
  //         y.getClass() match
  //           case SendToUser => true
  //           case _ => false
  //       case ChatMsg =>
  //         y.getClass() match
  //           case ChatMsg => true
  //           case _ => false        
  // }

  // test("register('herbert') should return SuccessfulRegistration") {

  //   command.flatMap(_.register("herbert")).map{om => 
  //     om match
  //       case SuccessfulRegistration(user, msg) => expect.eql("herbert", user.name)
  //       case msg  @ _ => expect.eql(msg.getClass(), SuccessfulRegistration) 
  //   } 
  // }

  // test("register('herbertkateu') should return ParsingError") {
  //   given Eq[Option[User]] with {
  //     override def eqv(x: Option[User], y: Option[User]): Boolean =
  //       if(x.isDefined && y.isDefined){
  //         x.get.name == y.get.name
  //       }else{
  //         x == y
  //       } 
  //   }
  //   command.flatMap(_.register("herbertkateu")).map{om => 
  //     om match
  //       case ParsingError(user, msg) => expect.eql(None, user)
  //       case msg @ _ => expect.eql(msg.getClass(), ParsingError) 
  //   } 
  // }

  test("isUsernameInUse('herbert') should return false") {
    command.flatMap(_.isUsernameInUse("herbert")).map{b => 
      expect.eql(false, b)
    } 
  }

  // test("help('User('herbert')') should return SendToUser") {
  //       User("herbert") match
  //         case Valid(u) =>     
  //           command.flatMap(_.help(u)).map{om => 
  //             om match
  //               case SendToUser(user, msg) => 
  //                 expect.eql("herbert", user.name)
  //               case msg @ _ => expect.eql(msg.getClass(), SendToUser) 
                  
  //           }
  //         case Invalid(e) => expect("herbert", e)
  // }

  // test("listRooms('User('Herbert')') should return SendToUser") {
  //   User("herbert") match
  //     case Valid(u) =>     
  //       command.flatMap(_.listRooms(u)).map{om => 
  //         om match
  //           case SendToUser(user, msg) => expect.eql("herbert", user.name)
  //           case msg @ _ => expect.eql(msg.getClass(), SendToUser) 
  //       }
  //     case Invalid(e) => expect("herbert", e)
  // }

  // test("listMembers('User('Herbert')') should return SendToUser") {
  //   User("herbert") match
  //     case Valid(u) =>     
  //       command.flatMap(_.listRooms(u)).map{om => 
  //         om match
  //           case SendToUser(user, msg) => expect.eql("herbert", user.name)
  //           case msg @ _ => expect.eql(msg.getClass(), SendToUser) 
  //       }
  //     case Invalid(e) => expect("herbert", e)
  // }

  // test("enterRoom('User('Herbert')') should return SendToUser") {
  //   User("herbert") match
  //     case Valid(u) =>     
  //       Room("Scala") match
  //         case Valid(r) => 
  //           command.flatMap(_.enterRoom(u, r)).map{om => 
  //             om match
  //               case SendToUser(user, msg) => expect.eql("herbert", user.name)
  //               case msg @ _ => expect.eql(msg.getClass(), SendToUser) 
  //           }
  //         case Invalid(e) => expect.eql("Scala", e)
  //     case Invalid(e) => expect.eql("herbert", e)
  // }
}
