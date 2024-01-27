package rockthejvm.websockets

import weaver.*

object UtilSpec extends SimpleIOSuite {
  pureTest("Test for a valid user") {
    val okUser: Boolean = User("herbert").isValid
    expect.eql(true, okUser)
  }

  pureTest("Test for an invalid user") {
    val longName: Boolean = User("herbertkateu").isInvalid
    val shortName: Boolean = User("h").isInvalid
    expect.all(true == longName, true == shortName)
  }

  pureTest("Test for a valid room") {
    val okRoom: Boolean = Room("scala").isValid
    expect.eql(true, okRoom)
  }

  pureTest("Test for an invalid room") {
    val longName: Boolean = Room("scalaIsAwesome").isInvalid
    val shortName: Boolean = User("s").isInvalid
    expect.all(true == longName, true == shortName)
  }

  pureTest(
    "ChatState must initialize to Map[User, Room] and Map[Room, Set[User]]"
  ) {
    val emptyChatState =
      ChatState(Map.empty[User, Room], Map.empty[Room, Set[User]])
    expect.all(
      emptyChatState.userRooms == Map.empty[User, Room],
      emptyChatState.roomMembers == Map.empty[Room, Set[User]]
    )
  }
}
