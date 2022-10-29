package calculator

import TweetLength._

class TweetLengthSuite extends munit.FunSuite {
  /******************
   ** TWEET LENGTH **
   ******************/

  def tweetLength(text: String): Int =
    text.codePointCount(0, text.length)

  test("tweetRemainingCharsCount with a constant signal") {
    val result = tweetRemainingCharsCount(Var("hello world"))
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    val tooLong = "foo" * 200
    val result2 = tweetRemainingCharsCount(Var(tooLong))
    assert(result2() == MaxTweetLength - tweetLength(tooLong))
  }

  test("tweetRemainingCharsCount with a supplementary char") {
    val result = tweetRemainingCharsCount(Var("foo blabla \uD83D\uDCA9 bar"))
    assert(result() == MaxTweetLength - tweetLength("foo blabla \uD83D\uDCA9 bar"))
  }

  test("tweetRemainingCharsCount's result signal should follow the input signal") {
    val input = Var("hello world")
    val result = tweetRemainingCharsCount(input)
    assert(result() == MaxTweetLength - tweetLength("hello world"))

    input() = "foobar"
    assert(result() == MaxTweetLength - tweetLength("foobar"))
    input() = "こんにちは"
    assert(result() == MaxTweetLength - tweetLength("こんにちは"))
  }

  test("colorForRemainingCharsCount with a constant signal") {
    val resultGreen1 = colorForRemainingCharsCount(Var(52))
    assertEquals(resultGreen1(), "green")
    val resultGreen2 = colorForRemainingCharsCount(Var(15))
    assertEquals(resultGreen2(), "green")

    val resultOrange1 = colorForRemainingCharsCount(Var(12))
    assertEquals(resultOrange1(), "orange")
    val resultOrange2 = colorForRemainingCharsCount(Var(0))
    assertEquals(resultOrange2(), "orange")

    val resultRed1 = colorForRemainingCharsCount(Var(-1))
    assertEquals(resultRed1(), "red")
    val resultRed2 = colorForRemainingCharsCount(Var(-5))
    assertEquals(resultRed2(), "red")
  }

  import scala.concurrent.duration._
  override val munitTimeout = 10.seconds
}

