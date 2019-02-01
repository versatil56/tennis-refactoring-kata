package tennis

trait TennisGame {
  def wonPoint(x: String)

  def calculateScore(): String
}

case class Result(P1point:Int = 0)

class TennisGameImplementation(val player1Name: String, val player2Name: String) extends TennisGame {

  var P1point = 0
  var P2point = 0

  var P1res = ""
  var P2res = ""

  def calculateScore(): String = {
    var score = ""



    (P1point, P2point) match {
      case (p1, p2) if p1 == p2 =>
        score = P1point match {
          case 0 => "Love-All"
          case 1 => "Fifteen-All"
          case 2 => "Thirty-All"
          case _ => "Deuce"

        }

      case (p1, 0) if p1 > 0 =>
        P1res = processScoreIfOnePlayerIsZero(P1point)
        P2res = "Love"
        score = P1res + "-" + P2res

      case (0, p2) if p2 > 0 =>
        P2res = processScoreIfOnePlayerIsZero(P2point)
        P1res = "Love"
        score = P1res + "-" + P2res

      case _ => ()
    }

    if (playerOneIsLeading) {
      P1res = scoreIf30or40(P1point).getOrElse(P1res)

      P2res = scoreIf15or30(P2point).getOrElse(P2res)
      score = P1res + "-" + P2res
    }

    if (playerTwoIsLeading) {
      P2res = scoreIf30or40(P2point).getOrElse(P2res)
      P1res = scoreIf15or30(P1point).getOrElse(P1res)
      score = P1res + "-" + P2res
    }

    score = outcome(P1point, P2point, "player1").getOrElse(score)
    outcome(P2point, P1point, "player2").getOrElse(score)

  }

  def processScoreIfOnePlayerIsZero(point: Int): String ={
    point match {
      case 1 => "Fifteen"
      case 2 => "Thirty"
      case 3 => "Forty"
      case _ => ""
    }
  }

  private def playerTwoIsLeading = {
    P2point > P1point && P2point < 4
  }

  private def playerOneIsLeading = {
    P1point > P2point && P1point < 4
  }




  def scoreIf30or40(point1: Int): Option[String] = {
    point1 match {
      case 2 => Some("Thirty")
      case 3 => Some("Forty")
      case _ => None
    }
  }

  def scoreIf15or30(point1: Int): Option[String] = {
    point1 match {
      case 1 => Some("Fifteen")
      case 2 => Some("Thirty")
      case _ => None
    }
  }

  private def outcome(point1: Int, point2:Int, playerAdv: String): Option[String] = {

    if (point1 >= 4 && point2 >= 0 && (point1 - point2) >= 2) {
      Some(s"Win for $playerAdv")
    }
    else if (point1 > point2 && point2 >= 3) {
      Some(s"Advantage $playerAdv")
    }
    else {
      None
    }
  }


  def SetP1Score(number: Int) {

    for (i <- 0 until number by 1) {
      P1Score()
    }

  }

  def SetP2Score(number: Int) {

    for (i <- 0 until number by 1) {
      P2Score()
    }

  }

  def P1Score() {
    P1point += 1
  }

  def P2Score() {
    P2point += 1
  }

  def wonPoint(player: String) {
    if (player == "player1")
      P1Score()
    else
      P2Score()
  }
}