package com.krishna.chapter1

/**
  * Created by mishrk3 on 2/22/2016.
  */

/**
  * Declaring Data Type
  */
case class Player(name: String, score: Int) {

  /**
    * The below code does not return anything but have side effect
    *
    * @param p A player object
    *
    */
  def printWinner(p: Player): Unit =
    println(p.name + " is the winner!")

  /**
    * Compares the score of two player and declare the Winner
    *
    * @param p1 A player object
    * @param p2 A player object
    */
  def declareWinner(p1: Player, p2: Player): Unit =
    if (p1.score > p2.score) printWinner(p1)
    else printWinner(p2)
}

case class PlayerMoreModular(name: String, score: Int) {

  def printWinner(p: PlayerMoreModular): Unit =
    println(p.name + " is the winner!")

  def winner(p1: PlayerMoreModular, p2: PlayerMoreModular): PlayerMoreModular =
    if (p1.score > p2.score) p1 else p2

  def declareWinner(p1: PlayerMoreModular, p2: PlayerMoreModular): Unit =
    printWinner(winner(p1, p2))

  val players = List(PlayerMoreModular("Sue", 7),
    PlayerMoreModular("Bob", 8),
    PlayerMoreModular("Joe", 4))
  /**
    * separating the logic of calculating winner from calling printWinner
    * helped us to use the code even for list of players.
    * reduceLeft on List data type is from the standard Scala library. The expression will compare all the players
    * in the list and return the one with the highest score.
    * This usage of winner would not have been possible when the side effect of displaying
    * the result was interleaved with the logic for computing the winner.
    */
  val p = players.reduceLeft(winner)
  printWinner(p)
}