package monopoly

import scala.util.Random
import scala.collection.mutable

class Player(val initialBalance: Int) {
  private var balance: Int = initialBalance

  def getBalance(): Int = balance

  def increaseBalance(valueToIncrease: Int): Unit =
    balance += valueToIncrease

  def decreaseBalance(valueToDecrease: Int): Unit =
    balance -= valueToDecrease
}

object Player {
  def rollDie(sizes: Int = 6): Int = {
    val start = 1
    val randomInt = Random.nextInt(sizes)

    start + randomInt
  }

  def transfer(from: Player, to: Player, amount: Int): Unit = {
    if (!(from eq to)) {
      to.increaseBalance(from.getBalance() min amount)
      from.decreaseBalance(amount)
    }
  }
}

trait BuyStrategy {
  def decideToBuy(property: Property): Boolean
}

abstract class AbstractPlayerWithStrategy(initialBalance: Int)
    extends Player(initialBalance) with BuyStrategy

trait ImpulsiveBuyStrategy extends BuyStrategy {
  def decideToBuy(property: Property): Boolean = true
}

trait DemandingBuyStrategy extends BuyStrategy {
  def decideToBuy(property: Property): Boolean = property.rentCost >= 50
}

trait RandomBuyStrategy extends BuyStrategy {
  def decideToBuy(property: Property): Boolean = Random.nextDouble() >= 0.5
}

class ImpulsivePlayer(initialBalance: Int)
    extends AbstractPlayerWithStrategy(initialBalance)
    with ImpulsiveBuyStrategy

class DemandingPlayer(initialBalance: Int)
    extends AbstractPlayerWithStrategy(initialBalance)
    with DemandingBuyStrategy

class RandomPlayer(initialBalance: Int)
    extends AbstractPlayerWithStrategy(initialBalance)
    with RandomBuyStrategy

class CautiousPlayer(initialBalance: Int)
    extends AbstractPlayerWithStrategy(initialBalance) {
  def decideToBuy(property: Property): Boolean = getBalance() >= property.sellCost + 80
}

class Property(val sellCost: Int, val rentCost: Int) {
  private var owner: Option[Player] = None

  def resetOwnership(): Unit =
    owner = None

  def getOwner(): Option[Player] = owner
}

object Property {
  def changeOwnership(property: Property, newOwner: Player): Boolean = {
    val changeIsFeasible: Boolean = newOwner.getBalance() >= property.sellCost

    if (changeIsFeasible) {
      newOwner decreaseBalance property.sellCost
      property.owner = Some(newOwner)
    }

    changeIsFeasible
  }

  def random(maxSellCost: Int = 300, maxRentCost: Int = 80): Property = {
    val start = 1
    val sellCost = Random.nextInt(maxSellCost)
    val rentCost = Random.nextInt(maxRentCost)

    new Property(start + sellCost, start + rentCost)
  }
}

class Board(val properties: Array[Property])

object Board {
  def random(numProperties: Int = 20, maxSellCost: Int = 300, maxRentCost: Int = 80): Board = {
    val properties = LazyList
      .continually(Property.random(maxSellCost, maxRentCost))
      .take(numProperties)
      .toArray

    new Board(properties)
  }
}

class Game(val players: Seq[AbstractPlayerWithStrategy], val board: Board) {
  private val position: mutable.Map[AbstractPlayerWithStrategy, Int] = mutable.Map.from(
    players zip LazyList.continually(0)
  )

  private val playersSequence: Iterator[AbstractPlayerWithStrategy] = LazyList
    .continually(activePlayers)
    .flatten
    .iterator

  private def activePlayers: Seq[AbstractPlayerWithStrategy] = players.filter(_.getBalance() >= 0)

  private def resetOwnershipOfInactivePlayersProperties(): Unit = {
    board.properties
      .filter(_.getOwner().isDefined)
      .filter(_.getOwner().get.getBalance() < 0)
      .foreach(_.resetOwnership())
  }

  private def updatePlayerPosition(
    player: AbstractPlayerWithStrategy,
    squaresToAdvance: Int
  ): Unit = {
    position(player) = position(player) + squaresToAdvance

    if (position(player) >= board.properties.length) {
      player.increaseBalance(Game.rewardForCompletingTrack)

      position(player) %= board.properties.length
    }
  }

  private def performPlayerInteractionWithProperty(
    player: AbstractPlayerWithStrategy,
    property: Property
  ): Unit = {
    if (property.getOwner() == None) {
      if (player.decideToBuy(property)) {
        Property.changeOwnership(property, player)
      }
    } else {
      val owner = property.getOwner().get

      Player.transfer(player, owner, property.rentCost)
    }
  }

  def turn(): Unit = {
    resetOwnershipOfInactivePlayersProperties()

    val player = playersSequence.next()

    val dieOutcome = Player.rollDie()

    updatePlayerPosition(player, dieOutcome)

    val property = board properties position(player)

    performPlayerInteractionWithProperty(player, property)
  }

  def getWinner(): Option[AbstractPlayerWithStrategy] = {
    activePlayers match {
      case Seq(onlyActivePlayer) => Some(onlyActivePlayer)
      case _ => None
    }
  }
}

object Game {
  val rewardForCompletingTrack = 100

  def random(
    players: Seq[AbstractPlayerWithStrategy],
    numProperties: Int = 20,
    maxSellCost: Int = 300,
    maxRentCost: Int = 80
  ): Game = {
    val board = Board.random(numProperties, maxSellCost, maxRentCost)

    new Game(players, board)
  }
}

class GameRunner(
  val playersGenerator: () => Seq[AbstractPlayerWithStrategy],
  val maxNumTurns: Int = 1000
) {
  private class Status(val numTurns: Int, val winner: AbstractPlayerWithStrategy)

  private var gameStatus: mutable.Map[Game, Status] = mutable.Map.empty

  def numTimedOutGames: Int = gameStatus.values.count(_.numTurns == maxNumTurns)

  def averageNumTurns: Double = gameStatus.values.map(_.numTurns).sum.toDouble / gameStatus.size

  def winningPercentagePerBehavior: Map[String, Double] = Map.from(
    Seq("Impulsive", "Demanding", "Cautious", "Random") zip
      Seq(
        gameStatus.values.count(_.winner.isInstanceOf[ImpulsivePlayer]),
        gameStatus.values.count(_.winner.isInstanceOf[DemandingPlayer]),
        gameStatus.values.count(_.winner.isInstanceOf[CautiousPlayer]),
        gameStatus.values.count(_.winner.isInstanceOf[RandomPlayer])
      ).map(_.toDouble / gameStatus.size)
  )

  def mostSuccessfullBehavior: String = winningPercentagePerBehavior.maxBy(_._2)._1

  def playGame(): Unit = {
    val players = playersGenerator()

    val game = Game.random(players)

    var numTurns = 0

    while (game.getWinner().isEmpty && numTurns < maxNumTurns) {
      game.turn()

      numTurns += 1
    }

    val winner = game.getWinner().getOrElse(players.maxBy(_.getBalance()))

    val status = new Status(numTurns, winner)

    gameStatus(game) = status
  }
}

object Simulation extends App {
  val numSimulations = 300
  val initialBalamce = 300

  val gameRunner = new GameRunner(
    () => Seq(
      new ImpulsivePlayer(initialBalamce),
      new DemandingPlayer(initialBalamce),
      new CautiousPlayer(initialBalamce),
      new RandomPlayer(initialBalamce)
    )
  )

  for (_ <- 0 to numSimulations) {
    gameRunner.playGame()
  }

  println(s"Number of timed out matches: ${gameRunner.numTimedOutGames}")
  println(s"Number of turns on average: ${gameRunner.averageNumTurns}")

  println("Winning percentage of each behavior:")
  gameRunner.winningPercentagePerBehavior.foreach(tuple => {
    val behavior = tuple._1
    val percentage = tuple._2

    println(s"  - ${behavior}: ${percentage}")
  })

  println(s"Most successful behavior: ${gameRunner.mostSuccessfullBehavior}")
}
