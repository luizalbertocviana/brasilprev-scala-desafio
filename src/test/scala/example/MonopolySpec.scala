package monopoly

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.concurrent.Eventually
import org.scalatest.Inspectors
import org.scalatest.PrivateMethodTester

abstract class TestSuite
    extends AnyFlatSpec
    with Matchers
    with Inspectors
    with PrivateMethodTester

abstract class TestSuiteForRandomness extends TestSuite with Eventually

class PlayerSpec extends TestSuiteForRandomness {
  "The Player class" should "instantiate player with given balance" in {
    val balance = 300
    val player = new Player(balance)

    player.getBalance() shouldEqual balance
  }

  it should "be able to manage its balance correctly" in {
    val balance = 300
    val player = new Player(balance)
    val amountToIncrease = 50
    val amountToDecrease = 40

    player increaseBalance amountToIncrease
    player.getBalance() shouldEqual (balance + amountToIncrease)

    player decreaseBalance amountToDecrease
    player.getBalance() shouldEqual (balance + amountToIncrease - amountToDecrease)
  }

  "The Player object" should "provide a rollDie method returning numbers between 1 and 6" in {
    eventually {Player.rollDie() shouldEqual 1}
    eventually {Player.rollDie() shouldEqual 2}
    eventually {Player.rollDie() shouldEqual 3}
    eventually {Player.rollDie() shouldEqual 4}
    eventually {Player.rollDie() shouldEqual 5}
    eventually {Player.rollDie() shouldEqual 6}
  }

  it should "provide a transfer method which correctly updates players' balances to simulate a transfer" in {
    val initialBalance = 100
    val amountToTransfer = 50
    val playerA = new Player(initialBalance)
    val playerB = new Player(initialBalance)

    Player.transfer(playerA, playerB, amountToTransfer)
    playerA.getBalance() shouldEqual (initialBalance - amountToTransfer)
    playerB.getBalance() shouldEqual (initialBalance + amountToTransfer)
  }
}

class ImpulsiveBuyStrategySpec extends TestSuite {
  "The ImpulsiveBuyStrategy trait" should "provide a decideToBuy method which always returns true" in {
    val impulsiveBuyStrategy = new ImpulsiveBuyStrategy {}
    val randomProperty = Property.random()

    impulsiveBuyStrategy.decideToBuy(randomProperty) shouldEqual true
  }
}

class DemandingBuyStrategySpec extends TestSuite {
  "The DemandingBuyStrategy trait" should "provide a decideToBuy method which decides against buying properties with low rentCost" in {
    val demandingBuyStrategy = new DemandingBuyStrategy {}
    val sellCost = 100
    val lowRentCost = 49

    val lowRentCostProperty = new Property(sellCost, lowRentCost)
    demandingBuyStrategy.decideToBuy(lowRentCostProperty) shouldEqual false
  }

  it should "provide a decideToBuy method which decides in favor of buying properties with high rentCost" in {
    val demandingBuyStrategy = new DemandingBuyStrategy {}
    val sellCost = 100
    val highRentCost = 50

    val highRentCostProperty = new Property(sellCost, highRentCost)
    demandingBuyStrategy.decideToBuy(highRentCostProperty) shouldEqual true
  }
}

class RandomBuyStrategySpec extends TestSuiteForRandomness {
  "The RandomBuyStrategySpec trait" should "provide a decideToBuy method which returns arbitrarily both true and false" in {
    val randomBuyStrategy = new RandomBuyStrategy {}

    val randomProperty = Property.random()
    eventually {randomBuyStrategy.decideToBuy(randomProperty) shouldEqual true}
    eventually {randomBuyStrategy.decideToBuy(randomProperty) shouldEqual false}
  }
}

class CautiousPlayerSpec extends TestSuite {
  "The CautiousPlayer class" should "provide a decideToBuy method which decides against buying properties whenever that would result in a low balance" in {
    val sellCost = 100
    val rentCost = 50
    val initialBalance = 170
    val property = new Property(sellCost, rentCost)

    val cautiousPlayer = new CautiousPlayer(initialBalance)
    cautiousPlayer.decideToBuy(property) shouldEqual false
  }

  it should "provide a decideToBuy method which decides in favor of buying properties whenever that would result in a reasonable balance" in {
    val sellCost = 100
    val rentCost = 50
    val initialBalance = 180
    val property = new Property(sellCost, rentCost)

    val cautiousPlayer = new CautiousPlayer(initialBalance)
    cautiousPlayer.decideToBuy(property) shouldEqual true
  }
}

class PropertySpec extends TestSuite {
  "The Property class" should "be instantiated with certain sell and rent costs and no owner" in {
    val sellCost = 100
    val rentCost = 50
    val property = new Property(sellCost, rentCost)

    property.sellCost shouldEqual sellCost
    property.rentCost shouldEqual rentCost

    property.getOwner() shouldEqual None
  }

  it should "be able to have its ownership changed when the new owner has enough balance" in {
    val sellCost = 100
    val rentCost = 50
    val property = new Property(sellCost, rentCost)

    val balance = 300
    val player = new Player(balance)

    Property.changeOwnership(property, player) shouldEqual true
    property.getOwner() shouldEqual Some(player)
    player.getBalance() shouldEqual (balance - sellCost)
  }

  it should "not be able to have its ownership changed when the new owner does not have enough balance" in {
    val sellCost = 100
    val rentCost = 50
    val property = new Property(sellCost, rentCost)

    val balance = 90
    val player = new Player(balance)

    Property.changeOwnership(property, player) shouldEqual false
    property.getOwner() shouldEqual None
    player.getBalance() shouldEqual balance
  }

  it should "have its ownership restored to None when a ownership reset is performed" in {
    val sellCost = 100
    val rentCost = 50
    val property = new Property(sellCost, rentCost)

    val balance = 300
    val player = new Player(balance)

    Property.changeOwnership(property, player) shouldEqual true

    property.resetOwnership()

    property.getOwner() shouldEqual None
  }
}

class BoardSpec extends TestSuite {
  "The Board object" should "provide a random method which generates a sequence of Property objects in accordance with the given parameters" in {
    val numProperties = 30
    val maxSellCost = 100
    val maxRentCost = 50
    val board = Board.random(numProperties, maxSellCost, maxRentCost)

    board.properties.size shouldEqual numProperties

    forAll (board.properties) {property => property.sellCost should be <= maxSellCost}
    forAll (board.properties) {property => property.rentCost should be <= maxRentCost}
  }
}

class GameSpec extends TestSuite {
  "The Game class" should "provide a private method activePlayers which returns the correct list of active players" in {
    val balanceOfInactivePlayer = -1
    val balanceOfActivePlayer = 1

    val inactivePlayer = new CautiousPlayer(balanceOfInactivePlayer)
    val activePlayer = new CautiousPlayer(balanceOfActivePlayer)

    val players = Seq(inactivePlayer, activePlayer)

    val board = Board.random()

    val game = new Game(players, board)

    val activePlayersPrivateMethod = PrivateMethod[Seq[AbstractPlayerWithStrategy]](Symbol("activePlayers"))

    val activePlayersPrivateMethodResult = game invokePrivate activePlayersPrivateMethod()

    activePlayersPrivateMethodResult shouldEqual Seq(activePlayer)
  }

  it should "provide a private method resetOwnershipOfInactivePlayersProperties which resets the properties of every inactive player" in {
    val initialBalance = 100
    val player = new CautiousPlayer(initialBalance)

    val rentAndSellCost = 10
    val property = new Property(rentAndSellCost, rentAndSellCost)

    Property.changeOwnership(property, player)

    player.decreaseBalance(initialBalance)

    val board = new Board(Array(property))
    val game = new Game(Seq(player), board)

    val resetOwnershipOfInactivePlayersPropertiesPrivateMethod = PrivateMethod[Unit](Symbol("resetOwnershipOfInactivePlayersProperties"))

    game invokePrivate resetOwnershipOfInactivePlayersPropertiesPrivateMethod()

    property.getOwner() shouldEqual None
  }

  it should "provide a private method performPlayerInteractionWithProperty which behaves correctly for properties without a owner" in {
    val initialBalance = 100
    val player = new CautiousPlayer(initialBalance)

    val rentAndSellCost = 10
    val property = new Property(rentAndSellCost, rentAndSellCost)

    val board = new Board(Array(property))
    val game = new Game(Seq(player), board)

    val performPlayerInteractionWithPropertyPrivateMethod = PrivateMethod[Unit](Symbol("performPlayerInteractionWithProperty"))

    game invokePrivate performPlayerInteractionWithPropertyPrivateMethod(player, property)

    player.getBalance() shouldEqual (initialBalance - rentAndSellCost)
  }

  it should "provide a private method performPlayerInteractionWithProperty which behaves correctly for properties with a owner" in {
    val initialBalance = 100
    val player = new CautiousPlayer(initialBalance)
    val propertyOwner = new CautiousPlayer(initialBalance)

    val rentAndSellCost = 10
    val property = new Property(rentAndSellCost, rentAndSellCost)
    Property.changeOwnership(property, propertyOwner)

    val board = new Board(Array(property))
    val game = new Game(Seq(player), board)

    val performPlayerInteractionWithPropertyPrivateMethod = PrivateMethod[Unit](Symbol("performPlayerInteractionWithProperty"))

    val ownerBalanceBeforeInteraction = propertyOwner.getBalance()

    game invokePrivate performPlayerInteractionWithPropertyPrivateMethod(player, property)

    player.getBalance() shouldEqual (initialBalance - rentAndSellCost)
    propertyOwner.getBalance() shouldEqual (ownerBalanceBeforeInteraction + rentAndSellCost)
  }

  it should "provide a method getWinner which returns the winner when there is only one active player" in {
    val balanceOfActivePlayer = 1
    val balanceOfInactivePlayer = -1
    val activePlayer = new CautiousPlayer(balanceOfActivePlayer)
    val inactivePlayer = new CautiousPlayer(balanceOfInactivePlayer)

    val board = Board.random()
    val game = new Game(Seq(activePlayer, inactivePlayer), board)

    game.getWinner() shouldEqual Some(activePlayer)
  }

  it should "provide a method getWinner which returns None when there is more than one active player" in {
    val balanceOfActivePlayer = 1
    val activePlayer = new CautiousPlayer(balanceOfActivePlayer)
    val otherActivePlayer = new CautiousPlayer(balanceOfActivePlayer)

    val board = Board.random()
    val game = new Game(Seq(activePlayer, otherActivePlayer), board)

    game.getWinner() shouldEqual None
  }
}
