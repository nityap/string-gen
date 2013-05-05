package org.sameersingh.stringgen

import scala.util.Random

/**
 * @author sameer
 */
class Grammar(val rules: RuleSet,
              val start: NonTerminal) {

  val random = new Random

  def sample: Seq[Terminal] = {
    rules(start, random).sample(rules, random)
  }
}

object SimpleGrammar {
  val start = NonTerminal("S")
  val wordList: WordList = WordList(start, ClassLoader.getSystemResourceAsStream("names.txt"))
  val rules = RuleSet(
    start ~(start, Space, start),
    start ~|("DONE1", "DONE2"),
    wordList)
  val grammar = new Grammar(rules, start)

  def main(args: Array[String]) {
    for (s <- 0 until 10) {
      println(grammar.sample.mkString)
    }
  }
}