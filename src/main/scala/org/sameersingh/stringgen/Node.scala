package org.sameersingh.stringgen

import scala.collection.mutable.HashMap

/**
 * @author sameer
 */
trait Node {
  def isTerminal = false

  def name: String
}

trait NonTerminal extends Node {
  def ~(ns: Node*): Rule = new Rule(this, ns)

  def ~|(strs: String*): Rule = new Rule(this, strs.map(s => Seq(Word(s))): _*)
}

object NonTerminal {
  val _map = new HashMap[String, NonTerminal]

  def apply(termName: String) = _map.getOrElseUpdate(termName, new NonTerminal {
    def name = termName
  })
}

trait Terminal extends Node {
  def string: String

  override def isTerminal = true

  override def toString = string
}

object Empty extends Terminal {
  def name = "EMPTY"

  def string = ""
}

object Space extends Terminal {
  def name = "SPACE"

  def string = " "
}

object Comma extends Terminal {
  def name = "Comma"

  def string = ","
}

object Colon extends Terminal {
  def name = "COLON"

  def string = ":"
}

class Word(val word: String) extends Terminal {
  def name = "WORD_" + word

  def string = word
}

object Word {
  def apply(word: String) = new Word(word)
}
