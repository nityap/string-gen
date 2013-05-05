package org.sameersingh.stringgen

import scala.util.Random
import scala.collection.mutable.HashMap
import scala.io.Source
import java.io.{FileInputStream, InputStream}
import scala.collection.mutable.{Buffer, ArrayBuffer}

/**
 * @author sameer
 */
class Rule(val head: NonTerminal, val tails: Seq[Node]*) {
  def sample(rules: RuleSet, random: Random): Seq[Terminal] = {
    // pick a tail
    val tail = tails(random.nextInt(tails.length))
    // pick the terminals (recursively)
    tail.flatMap(_ match {
      case t: Terminal => Seq(t)
      case n: NonTerminal => rules(n, random).sample(rules, random)
    })
  }
}

class WordList(head: NonTerminal, val words: Seq[String])
      extends Rule(head, words.map(w => Seq(Word(w))): _*)

object WordList {
  def apply(head: NonTerminal, stream: InputStream): WordList = {
    val words = Source.fromInputStream(stream).getLines().map(_.trim).toSeq
    new WordList(head, words.toSeq)
  }

  def apply(headName: String, stream: InputStream): WordList =
    apply(NonTerminal(headName), stream)

  def apply(headName: String, filename: String): WordList =
    apply(headName, new FileInputStream(filename))

  def apply(head: NonTerminal, filename: String): WordList =
    apply(head, new FileInputStream(filename))
}

class RuleSet(val rules: Seq[Rule]) {
  val _map = new HashMap[NonTerminal, Buffer[Rule]]
  rules.foreach(r => _map.getOrElseUpdate(r.head, new ArrayBuffer) += r)

  def apply(n: NonTerminal, random: Random): Rule = _map(n)(random.nextInt(_map(n).length))
}

object RuleSet {
  //def apply(rules: Seq[Rule]) = new RuleSet(rules)
  def apply(rules: Rule*): RuleSet = new RuleSet(rules.toSeq)
}