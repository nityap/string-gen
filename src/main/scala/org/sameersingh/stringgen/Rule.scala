package org.sameersingh.stringgen

import scala.util.Random
import scala.collection.mutable.HashMap
import scala.io.Source
import java.io.{FileInputStream, InputStream}
import scala.collection.mutable.{Buffer, ArrayBuffer}

/**
 * @author sameer
 */
trait AbstractRule {
  def head: NonTerminal

  def sample(rules: RuleSet, random: Random): Seq[Terminal]
}

class Rule(val head: NonTerminal, val tails: Seq[Node]*) extends AbstractRule {
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

class WithDeterminer(val head: NonTerminal, val tail: Node, val capitalized: Boolean = false) extends AbstractRule {

  def sample(rules: RuleSet, random: Random) = {
    // pick the terminals (recursively)
    val terms = tail match {
      case t: Terminal => Seq(t)
      case n: NonTerminal => rules(n, random).sample(rules, random)
    }
    val beg = terms(0).string(0).toLower
    var det = "a"
    if (beg == 'a' || beg == 'e' || beg == 'i' || beg == 'o' || beg == 'u') det = "an"
    if (random.nextDouble() < 0.5) det = "the"
    if (capitalized) det = det.capitalize
    Seq(Word(det), Space) ++ terms
  }
}

class RuleSet(val rules: Seq[AbstractRule]) {
  val _map = new HashMap[NonTerminal, Buffer[AbstractRule]]
  rules.foreach(r => _map.getOrElseUpdate(r.head, new ArrayBuffer) += r)

  def apply(n: NonTerminal, random: Random): AbstractRule = _map(n)(random.nextInt(_map(n).length))
}

object RuleSet {
  //def apply(rules: Seq[Rule]) = new RuleSet(rules)
  def apply(rules: AbstractRule*): RuleSet = new RuleSet(rules.toSeq)
}