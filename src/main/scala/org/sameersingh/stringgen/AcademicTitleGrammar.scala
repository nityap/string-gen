package org.sameersingh.stringgen

/**
 * @author sameer
 */

object AcademicTitleGrammar {
  def grammar(topicStr: String): Grammar = {
    // start
    val start = NonTerminal("S")
    // terminals
    val adjective = NonTerminal("ADJ")
    val noun = NonTerminal("NOUN")
    val verb = NonTerminal("VERB")
    val collective = NonTerminal("COLLECTIVE")
    val literary = NonTerminal("LITERARY")
    val topic = Word(topicStr)
    val andIn = NonTerminal("AND_IN")
    val and = Word("and")
    val in = Word("in")
    // actual types of titles
    val van = NonTerminal("TITLE_VAN")
    val vantc = NonTerminal("TITLE_VAN:TC")
    val tvann = NonTerminal("TITLE_T:VANN")
    val nnvct = NonTerminal("TITLE_NN:VCT")
    val vanldldt = NonTerminal("TITLE_VAN:LDLDT")
    val ldntvan = NonTerminal("TITLE_LDNT:VAN")
    val vantldn = NonTerminal("TITLE_VANT:LDN")
    val theAdj = NonTerminal("THE_ADJ")
    // the rules
    val rules = RuleSet(
      start ~ vantc,
      start ~ tvann,
      start ~ nnvct,
      start ~ vanldldt,
      start ~ ldntvan,
      start ~ vantldn,
      vantc ~(van, Colon, Space, topic, Space, andIn, Space, collective),
      tvann ~(topic, Colon, Space, van, Space, and, Space, noun),
      nnvct ~(noun, Space, and, Space, noun, Colon, Space, verb, Space, collective, Space, andIn, Space, topic),
      vanldldt ~(van, Colon, Space, literary, Space, and, Space, literary, Space, in, Space, topic),
      ldntvan ~(literary, Space, and, Space, noun, Space, in, Space, topic, Colon, Space, van),
      vantldn ~(verb, Space, theAdj, Space, noun, Colon, Space, topic, Space, andIn, Space, collective),
      van ~(verb, Space, adjective, Space, noun),
      WordList(adjective, ClassLoader.getSystemResourceAsStream("best-title/adjectives.txt")),
      WordList(noun, ClassLoader.getSystemResourceAsStream("best-title/noun.txt")),
      WordList(verb, ClassLoader.getSystemResourceAsStream("best-title/verb.txt")),
      WordList(collective, ClassLoader.getSystemResourceAsStream("best-title/collective.txt")),
      WordList(literary, ClassLoader.getSystemResourceAsStream("best-title/literary.txt")),
      new WithDeterminer(theAdj, adjective),
      andIn ~|("and", "in")
    )
    new Grammar(rules, start)
  }

  def main(args: Array[String]) {
    val grammar = AcademicTitleGrammar.grammar("Trafficking")
    for (s <- 0 until 100) {
      println(grammar.sample.mkString)
    }
  }
}
