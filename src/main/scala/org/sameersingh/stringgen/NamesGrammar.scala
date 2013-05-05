package org.sameersingh.stringgen

import scala.io.Source
import collection.mutable.ArrayBuffer
import java.io.FileInputStream

/**
 * @author sameer
 */
object NamesGrammar {

  def processLastName(name: String) : String = {
    var result = name.toLowerCase()
    result = result.capitalize
    var indexToCapitalize = -1
    if(result.startsWith("Mc") && result.length > 2) indexToCapitalize = 2
    if(result == "Mc") indexToCapitalize = 1
    if(result.startsWith("Mac") && result.length > 3) indexToCapitalize = 3
    if(indexToCapitalize >= 0) {
      val chars = result.toCharArray
      chars(indexToCapitalize) = chars(indexToCapitalize).toUpper
    }
    result
  }

  def processFirstName(name: String): String = processLastName(name)

  def readFromCensusFile(head: NonTerminal, filename: String): Rule = {
    val words = new ArrayBuffer[String]
    for (line <- Source.fromInputStream(ClassLoader.getSystemResourceAsStream(filename)).getLines()) {
      val word = line.split("\\s+")(0)
      words += processFirstName(word)
    }
    new WordList(head, words)
  }

  def main(args: Array[String]) {
    val start = NonTerminal("S")
    val lastName = NonTerminal("LAST")
    val firstMale = NonTerminal("FIRST_M")
    val firstFemale = NonTerminal("FIRST_F")
    val rules = RuleSet(
      start ~(firstMale, Space, lastName),
      start ~(firstFemale, Space, lastName),
      readFromCensusFile(firstMale, "us-census/dist.male.first"),
      readFromCensusFile(firstFemale, "us-census/dist.female.first"),
      readFromCensusFile(lastName, "us-census/dist.all.last")
    )
    val grammar = new Grammar(rules, start)
    for (s <- 0 until 10) {
      println(grammar.sample.mkString)
    }
  }

}
