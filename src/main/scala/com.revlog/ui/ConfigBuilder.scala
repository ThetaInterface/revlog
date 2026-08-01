package com.revlog.ui

import scala.io.StdIn
import scala.annotation.tailrec

import com.revlog.locales.LocaleManager
import com.revlog.utils.TerminalManager.clearScreen
import com.revlog.Main
import com.revlog.utils.ConfigManager

object ConfigBuilder {
    val defaultConfigTypes: Map[String, String] = Map(
        ("language", "language"),
        ("maxRating", "num"),
        ("fullStar", "text"),
        ("halfStar", "text"),
        ("emptyStar", "text"),
        ("logRotation", "bool"),
        ("logMaxEntries", "num"),
        ("exitPhrase", "text")
    )

    @tailrec
    final def askNewValue(oldValue: String, valueName: String, valueType: String): (Boolean, String) = {
        clearScreen()

        val language = ConfigManager.getValueOrDefault(ConfigManager.getConfig(), "language")

        val exitPhrase = ConfigManager.getValueOrDefault(ConfigManager.getConfig(), "exitPhrase")
        val request = LocaleManager.getEntryOf(language, "ui.configbuilder.field_request")
        val oldValuePreview = LocaleManager.getEntryOf(language, "ui.configbuilder.old_value_preview")
        val exitHint = LocaleManager.getEntryOf(language, "ui.exit_hint")

        valueType match {
            case "text" => {
                val mistakeMessage = LocaleManager.getEntryOf(language, "ui.configbuilder.field_request_text_mistake")

                print(s"$request '$valueName' [$valueType] ($oldValuePreview = $oldValue | $exitHint '$exitPhrase'): ")

                val userInput = StdIn.readLine().trim

                if (userInput.nonEmpty) {
                    if (userInput.equals(exitPhrase))
                        return (false, "exit")
                    else
                        return (true, userInput)
                } else {
                    printMistakeMessage(mistakeMessage)

                    askNewValue(oldValue, valueName, valueType)
                }
            }

            case "num" => {
                val mistakeNotNum = LocaleManager.getEntryOf(language, "ui.configbuilder.field_request_num_mistake_not_num")
                val mistakeOverflow = LocaleManager.getEntryOf(language, "ui.configbuilder.field_request_num_mistake_overflow")

                print(s"$request '$valueName' [$valueType] ($oldValuePreview = $oldValue | $exitHint '$exitPhrase'): ")

                val userInput = StdIn.readLine().trim

                if (userInput.nonEmpty) {
                    if (userInput.equals(exitPhrase))
                        return (false, "exit")
                    else {
                        userInput.toIntOption match {
                            case Some(value) => {
                                if (value < 0) {
                                    printMistakeMessage(mistakeOverflow)

                                    askNewValue(oldValue, valueName, valueType)
                                } else {
                                    return (true, userInput)
                                }
                            }

                            case None => {
                                printMistakeMessage(mistakeNotNum)

                                askNewValue(oldValue, valueName, valueType)
                            }
                        }
                    }
                } else {
                    printMistakeMessage(mistakeNotNum)

                    askNewValue(oldValue, valueName, valueType)
                }
            }

            case "bool" => {
                val mistakeMessage = LocaleManager.getEntryOf(language, "ui.configbuilder.field_request_bool_mistake_not_bool")

                print(s"$request '$valueName' [y/n] [$valueType] ($oldValuePreview = $oldValue | $exitHint '$exitPhrase'): ")

                val userInput = StdIn.readLine().trim

                if (userInput.nonEmpty) {
                    if (userInput.equals(exitPhrase))
                        return (false, "exit")
                    else {
                        userInput.toLowerCase match {
                            case "y" => return (true, "true")
                            case "n" => return (true, "false")

                            case _ => {
                                printMistakeMessage(mistakeMessage)

                                askNewValue(oldValue, valueName, valueType)
                            }
                        }
                    }
                } else {
                    printMistakeMessage(mistakeMessage)

                    askNewValue(oldValue, valueName, valueType)
                }
            }

            case "language" => {
                val languageRequest = LocaleManager.getEntryOf(language, "ui.configbuilder.language_request")
                val mistakeNotNum = LocaleManager.getEntryOf(language, "ui.configbuilder.field_request_num_mistake_not_num")
                val mistakeLanguageOverflow = LocaleManager.getEntryOf(language, "ui.configbuilder.field_request_language_mistake_overflow")

                var count = 0
                LocaleManager.getLocales().foreach { l =>
                    count += 1

                    print(s"\t$count) $l\n")
                }

                print(s"$languageRequest ($oldValuePreview = $oldValue | $exitHint '$exitPhrase'): ")

                val userInput = StdIn.readLine().trim

                if (userInput.nonEmpty) {
                    if (userInput.equals(exitPhrase))
                        return (false, "exit")
                    else {
                        userInput.toIntOption match {
                            case Some(value) => {
                                if (value > 0 && value <= LocaleManager.getLocales().length)
                                    return (true, LocaleManager.getLocales()(value - 1))
                                else {
                                    printMistakeMessage(s"$mistakeLanguageOverflow 1-${LocaleManager.getLocales().length}")

                                    askNewValue(oldValue, valueName, valueType)
                                }
                            }

                            case None => {
                                printMistakeMessage(mistakeNotNum)

                                askNewValue(oldValue, valueName, valueType)
                            }
                        }
                    }
                } else {
                    printMistakeMessage(mistakeNotNum)

                    askNewValue(oldValue, valueName, valueType)
                }
            }
        }
    }

    @tailrec
    final def modifyLocalConfig(): Unit = {
        val config = ConfigManager.getConfig()
        val fields = config.map(f => f._1).toList
        val showFields = config.map(p => s"${p._1} (${p._2})").toList

        val language = ConfigManager.getValueOrDefault(config, "language")

        var count = fields.length
        val exitID = count + 1

        val fieldText = appendStringList(showFields, true) + s"\n\t$exitID) ${LocaleManager.getEntryOf(language, "ui.exit")}"

        val userInput = UserInput.getInt(fieldText + "\n" + LocaleManager.getEntryOf(language, "ui.configbuilder.field_choose_request"), 
            LocaleManager.getEntryOf(language, "ui.configbuilder.field_choose_request_mistake"),
            1, count + 1, false)
        
        if (userInput == exitID)
            return ()
        else {
            val fieldName = fields(userInput - 1)

            val result = askNewValue(config.getOrElse(fieldName, ConfigManager.defaultConfig.get(fieldName).get), fieldName, defaultConfigTypes.get(fieldName).get)

            if (result._1 && !result._2.equals("exit")) {
                ConfigManager.writeConfig(config.removed(fieldName) + (fieldName -> result._2))
            }

            modifyLocalConfig()
        }
    }

    private def appendStringList(list: List[String], numeration: Boolean, first: Boolean = true, count: Int = 1, result: String = ""): String = {
        list match {
            case Nil => result

            case head :: tail => { 
                val prefix = if (first) "\t" else "\n\t"
                val appendPart = if (numeration) s"$prefix$count) $head" else s"$prefix$head"
                
                appendStringList(tail, numeration, false, count + 1, result + appendPart)
            }
        }
    }

    private def printMistakeMessage(text: String): Unit = {
        print(text)

        StdIn.readLine()
    }
}
