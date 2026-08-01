package com.revlog.ui

import java.nio.file.{Files, Path, StandardOpenOption}
import java.util.UUID
import scala.io.StdIn
import scala.annotation.tailrec
import java.io.IOException
import java.time.LocalDateTime

import com.revlog.utils.TerminalManager.clearScreen
import com.revlog.locales.LocaleManager
import com.revlog.Main
import com.revlog.assets.FieldType
import com.revlog.utils.ConfigManager
import com.revlog.utils.LogManager
import com.revlog.utils.LogLevel

object UserInput {
    @tailrec
    final def getString(textToShow: String, textIfMistake: String, isBlankOk: Boolean = false, nextLine: Boolean = false): String = {
        clearScreen()
        printOnScreen(textToShow, nextLine)

        val userInput = StdIn.readLine()

        if (userInput.isEmpty() || (!isBlankOk && userInput.isBlank())) 
        {
            println(textIfMistake)

            StdIn.readBoolean()

            getString(textToShow, textIfMistake, isBlankOk)
        }
        else
            userInput
    }

    @tailrec
    final def getStringExternal(fieldName: String, hintText: String, default: String = ""): String = {
        val hint = "<!-- " + hintText + " --!>\n\n"

        val tempFilePath = Path.of(Main.programDataDirectory, s"temp.txt")
        Files.deleteIfExists(tempFilePath)

        val writePath = Files.write(tempFilePath, 
            (hint + default).getBytes(), 
            StandardOpenOption.CREATE_NEW)

        try {
            val processBuilder = new ProcessBuilder("vim", "-n", writePath.toString).inheritIO()

            val process = processBuilder.start()
            val exitCode = process.waitFor()

            if (exitCode == 0) {
                val result = Files.readString(writePath).replace(hint, "").trim

                if (FieldType.Text.isMatchingTo(result)) {
                    result
                } else {
                    return getStringExternal(fieldName, s"${FieldType.Text.getMistakeMessage()}\n$hintText")
                }
            } else {
                throw new IOException(s"Vim exited with code '$exitCode'")
            }
        } catch {
            case e: IOException => 
                LogManager.log(s"Error occured while using Vim!\n${e.getMessage}", LogLevel.Fatal)
                throw e
        } finally {
            Files.deleteIfExists(writePath)
        }
    }


    @tailrec
    final def getInt(textToShow: String, textIfMistake: String, min: Int, max: Int, nextLine: Boolean = false): Int = {
        clearScreen()
        printOnScreen(textToShow, nextLine)

        val userInput = StdIn.readLine()

        if (userInput.isEmpty() || userInput.isBlank()) {
            println(textIfMistake)

            StdIn.readBoolean()

            getInt(textToShow, textIfMistake, min, max, nextLine)
        } else {
            userInput.toIntOption match {
                case Some(value) => {
                    if (value >= min && value <= max) {
                        value
                    } else {
                        getInt(textToShow, textIfMistake, min, max, nextLine)
                    }
                }

                case None => {
                    println(textIfMistake)

                    StdIn.readBoolean()

                    getInt(textToShow, textIfMistake, min, max, nextLine)
                }
            }
        }
    }

    @tailrec
    final def getField(textToShow: String, fieldType: FieldType, nextLine: Boolean = false): String = {
        clearScreen()
        printOnScreen(textToShow, nextLine)

        val userInput = StdIn.readLine()

        if (userInput.equals(ConfigManager.getValueOrDefault(ConfigManager.getConfig(), "exitPhrase")))
            return userInput

        if (fieldType.isMatchingTo(userInput)) {
            userInput
        } else {
            println(fieldType.getMistakeMessage())

            StdIn.readBoolean()

            getField(textToShow, fieldType, nextLine)
        }
    }

    private def printOnScreen(text: String, nextLine: Boolean): Unit = {
        print(text)

        if (nextLine)
            println()
    }
}
