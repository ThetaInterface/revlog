package com.revlog

import java.nio.file.{Files, Path}
import java.io.File

import utils.*
import utils.TerminalManager.*
import locales.*
import assets.*
import com.revlog.ui.*

object Main extends App {
    val programDataDirectory = System.getProperty("user.dir")

    val localesDirectory = Path.of(programDataDirectory, "locales").toString
    val reviewsDirectory = Path.of(programDataDirectory, "reviews").toString
    val dataDirectory = Path.of(programDataDirectory, "data").toString
    val templateDirectory = Path.of(programDataDirectory, "templates").toString
    val configFilePath = Path.of(programDataDirectory, "config.json").toString
    val logFilePath = Path.of(programDataDirectory, "log.txt").toString

    createPaths()
    checkConfig()
    clearScreen()

    cycle(defineLanguage())

    private def createPaths(): Unit = {
        if (Files.exists(Path.of(programDataDirectory))) {
            Files.createDirectories(Path.of(localesDirectory))
            Files.createDirectories(Path.of(reviewsDirectory))
            Files.createDirectories(Path.of(templateDirectory))
            Files.createDirectories(Path.of(dataDirectory))
        }
    }

    private def defineLanguage(): String = {
        ConfigManager.getValueOrDefault(ConfigManager.getConfig(), "language")
    }

    private def checkConfig(): Unit = {
        val config = ConfigManager.getConfig()
        val repaired = ConfigManager.repairConfig(config)
        ConfigManager.writeConfig(repaired)
    }

    private def cycle(language: String): Unit = {
        clearScreen()

        val actions = s"1) ${LocaleManager.getEntryOf(language, "ui.review_creation")}\n" +
            s"2) ${LocaleManager.getEntryOf(language, "ui.review_view")}\n" +
            s"3) ${LocaleManager.getEntryOf(language, "ui.review_modify")}\n" +
            s"4) ${LocaleManager.getEntryOf(language, "ui.review_delete")}\n" +
            s"5) ${LocaleManager.getEntryOf(language, "ui.config")}\n" +
            s"6) ${LocaleManager.getEntryOf(language, "ui.exit")}\n"

        UserInput.getInt(actions + "\n" + LocaleManager.getEntryOf(language, "ui.choose_action_request"), 
            LocaleManager.getEntryOf(language, "ui.choose_action_request_mistake"), 
            1, 6, false) match {
            case 1 => {
                ReviewBuilder.createReview() match {
                    case Some(review) => {
                        processReview(review)
                    }
                    case None => ()
                }

                cycle(defineLanguage())
            }

            case 2 => {
                ReviewBuilder.getReview() match {
                    case Some(review) => showReview(Path.of(reviewsDirectory, review.title + ".md").toString())

                    case None => ()
                }

                cycle(defineLanguage())
            }

            case 3 => {
                ReviewBuilder.getReview() match {
                    case Some(review) => {
                        review.clearData()

                        processReview(ReviewBuilder.modifyReview(review))
                    }

                    case None => ()
                }

                cycle(defineLanguage())
            }

            case 4 => {
                ReviewBuilder.getReview() match {
                    case Some(review) => {
                        review.clearData()
                    }

                    case None => ()
                }

                cycle(defineLanguage())
            }

            case 5 => {
                ConfigBuilder.modifyLocalConfig()

                cycle(defineLanguage())
            }

            case 6 => {
                return ()
            }
        }
    }

    private def showReview(reviewPath: String): Unit = {
        val processBuilder = ProcessBuilder("marktext", reviewPath)

        val process = processBuilder.start()
        process.waitFor()
    }

    private def processReview(review: Review): Unit = {
        review.writeTo(dataDirectory)

        val template = review.getLinkedTemplate()

        if (Template.verifyFields(review.getAllFields())) {
            val finalFields = Template.buildFields(review.getAllFields())

            val md = template.generateMarkdown(finalFields)
            Template.writeMarkdown(md, review.title)
        }
    }
}