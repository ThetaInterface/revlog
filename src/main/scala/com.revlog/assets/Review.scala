package com.revlog.assets

import java.nio.file.{Files, Path}
import java.nio.charset.StandardCharsets
import java.io.File
import java.time.LocalDateTime
import scala.io.Source
import upickle.default.{ReadWriter, macroRW}
import upickle.{read, write}

import com.revlog.utils.{LogManager, LogLevel}
import com.revlog.Main

case class Review(
    val title: String, 
    val rating: Float,
    val usedTemplate: String,
    val customFields: Map[String, String], 
    val creationDate: String, 
    val modifyDate: String
) {
    def clearData(): Unit = {
        val dataFile = File(Path.of(Main.dataDirectory, title + ".json").toString())
        val reviewFile = File(Path.of(Main.reviewsDirectory, title + ".md").toString())

        if (dataFile.exists())
            dataFile.delete()
        
        if (reviewFile.exists())
            reviewFile.delete()
    }

    def getLinkedTemplate(): Template = {
        val foundTemplate = Template.getAllFrom(Main.templateDirectory)

        if (foundTemplate.length < 1)
            Template.default
        else {
            foundTemplate.find(t => t.templateName.equals(usedTemplate)) match {
                case Some(value) => value

                case None => {
                    LogManager.log(s"'$usedTemplate' wasn't found! Using default...", LogLevel.Warning)

                    Template.default
                }
            }
        }
    }

    def modifyField(fieldName: String, newValue: String): Review = {
        if (fieldName.equals("title"))
            return Review(newValue, rating, usedTemplate, customFields, creationDate, LocalDateTime.now().toString())
        else if (fieldName.equals("rating"))
            return Review(title, newValue.toFloat, usedTemplate, customFields, creationDate, LocalDateTime.now().toString())
        else {
            val newCustomFields = customFields.removed(fieldName) + (fieldName -> newValue)

            return Review(title, rating, usedTemplate, newCustomFields, creationDate, LocalDateTime.now().toString())
        }
    }

    def getModifiableFields(): Map[String, String] = {
        customFields + ("title" -> title)
            + ("rating" -> rating.toString)
    }

    def getAllFields(): Map[String, String] = {
        customFields + ("title" -> title)
            + ("rating" -> rating.toString)
            + ("usedTemplate" -> usedTemplate)
            + ("creationDate" -> creationDate)
            + ("modifyDate" -> modifyDate)
    }

    def writeTo(dirPath: String): Unit = {
        try {
            val json = write[Review](this, indent = 4)

            Files.write(Path.of(dirPath, title + ".json"), json.getBytes(StandardCharsets.UTF_8))
        } catch {
            case e: Exception => {
                LogManager.log("Unable to serialize review!", LogLevel.Fatal)

                throw e
            }
        }
    }
}

object Review {
    implicit val reviewRW: ReadWriter[Review] = macroRW

    def fromFile(path: String): Option[Review] = {
        val file = new File(path)
        if (file.exists() && file.isFile) {
            val source = Source.fromFile(file)

            try {
                val json = source.mkString

                Some(read[Review](json))
            } catch {
                case e: Exception => {
                    LogManager.log(s"Unable to read file '$file'!", LogLevel.Fatal)

                    throw e
                }
            } finally {
                source.close()
            }
        }
        else {
            LogManager.log(s"No file '$path' exists!", LogLevel.Warning)

            None
        }
    }

    def fromDirectory(path: String): List[Review] = {
        val file = File(path)
        if (file.exists()) {
            val reviewFiles = file.listFiles()
                .filter(f => f.isFile())
                .map(f => f.getPath())
                .filter(f => f.endsWith("json"))
                .toList

            reviewFiles.flatMap(f => fromFile(f))
        } else {
            LogManager.log("No review was found.", LogLevel.Log)

            Nil
        }
    }
}