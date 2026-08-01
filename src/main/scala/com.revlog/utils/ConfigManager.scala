package com.revlog.utils

import java.nio.file.{Files, Path}
import java.nio.charset.StandardCharsets
import java.io.File
import scala.io.Source
import upickle.*

import com.revlog.Main

object ConfigManager {
    val defaultConfig: Map[String, String] = Map(
        ("language", "en_US"),
        ("maxRating", "10"),
        ("fullStar", "★"),
        ("halfStar", "½"),
        ("emptyStar", "☆"),
        ("logRotation", "true"),
        ("logMaxEntries", "130"),
        ("exitPhrase", "q")
    )

    def getConfig(): Map[String, String] = {
        val configFile = new File(Main.configFilePath)
        if (configFile.exists()) {
            val source = Source.fromFile(configFile)

            try {
                read[Map[String, String]](source.mkString)
            } catch {
                case e: Exception => defaultConfig
                
            } finally {
                source.close()
            }
        }
        else {
            writeConfig(defaultConfig)
            defaultConfig
        }
    }

    def writeConfig(configData: Map[String, String]): Unit = {
        try {
            val json = write[Map[String, String]](configData, indent = 4)

            Files.write(Path.of(Main.configFilePath), json.getBytes(StandardCharsets.UTF_8))
        } catch {
            case e: Exception => LogManager.log("Failed to write config file!", LogLevel.Fatal)
        }
    }
    
    def repairConfig(config: Map[String, String]): Map[String, String] = {
        val onlyDefault = defaultConfig.filter((key, value) => !config.contains(key))
        val clearConfig = config.filter((key, value) => defaultConfig.contains(key))
        
        clearConfig ++ onlyDefault
    }

    def getValueOrDefault(config: Map[String, String], key: String): String = {
        config.get(key) match {
            case Some(value) => value

            case None => {
                defaultConfig.get(key) match {
                    case Some(value) => value

                    case None => {
                        LogManager.log(s"There's no key '$key'!", LogLevel.Fatal)

                        throw new NoSuchElementException()
                    }
                }
            }
        }
    }
}