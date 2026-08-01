package com.revlog.utils

import java.nio.file.{Files, Path, StandardOpenOption}
import java.nio.charset.StandardCharsets
import java.io.File
import java.time.LocalDateTime
import scala.io.Source

import com.revlog.utils.ConfigManager
import com.revlog.Main

object LogManager {
    def log(text: String, logLevel: LogLevel): Unit = {
        val currentDateTime = LocalDateTime.now()
        val message = s"[${currentDateTime}] ${logLevel.logTitle}: $text\n"

        val path = Path.of(Main.logFilePath)

        writeLogFile(path, message.getBytes(StandardCharsets.UTF_8))
        rotation(path.toString)
    }

    private def writeLogFile(path: Path, bytes: Array[Byte]): Unit = {
        Files.write(path, bytes,
            StandardOpenOption.CREATE,
            StandardOpenOption.APPEND)
    }

    private def rotation(path: String): Unit = {
        val config = ConfigManager.getConfig()

        val rotation = ConfigManager.getValueOrDefault(config, "logRotation")
        val maxEntries = ConfigManager.getValueOrDefault(config, "logMaxEntries")

        rotation match {
            case "false" => ()
            case _ => {
                val file = new File(path)
                val source = Source.fromFile(file)

                try {
                    val lines = source.mkString.split("\n")

                    if (lines.length > maxEntries.toInt)
                        Files.delete(Path.of(path))
                } finally {
                    source.close()
                }
            }
        }
    }
}