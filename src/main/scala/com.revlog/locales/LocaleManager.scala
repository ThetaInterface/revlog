package com.revlog.locales

import java.io.File
import scala.io.Source
import scala.annotation.tailrec
import upickle.*

import com.revlog.Main
import com.revlog.utils.{LogManager, LogLevel}


object LocaleManager {
    private val locales: List[Locale] = init(Main.localesDirectory)

    def getLocales(): Array[String] = locales.map(e => e.localeName).toArray

    def getEntryOf(localeName: String, entryName: String): String = {
        locales.find(l => l.localeName.equals(localeName)) match {
            case Some(locale) => locale.getLocaleEntry(entryName)

            case None => {
                LogManager.log(s"Locale '$localeName' wasn't found!", LogLevel.Fatal)

                throw new NoSuchElementException()
            }
        }
    }

    private def init(localesDir: String): List[Locale] = {
        val file = new File(localesDir)

        if (file.exists()) {
            val localePaths: List[String] = file.listFiles
                .filter(f => f.isFile)
                .map(f => f.getPath)
                .toList

            fromPaths(localePaths)
        }
        else {
            LogManager.log("Missing locale directory!", LogLevel.Fatal)

            List(new Locale("DIR_NOT_EXIST_ERR", Map()))
        }
    }

    @tailrec
    private def fromPaths(localePaths: List[String], locales: List[Locale] = Nil): List[Locale] = {
        localePaths match {
            case Nil => locales

            case head :: tail => {
                val file = new File(head)
                if (file.exists()) {
                    val source = Source.fromFile(file)
                    val locale = try {
                        read[Locale](source.mkString)
                    } catch {
                        case e: Exception => {
                            LogManager.log(s"Failed to read '$head' file", LogLevel.Warning)

                            new Locale("READ_ERR", Map())
                        }
                    } finally {
                        source.close()
                    }

                    fromPaths(tail, locales.appended(locale))
                }
                else {
                    LogManager.log(s"File '$head' doesn't exist!", LogLevel.Fatal)

                    fromPaths(tail, locales.appended(new Locale("DOESN'T_EXIST_ERR", Map())))
                }
            }
        }
    }
}