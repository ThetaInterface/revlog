package com.revlog.locales

import upickle.default.{ReadWriter, macroRW}

import com.revlog.utils.{LogManager, LogLevel}

case class Locale(val localeName: String, val entries: Map[String, String]) {
    def getLocaleEntry(entryName: String): String = {
        entries.get(entryName) match {
            case Some(value) => value
            case None => {
                LogManager.log(s"'${entryName}' wasn't found in '$localeName' localization!", LogLevel.Warning)

                "ENTRY_WASN'T_FOUND_ERR"
            }
        }
    }
}

object Locale {
    implicit val rw: ReadWriter[Locale] = macroRW
}