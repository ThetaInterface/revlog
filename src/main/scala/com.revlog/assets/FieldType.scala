package com.revlog.assets

import com.revlog.Main
import com.revlog.locales.LocaleManager
import com.revlog.utils.ConfigManager

enum FieldType {
    case Text
    case Float
    case Boolean
    case Computed

    def isMatchingTo(text: String): Boolean = this match {
        case Boolean => {
            val lower = text.toLowerCase()

            lower == "y" 
                || lower == "n" 
                || lower == "yes" 
                || lower == "no"
        }

        case Float => {
            text.toFloatOption match {
                case Some(value) => true
                case None => false
            }
        }

        case Text => text.nonEmpty
        
        case Computed => text.nonEmpty 
    }

    def getMistakeMessage(): String = {
        val entryId = this match {
            case Boolean => "ui.reviewbuilder.template_field_request_mistake_boolean"
            case Float => "ui.reviewbuilder.template_field_request_mistake_float"
            case Text => "ui.reviewbuilder.template_field_request_mistake_text"
            case Computed => "ui.reviewbuilder.template_field_request_mistake_computed"
        }

        val language = ConfigManager.getValueOrDefault(ConfigManager.getConfig(), "language")
        LocaleManager.getEntryOf(language, entryId)
    }
}