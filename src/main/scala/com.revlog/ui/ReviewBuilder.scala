package com.revlog.ui

import com.revlog.assets.Review
import com.revlog.utils.ConfigManager
import com.revlog.locales.LocaleManager
import com.revlog.Main
import com.revlog.assets.Template
import com.revlog.utils.TerminalManager.clearScreen
import scala.annotation.tailrec
import com.revlog.assets.FieldType
import java.time.LocalDateTime

object ReviewBuilder {
    def getReview(): Option[Review] = {
        val config = ConfigManager.getConfig()
        val exitPhrase = ConfigManager.getValueOrDefault(config, "exitPhrase")
        val language = ConfigManager.getValueOrDefault(config, "language")

        val allReviews = Review.fromDirectory(Main.dataDirectory)

        val reviewText = namesToString(allReviews.map(r => r.title).toList) + 
            s"\n${allReviews.length + 1}) ${LocaleManager.getEntryOf(language, "ui.exit")}" +
            s"\n\n${LocaleManager.getEntryOf(language, "ui.reviewbuilder.choose_review_request")}"

        val choosedReviewIndex = UserInput.getInt(reviewText, 
            LocaleManager.getEntryOf(language, "ui.reviewbuilder.choose_review_request_mistake"), 
            1, allReviews.length + 1)

        if (choosedReviewIndex == (allReviews.length + 1))
            return None

        return Some(allReviews(choosedReviewIndex - 1))
    }

    @tailrec
    final def modifyReview(review: Review): Review = {
        val config = ConfigManager.getConfig()
        val exitPhrase = ConfigManager.getValueOrDefault(config, "exitPhrase")
        val language = ConfigManager.getValueOrDefault(config, "language")

        val fields = review.getModifiableFields().map(f => f._1).toList

        val reviewText = namesToString(fields) + 
            s"\n${fields.length + 1}) ${LocaleManager.getEntryOf(language, "ui.exit")}" +
            s"\n\n${LocaleManager.getEntryOf(language, "ui.reviewbuilder.modify_choose_field_request")}"

        val choosedFieldIndex = UserInput.getInt(reviewText, 
            LocaleManager.getEntryOf(language, "ui.reviewbuilder.modify_choose_field_request_mistake"), 
            1, fields.length + 1)

        if (choosedFieldIndex == (fields.length + 1))
            return review

        val usedTemplateFields = review.getLinkedTemplate().templateFields

        val fieldName = fields(choosedFieldIndex - 1)
        val fieldValue = review.getModifiableFields()(fieldName)
        val fieldType = usedTemplateFields(fieldName)

        val newValue = (fieldType match {
            case FieldType.Text => {
                val text = LocaleManager.getEntryOf(language, "ui.reviewbuilder.modify_field_request") +
                    s" '$fieldName' [$fieldType] ('$exitPhrase'" +
                    s" ${LocaleManager.getEntryOf(language, "ui.exit_hint")})"

                UserInput.getStringExternal(fieldName, text, fieldValue)
            }

            case _ => {
                val text = LocaleManager.getEntryOf(language, "ui.reviewbuilder.modify_field_request") +
                    s" '$fieldName' [$fieldType] (" + 
                    LocaleManager.getEntryOf(language, "ui.reviewbuilder.modify_field_old_value") +
                    s" = $fieldValue | '$exitPhrase'" +
                    s" ${LocaleManager.getEntryOf(language, "ui.exit_hint")}): "

                UserInput.getField(text, fieldType)
            }
        }).trim()

        if (newValue.equals(exitPhrase))
            return review


        modifyReview(review.modifyField(fieldName, newValue))
    }

    def createReview(): Option[Review] = {
        val config = ConfigManager.getConfig()
        val exitPhrase = ConfigManager.getValueOrDefault(config, "exitPhrase")
        val language = ConfigManager.getValueOrDefault(config, "language")

        val allTemplates = (Template.default::Template.getAllFrom(Main.templateDirectory))
            .sortBy(t => t.templateName)

        val templateText = namesToString(allTemplates.map(t => t.templateName).toList) + 
            s"\n${allTemplates.length + 1}) ${LocaleManager.getEntryOf(language, "ui.exit")}" +
            s"\n\n${LocaleManager.getEntryOf(language, "ui.reviewbuilder.template_request")}" 

        val choosedTemplateIndex = UserInput.getInt(templateText, 
            LocaleManager.getEntryOf(language, "ui.reviewbuilder.template_request_mistake"), 
            1, allTemplates.length + 1)

        if (choosedTemplateIndex == (allTemplates.length + 1))
            return None

        val usedTemplate = allTemplates(choosedTemplateIndex - 1)
        val fieldsResult = fillTemplateFields(Template.repairTemplateFields(usedTemplate.templateFields), exitPhrase)

        if (fieldsResult._2)
            return None

        val separatedFields = separateFields(fieldsResult._1)

        Some(
            new Review(
                separatedFields._1, 
                separatedFields._2.toFloat, 
                usedTemplate.templateName, 
                separatedFields._3, 
                LocalDateTime.now().toString(), 
                LocalDateTime.now().toString()
            )
        )
    }

    @tailrec
    private def separateFields(fields: Map[String, String], title: String = "", rating: String = "", customFields: Map[String, String] = Map()): (String, String, Map[String, String]) = {
        if (fields.isEmpty)
            (title, rating, customFields)
        else {
            val head = fields.head

            head._1 match {
                case "title" => separateFields(fields.tail, head._2, rating, customFields)
                case "rating" => separateFields(fields.tail, title, head._2, customFields)

                case _ => separateFields(fields.tail, title, rating, customFields + (head._1 -> head._2))
            }
        }
    }

    @tailrec
    private def fillTemplateFields(templateFields: Map[String, FieldType], exitPhrase: String, result: Map[String, String] = Map()): (Map[String, String], Boolean) = {
        if (templateFields.isEmpty) {
            (result, false)
        } else {
            val language = ConfigManager.getValueOrDefault(ConfigManager.getConfig(), "language")

            val head = templateFields.head

            if (head._2 == FieldType.Computed)
                return fillTemplateFields(templateFields.tail, exitPhrase, result)
            
            if (head._2 == FieldType.Text)
                return fillTemplateFields(templateFields.tail, exitPhrase, result + 
                    (head._1 -> UserInput.getStringExternal(head._1, 
                        LocaleManager.getEntryOf(language, "ui.reviewbuilder.template_field_request") + 
                        s" ${head._1} [Text]")))
            
            val textToShow = LocaleManager.getEntryOf(language, "ui.reviewbuilder.template_field_request") +
                s" '${head._1}' [${head._2}] ('${exitPhrase}'" +
                s" ${LocaleManager.getEntryOf(language, "ui.exit_hint")}): "

            val fieldValue = UserInput.getField(textToShow, head._2)

            if (fieldValue.equals(exitPhrase))
                return (result, true)

            fillTemplateFields(templateFields.tail, exitPhrase, result + (head._1 -> fieldValue))
        }
    }

    @tailrec
    private def namesToString(templates: List[String], result: String = "", index: Int = 0): String = {
        templates match {
            case Nil => result.trim()

            case head :: tail => {
                val newBlock = s"${index + 1}) $head\n"

                namesToString(tail, result + newBlock, index + 1)
            }
        }
    }
}