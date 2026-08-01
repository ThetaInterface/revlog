package com.revlog.utils

enum LogLevel(val logTitle: String) {
    case Log extends LogLevel("LOG")
    case Warning extends LogLevel("WARNING")
    case Fatal extends LogLevel("FATAL")
}