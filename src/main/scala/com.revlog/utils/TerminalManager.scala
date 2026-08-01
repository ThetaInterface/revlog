package com.revlog.utils

import org.jline.terminal.TerminalBuilder
import org.jline.utils.InfoCmp.Capability

import com.revlog.Main

object TerminalManager {
    val terminal = TerminalBuilder.terminal()

    def clearScreen(): Unit = {
        terminal.puts(Capability.clear_screen)
        terminal.flush()
    }
}