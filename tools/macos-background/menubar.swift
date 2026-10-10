// menubar.swift --- Menu bar control for a background Emacs.
//
// A status item that drives `bin/emacs-app'.  It holds no state of its own:
// every menu is rebuilt from `emacs-app status'.  Built by `bin/emacs-app
// build' into var/macos-background/.

import AppKit

final class Controller: NSObject, NSApplicationDelegate, NSMenuDelegate {
    let script: String
    let item = NSStatusBar.system.statusItem(withLength: NSStatusItem.variableLength)
    var timer: Timer?

    init(script: String) {
        self.script = script
    }

    func applicationDidFinishLaunching(_ notification: Notification) {
        let menu = NSMenu()
        menu.delegate = self
        item.menu = menu
        refreshIcon()
        timer = Timer.scheduledTimer(withTimeInterval: 10, repeats: true) { [weak self] _ in
            self?.refreshIcon()
        }
    }

    /// Run `emacs-app' with ARGUMENTS; return trimmed stdout when WAIT is true.
    @discardableResult
    func run(_ arguments: [String], wait: Bool = false) -> String {
        let process = Process()
        let pipe = Pipe()
        process.executableURL = URL(fileURLWithPath: script)
        process.arguments = arguments
        process.standardOutput = wait ? pipe : FileHandle.nullDevice
        process.standardError = FileHandle.nullDevice
        guard (try? process.run()) != nil, wait else { return "" }
        let data = pipe.fileHandleForReading.readDataToEndOfFile()
        process.waitUntilExit()
        return String(decoding: data, as: UTF8.self)
            .trimmingCharacters(in: .whitespacesAndNewlines)
    }

    func refreshIcon() {
        let symbol: String
        switch run(["status"], wait: true) {
        case "visible": symbol = "terminal.fill"
        case "background": symbol = "terminal"
        default: symbol = "moon.zzz"
        }
        let image = NSImage(systemSymbolName: symbol, accessibilityDescription: "Emacs")
        image?.isTemplate = true
        item.button?.image = image
    }

    func menuNeedsUpdate(_ menu: NSMenu) {
        let status = run(["status"], wait: true)
        menu.removeAllItems()
        let header = menu.addItem(withTitle: "Emacs: \(status)", action: nil, keyEquivalent: "")
        header.isEnabled = false
        menu.addItem(.separator())
        switch status {
        case "visible":
            add(menu, "Hide Emacs", "hide")
            add(menu, "M-x…", "mx")
            add(menu, "New Terminal", "terminal")
            add(menu, "Quit Emacs", "quit")
        case "background":
            add(menu, "Show Emacs", "show")
            add(menu, "M-x…", "mx")
            add(menu, "New Terminal", "terminal")
            add(menu, "Quit Emacs", "quit")
        case "busy":
            add(menu, "Show Emacs", "show")
        default:
            add(menu, "Start Emacs", "start")
            add(menu, "Start Emacs in Background", "start", "--background")
        }
        menu.addItem(.separator())
        let quit = menu.addItem(withTitle: "Remove Menu Bar Icon",
                                action: #selector(NSApplication.terminate(_:)), keyEquivalent: "")
        quit.target = NSApp
        refreshIcon()
    }

    func add(_ menu: NSMenu, _ title: String, _ arguments: String...) {
        let entry = menu.addItem(withTitle: title, action: #selector(invoke(_:)), keyEquivalent: "")
        entry.target = self
        entry.representedObject = arguments
    }

    @objc func invoke(_ sender: NSMenuItem) {
        guard let arguments = sender.representedObject as? [String] else { return }
        run(arguments)
        DispatchQueue.main.asyncAfter(deadline: .now() + 1.5) { [weak self] in
            self?.refreshIcon()
        }
    }
}

let arguments = CommandLine.arguments
let script = arguments.count > 1
    ? arguments[1]
    : NSString(string: "~/.config/emacs/bin/emacs-app").expandingTildeInPath
let application = NSApplication.shared
let controller = Controller(script: script)
application.delegate = controller
application.setActivationPolicy(.accessory)
application.run()
