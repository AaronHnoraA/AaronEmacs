import CoreServices
import Foundation

let bundle = "com.hc.EmacsTerminal" as CFString
for type in ["public.unix-executable", "com.apple.terminal.shell-script"] {
    let uti = type as CFString
    let status = LSSetDefaultRoleHandlerForContentType(uti, .shell, bundle)
    let actual = LSCopyDefaultRoleHandlerForContentType(uti, .shell)?.takeRetainedValue() as String? ?? "(none)"
    print("\(type): \(actual) (status \(status))")
    guard status == 0 && actual.lowercased() == (bundle as String).lowercased() else {
        exit(1)
    }
}
