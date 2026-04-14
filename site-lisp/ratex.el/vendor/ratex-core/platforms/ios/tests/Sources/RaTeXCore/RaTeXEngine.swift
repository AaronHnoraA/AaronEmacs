// RaTeXEngine.swift (macOS / test) — calls the C ABI directly (no UIKit dependency).

import Foundation
import CRaTeX

public enum RaTeXError: Error, LocalizedError {
    case parseError(String)

    public var errorDescription: String? {
        if case .parseError(let msg) = self { return "RaTeX: \(msg)" }
        return nil
    }
}

public final class RaTeXEngine {
    public static let shared = RaTeXEngine()
    private init() {}

    public func parse(_ latex: String) throws -> DisplayList {
        guard let ptr = ratex_parse_and_layout(latex) else {
            let msg = ratex_get_last_error().map { String(cString: $0) } ?? "unknown error"
            throw RaTeXError.parseError(msg)
        }
        defer { ratex_free_display_list(ptr) }
        let json = String(cString: ptr)
        do {
            return try JSONDecoder().decode(DisplayList.self, from: Data(json.utf8))
        } catch {
            throw RaTeXError.parseError("JSON decode failed: \(error)")
        }
    }
}
