require "json"

package = JSON.parse(File.read(File.join(__dir__, "package.json")))

Pod::Spec.new do |s|
  s.name           = "ratex-react-native"
  s.version        = package["version"]
  s.summary        = package["description"]
  s.homepage       = "https://github.com/erweixin/RaTeX"
  s.license        = package["license"]
  s.authors        = { "erweixin" => "https://github.com/erweixin" }
  s.platforms      = { :ios => "14.0" }
  s.source         = { :git => "https://github.com/erweixin/RaTeX.git", :tag => s.version.to_s }

  # Swift source files + ObjC++ bridge
  s.source_files   = "ios/**/*.{h,m,mm,swift}"
  s.swift_version  = "5.9"

  # Prebuilt static library (libratex_ffi.a) packaged as XCFramework
  s.vendored_frameworks = "ios/RaTeX.xcframework"

  # KaTeX fonts — loaded at runtime from this bundle
  s.resource_bundles = {
    "RaTeXFonts" => ["ios/Fonts/*.ttf"]
  }

  # install_modules_dependencies handles React Core / Fabric / Codegen
  # dependencies automatically based on RCT_NEW_ARCH_ENABLED.
  install_modules_dependencies(s)
end
