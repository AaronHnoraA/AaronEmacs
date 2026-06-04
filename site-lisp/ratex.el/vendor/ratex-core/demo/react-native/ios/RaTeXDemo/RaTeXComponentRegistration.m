// RaTeXComponentRegistration.m
// Persistently registers RaTeXViewComponentView with Fabric's component provider
// via +load / method swizzling — survives every pod install.
#import <Foundation/Foundation.h>
#import <objc/runtime.h>

// Minimal forward declaration — no React headers needed.
@interface RCTAppDependencyProvider : NSObject
@end

@implementation RCTAppDependencyProvider (RaTeXRegistration)

+ (void)load {
    Method original = class_getInstanceMethod(self, @selector(thirdPartyFabricComponents));
    Method swizzled = class_getInstanceMethod(self, @selector(ratex_thirdPartyFabricComponents));
    if (original && swizzled) {
        method_exchangeImplementations(original, swizzled);
    }
}

- (id)ratex_thirdPartyFabricComponents {
    NSMutableDictionary *components = [[self ratex_thirdPartyFabricComponents] mutableCopy];
    Class cls = NSClassFromString(@"RaTeXViewComponentView");
    if (cls) {
        components[@"RaTeXView"] = cls;
    }
    return [components copy];
}

@end
