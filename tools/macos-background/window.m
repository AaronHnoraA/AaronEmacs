/* window.m --- Emacs module: macOS window behaviour Emacs has no Lisp API for.

   `macos-window-set-accessory' switches the application activation policy.
   An accessory application keeps its windows and keyboard focus but has no
   Dock icon, no Cmd-Tab entry and no main menu.

   `macos-window-install-drag-handles' gives every top-level Emacs window a
   strip along its top edge that moves the window, so undecorated frames can
   be dragged.

   Built by `bin/emacs-app build' into var/macos-background/.  */

#import <AppKit/AppKit.h>
#include <emacs-module.h>

int plugin_is_GPL_compatible;

@interface EmacsDragHandle : NSView
@property (nonatomic) BOOL hovered;
@end

@implementation EmacsDragHandle
- (void)mouseDown:(NSEvent *)event
{
  [self.window performWindowDragWithEvent:event];
}

- (BOOL)acceptsFirstMouse:(NSEvent *)event
{
  return YES;
}

/* The strip is invisible until the pointer is over it; then it shows a
   grabber and an open hand, so it can be found.  */
- (void)updateTrackingAreas
{
  [super updateTrackingAreas];
  for (NSTrackingArea *area in [self.trackingAreas copy])
    [self removeTrackingArea:area];
  [self addTrackingArea:
          [[NSTrackingArea alloc]
            initWithRect:NSZeroRect
                 options:(NSTrackingMouseEnteredAndExited
                          | NSTrackingActiveAlways | NSTrackingInVisibleRect)
                   owner:self
                userInfo:nil]];
}

- (void)mouseEntered:(NSEvent *)event
{
  self.hovered = YES;
  [[NSCursor openHandCursor] set];
  self.needsDisplay = YES;
}

- (void)mouseExited:(NSEvent *)event
{
  self.hovered = NO;
  [[NSCursor arrowCursor] set];
  self.needsDisplay = YES;
}

- (void)drawRect:(NSRect)dirty
{
  if (!self.hovered)
    return;
  NSRect bounds = self.bounds;
  CGFloat height = MIN (4, bounds.size.height);
  NSRect grabber = NSMakeRect (NSMidX (bounds) - 28, NSMidY (bounds) - height / 2,
                               56, height);
  [[NSColor colorWithWhite:1 alpha:0.55] setFill];
  [[NSBezierPath bezierPathWithRoundedRect:grabber
                                   xRadius:height / 2
                                   yRadius:height / 2] fill];
}
@end

static emacs_value
set_accessory (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  NSApplicationActivationPolicy policy
    = (env->is_not_nil (env, args[0])
       ? NSApplicationActivationPolicyAccessory
       : NSApplicationActivationPolicyRegular);
  /* Setting the policy it already has still reactivates the application,
     which takes the keyboard from a focused xwidget page.  */
  if ([NSApp activationPolicy] != policy)
    [NSApp setActivationPolicy:policy];
  return args[0];
}

static emacs_value
install_drag_handles (emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                      void *data)
{
  CGFloat height = env->extract_integer (env, args[0]);
  intmax_t installed = 0;
  for (NSWindow *window in [NSApp windows])
    {
      NSView *content = window.contentView;
      /* Child frames follow their parent; only top-level frames move.  */
      if (!content || window.parentWindow
          || ![NSStringFromClass ([window class]) hasPrefix:@"Emacs"])
        continue;
      EmacsDragHandle *handle = nil;
      for (NSView *view in content.subviews)
        if ([view isKindOfClass:[EmacsDragHandle class]])
          handle = (EmacsDragHandle *) view;
      if (!handle)
        {
          handle = [[EmacsDragHandle alloc] initWithFrame:NSZeroRect];
          [content addSubview:handle];
          installed++;
        }
      NSRect bounds = content.bounds;
      BOOL flipped = content.isFlipped;
      handle.frame = NSMakeRect (0, flipped ? 0 : bounds.size.height - height,
                                 bounds.size.width, height);
      handle.autoresizingMask = (NSViewWidthSizable
                                 | (flipped ? NSViewMaxYMargin
                                            : NSViewMinYMargin));
    }
  return env->make_integer (env, installed);
}

static void
define (emacs_env *env, const char *name, ptrdiff_t arity,
        emacs_value (*function) (emacs_env *, ptrdiff_t, emacs_value *, void *),
        const char *doc)
{
  emacs_value args[] = {
    env->intern (env, name),
    env->make_function (env, arity, arity, function, doc, NULL)
  };
  env->funcall (env, env->intern (env, "fset"), 2, args);
}

int
emacs_module_init (struct emacs_runtime *runtime)
{
  emacs_env *env = runtime->get_environment (runtime);
  define (env, "macos-window-set-accessory", 1, set_accessory,
          "Hide the Dock icon when ACCESSORY is non-nil, else show it.");
  define (env, "macos-window-install-drag-handles", 1, install_drag_handles,
          "Give each top-level frame a drag strip HEIGHT pixels tall.\n"
          "The strip spans the top edge.  Return the number added.");
  emacs_value feature = env->intern (env, "macos-window");
  env->funcall (env, env->intern (env, "provide"), 1, &feature);
  return 0;
}
