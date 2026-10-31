/* window.m --- Emacs module: macOS window behaviour Emacs has no Lisp API for.

   `macos-window-set-accessory' switches the application activation policy.
   An accessory application keeps its windows and keyboard focus but has no
   Dock icon, no Cmd-Tab entry and no main menu.

   `macos-window-install-drag-handles' routes a mouse-down on a top-level
   Emacs window's actual top border to the native window-drag operation, and
   shows a grip above the window as soon as the pointer comes near that border.
   The grip is a window of its own, so the frame's content never moves.  A window
   with no room above it gets the grip over its top edge.

   Built by `bin/emacs-app build' into var/macos-background/.  */

#import <AppKit/AppKit.h>
#include <emacs-module.h>

int plugin_is_GPL_compatible;

void macos_system_init (emacs_env *env);

static CGFloat drag_edge_height = 0;
static id drag_edge_monitor = nil;

/* The pointer is near the top edge from GRIP_ZONE below it to GRIP_REACH above
   it: a pointer coming from above stops short of the edge, and a fast one
   leaves few samples.  */
enum { GRIP_WIDTH = 92, GRIP_HEIGHT = 12, GRIP_GAP = 3, GRIP_ZONE = 10,
       GRIP_REACH = 24 };

static NSPanel *grip_panel = nil;
static NSWindow *grip_window = nil;
static id grip_local_monitor = nil;
static id grip_global_monitor = nil;
static const char *grip_last_drag = "none";
static NSPoint grip_press;
static NSPoint grip_origin;
static NSTimer *grip_drag_timer = nil;
static BOOL grip_shown = NO;

static BOOL
top_level_emacs_window (NSWindow *window)
{
  return (window && window.contentView && !window.parentWindow
          && [NSStringFromClass ([window class]) hasPrefix:@"Emacs"]);
}

static void grip_end_drag (void);

/* Dragging the frame on Emacs's main thread falls behind whenever Lisp is busy,
   and the window server refuses a press that landed in another window.  So it
   drags the grip, the window really pressed, with the frame as the grip's child
   for that drag: the two move as one group without Emacs.  Should it still
   refuse, the grip gets the drag events and follows them.  */
@interface AaronFrameGrip : NSView
@end

@implementation AaronFrameGrip
- (BOOL)acceptsFirstMouse:(NSEvent *)event { return YES; }

- (void)resetCursorRects
{
  [self addCursorRect:self.bounds cursor:NSCursor.openHandCursor];
}

- (void)drawRect:(NSRect)dirty
{
  NSRect pill = NSInsetRect (self.bounds, 0.5, 0.5);
  [[NSColor.windowBackgroundColor colorWithAlphaComponent:0.92] setFill];
  NSBezierPath *body
    = [NSBezierPath bezierPathWithRoundedRect:pill
                                      xRadius:GRIP_HEIGHT / 2.0
                                      yRadius:GRIP_HEIGHT / 2.0];
  [body fill];
  [[NSColor.separatorColor colorWithAlphaComponent:0.8] setStroke];
  [body stroke];
  [NSColor.secondaryLabelColor setFill];
  NSRect bar = NSMakeRect (NSMidX (pill) - 14, NSMidY (pill) - 1, 28, 2);
  [[NSBezierPath bezierPathWithRoundedRect:bar xRadius:1 yRadius:1] fill];
}

- (void)mouseDown:(NSEvent *)event
{
  NSWindow *window = grip_window;
  if (!window)
    return;
  grip_press = NSEvent.mouseLocation;
  grip_origin = grip_panel.frame.origin;
  grip_last_drag = "window-server";
  [window removeChildWindow:grip_panel];
  [grip_panel addChildWindow:window ordered:NSWindowBelow];
  /* Depending on the system the drag returns at once or at the release, and
     the release reaches neither the view nor a monitor, so ask for it.  */
  grip_drag_timer
    = [NSTimer timerWithTimeInterval:0.05 repeats:YES
                               block:^(NSTimer *timer) {
        if (NSEvent.pressedMouseButtons == 0)
          grip_end_drag ();
      }];
  [NSRunLoop.mainRunLoop addTimer:grip_drag_timer
                          forMode:NSRunLoopCommonModes];
  [grip_panel performWindowDragWithEvent:event];
}

- (void)mouseDragged:(NSEvent *)event
{
  NSPoint now = NSEvent.mouseLocation;
  grip_last_drag = "manual";
  [grip_panel setFrameOrigin:NSMakePoint (grip_origin.x + now.x - grip_press.x,
                                          grip_origin.y + now.y - grip_press.y)];
}
@end

static NSRect
grip_frame (NSWindow *window)
{
  NSRect frame = window.frame;
  CGFloat top = NSMaxY (window.screen.visibleFrame);
  CGFloat y = NSMaxY (frame) + GRIP_GAP;
  if (y + GRIP_HEIGHT > top)
    y = NSMaxY (frame) - GRIP_HEIGHT - GRIP_GAP;
  return NSMakeRect (round (NSMidX (frame) - GRIP_WIDTH / 2.0), y,
                     GRIP_WIDTH, GRIP_HEIGHT);
}

static void
grip_create (void)
{
  if (grip_panel)
    return;
  grip_panel = [[NSPanel alloc]
                 initWithContentRect:NSMakeRect (0, 0, GRIP_WIDTH, GRIP_HEIGHT)
                           styleMask:(NSWindowStyleMaskBorderless
                                      | NSWindowStyleMaskNonactivatingPanel)
                             backing:NSBackingStoreBuffered
                               defer:YES];
  grip_panel.releasedWhenClosed = NO;
  grip_panel.hidesOnDeactivate = NO;
  grip_panel.opaque = NO;
  grip_panel.backgroundColor = NSColor.clearColor;
  grip_panel.hasShadow = YES;
  grip_panel.animationBehavior = NSWindowAnimationBehaviorNone;
  grip_panel.alphaValue = 0;
  grip_panel.ignoresMouseEvents = YES;
  grip_panel.contentView
    = [[AaronFrameGrip alloc]
        initWithFrame:NSMakeRect (0, 0, GRIP_WIDTH, GRIP_HEIGHT)];
}

static void
grip_end_drag (void)
{
  [grip_drag_timer invalidate];
  grip_drag_timer = nil;
  if (grip_window.parentWindow != grip_panel)
    return;
  [grip_panel removeChildWindow:grip_window];
  [grip_window addChildWindow:grip_panel ordered:NSWindowAbove];
}

/* The grip stays a child of its window, and showing it only changes its alpha:
   ordering a window in and out on every approach is what flashes.  */
static void
grip_hide (void)
{
  grip_shown = NO;
  grip_panel.alphaValue = 0;
  grip_panel.ignoresMouseEvents = YES;
}

static void
grip_detach (void)
{
  if (!grip_window)
    return;
  grip_end_drag ();
  grip_hide ();
  [grip_window removeChildWindow:grip_panel];
  [grip_panel orderOut:nil];
  grip_window = nil;
}

static void
grip_show (NSWindow *window)
{
  grip_create ();
  if (grip_window != window)
    {
      grip_detach ();
      grip_window = window;
      [window addChildWindow:grip_panel ordered:NSWindowAbove];
    }
  [grip_panel setFrameOrigin:grip_frame (window).origin];
  grip_panel.ignoresMouseEvents = NO;
  grip_panel.alphaValue = 1;
  grip_shown = YES;
}

static NSRect
grip_band (NSWindow *window)
{
  NSRect frame = window.frame;
  return NSMakeRect (NSMinX (frame), NSMaxY (frame) - GRIP_ZONE,
                     NSWidth (frame), GRIP_ZONE + GRIP_REACH);
}

/* The top-level Emacs window whose exposed top edge POINT is near, or nil.
   A child frame on the edge is part of its window.  */
static NSWindow *
grip_edge_window (NSPoint point)
{
  for (NSWindow *window in NSApp.windows)
    {
      if (!NSPointInRect (point, grip_band (window))
          || !top_level_emacs_window (window) || !window.visible
          || (window.styleMask & NSWindowStyleMaskFullScreen))
        continue;
      NSPoint edge = NSMakePoint (point.x, NSMaxY (window.frame) - 1);
      NSWindow *above
        = [NSApp windowWithWindowNumber:
                   [NSWindow windowNumberAtPoint:edge
                     belowWindowWithWindowNumber:0]];
      while (above.parentWindow)
        above = above.parentWindow;
      if (above == window)
        return window;
    }
  return nil;
}

static void
grip_pointer_moved (void)
{
  if (NSEvent.pressedMouseButtons != 0)
    return;
  grip_end_drag ();
  NSPoint point = NSEvent.mouseLocation;
  if (grip_window
      && (!grip_window.visible
          || (grip_window.styleMask & NSWindowStyleMaskFullScreen)))
    grip_detach ();
  if (grip_shown)
    {
      /* Wider than the band that shows the grip, or a pointer resting on the
         band's edge would flip it on and off.  */
      NSRect keep = NSUnionRect (NSInsetRect (grip_panel.frame, -10, -6),
                                 NSInsetRect (grip_band (grip_window), 0, -6));
      if (!NSPointInRect (point, keep))
        grip_hide ();
      return;
    }
  NSWindow *window = grip_edge_window (point);
  if (window)
    grip_show (window);
}

static void
grip_install (BOOL enabled)
{
  if (enabled && !grip_local_monitor)
    {
      grip_local_monitor =
        [NSEvent addLocalMonitorForEventsMatchingMask:NSEventMaskMouseMoved
                                              handler:^NSEvent *(NSEvent *event) {
            grip_pointer_moved ();
            return event;
          }];
      grip_global_monitor =
        [NSEvent addGlobalMonitorForEventsMatchingMask:NSEventMaskMouseMoved
                                               handler:^(NSEvent *event) {
            grip_pointer_moved ();
          }];
    }
  else if (!enabled && grip_local_monitor)
    {
      [NSEvent removeMonitor:grip_local_monitor];
      [NSEvent removeMonitor:grip_global_monitor];
      grip_local_monitor = grip_global_monitor = nil;
      grip_detach ();
    }
}

static NSEvent *
drag_top_edge (NSEvent *event)
{
  NSWindow *window = event.window;
  if (!top_level_emacs_window (window)
      || (window.styleMask & NSWindowStyleMaskFullScreen))
    return event;

  /* locationInWindow uses window coordinates, not the content view's
     coordinates.  This catches the real resize border before AppKit gives it
     to the window, leaving the tab line and the other three edges alone. */
  NSPoint point = event.locationInWindow;
  NSRect frame = window.frame;
  if (point.y < frame.size.height - drag_edge_height
      || point.y > frame.size.height
      || point.x < 8 || point.x > frame.size.width - 8)
    return event;

  [window performWindowDragWithEvent:event];
  return nil;
}

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
last_drag (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  return env->intern (env, grip_last_drag);
}

static emacs_value
install_drag_handles (emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                      void *data)
{
  drag_edge_height = MAX (0, env->extract_integer (env, args[0]));

  /* Remove the old content overlay when updating a running Emacs.  Its
     three-pixel view never received clicks on the actual resize border. */
  for (NSWindow *window in [NSApp windows])
    {
      if (!top_level_emacs_window (window))
        continue;
      for (NSView *view in [window.contentView.subviews copy])
        if ([NSStringFromClass ([view class]) isEqualToString:@"EmacsDragHandle"])
          [view removeFromSuperview];
    }

  if (drag_edge_height > 0 && !drag_edge_monitor)
    drag_edge_monitor =
      [NSEvent addLocalMonitorForEventsMatchingMask:NSEventMaskLeftMouseDown
                                            handler:^NSEvent *(NSEvent *event) {
        return drag_top_edge (event);
      }];
  else if (drag_edge_height == 0 && drag_edge_monitor)
    {
      [NSEvent removeMonitor:drag_edge_monitor];
      drag_edge_monitor = nil;
    }
  grip_install (drag_edge_height > 0);
  return env->make_integer (env, drag_edge_monitor ? 1 : 0);
}

void
macos_define (emacs_env *env, const char *name, ptrdiff_t arity,
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
  macos_define (env, "macos-window-set-accessory", 1, set_accessory,
          "Hide the Dock icon when ACCESSORY is non-nil, else show it.");
  macos_define (env, "macos-window-install-drag-handles", 1, install_drag_handles,
          "Drag top-level Emacs windows from the top HEIGHT-point border.\n"
          "Coming near that border shows a grip above the window, leaving the\n"
          "content and its tab line unobstructed.");
  macos_define (env, "macos-window-last-drag", 0, last_drag,
                "Return how the grip last moved a frame: window-server or manual.");
  macos_system_init (env);
  emacs_value feature = env->intern (env, "macos-window");
  env->funcall (env, env->intern (env, "provide"), 1, &feature);
  return 0;
}
