/* window.m --- Emacs module: macOS window behaviour Emacs has no Lisp API for.

   `macos-window-set-accessory' switches the application activation policy.
   An accessory application keeps its windows and keyboard focus but has no
   Dock icon, no Cmd-Tab entry and no main menu.

   `macos-window-install-drag-handles' routes a mouse-down on a top-level
   Emacs window's actual top border to the native window-drag operation, and
   shows a grip above the window while the pointer rests on that border.  The
   grip is a window of its own, so the frame's content never moves.  A window
   with no room above it gets the grip over its top edge.

   Built by `bin/emacs-app build' into var/macos-background/.  */

#import <AppKit/AppKit.h>
#include <emacs-module.h>

int plugin_is_GPL_compatible;

void macos_system_init (emacs_env *env);

static CGFloat drag_edge_height = 0;
static id drag_edge_monitor = nil;

enum { GRIP_WIDTH = 92, GRIP_HEIGHT = 12, GRIP_GAP = 3, GRIP_ZONE = 8 };
static const NSTimeInterval GRIP_DELAY = 0.12;

static NSPanel *grip_panel = nil;
static NSWindow *grip_window = nil;
static id grip_local_monitor = nil;
static id grip_global_monitor = nil;
static NSUInteger grip_generation = 0;

static BOOL
top_level_emacs_window (NSWindow *window)
{
  return (window && window.contentView && !window.parentWindow
          && [NSStringFromClass ([window class]) hasPrefix:@"Emacs"]);
}

/* The grip moves the window itself: the press belongs to the grip's own
   window, which `performWindowDragWithEvent:' on the parent does not accept. */
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
  NSPoint start = NSEvent.mouseLocation;
  NSPoint origin = window.frame.origin;
  [NSCursor.closedHandCursor push];
  for (;;)
    {
      NSEvent *next
        = [NSApp nextEventMatchingMask:(NSEventMaskLeftMouseDragged
                                        | NSEventMaskLeftMouseUp)
                             untilDate:NSDate.distantFuture
                                inMode:NSEventTrackingRunLoopMode
                               dequeue:YES];
      if (next.type == NSEventTypeLeftMouseUp)
        break;
      NSPoint now = NSEvent.mouseLocation;
      [window setFrameOrigin:NSMakePoint (origin.x + now.x - start.x,
                                          origin.y + now.y - start.y)];
    }
  [NSCursor pop];
}
@end

static NSRect
grip_frame (NSWindow *window, CGFloat pointer_x)
{
  NSRect frame = window.frame;
  CGFloat top = NSMaxY (window.screen.visibleFrame);
  CGFloat y = NSMaxY (frame) + GRIP_GAP;
  if (y + GRIP_HEIGHT > top)
    y = NSMaxY (frame) - GRIP_HEIGHT - GRIP_GAP;
  CGFloat x = MIN (MAX (pointer_x - GRIP_WIDTH / 2.0, NSMinX (frame)),
                   NSMaxX (frame) - GRIP_WIDTH);
  return NSMakeRect (x, y, GRIP_WIDTH, GRIP_HEIGHT);
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
  grip_panel.contentView
    = [[AaronFrameGrip alloc]
        initWithFrame:NSMakeRect (0, 0, GRIP_WIDTH, GRIP_HEIGHT)];
}

static void
grip_hide (void)
{
  grip_generation++;
  if (!grip_window)
    return;
  [grip_window removeChildWindow:grip_panel];
  [grip_panel orderOut:nil];
  grip_window = nil;
}

static void
grip_show (NSWindow *window, CGFloat pointer_x)
{
  grip_create ();
  NSRect frame = grip_frame (window, pointer_x);
  grip_window = window;
  [grip_panel setFrame:NSOffsetRect (frame, 0, -GRIP_HEIGHT / 2.0) display:NO];
  grip_panel.alphaValue = 0;
  [window addChildWindow:grip_panel ordered:NSWindowAbove];
  [NSAnimationContext runAnimationGroup:^(NSAnimationContext *context) {
      context.duration = 0.12;
      grip_panel.animator.alphaValue = 1;
      [grip_panel.animator setFrame:frame display:YES];
    }];
}

/* The top-level Emacs window whose top edge POINT rests on, or nil. */
static NSWindow *
grip_edge_window (NSPoint point)
{
  NSInteger number = [NSWindow windowNumberAtPoint:point
                       belowWindowWithWindowNumber:0];
  NSWindow *window = [NSApp windowWithWindowNumber:number];
  if (!top_level_emacs_window (window)
      || (window.styleMask & NSWindowStyleMaskFullScreen)
      || point.y < NSMaxY (window.frame) - GRIP_ZONE)
    return nil;
  return window;
}

static void
grip_pointer_moved (void)
{
  if (NSEvent.pressedMouseButtons != 0)
    return;
  NSPoint point = NSEvent.mouseLocation;
  if (grip_window)
    {
      NSRect keep = NSUnionRect (NSInsetRect (grip_panel.frame, -10, -6),
                                 NSMakeRect (NSMinX (grip_window.frame),
                                             NSMaxY (grip_window.frame) - GRIP_ZONE,
                                             NSWidth (grip_window.frame),
                                             GRIP_ZONE));
      if (!NSPointInRect (point, keep) || !grip_window.visible)
        grip_hide ();
      return;
    }
  NSWindow *window = grip_edge_window (point);
  if (!window)
    {
      grip_generation++;
      return;
    }
  /* Passing through on the way to another window shows nothing. */
  NSUInteger generation = ++grip_generation;
  dispatch_after (dispatch_time (DISPATCH_TIME_NOW,
                                 (int64_t) (GRIP_DELAY * NSEC_PER_SEC)),
                  dispatch_get_main_queue (), ^{
      NSPoint now = NSEvent.mouseLocation;
      if (generation == grip_generation && !grip_window
          && grip_edge_window (now) == window)
        grip_show (window, now.x);
    });
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
      grip_hide ();
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
          "Resting on that border shows a grip above the window, leaving the\n"
          "content and its tab line unobstructed.");
  macos_system_init (env);
  emacs_value feature = env->intern (env, "macos-window");
  env->funcall (env, env->intern (env, "provide"), 1, &feature);
  return 0;
}
