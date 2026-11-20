/* window.m --- Emacs module: macOS window behaviour Emacs has no Lisp API for.

   `macos-window-set-accessory' switches the application activation policy.
   An accessory application keeps its windows and keyboard focus but has no
   Dock icon, no Cmd-Tab entry and no main menu.

   `macos-window-install-drag-handles' routes a mouse-down on a top-level
   Emacs window's actual top border to the native window-drag operation.

   Built by `bin/emacs-app build' into var/macos-background/.  */

#import <AppKit/AppKit.h>
#include <emacs-module.h>
#include <fcntl.h>
#include <unistd.h>

int plugin_is_GPL_compatible;

void macos_system_init (emacs_env *env);

static CGFloat drag_edge_height = 0;
static id drag_edge_monitor = nil;
static int activation_fd = -1;
static id activation_observer = nil;

static emacs_value
watch_activation (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  int fd = env->open_channel (env, args[0]);
  if (fd < 0) return env->intern (env, "nil");
  fcntl (fd, F_SETFL, fcntl (fd, F_GETFL) | O_NONBLOCK);
  fcntl (fd, F_SETNOSIGPIPE, 1);
  if (activation_fd >= 0) close (activation_fd);
  activation_fd = fd;
  if (!activation_observer)
    activation_observer = [[NSNotificationCenter defaultCenter]
      addObserverForName:NSApplicationDidBecomeActiveNotification object:NSApp
      queue:nil usingBlock:^(NSNotification *note) {
        (void) write (activation_fd, "1", 1);
      }];
  return env->intern (env, "t");
}

static emacs_value
app_active_p (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  return env->intern (env, [NSApp isActive] ? "t" : "nil");
}

static BOOL
top_level_emacs_window (NSWindow *window)
{
  return (window && window.contentView && !window.parentWindow
          && [NSStringFromClass ([window class]) hasPrefix:@"Emacs"]);
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
  macos_define (env, "macos-window-watch-activation", 1, watch_activation,
          "Notify pipe process CHANNEL when macOS activates Emacs.");
  macos_define (env, "macos-window-app-active-p", 0, app_active_p,
          "Return non-nil when macOS considers Emacs the active application.");
  macos_define (env, "macos-window-set-accessory", 1, set_accessory,
          "Hide the Dock icon when ACCESSORY is non-nil, else show it.");
  macos_define (env, "macos-window-install-drag-handles", 1, install_drag_handles,
          "Drag top-level Emacs windows from the top HEIGHT-point border.");
  macos_system_init (env);
  emacs_value feature = env->intern (env, "macos-window");
  env->funcall (env, env->intern (env, "provide"), 1, &feature);
  return 0;
}
