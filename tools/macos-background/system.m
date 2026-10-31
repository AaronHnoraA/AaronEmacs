/* system.m --- macOS services Emacs has no Lisp API for.

   Notifications go through the system's notification center, so clicking
   one reaches Emacs: the id is written to the pipe process given to
   `macos-notify-start'.  The Dock badge and the files on the pasteboard are
   here too.  Part of the `macos-window' module.  */

#import <AppKit/AppKit.h>
#import <UserNotifications/UserNotifications.h>
#include <emacs-module.h>
#include <unistd.h>

void macos_define (emacs_env *env, const char *name, ptrdiff_t arity,
                   emacs_value (*function) (emacs_env *, ptrdiff_t,
                                            emacs_value *, void *),
                   const char *doc);

static int notify_fd = -1;
static BOOL notify_ready = NO;

@interface AaronNotifyDelegate : NSObject <UNUserNotificationCenterDelegate>
@end

@implementation AaronNotifyDelegate
- (void)userNotificationCenter:(UNUserNotificationCenter *)center
       willPresentNotification:(UNNotification *)notification
         withCompletionHandler:(void (^) (UNNotificationPresentationOptions))done
{
  done (UNNotificationPresentationOptionBanner
        | UNNotificationPresentationOptionList);
}

- (void)userNotificationCenter:(UNUserNotificationCenter *)center
didReceiveNotificationResponse:(UNNotificationResponse *)response
         withCompletionHandler:(void (^) (void))done
{
  NSString *line = [response.notification.request.identifier
                     stringByAppendingString:@"\n"];
  if (notify_fd >= 0)
    write (notify_fd, line.UTF8String, strlen (line.UTF8String));
  done ();
}
@end

static AaronNotifyDelegate *notify_delegate = nil;

static NSString *
lisp_string (emacs_env *env, emacs_value value)
{
  ptrdiff_t size = 0;
  env->copy_string_contents (env, value, NULL, &size);
  char *buffer = malloc (size);
  env->copy_string_contents (env, value, buffer, &size);
  NSString *string = [NSString stringWithUTF8String:buffer];
  free (buffer);
  return string ? string : @"";
}

static emacs_value
lisp_bool (emacs_env *env, BOOL value)
{
  return env->intern (env, value ? "t" : "nil");
}

static emacs_value
notify_start (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  notify_fd = env->open_channel (env, args[0]);
  UNUserNotificationCenter *center
    = UNUserNotificationCenter.currentNotificationCenter;
  if (!notify_delegate)
    {
      notify_delegate = [AaronNotifyDelegate new];
      center.delegate = notify_delegate;
    }
  /* The first call asks the user; the answer arrives later, and until then
     notifications take the script path.  */
  [center requestAuthorizationWithOptions:(UNAuthorizationOptionAlert
                                           | UNAuthorizationOptionSound
                                           | UNAuthorizationOptionBadge)
                        completionHandler:^(BOOL granted, NSError *error) {
      notify_ready = granted;
    }];
  return args[0];
}

static emacs_value
notify_ready_p (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  return lisp_bool (env, notify_ready);
}

static emacs_value
notify_post (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  UNMutableNotificationContent *content = [UNMutableNotificationContent new];
  content.title = lisp_string (env, args[1]);
  content.body = lisp_string (env, args[2]);
  content.threadIdentifier = lisp_string (env, args[3]);
  UNNotificationRequest *request
    = [UNNotificationRequest requestWithIdentifier:lisp_string (env, args[0])
                                           content:content
                                           trigger:nil];
  [UNUserNotificationCenter.currentNotificationCenter
      addNotificationRequest:request withCompletionHandler:nil];
  return args[0];
}

static emacs_value
dock_badge (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  NSString *label = lisp_string (env, args[0]);
  NSApp.dockTile.badgeLabel = label.length ? label : nil;
  return args[0];
}

static emacs_value
clipboard_files (emacs_env *env, ptrdiff_t nargs, emacs_value *args, void *data)
{
  NSArray<NSURL *> *urls
    = [NSPasteboard.generalPasteboard
        readObjectsForClasses:@[NSURL.class]
                      options:@{NSPasteboardURLReadingFileURLsOnlyKey: @YES}];
  emacs_value list = env->intern (env, "nil");
  emacs_value cons = env->intern (env, "cons");
  for (NSURL *url in urls.reverseObjectEnumerator)
    {
      const char *path = url.path.UTF8String;
      emacs_value pair[] = { env->make_string (env, path, strlen (path)), list };
      list = env->funcall (env, cons, 2, pair);
    }
  return list;
}

static emacs_value
clipboard_set_files (emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                     void *data)
{
  NSMutableArray<NSURL *> *urls = [NSMutableArray array];
  emacs_value car = env->intern (env, "car");
  emacs_value cdr = env->intern (env, "cdr");
  emacs_value list = args[0];
  while (env->is_not_nil (env, list))
    {
      [urls addObject:[NSURL fileURLWithPath:
                         lisp_string (env, env->funcall (env, car, 1, &list))]];
      list = env->funcall (env, cdr, 1, &list);
    }
  [NSPasteboard.generalPasteboard clearContents];
  return lisp_bool (env, [NSPasteboard.generalPasteboard writeObjects:urls]);
}

void
macos_system_init (emacs_env *env)
{
  macos_define (env, "macos-notify-start", 1, notify_start,
                "Start notifications, reporting clicks to the pipe CHANNEL.");
  macos_define (env, "macos-notify-ready-p", 0, notify_ready_p,
                "Return non-nil when the user allowed notifications.");
  macos_define (env, "macos-notify-post", 4, notify_post,
                "Post a notification with ID, TITLE, BODY, grouped by THREAD.");
  macos_define (env, "macos-dock-badge", 1, dock_badge,
                "Show LABEL on the Dock icon, nothing when it is empty.");
  macos_define (env, "macos-clipboard-files", 0, clipboard_files,
                "Return the files on the general pasteboard.");
  macos_define (env, "macos-clipboard-set-files", 1, clipboard_set_files,
                "Put FILES on the general pasteboard; non-nil on success.");
}
