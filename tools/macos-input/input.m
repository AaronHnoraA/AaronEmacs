/* input.m --- Emacs module: read and select the macOS keyboard input source.

   `macos-input-source' returns the ID of the current keyboard input
   source, such as "com.apple.keylayout.ABC" or
   "com.apple.inputmethod.SCIM.ITABC".  `macos-input-select-source' makes
   the source with that ID current.  Emacs has no Lisp API for either; Evil
   uses them to leave a Chinese input method off outside Insert state.

   Built by `bin/emacs-app build' into var/macos-input/.  */

#import <Carbon/Carbon.h>
#import <Foundation/Foundation.h>
#include <emacs-module.h>
#include <string.h>

int plugin_is_GPL_compatible;

static emacs_value
current_source (emacs_env *env, ptrdiff_t nargs, emacs_value *args,
                void *data)
{
  TISInputSourceRef source = TISCopyCurrentKeyboardInputSource ();
  if (!source)
    return env->intern (env, "nil");
  NSString *identifier
    = [(__bridge NSString *) TISGetInputSourceProperty
         (source, kTISPropertyInputSourceID) copy];
  CFRelease (source);
  if (!identifier)
    return env->intern (env, "nil");
  const char *utf8 = identifier.UTF8String;
  return env->make_string (env, utf8, strlen (utf8));
}

static emacs_value
select_source (emacs_env *env, ptrdiff_t nargs, emacs_value *args,
               void *data)
{
  ptrdiff_t size = 0;
  env->copy_string_contents (env, args[0], NULL, &size);
  if (env->non_local_exit_check (env) != emacs_funcall_exit_return)
    return env->intern (env, "nil");
  char buffer[size];
  env->copy_string_contents (env, args[0], buffer, &size);

  NSDictionary *filter = @{
    (__bridge NSString *) kTISPropertyInputSourceID :
      [NSString stringWithUTF8String:buffer]
  };
  /* Only enabled sources: selecting one the user has not enabled fails.  */
  CFArrayRef sources
    = TISCreateInputSourceList ((__bridge CFDictionaryRef) filter, false);
  if (!sources)
    return env->intern (env, "nil");
  OSStatus status = paramErr;
  if (CFArrayGetCount (sources) > 0)
    status = TISSelectInputSource
      ((TISInputSourceRef) CFArrayGetValueAtIndex (sources, 0));
  CFRelease (sources);
  return env->intern (env, status == noErr ? "t" : "nil");
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
  define (env, "macos-input-source", 0, current_source,
          "Return the ID of the current keyboard input source, or nil.");
  define (env, "macos-input-select-source", 1, select_source,
          "Make the enabled input source with ID current.\n"
          "Return t on success, nil when no enabled source has that ID.");
  emacs_value feature = env->intern (env, "macos-input");
  env->funcall (env, env->intern (env, "provide"), 1, &feature);
  return 0;
}
