/* window.m --- Emacs module: macOS window behaviour Emacs has no Lisp API for.

   `macos-window-set-accessory' switches the application activation policy.
   An accessory application keeps its windows and keyboard focus but has no
   Dock icon, no Cmd-Tab entry and no main menu.

   Built by `bin/emacs-app build' into var/macos-background/.  */

#import <AppKit/AppKit.h>
#include <emacs-module.h>

int plugin_is_GPL_compatible;

void macos_system_init (emacs_env *env);

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
  macos_system_init (env);
  emacs_value feature = env->intern (env, "macos-window");
  env->funcall (env, env->intern (env, "provide"), 1, &feature);
  return 0;
}
