export function kindSurface(context = {}) {
  const article = context.article;
  if (article instanceof HTMLElement) return article;
  return context.host?.querySelector?.(".cm-editor")
    || document.querySelector("#content")
    || document.querySelector(".cm-editor");
}

export function defineKind(kind, mount) {
  return function setup(initialContext = {}) {
    let context = initialContext;
    let cleanup = null;
    let timer = 0;
    let alive = true;

    function disposeMounted() {
      if (typeof cleanup !== "function") return;
      const fn = cleanup;
      cleanup = null;
      fn();
    }

    function run(nextContext = context) {
      if (!alive) return;
      context = nextContext;
      disposeMounted();
      const disposers = [];
      const api = {
        context,
        surface: kindSurface(context),
        onCleanup(fn) {
          if (typeof fn === "function") disposers.push(fn);
        },
        rebuild: schedule,
      };
      api.observe = (target = api.surface, options = { childList: true, subtree: false }) => {
        if (!target) return null;
        const observer = new MutationObserver(schedule);
        observer.observe(target, options);
        api.onCleanup(() => observer.disconnect());
        return observer;
      };
      const returned = mount(api);
      cleanup = () => {
        if (typeof returned === "function") returned();
        for (const fn of disposers.splice(0).reverse()) fn();
      };
    }

    function schedule() {
      window.clearTimeout(timer);
      timer = window.setTimeout(() => run(context), 30);
    }

    function onReady(event) {
      if (event?.detail?.kind === kind) run(event.detail);
    }

    window.addEventListener("aaronnote:kind-ready", onReady);
    run(context);
    window.requestAnimationFrame(() => run(context));
    window.requestAnimationFrame(() => window.requestAnimationFrame(() => run(context)));

    return () => {
      alive = false;
      window.removeEventListener("aaronnote:kind-ready", onReady);
      window.clearTimeout(timer);
      disposeMounted();
    };
  };
}
