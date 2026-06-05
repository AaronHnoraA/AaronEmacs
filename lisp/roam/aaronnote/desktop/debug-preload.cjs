const { contextBridge, ipcRenderer } = require("electron");

contextBridge.exposeInMainWorld("AaronnoteDebug", {
  close() {
    ipcRenderer.send("aaronnote:debug:close");
  },
  onSnapshot(handler) {
    if (typeof handler !== "function") return () => {};
    const listener = (_event, snapshot) => handler(snapshot);
    ipcRenderer.on("aaronnote:debug:snapshot", listener);
    return () => ipcRenderer.removeListener("aaronnote:debug:snapshot", listener);
  },
});
