import { afterEach, describe, expect, test } from "@voidzero-dev/vite-plus-test";
import { createServer, type Server } from "node:http";
import { WebSocket, WebSocketServer } from "ws";

// @ts-ignore Node ESM helper outside the TS application graph.
import { installJupyterWidgetProxy, jupyterProxyKernelId } from "../server/lib/jupyter-widget-proxy.mjs";

const servers: Server[] = [];
const proxies: Array<{ close(): void }> = [];
const webSocketServers: WebSocketServer[] = [];

function listen(server: Server): Promise<number> {
  return new Promise((resolve, reject) => {
    server.once("error", reject);
    server.listen(0, "127.0.0.1", () => {
      const address = server.address();
      if (!address || typeof address === "string") reject(new Error("Missing test server address"));
      else resolve(address.port);
    });
  });
}

function closeServer(server: Server): Promise<void> {
  return new Promise((resolve) => server.close(() => resolve()));
}

afterEach(async () => {
  for (const proxy of proxies.splice(0)) proxy.close();
  for (const webSocketServer of webSocketServers.splice(0)) {
    for (const client of webSocketServer.clients) client.terminate();
    webSocketServer.close();
  }
  await Promise.all(servers.splice(0).map(closeServer));
});

describe("Jupyter widget proxy", () => {
  test("extracts only a kernel channels id", () => {
    expect(jupyterProxyKernelId("/jupyter/api/kernels/kernel%201/channels")).toBe("kernel 1");
    expect(jupyterProxyKernelId("/jupyter/api/kernels/kernel-1/restart")).toBe("");
  });

  test("relays websocket protocol and binary messages while touching the kernel", async () => {
    const upstreamServer = createServer();
    servers.push(upstreamServer);
    const upstreamWss = new WebSocketServer({ noServer: true });
    webSocketServers.push(upstreamWss);
    upstreamServer.on("upgrade", (request, socket, head) => {
      upstreamWss.handleUpgrade(request, socket, head, (client) => {
        client.on("message", (data, isBinary) => client.send(data, { binary: isBinary }));
      });
    });
    const upstreamPort = await listen(upstreamServer);

    const proxyServer = createServer();
    servers.push(proxyServer);
    let touches = 0;
    const proxy = installJupyterWidgetProxy({
      server: proxyServer,
      resolveTarget: (pathname: string, search: string, websocket: boolean) => websocket && jupyterProxyKernelId(pathname)
        ? `ws://127.0.0.1:${upstreamPort}/api/kernels/kernel-1/channels${search}`
        : null,
      touchKernel: () => { touches += 1; return true; },
      stderr: { write() {} },
    });
    proxies.push(proxy);
    const proxyPort = await listen(proxyServer);

    const client = new WebSocket(
      `ws://127.0.0.1:${proxyPort}/jupyter/api/kernels/kernel-1/channels?session_id=test`,
      "v1.kernel.websocket.jupyter.org",
    );
    await new Promise<void>((resolve, reject) => {
      client.once("open", resolve);
      client.once("error", reject);
    });
    expect(client.protocol).toBe("v1.kernel.websocket.jupyter.org");
    const echoed = new Promise<{ bytes: number[]; binary: boolean }>((resolve) => {
      client.once("message", (data, isBinary) => resolve({ bytes: Array.from(data as Buffer), binary: isBinary }));
    });
    client.send(Buffer.from([1, 4, 9]));
    await expect(echoed).resolves.toEqual({ bytes: [1, 4, 9], binary: true });
    expect(touches).toBeGreaterThan(0);
    client.close();
  });
});
