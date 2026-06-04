import { defineConfig } from "vite-plus";

export default defineConfig({
  root: "aaronnote",
  base: "/",
  server: {
    host: "127.0.0.1",
  },
  build: {
    outDir: "../dist/aaronnote",
    emptyOutDir: true,
  },
  lint: { options: { typeAware: true, typeCheck: true } },
});
