// Website bootstrap. The site dogfoods the public CM6 editor API.

import "../src/styles/widgets.css";
import "../src/styles/theme-typora.css";
import "./style.css";

import { startRouter } from "./router.ts";
import { homeRoute } from "./routes/home.ts";

const root = document.querySelector<HTMLDivElement>("#app")!;

startRouter(root, [
  { path: "/", handler: homeRoute },
]);
