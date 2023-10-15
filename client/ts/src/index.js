import ReactDOM from "react-dom/client";
import "./index.css";
import { AppWithSignature } from "./App.mjs";

import h from "react-hyperscript";

const rootElement = document.getElementById("root");
if (rootElement) {
  const root = ReactDOM.createRoot(rootElement);
  root.render(
    h(AppWithSignature)
  );
}
