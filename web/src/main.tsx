import { render } from "solid-js/web";
import { App } from "./App";
import "./styles/tokens.css";
import "./styles/frame.css";

const root = document.getElementById("root");
if (root === null) {
  throw new Error("no #root element");
}
render(() => <App />, root);
