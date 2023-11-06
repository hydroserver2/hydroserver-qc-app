import { defineConfig } from "vite";
import vue from "@vitejs/plugin-vue";
import { resolve } from "path";

// https://vitejs.dev/config/
export default defineConfig({
  plugins: [vue()],
  server: {
    // These headers are required to enable workers
    // https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/SharedArrayBuffer#security_requirements
    headers: {
      "Cross-Origin-Opener-Policy": "same-origin",
      "Cross-Origin-Resource-Policy": "cross-origin",
      "Cross-Origin-Embedder-Policy": "require-corp",
    },
  },
  resolve: {
    extensions: [".js", ".json", ".vue", ".less", ".scss", ".ts", ".py"],
    alias: {
      "@": resolve(__dirname, "src"),
    },
  },
});
