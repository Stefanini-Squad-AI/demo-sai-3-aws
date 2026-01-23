import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import { resolve } from "path";

export default defineConfig(({ mode }) => {
  const isDevelopment = mode === "development";

  return {
    plugins: [react()],
    base: "/demo-sai-3-aws/", // ⭐ CORREGIDO: nombre real del repositorio
    resolve: {
      alias: {
        "~": resolve(__dirname, "./app"),
      },
    },
    server: {
      port: 3000,
      open: true,
      host: true,
      proxy: {
        "/api": {
          target: "http://18.217.121.166:8082",
          changeOrigin: true,
          secure: false,
        },
      },
    },
    build: {
      target: "esnext",
      outDir: "dist",
      sourcemap: false,
      rollupOptions: {
        output: {
          manualChunks: {
            vendor: ["react", "react-dom"],
            mui: [
              "@mui/material",
              "@mui/icons-material",
              "@emotion/react",
              "@emotion/styled",
            ],
            redux: ["@reduxjs/toolkit", "react-redux"],
            router: ["react-router-dom"],
          },
        },
      },
    },
    define: {
      __DEV__: JSON.stringify(isDevelopment),
    },
  };
});