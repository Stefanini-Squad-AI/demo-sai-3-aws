import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";
import { resolve } from "path";

export default defineConfig(({ mode }) => {
  const isDevelopment = mode === "development";

  return {
    plugins: [react()],
    resolve: {
      alias: {
        "~": resolve(__dirname, "./app"), // Alias para simplificar imports
      },
    },
    server: {
      port: 3000,
      open: true,
      host: true,
      proxy: {
        "/api": {
          target: "http://18.217.121.166:8082", // Proxy hacia el backend en desarrollo
          changeOrigin: true,
          secure: false,
        },
      },
    },
    build: {
      target: "esnext", // Target de compilación moderno
      outDir: "dist",   // Directorio de salida para archivos generados
      sourcemap: false, // Deshabilitar sourcemaps en producción
      rollupOptions: {
        output: {
          manualChunks: {
            // Dividir dependencias en chunks separados
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
      __DEV__: JSON.stringify(isDevelopment), // Define una variable global __DEV__
    },
  };
});