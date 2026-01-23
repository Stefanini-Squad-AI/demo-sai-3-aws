// app/mocks/index.ts (actualizado)
export async function enableMocking() {
  const useMocks = import.meta.env.VITE_USE_MOCKS === 'true';
  
  if (useMocks) {
    const { worker } = await import('./browser');
    
    await worker.start({
      onUnhandledRequest: 'bypass',
      serviceWorker: {
        url: '/mockServiceWorker.js', // Asegúrate de que el archivo esté en esta ruta
      },
    });

    console.log('🔶 MSW enabled for development');
    return worker;
  }

  console.log('🔶 MSW disabled - using real backend');
  return null;
}

export async function disableMocking() {
  if (typeof window !== "undefined") {
    const { worker } = await import("./browser");
    await worker.stop();
    console.log("🔶 MSW disabled");
  }
}