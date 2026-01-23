// app/main.tsx (actualizado)
import React from 'react';
import ReactDOM from 'react-dom/client';
import { BrowserRouter } from 'react-router-dom';
import App from './App';
import { enableMocking } from './mocks';
import './styles/global.css';

async function prepare() {
  const useMocks = import.meta.env.VITE_USE_MOCKS === 'true';

  if (useMocks) {
    console.log('🔶 Starting application with MSW mocks');
    await enableMocking();
  } else {
    console.log('🔶 Mocking disabled, using real backend');
  }
}

prepare().then(() => {
  ReactDOM.createRoot(document.getElementById('root')!).render(
    <React.StrictMode>
      <BrowserRouter>
        <App />
      </BrowserRouter>
    </React.StrictMode>
  );
});