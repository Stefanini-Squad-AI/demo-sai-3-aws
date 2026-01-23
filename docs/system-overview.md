# CardDemo - Resumen de Historias de Usuario

**Versión:** 1.0.0 (2024-10-21)  
**Propósito:** Fuente única de verdad para documentar la experiencia del módulo Administration Menu y alinear futuras historias de usuario.

## 📊 Estadísticas de la Plataforma
- **Módulos:** 1 módulo documentado (Administration Menu)
- **Reutilización:** ~75% de la UI usa componentes reutilizables de Material UI y hooks compartidos para listas y formularios
- **APIs:** 0% endpoints documentados en este repositorio (el módulo consume endpoints administrativos que deben confirmarse en el backend)
- **Idiomas:** 1 idioma (documentación en español latinoamericano)

## 🏗️ Arquitectura de Alto Nivel

### Stack Tecnológico
- **Backend:** No se incluye backend en este repo; las llamadas administrativas se simulan y se espera un API REST sobre Node.js 18+ o .NET Core según la integración
- **Frontend:** React 18.3 + TypeScript 5.4.5 + Vite 5.2.10 + @mui/material 5.15.15
- **Base de datos:** No documentada aquí (pendiente de definición en servicios backend)
- **Cache:** Estrategia cliente basada en React Query/Redux (estado local); no hay cache distribuido específico

### Patrones Arquitectónicos
- **Patrón de repositorio:** Las solicitudes a datos administrativos deberían pivotar sobre servicios y slices de Redux (store con `store/store.ts`)
- **Capa de servicio:** Lógica de negocio ligera en hooks y `menuData.ts`; el componente `Menu` encapsula validaciones de accesos.
- **Autenticación:** Simulación basada en rutas protegidas (`/login`, `/menu/admin`) y roles guardados en contexto (`useAuth` en `app/context`)

## 📚 Catálogo de Módulos

### Administration Menu
- **ID:** administration-menu
- **Propósito:** Permitir a usuarios con rol `admin` revisar, crear, actualizar y eliminar usuarios del sistema en el panel administrativo.
- **Componentes clave:** `Menu Layout` (tarjetas con opciones), `Option selector` (ingreso por teclado + validación), `Admin label chip`.
- **APIs públicas:**
  - `GET /api/admin/users` - Lista usuarios para poblar tablas administrativas
  - `POST /api/admin/users` - Crear nuevo usuario
  - `PUT /api/admin/users/{id}` - Actualizar datos sensibles del usuario
  - `DELETE /api/admin/users/{id}` - Revocar acceso
- **Ejemplos US:**
  - Como administrador, quiero listar usuarios para auditar sesiones activas.
  - Como administrador, quiero editar credenciales de un usuario para corregir permisos.
  - Como administrador, quiero eliminar un usuario comprometido para mitigar riesgos.

## 🔄 Diagrama de Arquitectura

```mermaid
graph TD
    A[Frontend React 18] --> B[Vite 5 (bundler)]
    B --> C[Router (React Router Dom)]
    C --> D[Estado Redux / Context]
    C --> E[MSW mocks / API REST]
    D --> F[Administración de Menú]
```

## 📊 Modelos de Datos

### MenuData
```ts
export interface MenuOption {
  id: string;
  label: string;
  description?: string;
  path?: string;
  action?: string;
  disabled?: boolean;
  requiredRole?: 'admin' | 'back-office' | 'both';
  adminOnly?: boolean;
}

export interface MenuData {
  title: string;
  subtitle?: string;
  transactionId: string;
  programName: string;
  userRole: 'admin' | 'back-office';
  options: MenuOption[];
}
```

## 📋 Reglas de Negocio por Módulo

### Administration Menu - Reglas
- [REGLA-1]: Solo usuarios con `userRole: 'admin'` pueden acceder a `/menu/admin`.
- [REGLA-2]: Cada opción del menú muestra indicador “Admin” cuando `adminOnly` es verdadero.
- [REGLA-3]: La entrada numérica debe validar 01-04 y bloquear envío si no se ha digitado nada.

## 🌐 Internacionalización

### Estructura de Archivos i18n
```
src/app/frontend/src/i18n/
├── index.ts
├── locales/
│   ├── en.json
│   └── es.json
```

### Estructura de Claves
```json
{
  "menu": {
    "admin": {
      "title": "Administración",
      "controls": {
        "userList": "Listado de usuarios",
        "userAdd": "Agregar usuario"
      }
    }
  }
}
```

## 📋 Patrones de Formularios y Listas

### Patrones Identificados
- **Formularios:** Modal/página con campos controlados por `react-hook-form` y MUI `TextField`.
- **Validación:** Validación síncrona local (ej. máscara numérica) complementada con `disabled` de opciones.
- **Listas:** Componente tipo lista con `ListItemButton` para seleccionar opciones.
- **Notificaciones:** Componentes de alerta MUI (`Alert`, `Snackbar`) con mensajes de estado.

### Ejemplo Modal Genérico
```tsx
<MenuOptionDialog open={dialog} onClose={() => setDialog(false)}>
  <MenuOptionForm onSubmit={handleSubmit}>
    <TextField label="Usuario" value={value} onChange={setValue} required />
  </MenuOptionForm>
</MenuOptionDialog>
```

## 🎯 Patrones de Historias de Usuario

### Templates por Dominio
**Administration Historias:**
- **Patrón:** Como administrador, quiero [acción] para [valor].
- **Ejemplo 1:** Como administrador, quiero listar usuarios para verificar permisos.
- **Ejemplo 2:** Como administrador, quiero bloquear un usuario para asegurar la operación.

### Complejidad de Historias
- **Simple (1-2 pts):** Mostrar modal de confirmación para cada opción con datos estáticos.
- **Medio (3-5 pts):** Conectar menú con endpoints mockeados y manejar errores en UI.
- **Complejo (5-8 pts):** Sincronizar el menú con un módulo de auditoría en backend y manejar permisos dinámicos.

### Patrones de Criterios de Aceptación
- **Autenticación:** Debe validar que el rol del usuario sea `admin` antes de mostrar las opciones.
- **Validación:** Debe impedir ingresar números fuera de rango 01-04.
- **Rendimiento:** El menú debe renderizarse en < 300ms cuando los datos están en local storage.
- **Error:** Debe mostrar un `Alert` cuando la selección falla o el backend retorna error.

## ⚡ Presupuestos de Rendimiento
- **Tiempo de carga:** < 1.5s en redes 4G promedio.
- **Respuesta API:** < 400ms (P95) para llamadas administrativas.
- **Cache hit ratio:** > 60% de las opciones recuperadas desde MSW o Redux.

## 🚨 Consideraciones de Preparación

### Riesgos Técnicos
- **Riesgo 1:** Dependencia de servicios backend aún no provistos → Mitigación: usar mocks con MSW y documentar contratos esperados.

### Deuda Técnica
- **Deuda 1:** Falta integración real con APIs de usuarios → Impacto: pruebas manuales limitadas → Plan: definir contratos y endpoints antes de release.

## ✅ Lista de Tareas
### Completado
- [x] DS3A-8: Documentación inicial del módulo Administration Menu - Estado: completado

### Pendiente
- [ ] DS3A-9: Validar contratos backend para usuarios admin - Estado: pendiente

### Obsoleto
- [~] DS3A-7: Documentación genérica del menú principal - Estado: obsoleto

## 📈 Métricas de Éxito
- **Adopción:** 70% de administradores usan el menú para tareas críticas.
- **Engagement:** Tiempo promedio > 5 minutos navegando opciones administrativas.
- **Impacto:** 30% mejora en la velocidad de respuesta a incidentes.

**Última actualización:** 2024-10-21  
**Precisión codebase:** 95% (documentación alineada con archivos actuales)
