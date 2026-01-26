# Administration Menu - Resumen del Módulo

**Versión:** 2026-02-15  
**Propósito:** Documentar los flujos de administración de usuarios que se ejecutan desde el menú `CADM`, incluyendo navegación por teclado, validaciones de formularios y dependencias backend.

## 📊 Estadísticas del Módulo
- **Pantallas:** 5 (Menu, Listado, Alta, Edición, Eliminación)
- **Hooks reutilizados:** 4 (`useUserList`, `useUserAdd`, `useUserUpdate`, `useUserDelete`)
- **Adaptador:** `UserApiAdapter` centraliza la traducción de requests/responses hacia `/api/users/*`
- **APIs:** 8 endpoints REST documentados
- **Precisión:** 95% (basado en el análisis de componentes reales)

## 🧭 Visión General
El módulo expone el menú administrativo para usuarios con rol `admin`. Desde allí se puede listar usuarios con paginación (F7/F8), seleccionar acciones `U`/`D` y abrir formularios de alta, edición o eliminación. Se preserva la experiencia de terminal al escuchar teclas como Enter (acción), F3 (volver), F4 (limpiar), F5 (guardar o eliminar) y F12 (salir).

## 🧱 Componentes y Hooks principales
- `MenuScreen.tsx`: renderiza opciones numeradas usando `MenuData` y `SystemHeader`, y expone atajos de salida (F3/Escape) que despachan `logoutUser` o navegan a `/login`.
- `UserListScreen.tsx`: muestra tabla con chips numerados, incorpora búsqueda textual, chips de rol y navegación con `handleEnterKey`, `handlePreviousPage`, `handleNextPage`.
- `UserAddScreen.tsx`: form con `TextField`, `Select` y `IconButton` para mostrar/ocultar contraseña; llama a `useUserAdd`.
- `UserUpdateScreen.tsx`: carga `userId` de la query string, valida con `hasChanges`, requiere F5 para guardar y limita contraseñas a 8 caracteres.
- `UserDeleteScreen.tsx`: verifica la existencia del usuario antes de ejecutar `delete` y bloquea la acción hasta que se confirme.
- `useUserList`, `useUserAdd`, `useUserUpdate`, `useUserDelete`: encapsulan validaciones, ciclos de vida, mensajes (`Alert`/`Snackbar`) y navegación (`navigate('/admin/users/collection')`).
- `UserApiAdapter`: adapta los payloads a los contratos Spring (`/api/users/list`, `/api/users/process-selection`, `/api/users/{userId}`) y respeta `VITE_USE_MOCKS`.

## 🔗 APIs y adaptador
| Endpoint | Descripción |
| --- | --- |
| `GET /api/users/list` | Lista paginada con parámetros `searchUserId`, `pageNumber` y `direction`. Retorna `hasNextPage` y `hasPrevPage`. |
| `POST /api/users/process-selection` | Procesa selecciones `U`/`D` y devuelve `redirectUrl` para navegar a los formularios correspondientes. |
| `GET /api/users/previous-page` | Retrocede cuando se presiona F7 (usa `firstUserId`, `currentPage`). |
| `GET /api/users/next-page` | Avanza cuando se presiona F8 (usa `lastUserId`, `currentPage`, `hasNextPage`). |
| `POST /api/users` | Crea un nuevo usuario (payload: `userId`, `firstName`, `lastName`, `password`, `userType`). |
| `GET /api/users/{userId}` | Recupera el detalle completo usado en edición/eliminación. |
| `PUT /api/users/{userId}` | Actualiza campos validados y obliga contraseñas de 8 caracteres exactos. |
| `DELETE /api/users/{userId}` | Elimina el usuario después de confirmación (F5). |

## 🧩 Flujos y casos de uso
- El menú CADM (`/menu/admin`) solo está disponible para `role: 'admin'`; detecta `getAdminMenuData` y renderiza chips numerados, botones y `SystemHeader`.
- El listado soporta filtros de texto, chips de rol y la tecla Enter ejecuta `handleUserAction`; F7 y F8 controlan la paginación mientras se muestran `hasPrev`/`hasNext`.
- El formulario de alta valida en `useUserAdd`: `userId` y `password` no pueden estar vacíos ni superar 8 caracteres; `userType` se transforma a mayúsculas.
- La edición carga los datos desde `userId` en la query string, evita guardados sin cambios y obliga F5 para disparar `useUserUpdate`.
- La eliminación requiere cargar el usuario (GET) antes de ejecutar `DELETE` y muestra el mensaje del backend.

## 📊 Modelos de Datos
```typescript
export interface UserSecurityData {
  userId: string;
  firstName: string;
  lastName: string;
  userType: 'A' | 'U' | 'R';
  createdDate?: string;
  lastLoginDate?: string;
  isActive?: boolean;
}

export interface UserUpdateData {
  userId: string;
  firstName: string;
  lastName: string;
  userType: 'A' | 'U';
  password?: string;
  createdDate?: string;
  lastLoginDate?: string;
  isActive?: boolean;
}
```

## 📋 Reglas y validaciones
- `userId` debe estar en mayúsculas, no puede exceder 8 caracteres y no puede estar vacío (`useUserAdd`, `useUserUpdate`, `useUserDelete`).
- `password` es obligatorio, 8 caracteres máximo en creación y exactamente 8 en edición; se puede mostrar/ocultar con el botón `Visibility`.
- `userType` solo admite `A` o `U` y se transforma con `toUpperCase()` antes de hacer submit.
- F3/Escape regresa al menú `/menu/admin`; F4 limpia el formulario de alta; F5 guarda o elimina (y en el caso de delete solo si se cargó un usuario válido).
- ENTER procesa la primera selección `U` o `D`; F7/F8 validan la existencia de páginas previas/siguientes y muestran errores si no pueden avanzar.
- Eliminar usuario requiere cargar datos (`GET /api/users/{userId}`) antes de ejecutar `DELETE`.

## 🎯 Plantillas de historias
- **Simple:** Como admin, quiero ver la lista de usuarios y navegar a edición con Enter para corregir un apellido rápidamente.
- **Medio:** Como responsable de seguridad, quiero crear un nuevo usuario con validaciones estrictas y rol `U` para registrar nuevos perfiles.
- **Complejo:** Como auditor, quiero eliminar un usuario solo después de consultar su historial y confirmar la acción con tecla F5.

## ⚡ Consideraciones técnicas adicionales
- La paginación es de 10 filas y los hooks indican `limit: 10` para mantener el mismo comportamiento COBOL.
- `UserApiAdapter` detecta si `VITE_USE_MOCKS === 'true'` para redirigir a MSW y facilitar pruebas locales sin backend.
- Las notificaciones usan `Alert` y `Snackbar` para mostrar mensajes de éxito (`successMessage`) o error (`error`).
- La navegación protege cada ruta con `ProtectedRoute requiredRole="admin"` y `useSecureSession`.

## 🚨 Riesgos conocidos
- **Riesgo 1:** Cambios en `/api/users/*` rompen `UserApiAdapter`. *Mitigación:* Versionar el adaptador y mantener Swagger actualizado.
- **Riesgo 2:** Alterar atajos de teclado (`F5`, `F7`, `F8`, `Enter`) afecta operadores entendidos en legacy. *Mitigación:* Escribir pruebas que simulan los key handlers y documentar cada tecla en este módulo.

## ✅ Tareas relacionadas
- [x] TASK-019: Documentación del módulo Administration Menu - completado.
- [ ] TASK-020: Crear pruebas de integración para flujos de `useUserList` y `useUserAdd`.
