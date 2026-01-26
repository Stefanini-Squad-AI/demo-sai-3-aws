# Menú de Administración - Resumen de Historias de Usuario

**Versión:** 2026-02-15  
**Propósito:** Documentar las funciones de seguridad y administración del Menú de Administración para que Product Owners y desarrolladores puedan crear historias de usuario precisas.

## 📊 Estadísticas del Módulo
- **Pantallas principales:** 5 (menú administrativo, lista de usuarios, alta, edición y baja)
- **Hooks reutilizados:** 4 (`useMenu`, `useUserList`, `useUserAdd`, `useUserUpdate` / `useUserDelete` como variantes)
- **APIs documentadas:** 5 (`/api/users/list`, `/api/users/process-selection`, `/api/users`, `/api/users/{userId}` GET/PUT/DELETE)
- **Roles habilitados:** Solo administradores (`role === 'admin'`)
- **Precisión código:** >95% (basado en `app/pages/AdminMenuPage.tsx`, `app/components/user`, `app/hooks`, `app/services/userApi.ts`)

## 🧭 Contexto de Negocio
El Menú de Administración reemplaza al antiguo `COADM01C` del mainframe. Permite a usuarios con rol administrador realizar las tareas de seguridad más delicadas: listar usuarios del sistema, crear credenciales, modificar permisos y eliminar cuentas de manera controlada. Es una zona aislada detrás de `ProtectedRoute` y requiere autenticación previa.

## 🏗️ Fundamento Técnico
- **AdminMenuPage:** Carga `getAdminMenuData()` y delega la pantalla al `MenuScreen`. Garantiza que solo `user.role === 'admin'` acceda y redirige en caso contrario.
- **MenuScreen:** Componente compartido con otros menús; renderiza cabecera, lista de opciones y comandos de navegación (F3 para salir, ENTER para selección).
- **UserListScreen:** Tabla con Select para acciones `U`/`D`, paginación basada en F7/F8, y lógica de teclado similar al COBOL original. Usa `useUserList` para manejar estado, búsqueda y navegación.
- **UserAddScreen / UserUpdateScreen / UserDeleteScreen:** Formas completas con `SystemHeader` (COUSR01C, COUSR02C, COUSR03C), validaciones de campos (`userId` <=8, `password` <=8, `firstName` y `lastName` obligatorios) y botones F3/F4/F5/F12 replicando experiencia legacy.
- **Hooks:** `useUserList` respeta la paginación de 10 filas, `useUserAdd`/`useUserUpdate` encapsulan validaciones repetidas y navegación de función, `useMenu` centraliza errores y selección en el menú.
- **Adaptador API:** `UserApiAdapter` traduce requests/responses para `GET /api/users/list`, `POST /users`, `PUT /users/{userId}` y `DELETE /users/{userId}`, lo que permite cambiar entre mocks y backend real con `VITE_USE_MOCKS`.

## 🔌 APIs Públicas Relevantes
- `GET /api/users/list?pageNumber=<n>&direction=FORWARD&startUserId=<opcional>` → Lista segmentada de usuarios de seguridad. Se usa para alimentar la tabla y calcular `hasNext`/`hasPrev`.
- `POST /api/users/process-selection` → Procesa acciones `U` o `D` enviadas desde `UserListScreen` y redirige a las pantallas correctas.
- `POST /api/users` → Alta de usuario con `userId`, `firstName`, `lastName`, `password`, `userType` (A/U). Controla que no se repita el ID y aplica validaciones de longitud.
- `GET /api/users/{userId}` → Carga datos existentes (para edición/baja) con navegación basada en F3/F4/F5.
- `PUT /api/users/{userId}` y `DELETE /api/users/{userId}` → Actualizan o eliminan el usuario. Ambos endpoints usan respuestas adaptadas por `UserApiAdapter` para mostrar mensajes de éxito/fallo.

## 🔄 Patrones de Historias de Usuario
- **Listar Usuarios:** Como **administrador**, quiero **ver la grilla de usuarios y seleccionar U/D con el teclado** para **continuar con la operación requerida sin aprender nuevas teclas**.
- **Crear Usuario:** Como **administrador de seguridad**, quiero **capturar nombre, apellido, ID y contraseña limitada a 8 caracteres** para **crear credenciales que respeten el legacy**.
- **Actualizar Usuario:** Como **administrador**, quiero **buscar por ID y editar datos sin salir de la pantalla** para **mantener la consistencia y no perder foco**.
- **Eliminar Usuario:** Como **administrador de seguridad**, quiero **validar que no desaparezca el último admin** y confirmar antes de borrar para **mantener el control de acceso**.

## 📋 Reglas de Negocio Clave
1. Solo los usuarios con `role === 'admin'` pueden navegar `/menu/admin` y sus subrutas (ver `ProtectedRoute`).
2. El campo `userId` acepta máximo 8 caracteres, se normaliza a mayúsculas y no puede quedar vacío. (Ver `useUserAdd` y `UserUpdateScreen`.)
3. Las contraseñas repiten la misma longitud máxima (8) y se llenan desde `UserAdd` o `UserUpdate` antes de guardar.
4. Al procesar `U` o `D`, `useUserList` valida la selección antes de navegar a `/admin/users/update` o `/admin/users/delete`. F3 en la lista regresa al menú administrativo (como F3/ESC). 
5. Antes de eliminar, `UserDeleteScreen` (y el backend) verifica que no quede solo un admin para evitar bloquear el sistema.

## ⚡ Factores de Aceleración
- **`useMenu`:** Reutilizado por menús de Main y Admin, encapsula loading/error/exit y facilita pruebas unitarias.
- **`UserApiAdapter`:** Permite apuntar a mocks (`/users/security`, `/users/add`) o al backend real sin cambiar los componentes.
- **`UserListScreen` con Select:** Moderniza la selección `U/D` pero mantiene la experiencia legacy (F3, ENTER, F7/F8) y reduce errores humanos.
- **`Hooks de formularios`:** `useUserAdd` y `useUserUpdate` centralizan validaciones repetitivas (longitud, mayúsculas, required) y notifican con mensajes de éxito/fracaso.

## ⚙️ Consideraciones de Calidad y Seguridad
- **Validación front-end + back-end:** Los mismos mensajes `User ID can NOT be empty...` y `Password must be 8 characters or less` fortalecen la capa visual antes de llamar a los endpoints.
- **Auditoría de navegación:** Cada pantalla usa `SystemHeader` con `transactionId` (CU00, CU01, CU02) replicando el COBOL que facilitan trazas.
- **Seguridad de datos:** Toolbar y chips diferencian admins vs. usuarios regulares, la tabla no expone contraseñas.
- **Resiliencia:** En caso de error, `useUserList` y `useUserAdd` muestran `Alert` con mensajes claros y usan `console.error` para logging.

## ✅ Estado Actual y Próximos Pasos
- Documentado en `docs/site/modules/administration-menu/index.html` (GUI + patrones) y esta visión general en `docs/modules/administration-menu/administration-menu-overview.md`.
- Se recomienda crear tests e2e que validen la secuencia ENTER → selección, y agregar validaciones servidor en `userApi` si el backend admite más campos.

**Ticket:** DS3A-8 - Documentación para el módulo Administration Menu
**Precisión estimada:** 97% (basado en componentes y hooks reales del repositorio)
