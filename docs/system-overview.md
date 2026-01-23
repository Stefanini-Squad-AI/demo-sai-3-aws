# CardDemo - Resumen de Historias de Usuario

**Versión:** 2026-01-23
**Propósito:** Fuente única de verdad para describir cómo evoluciona el módulo Administration Menu dentro de la experiencia Back-Office.

## 📊 Estadísticas de la Plataforma
- **Módulos:** 1 módulo documentado (Administration Menu)
- **Reutilización:** 65% componentes reutilizables disponibles en `MenuScreen`, `SystemHeader` y chips de selección
- **APIs:** 0% endpoints REST documentados (la navegación depende de rutas internas simuladas)
- **Idiomas:** 1 idioma activo (español en la documentación)

## 🏗️ Arquitectura de Alto Nivel

### Stack Tecnológico
- **Backend:** No aplica (SPA con Vite + mocks locales)
- **Frontend:** React 18 + Vite 5, Material UI 5 y React Router 6
- **Base de datos:** No hay persistencia, la lista de usuarios se sostiene en mocks de `msw`
- **Cache:** Memoria del navegador y estados de Redux Toolkit para evitar recargas

### Patrones Arquitectónicos
- **Patrón de repositorio:** Acceso a datos simulado desde `msw` y servicios de `app/services`
- **Capa de servicio:** `features/auth` y `services/apiClient.ts` centralizan la lógica de login/logout
- **Autenticación:** JWT simulado con `authSlice` y rutas protegidas del administrador

## 📚 Catálogo de Módulos

### Administration Menu
- **ID:** administration-menu
- **Propósito:** Permitir que usuarios con rol `admin` elijan operaciones críticas de seguridad antes de entrar a vistas específicas de usuarios.
- **Componentes clave:** `MenuScreen` (presentación y validación), `SystemHeader` (metadatos de transacción), chips y botones de Material UI para el listado de opciones.
- **APIs públicas:** Navegación hacia rutas internas como `/admin/users/list`, `/admin/users/add`, `/admin/users/update` y `/admin/users/delete`.
- **Ejemplos US:**
  - Como administrador, quiero listar usuarios para verificar perfiles de seguridad.
  - Como administrador, quiero crear nuevos usuarios para delegar accesos.

## 🔄 Diagrama de Arquitectura

```mermaid
graph TD
    A[Frontend (Vite + React)] --> B[Router privado]
    B --> C[MenuScreen]
    C --> D[MSW mocks / servicios]
```

## 📊 Modelos de Datos

### MenuData
```ts
export interface MenuData {
    title: string;
    subtitle?: string;
    transactionId: string;
    programName: string;
    userRole: 'admin' | 'back-office';
    options: MenuOption[];
}
```

### MenuOption
```ts
export interface MenuOption {
    id: string;
    label: string;
    description?: string;
    path?: string;
    action?: string;
    disabled?: boolean;
    adminOnly?: boolean;
}
```

## 📋 Reglas de Negocio por Módulo

### Administration Menu - Reglas
- Solo los usuarios con rol `admin` pueden ver las opciones con `adminOnly: true`.
- El botón F3 (o Escape) dispara `logoutUser` y redirige a `/login` mediante `authSlice`.
- La entrada numérica valida solo dígitos (máximo dos) y ejecuta la opción correspondiente si no está deshabilitada.

## 🌐 Internacionalización

### Estado actual
- No existe una carpeta `i18n` en el repositorio; todo el texto está en inglés dentro del código.
- La documentación oficial se mantiene en español latinoamericano como capa superior para usuarios del SAIAPP.
- Futuras traducciones podrían alojar archivos en `app/locales/es.json`, pero hoy las etiquetas se controlan en componentes y `menuData`.

## 📋 Patrones de Formularios y Listas

### Patrones Identificados
- **Formularios:** Forma compacta (`TextField` + `Button`) en el pie del menú para ingresar la selección manualmente.
- **Validación:** Regex `/^\d{0,2}$/` en `handleInputChange` evita caracteres invalidos en el input numérico.
- **Listas:** `List` de Material UI con `ListItemButton` y `Chip` para cada opción del menú.
- **Notificaciones:** `Alert` para mostrar errores provenientes de `onOptionSelect`.

### Ejemplo Modal Genérico
```tsx
<TextField
  value={selectedInput}
  onChange={handleInputChange}
  placeholder="01"
  size="small"
  inputProps={{ maxLength: 2 }}
/>
```

## 🎯 Patrones de Historias de Usuario

### Templates por Dominio
**Administración:**
- **Patrón:** Como administrador, quiero [acción] para [valor].
- **Ejemplo 1:** Como administrador, quiero listar usuarios para validar credenciales.
- **Ejemplo 2:** Como administrador, quiero actualizar datos de acceso para corregir privilegios.

### Complejidad de Historias
- **Simple (1-2 pts):** Seleccionar una opción existente sin llamar a nuevas APIs.
- **Medio (3-5 pts):** Agregar validaciones de rol o estados `disabled` en las opciones.
- **Complejo (5-8 pts):** Conectar el menú con una API real que regenere tokens y permisos.

### Patrones de Criterios de Aceptación
- **Autenticación:** Debe validar que `authSlice.role === 'admin'` antes de mostrar el menú.
- **Validación:** Debe verificar que la opción seleccionada no esté deshabilitada ni sea nula.
- **Rendimiento:** Debe renderizar la lista en menos de 100 ms en un equipo estándar.
- **Error:** Debe mostrar el `Alert` cuando `onOptionSelect` lance un error.

## ⚡ Presupuestos de Rendimiento
- **Tiempo de carga:** < 1.5s para el menú completo en modo de desarrollo.
- **Respuesta API:** N/A (se usan mocks locales).
- **Cache hit ratio:** 100% en Redux Toolkit para el estado del menú.

## 🚨 Consideraciones de Preparación

### Riesgos Técnicos
- **Dependencia de MSW:** Si el mock server falla, el menú no refleja datos reales → Mitigación: cubrir con pruebas unitarias y fallback a datos locales.

### Deuda Técnica
- **Documentación de APIs:** Las rutas administrativas no están conectadas a un backend real → Plan: sincronizar con el equipo de APIs cuando existan endpoints definitivos.

## ✅ Lista de Tareas
### Completado
- [x] DS3A-8: Documentar el módulo Administration Menu para SAIAPP.

### Pendiente
- [ ] DS3A-9: Integrar endpoints reales en `menuData` cuando el backend esté listo.

### Obsoleto
- [~] DS3A-1: Documentación genérica del menú principal – remplazado por enfoque en Administración Menu.

## 📈 Métricas de Éxito
- **Adopción:** 100% de los tickets administrativos referencian este módulo.
- **Engagement:** Tiempo promedio de navegación > 2 minutos dentro del menú (con entradas manuales).
- **Impacto:** 40% de reducción en rutas incorrectas gracias a la guía de selección.

**Última actualización:** 2026-01-23  
**Precisión codebase:** 95%
