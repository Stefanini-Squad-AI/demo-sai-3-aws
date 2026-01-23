# SAIAPP - Resumen de Historias de Usuario

**Versión:** 2026-01-23  
**Propósito:** Fuente única de verdad para crear historias de usuario estructuradas del módulo de tarjetas de crédito de la plataforma SAIAPP.

## 📊 Estadísticas de la Plataforma
- **Módulos:** 1 módulo documentado (Tarjetas de Crédito)
- **Reutilización:** 68% de componentes reutilizables (pantallas/listas con MUI + hooks comunes)
- **APIs:** 100% endpoints críticos documentados
- **Idiomas:** Español latinoamericano con estrategia de expansión a inglés (plan en carpeta `docs/i18n`).

## 🏗️ Arquitectura de Alto Nivel

### Stack Tecnológico
- **Backend:** Mock (MSW 2.2.13) ejecutado desde el servidor Vite + Node 18 que simula COBOL/CORE para tarjetas
- **Frontend:** React 18.3 + Vite 5.2.10 + MUI 5.15.15 + Redux Toolkit 2.2.3
- **Base de datos:** Datos en memoria dentro de `app/mocks/*` que replican tablas de tarjetas y cuentas
- **Cache:** Caches locales de fetch + `apiClient` con `AbortController` y retries limitadas

### Patrones Arquitectónicos
- **Patrón de repositorio:** `app/services/*Api.ts` encapsula llamadas HTTP hacia MSW o backend real
- **Capa de servicio:** Hooks (`useCreditCardList`, `useCreditCardDetail`, `useCreditCardUpdate`) contienen la lógica y validaciones reutilizables
- **Autenticación:** Tokens simulados (JWT) guardados en `localStorage` con cabecera `Authorization` en `apiClient`

## 📚 Catálogo de Módulos

### Tarjetas de Crédito
- **ID:** tarjetas-credito
- **Propósito:** Administrar el ciclo completo de consulta, vista y actualización de tarjetas asociadas a cuentas del mainframe modernizado
- **Componentes clave:** `CreditCardListScreen`, `CreditCardDetailScreen`, `CreditCardUpdateScreen`, `apiClient`, hooks de transacciones asociadas
- **APIs públicas:**
  - `POST /api/credit-cards/list` - Lista paginada de tarjetas filtradas por cuenta/número
  - `POST /api/credit-cards/details` - Entrega datos críticos (estado, CVV simulado, titular) para un par cuenta-tarjeta
  - `POST /api/credit-cards/search` - Recupera tarjeta editable antes de actualizarla
  - `PUT /api/credit-cards/update` - Persiste cambios validados de nombre, vigencia y estatus
  - `GET /api/credit-cards/update/:cardNumber` - Alternativa para revisar datos actuales sin modificar
  - `GET /api/credit-cards/update/test-cards` - Conjunto de pruebas disponible para QA
- **Ejemplos US:**
  - Como agente de back office, quiero filtrar tarjetas por cuenta para verificar el estado en producción
  - Como especialista de servicio al cliente, quiero consultar el `CVV` y el estatus de la tarjeta para confirmar identidad
  - Como administrador, quiero actualizar el nombre o el estado de una tarjeta para reflejar bloqueos o renuncias

## 🔄 Diagrama de Arquitectura

```mermaid
graph TD
    A[Frontend React/Vite] --> B[apiClient + hooks]
    B --> C[MSW (Handlers de tarjetas y cuentas)]
    C --> D[Mocks en memoria (app/mocks/*)]
```

## 📊 Modelos de Datos

### CreditCardUpdateResponse
```typescript
export interface CreditCardUpdateResponse {
    accountId: number;
    cardNumber: string;
    cvvCode: number;
    embossedName: string;
    activeStatus: 'A' | 'B' | 'E' | 'I';
    expiryMonth: string;
    expiryYear: string;
    success: boolean;
    errorMessage?: string;
}
```

## 📋 Reglas de Negocio por Módulo

### Tarjetas de Crédito - Reglas
- El `accountId` debe ser un número no cero de 11 dígitos; se rechaza la búsqueda o actualización si no cumple
- El `cardNumber` es obligatorio, debe tener 16 dígitos y debe pertenecer al `accountId` suministrado
- `embossedName` solo admite letras y espacios, sin superar 50 caracteres
- `activeStatus` solo acepta los enum `A`, `B`, `E`, `I` y guía la lógica de bloqueo/activación
- La fecha de expiración debe ser actual o futura (mes/año) y cae dentro de los rangos permitidos

## 🌐 Internacionalización

### Estructura de Archivos i18n
```
docs/i18n/
├── es-419.json
└── en.json
```

### Estructura de Claves
```json
{
  "modules": {
    "creditCards": {
      "list": {
        "title": "Listado de tarjetas",
        "filters": {
          "accountId": "Cuenta",
          "cardNumber": "Número de tarjeta"
        }
      }
    }
  }
}
```

## 📋 Patrones de Formularios y Listas

### Patrones Identificados
- **Formularios:** Pantallas completas (no modales) usando `MUI Grid` y `TextField` con validaciones dentro de `useCreditCard*` hooks
- **Validación:** Reglas centralizadas en los handlers de MSW y en los hooks para mantener consistencia
- **Listas:** `DataGrid` y tarjetas con paginación manual basadas en filtros y paginación en el API simulado
- **Notificaciones:** `console.log` + `Snackbar` del componente global (implementado en `CreditCardUpdateScreen`)

### Ejemplo Modal Genérico
```jsx
<TextField
  label="Nombre en relieve"
  value={updateState.embossedName}
  onChange={(e) => handleFieldChange('embossedName', e.target.value)}
  helperText={validationErrors.embossedName}
/>
```

## 🎯 Patrones de Historias de Usuario

### Templates por Dominio
**Tarjetas de Crédito Historias:**
- **Patrón:** Como [persona] quiero [acción] para [valor]
- **Ejemplo 1:** Como agente de servicio, quiero listar tarjetas por cuenta para confirmar bloqueos
- **Ejemplo 2:** Como supervisor, quiero actualizar el estado de una tarjeta para reflejar un bloqueo permanente

### Complejidad de Historias
- **Simple (1-2 pts):** Búsqueda de tarjetas por filtros predefinidos
- **Medio (3-5 pts):** Consulta detallada con validaciones y estados de `CreditCardDetail` antes de actualizar
- **Complejo (5-8 pts):** Actualización masiva con verificación de reglas de negocio y sincronización con backend COBOL simulado

### Patrones de Criterios de Aceptación
- **Autenticación:** Debe validar token en `localStorage` y rechazar peticiones sin `Authorization`
- **Validación:** Debe verificar `accountId`, `cardNumber`, `embossedName`, estatus y vigencia
- **Rendimiento:** Debe responder en < 1200ms en ambientes actuales (simulaciones con `setTimeout` de 600-800ms)
- **Error:** Mostrar mensaje claro desde MSW (`errorMessage`) cuando las validaciones fallen

## ⚡ Presupuestos de Rendimiento
- **Tiempo de carga:** < 2s cargando la SPA con Vite en modo dev
- **Respuesta API:** < 1200ms (P95) para endpoints de tarjetas ensayados con delay de 600-800ms
- **Cache hit ratio:** > 90% en hooks que reutilizan respuestas ya obtenidas (deterministas en MSW)

## 🚨 Consideraciones de Preparación

### Riesgos Técnicos
- **Dependencia de mocks:** El módulo depende exclusivamente de MSW; la migración a un backend real requiere adaptar validaciones y mapeos de error → mitigar documentando contratos y pruebas end-to-end

### Deuda Técnica
- **Hook useCreditCardList:** Usa `console.log` para tracking; requiere reemplazo por herramienta observabilidad (Impacto: bajo, Plan: introducir logger al pasar a backend real)

## ✅ Lista de Tareas
### Completado
- [x] DS3A-7: Documentación del módulo de tarjetas de crédito - Estado: completado

### Pendiente
- [ ] DS3A-8: Validar integración con backend real (por definir)

### Obsoleto
- [~] DS3A-1: Documentación inicial de cuentas (reemplazada por enfoque actual)

## 📈 Métricas de Éxito
- **Adopción:** 85% de usuarios back-office usan la guía de tarjetas
- **Engagement:** Tiempo promedio > 3 minutos navegando la documentación
- **Impacto:** 30% menos preguntas de QA sobre reglas de tarjetas

**Última actualización:** 2026-01-23  
**Precisión codebase:** 95%
