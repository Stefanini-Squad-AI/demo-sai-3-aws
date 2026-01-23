# Tarjetas de Crédito - Visión general

## Propósito
Centralizar la documentación funcional y técnica para el módulo de tarjetas de crédito de SAIAPP, que cubre listados, consulta detallada y actualizaciones de tarjetas asociadas a cuentas. La guía comparte las reglas de negocio heredadas, los endpoints simulados y los componentes React/MUI que reutilizan los mismos hooks.

## APIs principales
- `POST /api/credit-cards/list`: filtros por `accountId`, `cardNumber`, paginación y validaciones básicas (accountId obligatorio para no-admin). Devuelve `CreditCardListResponse` con paginación tipo Spring.
- `POST /api/credit-cards/details`: recibe `accountId` (11 dígitos) y `cardNumber` (16 dígitos) y retorna campos como `cvvCode`, `activeStatus` y `embossedName`.
- `POST /api/credit-cards/search`: usado antes de actualizar, comparte validaciones de cuenta y tarjeta, y se apoya en `mockCardUpdateDetails` para mostrar los datos editables.
- `PUT /api/credit-cards/update`: valida accountId, cardNumber, nombre, estatus (`A|B|E|I`), mes y año de expiración, y persiste los cambios en el mock.
- `GET /api/credit-cards/update/:cardNumber`: alternativa para recuperar el estado actual antes de guardar.
- `GET /api/credit-cards/update/test-cards`: lista de tarjetas de prueba con descripción de estado para QA.

## Componentes React relevantes
- `CreditCardListScreen`: formulario de filtros, tabla de resultados y paginación manual.
- `CreditCardDetailScreen`: pantalla de consulta con mensajes de error/éxito del handler de detalles.
- `CreditCardUpdateScreen`: formulario completo con estados controlados, validación y botones de prueba.
- Hooks `useCreditCardList`, `useCreditCardDetail`, `useCreditCardUpdate` coordinan llamadas al `apiClient` y normalizan mensajes.

## Reglas de negocio destacadas
1. Las búsquedas solo se permiten con `accountId` válido salvo que el usuario sea administrador.
2. Todas las tarjetas deben pertenecer a la cuenta consultada; si no coinciden, se lanza `Did not find cards for this search condition`.
3. El `activeStatus` solo acepta los enums A (Active), B (Blocked), E (Expired), I (Inactive); cualquier otro valor se rechaza.
4. El nombre de la tarjeta solo permite letras y espacios, máximo 50 caracteres.
5. La fecha de expiración no puede estar en el pasado y el año debe estar entre el actual y 2099.

## Datos de prueba y helpers
- `mockCardDetails` y `mockCardUpdateDetails` contienen tarjetas con múltiples estados (Active, Inactive, Blocked, Expired).
- Helpers `getStatusDescription`, `isValidCardStatus`, `getMockCardForTesting`, `updateMockCardForTesting` apoyan el testing manual.

## Consideraciones de UX
- Todas las pantallas muestran `console.log` para rastrear peticiones; se recomienda activar un logger real al migrar a producción.
- Los formularios usan `TextField` de MUI, validaciones en cada hook y mensajes de error enviados desde MSW.
- Los endpoints simulan latencias de 400-800ms para representar un backend mainframe.
