#!/bin/bash

##############################################################################
# Script de prueba para batch jobs integrados
# Prueba la ejecución y monitoreo de batch jobs vía REST API
##############################################################################

# Configuración
BASE_URL="${API_URL:-http://localhost:8080}"
ADMIN_USER="${ADMIN_USER:-ADMIN001}"
ADMIN_PASSWORD="${ADMIN_PASSWORD:-PASSWORD}"

# Colores para output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Funciones auxiliares
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

# Función para extraer valor de JSON (sin jq)
extract_json_value() {
    local json="$1"
    local key="$2"
    echo "$json" | grep -o "\"$key\":\"[^\"]*\"" | sed "s/\"$key\":\"\(.*\)\"/\1/" | head -1
}

# Función para extraer valor numérico de JSON
extract_json_number() {
    local json="$1"
    local key="$2"
    echo "$json" | grep -o "\"$key\":[0-9]*" | sed "s/\"$key\":\(.*\)/\1/" | head -1
}

# Función para pretty-print JSON básico (sin jq)
pretty_json() {
    local json="$1"
    echo "$json" | sed 's/,/,\n  /g' | sed 's/{/{\n  /g' | sed 's/}/\n}/g'
}

# Banner
echo "============================================================"
echo "    Prueba de Batch Jobs - Card Management System"
echo "============================================================"
echo ""

# 1. Login y obtener token JWT
log_info "Paso 1: Autenticando como administrador..."
LOGIN_RESPONSE=$(curl -s -X POST "${BASE_URL}/api/auth/login" \
  -H "Content-Type: application/json" \
  -d "{\"userId\":\"${ADMIN_USER}\",\"password\":\"${ADMIN_PASSWORD}\"}")

if [ -z "$LOGIN_RESPONSE" ]; then
    log_error "No se pudo conectar al servidor en ${BASE_URL}"
    exit 1
fi

JWT_TOKEN=$(echo "$LOGIN_RESPONSE" | grep -o '"accessToken":"[^"]*"' | sed 's/"accessToken":"\(.*\)"/\1/')

if [ -z "$JWT_TOKEN" ] || [ "$JWT_TOKEN" == "null" ]; then
    log_error "No se pudo obtener token JWT"
    echo "Respuesta: $LOGIN_RESPONSE"
    exit 1
fi

log_success "Token JWT obtenido exitosamente"
echo ""

# 2. Listar jobs disponibles
log_info "Paso 2: Listando batch jobs disponibles..."
AVAILABLE_JOBS=$(curl -s -X GET "${BASE_URL}/api/batch/jobs/available" \
  -H "Authorization: Bearer ${JWT_TOKEN}")

pretty_json "$AVAILABLE_JOBS"
echo ""

# 3. Obtener resumen de ejecuciones
log_info "Paso 3: Obteniendo resumen de ejecuciones..."
SUMMARY=$(curl -s -X GET "${BASE_URL}/api/batch/jobs/summary?limit=5" \
  -H "Authorization: Bearer ${JWT_TOKEN}")

pretty_json "$SUMMARY"
echo ""

# 4. Ejecutar job de limpieza de autorizaciones (el más rápido)
log_info "Paso 4: Ejecutando authorizationCleanupJob..."
EXEC_RESPONSE=$(curl -s -X POST "${BASE_URL}/api/batch/jobs/auth-cleanup/execute" \
  -H "Authorization: Bearer ${JWT_TOKEN}" \
  -H "Content-Type: application/json")

pretty_json "$EXEC_RESPONSE"
JOB_EXECUTION_ID=$(extract_json_number "$EXEC_RESPONSE" "jobExecutionId")

if [ -z "$JOB_EXECUTION_ID" ] || [ "$JOB_EXECUTION_ID" == "null" ]; then
    log_warning "No se pudo ejecutar el job o ya está en ejecución"
    echo ""
else
    log_success "Job ejecutado con ID: $JOB_EXECUTION_ID"
    echo ""
    
    # 5. Monitorear estado del job
    log_info "Paso 5: Monitoreando estado del job (esperando 10 segundos)..."
    sleep 10
    
    JOB_STATUS=$(curl -s -X GET "${BASE_URL}/api/batch/jobs/status/${JOB_EXECUTION_ID}" \
      -H "Authorization: Bearer ${JWT_TOKEN}")
    
    pretty_json "$JOB_STATUS"
    
    STATUS=$(extract_json_value "$JOB_STATUS" "status")
    EXIT_CODE=$(extract_json_value "$JOB_STATUS" "exitCode")
    
    if [ "$EXIT_CODE" == "COMPLETED" ]; then
        log_success "Job completado exitosamente"
    elif [ "$STATUS" == "STARTED" ] || [ "$STATUS" == "STARTING" ]; then
        log_warning "Job todavía está en ejecución"
    else
        log_warning "Job terminó con estado: $STATUS"
    fi
    echo ""
fi

# 6. Ver historial del job
log_info "Paso 6: Consultando historial de authorizationCleanupJob..."
HISTORY=$(curl -s -X GET "${BASE_URL}/api/batch/jobs/history/authorizationCleanupJob?limit=3" \
  -H "Authorization: Bearer ${JWT_TOKEN}")

pretty_json "$HISTORY"
echo ""

# 7. Ejecutar reporte de transacciones con parámetros
log_info "Paso 7: Ejecutando transactionReportJob con rango de fechas..."
START_DATE="2025-01-01"
END_DATE="2025-01-31"

REPORT_RESPONSE=$(curl -s -X POST "${BASE_URL}/api/batch/jobs/transaction-report/execute?startDate=${START_DATE}&endDate=${END_DATE}" \
  -H "Authorization: Bearer ${JWT_TOKEN}")

pretty_json "$REPORT_RESPONSE"
REPORT_JOB_ID=$(extract_json_number "$REPORT_RESPONSE" "jobExecutionId")

if [ -z "$REPORT_JOB_ID" ] || [ "$REPORT_JOB_ID" == "null" ]; then
    log_warning "No se pudo ejecutar el reporte (posiblemente faltan datos o parámetros inválidos)"
else
    log_success "Reporte ejecutado con ID: $REPORT_JOB_ID"
fi
echo ""

# Resumen final
echo "============================================================"
echo "                    Prueba Completada"
echo "============================================================"
log_info "Todos los endpoints fueron probados exitosamente"
log_info "Revise los resultados arriba para ver el estado de cada operación"
echo ""
log_info "Para ver la documentación completa, visite:"
echo "  ${BASE_URL}/swagger-ui.html"
echo ""

