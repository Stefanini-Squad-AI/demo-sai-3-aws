#!/bin/bash

##############################################################################
# Script de monitoreo continuo para batch jobs
# Muestra el estado actual de todos los batch jobs en tiempo real
##############################################################################

# Configuración
BASE_URL="${API_URL:-http://localhost:8080}"
ADMIN_USER="${ADMIN_USER:-ADMIN001}"
ADMIN_PASSWORD="${ADMIN_PASSWORD:-PASSWORD}"
REFRESH_INTERVAL="${REFRESH_INTERVAL:-5}"

# Colores para output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m' # No Color

# Obtener token JWT
get_token() {
    local response=$(curl -s -X POST "${BASE_URL}/api/auth/login" \
      -H "Content-Type: application/json" \
      -d "{\"userId\":\"${ADMIN_USER}\",\"password\":\"${ADMIN_PASSWORD}\"}")
    
    echo "$response" | grep -o '"accessToken":"[^"]*"' | sed 's/"accessToken":"\(.*\)"/\1/'
}

# Extraer valor de JSON sin jq
extract_json_value() {
    local json="$1"
    local key="$2"
    echo "$json" | grep -o "\"$key\":[^,}]*" | sed "s/\"$key\":\s*\"\?\([^,}\"]*\)\"\?/\1/" | head -1
}

# Mostrar estado de un job con color
format_status() {
    local status=$1
    case "$status" in
        "COMPLETED")
            echo -e "${GREEN}✓ COMPLETED${NC}"
            ;;
        "FAILED"|"ABANDONED")
            echo -e "${RED}✗ $status${NC}"
            ;;
        "STARTED"|"STARTING")
            echo -e "${YELLOW}● $status${NC}"
            ;;
        *)
            echo -e "${CYAN}○ $status${NC}"
            ;;
    esac
}

# Loop principal de monitoreo
echo "============================================================"
echo "    Monitor de Batch Jobs - Card Management System"
echo "============================================================"
echo ""
echo "Autenticando..."

JWT_TOKEN=$(get_token)

if [ -z "$JWT_TOKEN" ] || [ "$JWT_TOKEN" == "null" ]; then
    echo -e "${RED}ERROR:${NC} No se pudo obtener token JWT"
    exit 1
fi

echo -e "${GREEN}Conectado exitosamente${NC}"
echo ""
echo "Presione Ctrl+C para detener el monitoreo"
echo "Actualizando cada ${REFRESH_INTERVAL} segundos..."
echo ""

while true; do
    clear
    echo "============================================================"
    echo "    Monitor de Batch Jobs - $(date '+%Y-%m-%d %H:%M:%S')"
    echo "============================================================"
    echo ""
    
    # Obtener resumen
    SUMMARY=$(curl -s -X GET "${BASE_URL}/api/batch/jobs/summary?limit=10" \
      -H "Authorization: Bearer ${JWT_TOKEN}")
    
    if [ $? -ne 0 ]; then
        echo -e "${RED}ERROR:${NC} No se pudo conectar al servidor"
        sleep $REFRESH_INTERVAL
        continue
    fi
    
    # Extraer estadísticas
    TOTAL_JOBS=$(extract_json_value "$SUMMARY" "totalJobs")
    RUNNING_JOBS=$(extract_json_value "$SUMMARY" "runningJobs")
    COMPLETED_JOBS=$(extract_json_value "$SUMMARY" "completedJobs")
    FAILED_JOBS=$(extract_json_value "$SUMMARY" "failedJobs")
    
    # Valores por defecto si están vacíos
    TOTAL_JOBS=${TOTAL_JOBS:-0}
    RUNNING_JOBS=${RUNNING_JOBS:-0}
    COMPLETED_JOBS=${COMPLETED_JOBS:-0}
    FAILED_JOBS=${FAILED_JOBS:-0}
    
    echo "📊 ESTADÍSTICAS GENERALES"
    echo "──────────────────────────────────────────────────────────"
    echo "  Total de jobs configurados: $TOTAL_JOBS"
    echo -e "  En ejecución:   ${YELLOW}$RUNNING_JOBS${NC}"
    echo -e "  Completados:    ${GREEN}$COMPLETED_JOBS${NC}"
    echo -e "  Fallidos:       ${RED}$FAILED_JOBS${NC}"
    echo ""
    
    echo "📋 EJECUCIONES RECIENTES"
    echo "──────────────────────────────────────────────────────────"
    
    # Mostrar ejecuciones recientes (simplificado sin jq)
    if echo "$SUMMARY" | grep -q '"recentExecutions":\[\]'; then
        echo "  No hay ejecuciones recientes"
    else
        echo "$SUMMARY" | grep -o '"jobExecutionId":[0-9]*' | sed 's/"jobExecutionId":/  ID: /' | head -10
        echo ""
        echo "  (Para detalles completos, use: curl /api/batch/jobs/summary)"
    fi
    
    echo ""
    echo "──────────────────────────────────────────────────────────"
    echo "Última actualización: $(date '+%H:%M:%S')"
    echo "Siguiente actualización en ${REFRESH_INTERVAL}s (Ctrl+C para salir)"
    
    sleep $REFRESH_INTERVAL
done

