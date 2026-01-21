package com.card.management.batch.steps;

import com.card.management.DTOs.TransactionReportResultDto;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Component;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Holder thread-safe para almacenar resultados de reportes de transacciones
 * Permite que el servicio recupere los resultados después de la ejecución del batch job
 */
@Component
@Slf4j
public class TransactionReportResultHolder {
    
    // Map thread-safe: jobExecutionId -> resultado del reporte
    private final Map<Long, TransactionReportResultDto> results = new ConcurrentHashMap<>();
    
    /**
     * Almacena el resultado de un reporte
     * @param jobExecutionId ID de la ejecución del job
     * @param result Resultado del reporte
     */
    public void storeResult(Long jobExecutionId, TransactionReportResultDto result) {
        results.put(jobExecutionId, result);
        log.debug("Stored report result for job execution {}", jobExecutionId);
    }
    
    /**
     * Recupera y elimina el resultado de un reporte
     * @param jobExecutionId ID de la ejecución del job
     * @return Resultado del reporte, o null si no existe
     */
    public TransactionReportResultDto retrieveAndRemove(Long jobExecutionId) {
        TransactionReportResultDto result = results.remove(jobExecutionId);
        if (result != null) {
            log.debug("Retrieved and removed report result for job execution {}", jobExecutionId);
        } else {
            log.warn("No report result found for job execution {}", jobExecutionId);
        }
        return result;
    }
    
    /**
     * Limpia resultados antiguos (por si acaso quedan huérfanos)
     */
    public void cleanup() {
        int size = results.size();
        if (size > 10) {
            log.warn("TransactionReportResultHolder has {} results, clearing old ones", size);
            results.clear();
        }
    }
}

