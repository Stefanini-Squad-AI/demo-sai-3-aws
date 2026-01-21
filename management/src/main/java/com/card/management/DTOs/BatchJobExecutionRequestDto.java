package com.card.management.DTOs;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import jakarta.validation.constraints.NotNull;

import java.util.Map;

/**
 * DTO para solicitud de ejecución de batch jobs
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class BatchJobExecutionRequestDto {
  
  /**
   * Nombre del job a ejecutar
   */
  @NotNull(message = "Job name is required")
  private String jobName;
  
  /**
   * Parámetros específicos del job
   * Ejemplos:
   * - transactionReportJob: {"startDate": "2025-01-01", "endDate": "2025-01-31"}
   * - transactionTypeMaintenanceJob: {"inputFile": "path/to/file.txt"}
   */
  private Map<String, String> parameters;
  
  /**
   * Indica si la ejecución debe ser asíncrona
   */
  @Builder.Default
  private boolean async = true;
}

