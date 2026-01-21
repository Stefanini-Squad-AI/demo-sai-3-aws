package com.card.management.DTOs;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.Map;

/**
 * DTO para consulta de estado de batch jobs
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class BatchJobStatusDto {
  
  /**
   * ID de la ejecución del job
   */
  private Long jobExecutionId;
  
  /**
   * Nombre del job
   */
  private String jobName;
  
  /**
   * Estado actual
   */
  private String status;
  
  /**
   * Código de salida
   */
  private String exitCode;
  
  /**
   * Mensaje de salida
   */
  private String exitMessage;
  
  /**
   * Timestamp de inicio
   */
  private LocalDateTime startTime;
  
  /**
   * Timestamp de fin
   */
  private LocalDateTime endTime;
  
  /**
   * Timestamp de última actualización
   */
  private LocalDateTime lastUpdated;
  
  /**
   * Parámetros del job
   */
  private Map<String, Object> jobParameters;
  
  /**
   * Número de items leídos
   */
  private Long readCount;
  
  /**
   * Número de items escritos
   */
  private Long writeCount;
  
  /**
   * Número de items procesados
   */
  private Long commitCount;
  
  /**
   * Número de errores
   */
  private Long rollbackCount;
}

