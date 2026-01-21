package com.card.management.DTOs;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;
import java.util.List;

/**
 * DTO para resumen de ejecuciones recientes de batch jobs
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class BatchJobExecutionSummaryDto {
  
  /**
   * Resumen general de todos los jobs
   */
  private String message;
  
  /**
   * Total de jobs en el sistema
   */
  private int totalJobs;
  
  /**
   * Jobs ejecutándose actualmente
   */
  private int runningJobs;
  
  /**
   * Jobs completados recientemente
   */
  private int completedJobs;
  
  /**
   * Jobs fallidos recientemente
   */
  private int failedJobs;
  
  /**
   * Timestamp del resumen
   */
  private LocalDateTime timestamp;
  
  /**
   * Lista de ejecuciones recientes
   */
  private List<BatchJobStatusDto> recentExecutions;
}

