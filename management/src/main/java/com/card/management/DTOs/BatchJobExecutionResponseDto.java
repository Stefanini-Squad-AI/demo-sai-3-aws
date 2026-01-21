package com.card.management.DTOs;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

/**
 * DTO para respuesta de ejecución de batch jobs
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class BatchJobExecutionResponseDto {
  
  /**
   * ID de la ejecución del job en Spring Batch
   */
  private Long jobExecutionId;
  
  /**
   * Nombre del job ejecutado
   */
  private String jobName;
  
  /**
   * Estado de la ejecución
   */
  private String status;
  
  /**
   * Mensaje descriptivo
   */
  private String message;
  
  /**
   * Timestamp de inicio de la ejecución
   */
  private LocalDateTime startTime;
  
  /**
   * Timestamp de fin de la ejecución (si ya terminó)
   */
  private LocalDateTime endTime;
  
  /**
   * Indica si la operación fue exitosa
   */
  private boolean success;
  
  /**
   * Mensaje de error (si existe)
   */
  private String errorMessage;
}

