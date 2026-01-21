package com.card.management.DTOs;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.time.LocalDateTime;

/**
 * DTO para respuestas de reporte
 * Equivalente a los mensajes de salida del programa COBOL
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "Respuesta de generación de reporte")
public class ReportResponseDto {
  @Schema(description = "Indica si la operación fue exitosa")
  private boolean success;

  @Schema(description = "Mensaje de respuesta")
  private String message;

  @Schema(description = "Tipo de reporte generado")
  private String reportType;

  @Schema(description = "ID del trabajo enviado")
  private String jobId;

  @Schema(description = "Timestamp de la solicitud")
  private LocalDateTime timestamp;
  
  @Schema(description = "Datos del reporte de transacciones (cuando está disponible)")
  private TransactionReportResultDto reportData;
}
