package com.card.management.DTOs;

import io.swagger.v3.oas.annotations.media.Schema;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import jakarta.validation.constraints.NotNull;
import java.time.LocalDate;

/**
 * DTO para solicitudes de reporte
 * Equivalente a los campos de entrada del copybook CORPT0AI
 */
@Data
@Builder
@NoArgsConstructor
@AllArgsConstructor
@Schema(description = "Solicitud para generar reporte de transacciones")
public class ReportRequestDto {
  @Schema(description = "Fecha de inicio para reporte personalizado", example = "2024-01-01")
  private LocalDate startDate;

  @Schema(description = "Fecha de fin para reporte personalizado", example = "2024-12-31")
  private LocalDate endDate;

  @NotNull
  @Schema(description = "Confirmación para generar el reporte", example = "true")
  private boolean confirmed;
}
