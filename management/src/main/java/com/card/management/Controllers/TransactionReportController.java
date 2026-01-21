package com.card.management.Controllers;

import com.card.management.DTOs.ReportRequestDto;
import com.card.management.DTOs.ReportResponseDto;
import com.card.management.Services.TransactionReportService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import jakarta.validation.Valid;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;

/**
 * Controller para manejar reportes de transacciones
 * Migrado desde CORPT00C.CBL - Programa CICS COBOL para imprimir reportes de
 * transacciones
 */
@RestController
@RequestMapping("/api/v1/reports/transactions")
@RequiredArgsConstructor
@Slf4j
@Tag(name = "Transaction Reports", description = "API para generar reportes de transacciones")
public class TransactionReportController {
  private final TransactionReportService transactionReportService;

  // Constantes equivalentes a las variables WS del COBOL
  private static final String PROGRAM_NAME = "CORPT00C";
  private static final String TRANSACTION_ID = "CR00";
  private static final String DATE_FORMAT = "yyyy-MM-dd";

  /**
   * Genera reporte mensual de transacciones
   * Equivalente a la lógica WHEN MONTHLYI OF CORPT0AI NOT = SPACES AND LOW-VALUES
   */
  @PostMapping("/monthly")
  @Operation(summary = "Generar reporte mensual", description = "Genera un reporte de transacciones para el mes actual")
  public ResponseEntity<ReportResponseDto> generateMonthlyReport(
      @Valid @RequestBody ReportRequestDto request) {

    log.info("Procesando solicitud de reporte mensual");

    try {
      // Lógica equivalente al cálculo de fechas del mes actual en COBOL
      LocalDate currentDate = LocalDate.now();
      LocalDate startDate = currentDate.withDayOfMonth(1);
      LocalDate endDate = currentDate.plusMonths(1).withDayOfMonth(1).minusDays(1);

      ReportResponseDto response = transactionReportService.generateReport(
          "Monthly", startDate, endDate, request.isConfirmed());

      return ResponseEntity.ok(response);

    } catch (Exception e) {
      log.error("Error generando reporte mensual: {}", e.getMessage());
      return ResponseEntity.badRequest()
          .body(ReportResponseDto.builder()
              .success(false)
              .message("Error generando reporte mensual: " + e.getMessage())
              .build());
    }
  }

  /**
   * Genera reporte anual de transacciones
   * Equivalente a la lógica WHEN YEARLYI OF CORPT0AI NOT = SPACES AND LOW-VALUES
   */
  @PostMapping("/yearly")
  @Operation(summary = "Generar reporte anual", description = "Genera un reporte de transacciones para el año actual")
  public ResponseEntity<ReportResponseDto> generateYearlyReport(
      @Valid @RequestBody ReportRequestDto request) {

    log.info("Procesando solicitud de reporte anual");

    try {
      // Lógica equivalente al cálculo de fechas del año actual en COBOL
      LocalDate currentDate = LocalDate.now();
      LocalDate startDate = LocalDate.of(currentDate.getYear(), 1, 1);
      LocalDate endDate = LocalDate.of(currentDate.getYear(), 12, 31);

      ReportResponseDto response = transactionReportService.generateReport(
          "Yearly", startDate, endDate, request.isConfirmed());

      return ResponseEntity.ok(response);

    } catch (Exception e) {
      log.error("Error generando reporte anual: {}", e.getMessage());
      return ResponseEntity.badRequest()
          .body(ReportResponseDto.builder()
              .success(false)
              .message("Error generando reporte anual: " + e.getMessage())
              .build());
    }
  }

  /**
   * Genera reporte personalizado de transacciones
   * Equivalente a la lógica WHEN CUSTOMI OF CORPT0AI NOT = SPACES AND LOW-VALUES
   */
  @PostMapping("/custom")
  @Operation(summary = "Generar reporte personalizado", description = "Genera un reporte de transacciones para un rango de fechas específico")
  public ResponseEntity<ReportResponseDto> generateCustomReport(
      @Valid @RequestBody ReportRequestDto request) {

    log.info("Procesando solicitud de reporte personalizado");

    try {
      // Validaciones equivalentes a las del COBOL para fechas personalizadas
      if (request.getStartDate() == null) {
        return ResponseEntity.badRequest()
            .body(ReportResponseDto.builder()
                .success(false)
                .message("Start Date can NOT be empty...")
                .build());
      }

      if (request.getEndDate() == null) {
        return ResponseEntity.badRequest()
            .body(ReportResponseDto.builder()
                .success(false)
                .message("End Date can NOT be empty...")
                .build());
      }

      // Validación de que la fecha de inicio no sea posterior a la fecha de fin
      if (request.getStartDate().isAfter(request.getEndDate())) {
        return ResponseEntity.badRequest()
            .body(ReportResponseDto.builder()
                .success(false)
                .message("Start Date cannot be after End Date...")
                .build());
      }

      ReportResponseDto response = transactionReportService.generateReport(
          "Custom", request.getStartDate(), request.getEndDate(), request.isConfirmed());

      return ResponseEntity.ok(response);

    } catch (Exception e) {
      log.error("Error generando reporte personalizado: {}", e.getMessage());
      return ResponseEntity.badRequest()
          .body(ReportResponseDto.builder()
              .success(false)
              .message("Error generando reporte personalizado: " + e.getMessage())
              .build());
    }
  }
}
