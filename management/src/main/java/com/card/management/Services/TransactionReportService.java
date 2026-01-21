package com.card.management.Services;

import com.card.management.DTOs.BatchJobExecutionResponseDto;
import com.card.management.DTOs.ReportResponseDto;
import com.card.management.DTOs.TransactionReportResultDto;
import com.card.management.batch.steps.TransactionReportResultHolder;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.time.LocalDateTime;

/**
 * Servicio para manejar la generación de reportes de transacciones
 * Integrado con BatchJobService para ejecutar el batch job real transactionReportJob
 * Devuelve los datos del reporte como JSON en lugar de escribir a archivo
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class TransactionReportService {
  
  private final BatchJobService batchJobService;
  private final TransactionReportResultHolder resultHolder;

  /**
   * Genera un reporte de transacciones ejecutando el batch job real
   * Devuelve los datos del reporte como JSON
   */
  public ReportResponseDto generateReport(String reportType, LocalDate startDate,
      LocalDate endDate, boolean confirmed) {

    if (!confirmed) {
      return ReportResponseDto.builder()
          .success(false)
          .message("Please confirm to generate the " + reportType + " report...")
          .timestamp(LocalDateTime.now())
          .build();
    }

    try {
      // Ejecutar el batch job real usando BatchJobService
      BatchJobExecutionResponseDto batchResponse = batchJobService.executeTransactionReportJob(startDate, endDate);

      if (batchResponse.isSuccess()) {
        log.info("Reporte {} generado exitosamente. Job Execution ID: {}", reportType, batchResponse.getJobExecutionId());

        // Obtener los datos del reporte desde el holder usando el jobExecutionId
        TransactionReportResultDto reportData = resultHolder.retrieveAndRemove(batchResponse.getJobExecutionId());
        
        if (reportData != null) {
          // Agregar información de fechas al reporte
          reportData.setReportType(reportType);
          reportData.setStartDate(startDate);
          reportData.setEndDate(endDate);
          
          log.debug("Report data retrieved: {} transactions, {} accounts", 
              reportData.getTotalTransactionCount(), reportData.getAccountCount());
        } else {
          log.warn("No report data found for job execution {}", batchResponse.getJobExecutionId());
        }

        return ReportResponseDto.builder()
            .success(true)
            .message(reportType + " report generated successfully")
            .reportType(reportType)
            .jobId(String.valueOf(batchResponse.getJobExecutionId()))
            .timestamp(LocalDateTime.now())
            .reportData(reportData)
            .build();
      } else {
        log.error("Error ejecutando batch job: {}", batchResponse.getErrorMessage());
        return ReportResponseDto.builder()
            .success(false)
            .message("Unable to generate report: " + batchResponse.getErrorMessage())
            .timestamp(LocalDateTime.now())
            .build();
      }

    } catch (Exception e) {
      log.error("Error generando reporte: {}", e.getMessage(), e);
      return ReportResponseDto.builder()
          .success(false)
          .message("Unable to generate report: " + e.getMessage())
          .timestamp(LocalDateTime.now())
          .build();
    }
  }
}
