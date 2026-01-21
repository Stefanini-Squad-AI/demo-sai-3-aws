package com.card.management.Controllers;

import com.card.management.DTOs.BatchJobExecutionRequestDto;
import com.card.management.DTOs.BatchJobExecutionResponseDto;
import com.card.management.DTOs.BatchJobStatusDto;
import com.card.management.DTOs.ReportResponseDto;
import com.card.management.Services.BatchJobService;
import com.card.management.Services.TransactionReportService;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.time.LocalDate;
import java.util.List;
import java.util.Map;

/**
 * Controller para ejecutar y monitorear batch jobs
 * Proporciona endpoints REST para integrar los procesos batch con la API
 */
@RestController
@RequestMapping("/api/batch/jobs")
@RequiredArgsConstructor
@Slf4j
@Tag(name = "Batch Jobs", description = "Endpoints para ejecutar y monitorear batch jobs")
// @SecurityRequirement(name = "JWT Authentication")
@PreAuthorize("hasRole('ADMIN')") // Solo administradores pueden ejecutar batch jobs
public class BatchJobController {
  
  private final BatchJobService batchJobService;
  private final TransactionReportService transactionReportService;
  
  /**
   * Ejecuta un batch job genérico con parámetros personalizados
   */
  @PostMapping("/execute")
  @Operation(summary = "Ejecutar batch job genérico", 
             description = "Ejecuta cualquier batch job disponible con parámetros personalizados")
  @ApiResponses(value = {
      @ApiResponse(responseCode = "200", description = "Job ejecutado exitosamente"),
      @ApiResponse(responseCode = "400", description = "Parámetros inválidos"),
      @ApiResponse(responseCode = "403", description = "Acceso denegado - se requiere rol ADMIN"),
      @ApiResponse(responseCode = "500", description = "Error interno del servidor")
  })
  public ResponseEntity<BatchJobExecutionResponseDto> executeJob(
      @Valid @RequestBody BatchJobExecutionRequestDto request) {
    
    log.info("Recibida solicitud para ejecutar batch job: {}", request.getJobName());
    
    BatchJobExecutionResponseDto response = batchJobService.executeJob(request);
    
    if (response.isSuccess()) {
      return ResponseEntity.ok(response);
    } else {
      return ResponseEntity.badRequest().body(response);
    }
  }
  
  /**
   * Ejecuta el job de procesamiento de transacciones diarias
   */
  @PostMapping("/transaction-posting/execute")
  @Operation(summary = "Ejecutar procesamiento de transacciones", 
             description = "Procesa todas las transacciones diarias pendientes (CBTRN02C)")
  public ResponseEntity<BatchJobExecutionResponseDto> executeTransactionPostingJob() {
    log.info("Ejecutando transactionPostingJob");
    BatchJobExecutionResponseDto response = batchJobService.executeTransactionPostingJob();
    return response.isSuccess() ? ResponseEntity.ok(response) : ResponseEntity.badRequest().body(response);
  }
  
  /**
   * Ejecuta el job de cálculo de intereses
   */
  @PostMapping("/interest-calculation/execute")
  @Operation(summary = "Ejecutar cálculo de intereses", 
             description = "Calcula intereses para todas las cuentas basado en balances de categorías (CBACT04C)")
  public ResponseEntity<BatchJobExecutionResponseDto> executeInterestCalculationJob() {
    log.info("Ejecutando interestCalculationJob");
    BatchJobExecutionResponseDto response = batchJobService.executeInterestCalculationJob();
    return response.isSuccess() ? ResponseEntity.ok(response) : ResponseEntity.badRequest().body(response);
  }
  
  /**
   * Ejecuta el job de generación de estados de cuenta
   */
  @PostMapping("/statement-generation/execute")
  @Operation(summary = "Ejecutar generación de estados de cuenta", 
             description = "Genera estados de cuenta en texto y HTML para todas las tarjetas (CBSTM03A)")
  public ResponseEntity<BatchJobExecutionResponseDto> executeStatementGenerationJob() {
    log.info("Ejecutando statementGenerationJob");
    BatchJobExecutionResponseDto response = batchJobService.executeStatementGenerationJob();
    return response.isSuccess() ? ResponseEntity.ok(response) : ResponseEntity.badRequest().body(response);
  }
  
  /**
   * Ejecuta el job de reporte de transacciones y devuelve los datos como JSON
   */
  @PostMapping("/transaction-report/execute")
  @Operation(summary = "Ejecutar reporte de transacciones", 
             description = "Genera reporte detallado de transacciones para un rango de fechas (CBTRN03C) y devuelve los datos como JSON")
  public ResponseEntity<ReportResponseDto> executeTransactionReportJob(
      @Parameter(description = "Fecha de inicio (formato: YYYY-MM-DD)") 
      @RequestParam String startDate,
      @Parameter(description = "Fecha de fin (formato: YYYY-MM-DD)") 
      @RequestParam String endDate) {
    
    log.info("Ejecutando transactionReportJob desde {} hasta {}", startDate, endDate);
    
    try {
      LocalDate start = LocalDate.parse(startDate);
      LocalDate end = LocalDate.parse(endDate);
      
      if (start.isAfter(end)) {
        return ResponseEntity.badRequest().body(
            ReportResponseDto.builder()
                .success(false)
                .message("La fecha de inicio no puede ser posterior a la fecha de fin")
                .build()
        );
      }
      
      // Ejecutar el reporte de forma confirmada directamente
      ReportResponseDto response = transactionReportService.generateReport("Custom", start, end, true);
      return response.isSuccess() ? ResponseEntity.ok(response) : ResponseEntity.badRequest().body(response);
      
    } catch (Exception e) {
      log.error("Error al procesar fechas: {}", e.getMessage());
      return ResponseEntity.badRequest().body(
          ReportResponseDto.builder()
              .success(false)
              .message("Formato de fecha inválido. Use formato: YYYY-MM-DD")
              .build()
      );
    }
  }
  
  /**
   * Ejecuta el job de limpieza de autorizaciones expiradas
   */
  @PostMapping("/auth-cleanup/execute")
  @Operation(summary = "Ejecutar limpieza de autorizaciones", 
             description = "Elimina autorizaciones expiradas de la base de datos (CBPAUP0C)")
  public ResponseEntity<BatchJobExecutionResponseDto> executeAuthCleanupJob() {
    log.info("Ejecutando authorizationCleanupJob");
    BatchJobExecutionResponseDto response = batchJobService.executeAuthCleanupJob();
    return response.isSuccess() ? ResponseEntity.ok(response) : ResponseEntity.badRequest().body(response);
  }
  
  /**
   * Ejecuta el job de mantenimiento de tipos de transacción
   */
  @PostMapping("/transaction-type-maintenance/execute")
  @Operation(summary = "Ejecutar mantenimiento de tipos de transacción", 
             description = "Procesa archivo de operaciones A/U/D para tipos de transacción (COBTUPDT)")
  public ResponseEntity<BatchJobExecutionResponseDto> executeTransactionTypeMaintenanceJob(
      @Parameter(description = "Ruta al archivo de entrada con operaciones") 
      @RequestParam String inputFile) {
    
    log.info("Ejecutando transactionTypeMaintenanceJob con archivo: {}", inputFile);
    BatchJobExecutionResponseDto response = batchJobService.executeTransactionTypeMaintenanceJob(inputFile);
    return response.isSuccess() ? ResponseEntity.ok(response) : ResponseEntity.badRequest().body(response);
  }
  
  /**
   * Consulta el estado de una ejecución específica
   */
  @GetMapping("/status/{jobExecutionId}")
  @Operation(summary = "Consultar estado de ejecución", 
             description = "Obtiene el estado y métricas de una ejecución específica de batch job")
  @ApiResponses(value = {
      @ApiResponse(responseCode = "200", description = "Estado obtenido exitosamente"),
      @ApiResponse(responseCode = "404", description = "Ejecución no encontrada"),
      @ApiResponse(responseCode = "403", description = "Acceso denegado")
  })
  public ResponseEntity<BatchJobStatusDto> getJobStatus(
      @Parameter(description = "ID de la ejecución del job") 
      @PathVariable Long jobExecutionId) {
    
    log.info("Consultando estado del job execution ID: {}", jobExecutionId);
    BatchJobStatusDto status = batchJobService.getJobStatus(jobExecutionId);
    
    if (status == null) {
      return ResponseEntity.notFound().build();
    }
    
    return ResponseEntity.ok(status);
  }
  
  /**
   * Obtiene el historial de ejecuciones de un job específico
   */
  @GetMapping("/history/{jobName}")
  @Operation(summary = "Consultar historial de ejecuciones", 
             description = "Obtiene las últimas ejecuciones de un batch job específico")
  public ResponseEntity<List<BatchJobStatusDto>> getJobHistory(
      @Parameter(description = "Nombre del job") 
      @PathVariable String jobName,
      @Parameter(description = "Número máximo de resultados") 
      @RequestParam(defaultValue = "10") int limit) {
    
    log.info("Consultando historial del job: {} (límite: {})", jobName, limit);
    
    try {
      List<BatchJobStatusDto> history = batchJobService.getJobHistory(jobName, limit);
      return ResponseEntity.ok(history);
    } catch (Exception e) {
      log.error("Error consultando historial: {}", e.getMessage());
      return ResponseEntity.badRequest().build();
    }
  }
  
  /**
   * Lista todos los batch jobs disponibles
   */
  @GetMapping("/available")
  @Operation(summary = "Listar jobs disponibles", 
             description = "Obtiene la lista de todos los batch jobs disponibles en el sistema")
  public ResponseEntity<Map<String, List<String>>> getAvailableJobs() {
    log.info("Consultando lista de jobs disponibles");
    List<String> jobs = batchJobService.getAvailableJobs();
    return ResponseEntity.ok(Map.of("availableJobs", jobs));
  }
  
  /**
   * Obtiene un resumen ejecutivo de todas las ejecuciones recientes
   */
  @GetMapping("/summary")
  @Operation(summary = "Resumen ejecutivo de batch jobs", 
             description = "Obtiene un resumen con estadísticas y ejecuciones recientes de todos los jobs")
  public ResponseEntity<Map<String, Object>> getExecutionSummary(
      @Parameter(description = "Número máximo de ejecuciones recientes a mostrar") 
      @RequestParam(defaultValue = "20") int limit) {
    
    log.info("Consultando resumen de ejecuciones (límite: {})", limit);
    Map<String, Object> summary = batchJobService.getExecutionSummary(limit);
    return ResponseEntity.ok(summary);
  }
}

