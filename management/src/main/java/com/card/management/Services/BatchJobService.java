package com.card.management.Services;

import com.card.management.DTOs.BatchJobExecutionRequestDto;
import com.card.management.DTOs.BatchJobExecutionResponseDto;
import com.card.management.DTOs.BatchJobStatusDto;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.batch.core.*;
import org.springframework.batch.core.explore.JobExplorer;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.batch.core.repository.JobExecutionAlreadyRunningException;
import org.springframework.batch.core.repository.JobInstanceAlreadyCompleteException;
import org.springframework.batch.core.repository.JobRestartException;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.scheduling.annotation.Async;
import org.springframework.stereotype.Service;

import java.time.LocalDate;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Servicio centralizado para gestionar la ejecución de batch jobs
 */
@Service
@RequiredArgsConstructor
@Slf4j
public class BatchJobService {
  
  private final JobLauncher jobLauncher;
  private final JobExplorer jobExplorer;
  
  @Autowired
  @Qualifier("transactionPostingJob")
  private Job transactionPostingJob;
  
  @Autowired
  @Qualifier("interestCalculationJob")
  private Job interestCalculationJob;
  
  @Autowired
  @Qualifier("statementGenerationJob")
  private Job statementGenerationJob;
  
  @Autowired
  @Qualifier("transactionReportJob")
  private Job transactionReportJob;
  
  @Autowired
  @Qualifier("authorizationCleanupJob")
  private Job authorizationCleanupJob;
  
  @Autowired
  @Qualifier("transactionTypeMaintenanceJob")
  private Job transactionTypeMaintenanceJob;
  
  /**
   * Ejecuta un batch job con los parámetros proporcionados
   */
  public BatchJobExecutionResponseDto executeJob(BatchJobExecutionRequestDto request) {
    try {
      log.info("Ejecutando batch job: {} con parámetros: {}", request.getJobName(), request.getParameters());
      
      // Obtener el job correspondiente
      Job job = getJobByName(request.getJobName());
      
      // Validar parámetros requeridos
      validateJobParameters(request.getJobName(), request.getParameters());
      
      // Construir parámetros del job
      JobParameters jobParameters = buildJobParameters(request.getParameters());
      
      // Ejecutar el job
      JobExecution execution = jobLauncher.run(job, jobParameters);
      
      return BatchJobExecutionResponseDto.builder()
          .jobExecutionId(execution.getId())
          .jobName(request.getJobName())
          .status(execution.getStatus().name())
          .message("Job ejecutado exitosamente")
          .startTime(execution.getStartTime())
          .endTime(execution.getEndTime())
          .success(true)
          .build();
      
    } catch (JobExecutionAlreadyRunningException e) {
      log.error("El job ya está en ejecución: {}", request.getJobName(), e);
      return BatchJobExecutionResponseDto.builder()
          .jobName(request.getJobName())
          .success(false)
          .errorMessage("El job ya está en ejecución")
          .build();
      
    } catch (JobRestartException e) {
      log.error("Error al reiniciar el job: {}", request.getJobName(), e);
      return BatchJobExecutionResponseDto.builder()
          .jobName(request.getJobName())
          .success(false)
          .errorMessage("Error al reiniciar el job: " + e.getMessage())
          .build();
      
    } catch (JobInstanceAlreadyCompleteException e) {
      log.error("El job ya fue completado con estos parámetros: {}", request.getJobName(), e);
      return BatchJobExecutionResponseDto.builder()
          .jobName(request.getJobName())
          .success(false)
          .errorMessage("El job ya fue completado con estos parámetros. Use parámetros diferentes.")
          .build();
      
    } catch (JobParametersInvalidException e) {
      log.error("Parámetros inválidos para el job: {}", request.getJobName(), e);
      return BatchJobExecutionResponseDto.builder()
          .jobName(request.getJobName())
          .success(false)
          .errorMessage("Parámetros inválidos: " + e.getMessage())
          .build();
      
    } catch (IllegalArgumentException e) {
      log.error("Error de validación: {}", e.getMessage());
      return BatchJobExecutionResponseDto.builder()
          .jobName(request.getJobName())
          .success(false)
          .errorMessage(e.getMessage())
          .build();
      
    } catch (Exception e) {
      log.error("Error inesperado ejecutando el job: {}", request.getJobName(), e);
      return BatchJobExecutionResponseDto.builder()
          .jobName(request.getJobName())
          .success(false)
          .errorMessage("Error inesperado: " + e.getMessage())
          .build();
    }
  }
  
  /**
   * Ejecuta un batch job de forma asíncrona
   */
  @Async
  public void executeJobAsync(BatchJobExecutionRequestDto request) {
    executeJob(request);
  }
  
  /**
   * Consulta el estado de una ejecución específica
   */
  public BatchJobStatusDto getJobStatus(Long jobExecutionId) {
    JobExecution execution = jobExplorer.getJobExecution(jobExecutionId);
    
    if (execution == null) {
      return null;
    }
    
    // Obtener métricas de steps
    Collection<StepExecution> stepExecutions = execution.getStepExecutions();
    long totalRead = stepExecutions.stream().mapToLong(StepExecution::getReadCount).sum();
    long totalWrite = stepExecutions.stream().mapToLong(StepExecution::getWriteCount).sum();
    long totalCommit = stepExecutions.stream().mapToLong(StepExecution::getCommitCount).sum();
    long totalRollback = stepExecutions.stream().mapToLong(StepExecution::getRollbackCount).sum();
    
    return BatchJobStatusDto.builder()
        .jobExecutionId(execution.getId())
        .jobName(execution.getJobInstance().getJobName())
        .status(execution.getStatus().name())
        .exitCode(execution.getExitStatus().getExitCode())
        .exitMessage(execution.getExitStatus().getExitDescription())
        .startTime(execution.getStartTime())
        .endTime(execution.getEndTime())
        .lastUpdated(execution.getLastUpdated())
        .jobParameters(extractJobParameters(execution.getJobParameters()))
        .readCount(totalRead)
        .writeCount(totalWrite)
        .commitCount(totalCommit)
        .rollbackCount(totalRollback)
        .build();
  }
  
  /**
   * Obtiene el historial de ejecuciones de un job específico
   */
  public List<BatchJobStatusDto> getJobHistory(String jobName, int limit) {
    List<JobInstance> jobInstances = jobExplorer.findJobInstancesByJobName(jobName, 0, limit);
    
    return jobInstances.stream()
        .flatMap(instance -> jobExplorer.getJobExecutions(instance).stream())
        .map(execution -> getJobStatus(execution.getId()))
        .sorted(Comparator.comparing(BatchJobStatusDto::getStartTime, Comparator.nullsLast(Comparator.reverseOrder())))
        .collect(Collectors.toList());
  }
  
  /**
   * Obtiene todos los jobs disponibles
   */
  public List<String> getAvailableJobs() {
    return Arrays.asList(
        "transactionPostingJob",
        "interestCalculationJob",
        "statementGenerationJob",
        "transactionReportJob",
        "authorizationCleanupJob",
        "transactionTypeMaintenanceJob"
    );
  }
  
  /**
   * Obtiene un resumen de ejecuciones recientes de todos los jobs
   */
  public Map<String, Object> getExecutionSummary(int limit) {
    List<String> jobNames = getAvailableJobs();
    
    int totalJobs = jobNames.size();
    int runningJobs = 0;
    int completedJobs = 0;
    int failedJobs = 0;
    List<BatchJobStatusDto> recentExecutions = new ArrayList<>();
    
    // Recopilar información de todos los jobs
    for (String jobName : jobNames) {
      List<JobInstance> instances = jobExplorer.findJobInstancesByJobName(jobName, 0, limit);
      
      for (JobInstance instance : instances) {
        List<JobExecution> executions = jobExplorer.getJobExecutions(instance);
        for (JobExecution execution : executions) {
          BatchJobStatusDto status = getJobStatus(execution.getId());
          if (status != null) {
            recentExecutions.add(status);
            
            // Contar por estado
            switch (status.getStatus()) {
              case "STARTED", "STARTING" -> runningJobs++;
              case "COMPLETED" -> completedJobs++;
              case "FAILED", "ABANDONED" -> failedJobs++;
            }
          }
        }
      }
    }
    
    // Ordenar por fecha de inicio más reciente
    recentExecutions.sort(Comparator.comparing(
        BatchJobStatusDto::getStartTime, 
        Comparator.nullsLast(Comparator.reverseOrder())
    ));
    
    // Limitar resultados
    if (recentExecutions.size() > limit) {
      recentExecutions = recentExecutions.subList(0, limit);
    }
    
    Map<String, Object> summary = new HashMap<>();
    summary.put("totalJobs", totalJobs);
    summary.put("runningJobs", runningJobs);
    summary.put("completedJobs", completedJobs);
    summary.put("failedJobs", failedJobs);
    summary.put("timestamp", LocalDate.now());
    summary.put("recentExecutions", recentExecutions);
    
    return summary;
  }
  
  /**
   * Métodos de ejecución específicos por job (para usar desde controllers o scheduler)
   */
  
  public BatchJobExecutionResponseDto executeTransactionPostingJob() {
    BatchJobExecutionRequestDto request = BatchJobExecutionRequestDto.builder()
        .jobName("transactionPostingJob")
        .parameters(Map.of("timestamp", String.valueOf(System.currentTimeMillis())))
        .build();
    return executeJob(request);
  }
  
  public BatchJobExecutionResponseDto executeInterestCalculationJob() {
    BatchJobExecutionRequestDto request = BatchJobExecutionRequestDto.builder()
        .jobName("interestCalculationJob")
        .parameters(Map.of("timestamp", String.valueOf(System.currentTimeMillis())))
        .build();
    return executeJob(request);
  }
  
  public BatchJobExecutionResponseDto executeStatementGenerationJob() {
    BatchJobExecutionRequestDto request = BatchJobExecutionRequestDto.builder()
        .jobName("statementGenerationJob")
        .parameters(Map.of("timestamp", String.valueOf(System.currentTimeMillis())))
        .build();
    return executeJob(request);
  }
  
  public BatchJobExecutionResponseDto executeTransactionReportJob(LocalDate startDate, LocalDate endDate) {
    BatchJobExecutionRequestDto request = BatchJobExecutionRequestDto.builder()
        .jobName("transactionReportJob")
        .parameters(Map.of(
            "startDate", startDate.toString(),
            "endDate", endDate.toString(),
            "timestamp", String.valueOf(System.currentTimeMillis())
        ))
        .build();
    return executeJob(request);
  }
  
  public BatchJobExecutionResponseDto executeAuthCleanupJob() {
    BatchJobExecutionRequestDto request = BatchJobExecutionRequestDto.builder()
        .jobName("authorizationCleanupJob")
        .parameters(Map.of("timestamp", String.valueOf(System.currentTimeMillis())))
        .build();
    return executeJob(request);
  }
  
  public BatchJobExecutionResponseDto executeTransactionTypeMaintenanceJob(String inputFile) {
    BatchJobExecutionRequestDto request = BatchJobExecutionRequestDto.builder()
        .jobName("transactionTypeMaintenanceJob")
        .parameters(Map.of(
            "inputFile", inputFile,
            "timestamp", String.valueOf(System.currentTimeMillis())
        ))
        .build();
    return executeJob(request);
  }
  
  // Métodos auxiliares privados
  
  private Job getJobByName(String jobName) {
    return switch (jobName) {
      case "transactionPostingJob" -> transactionPostingJob;
      case "interestCalculationJob" -> interestCalculationJob;
      case "statementGenerationJob" -> statementGenerationJob;
      case "transactionReportJob" -> transactionReportJob;
      case "authorizationCleanupJob" -> authorizationCleanupJob;
      case "transactionTypeMaintenanceJob" -> transactionTypeMaintenanceJob;
      default -> throw new IllegalArgumentException("Job no encontrado: " + jobName);
    };
  }
  
  private void validateJobParameters(String jobName, Map<String, String> parameters) {
    if (parameters == null) {
      parameters = new HashMap<>();
    }
    
    switch (jobName) {
      case "transactionReportJob":
        if (!parameters.containsKey("startDate") || !parameters.containsKey("endDate")) {
          throw new IllegalArgumentException("transactionReportJob requiere parámetros 'startDate' y 'endDate'");
        }
        break;
      case "transactionTypeMaintenanceJob":
        if (!parameters.containsKey("inputFile")) {
          throw new IllegalArgumentException("transactionTypeMaintenanceJob requiere parámetro 'inputFile'");
        }
        break;
      // Los demás jobs no tienen parámetros obligatorios
    }
  }
  
  private JobParameters buildJobParameters(Map<String, String> parameters) {
    JobParametersBuilder builder = new JobParametersBuilder();
    
    if (parameters != null) {
      parameters.forEach(builder::addString);
    }
    
    // Agregar timestamp único para permitir re-ejecuciones
    builder.addLong("timestamp", System.currentTimeMillis());
    
    return builder.toJobParameters();
  }
  
  private Map<String, Object> extractJobParameters(JobParameters jobParameters) {
    Map<String, Object> params = new HashMap<>();
    jobParameters.getParameters().forEach((key, value) -> params.put(key, value.getValue()));
    return params;
  }
}

