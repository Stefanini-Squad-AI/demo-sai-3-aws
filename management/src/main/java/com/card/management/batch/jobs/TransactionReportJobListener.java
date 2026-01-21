package com.card.management.batch.jobs;

import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobExecutionListener;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Component;

import lombok.extern.slf4j.Slf4j;

/**
 * Job Listener para el reporte de transacciones
 * Equivalente a las rutinas de inicio y fin en CBTRN03C
 */
@Component
@Slf4j
public class TransactionReportJobListener implements JobExecutionListener {
  
  @Override
  public void beforeJob(@NonNull JobExecution jobExecution) {
    log.info("START OF EXECUTION OF PROGRAM CBTRN03C - Transaction Report Job");

    // Validar parámetros de fecha requeridos
    String startDate = jobExecution.getJobParameters().getString("startDate");
    String endDate = jobExecution.getJobParameters().getString("endDate");

    if (startDate == null || endDate == null) {
      log.error("Missing required job parameters: startDate and endDate");
      throw new IllegalArgumentException("startDate and endDate parameters are required");
    }

    log.info("Reporting from {} to {}", startDate, endDate);
  }

  @Override
  public void afterJob(@NonNull JobExecution jobExecution) {
    if (jobExecution.getStatus().isUnsuccessful()) {
      log.error("Transaction Report Job failed with status: {}", jobExecution.getStatus());
    } else {
      log.info("END OF EXECUTION OF PROGRAM CBTRN03C - Transaction Report Job completed successfully");
      
      // El resultado se almacena en el ExecutionContext por el StepExecutionListener
      log.debug("Report result stored in execution context for job {}", jobExecution.getId());
    }
  }
}
