package com.card.management.batch.jobs;

import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobExecutionListener;
import org.springframework.stereotype.Component;
import lombok.extern.slf4j.Slf4j;

/**
 * Listener para el job de cálculo de intereses
 * Equivalente a los mensajes de inicio y fin en CBACT04C
 */
@Component
@Slf4j
public class InterestCalculationJobListener implements JobExecutionListener {
  @Override
  public void beforeJob(JobExecution jobExecution) {
    log.info("START OF EXECUTION OF INTEREST CALCULATION JOB");
    // Equivalente a: DISPLAY 'START OF EXECUTION OF PROGRAM CBACT04C'
  }

  @Override
  public void afterJob(JobExecution jobExecution) {
    log.info("END OF EXECUTION OF INTEREST CALCULATION JOB");
    log.info("Job Status: {}", jobExecution.getStatus());
    // Equivalente a: DISPLAY 'END OF EXECUTION OF PROGRAM CBACT04C'
  }
}
