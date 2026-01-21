package com.card.management.batch.jobs;

import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobExecutionListener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import com.card.management.Repositories.RejectedTransactionRepository;

/**
 * Listener para el job de procesamiento de transacciones
 * Equivalente a los contadores WS-TRANSACTION-COUNT y WS-REJECT-COUNT en COBOL
 */
@Component
public class TransactionPostingJobListener implements JobExecutionListener {
  @Autowired
  private RejectedTransactionRepository rejectedTransactionRepository;

  @Override
  public void beforeJob(JobExecution jobExecution) {
    System.out.println("START OF EXECUTION OF PROGRAM CBTRN02C (Java equivalent)");
  }

  @Override
  public void afterJob(JobExecution jobExecution) {
    // Obtener contadores desde la ejecución del job
    long transactionCount = jobExecution.getStepExecutions().stream()
        .mapToLong(stepExecution -> stepExecution.getReadCount())
        .sum();

    // Contar rechazos desde la base de datos (o desde step execution)
    long rejectCount = jobExecution.getStepExecutions().stream()
        .mapToLong(stepExecution -> stepExecution.getWriteSkipCount())
        .sum();

    // Equivalente a los DISPLAY finales en COBOL
    System.out.println("TRANSACTIONS PROCESSED :" + transactionCount);
    System.out.println("TRANSACTIONS REJECTED  :" + rejectCount);

    // Equivalente al RETURN-CODE en COBOL
    if (rejectCount > 0) {
      jobExecution.setExitStatus(org.springframework.batch.core.ExitStatus.FAILED);
    }

    System.out.println("END OF EXECUTION OF PROGRAM CBTRN02C (Java equivalent)");
  }
}
