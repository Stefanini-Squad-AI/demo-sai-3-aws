package com.card.management.batch.jobs;

import org.springframework.batch.core.ExitStatus;
import org.springframework.batch.core.StepExecution;
import org.springframework.batch.core.StepExecutionListener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.lang.NonNull;
import org.springframework.stereotype.Component;

import com.card.management.DTOs.TransactionReportResultDto;
import com.card.management.batch.steps.TransactionReportResultHolder;
import com.card.management.batch.steps.TransactionReportWriter;

import lombok.extern.slf4j.Slf4j;

/**
 * Step Listener para el reporte de transacciones
 * Se encarga de construir y almacenar el resultado después de procesar todas las transacciones
 */
@Component
@Slf4j
public class TransactionReportStepListener implements StepExecutionListener {
    
    @Autowired
    private TransactionReportWriter transactionReportWriter;
    
    @Autowired
    private TransactionReportResultHolder resultHolder;
    
    @Override
    public void beforeStep(@NonNull StepExecution stepExecution) {
        log.debug("Starting transaction report step for job execution {}", 
            stepExecution.getJobExecutionId());
    }
    
    @Override
    public ExitStatus afterStep(@NonNull StepExecution stepExecution) {
        if (stepExecution.getExitStatus().getExitCode().equals(ExitStatus.COMPLETED.getExitCode())) {
            try {
                // Construir el resultado final
                transactionReportWriter.buildReportResult();
                TransactionReportResultDto result = transactionReportWriter.getReportResult();
                
                if (result != null) {
                    // Almacenar en el holder usando el jobExecutionId
                    Long jobExecutionId = stepExecution.getJobExecutionId();
                    resultHolder.storeResult(jobExecutionId, result);
                    
                    log.info("Transaction report result stored for job execution {}. " +
                            "Total transactions: {}, Accounts: {}, Grand total: {}", 
                            jobExecutionId,
                            result.getTotalTransactionCount(),
                            result.getAccountCount(),
                            result.getGrandTotal());
                } else {
                    log.warn("Transaction report result is null for job execution {}", 
                        stepExecution.getJobExecutionId());
                }
            } catch (Exception e) {
                log.error("Error building transaction report result", e);
                return ExitStatus.FAILED;
            }
        }
        
        return stepExecution.getExitStatus();
    }
}

