package com.card.management.batch.jobs;

import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobExecutionListener;
import org.springframework.stereotype.Component;

@Component
public class TransactionTypeMaintenanceJobListener implements JobExecutionListener {

  @Override
  public void beforeJob(JobExecution jobExecution) {
    System.out.println("OPEN FILE OK - Starting Transaction Type Maintenance Job");
  }

  @Override
  public void afterJob(JobExecution jobExecution) {
    System.out.println("Transaction Type Maintenance Job completed with status: " +
        jobExecution.getStatus());
  }
}