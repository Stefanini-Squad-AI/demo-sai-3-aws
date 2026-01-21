package com.card.management.batch.jobs;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.BatchStatus;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobExecutionListener;
import org.springframework.stereotype.Component;

@Component
public class StatementGenerationJobListener implements JobExecutionListener {
  private static final Logger logger = LoggerFactory.getLogger(StatementGenerationJobListener.class);

  @Override
  public void beforeJob(JobExecution jobExecution) {
    logger.info("Starting Statement Generation Job - CBSTM03A equivalent");
    logger.info("Job Parameters: {}", jobExecution.getJobParameters());
  }

  @Override
  public void afterJob(JobExecution jobExecution) {
    if (jobExecution.getStatus() == BatchStatus.COMPLETED) {
      logger.info("Statement Generation Job completed successfully");
      logger.info("Read Count: {}", jobExecution.getStepExecutions().iterator().next().getReadCount());
      logger.info("Write Count: {}", jobExecution.getStepExecutions().iterator().next().getWriteCount());
    } else {
      logger.error("Statement Generation Job failed with status: {}", jobExecution.getStatus());
      jobExecution.getAllFailureExceptions().forEach(throwable -> logger.error("Job failure: ", throwable));
    }
  }
}
