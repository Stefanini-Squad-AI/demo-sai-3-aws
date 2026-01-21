package com.card.management.batch.jobs;

import com.card.management.batch.steps.AuthCleanupWriter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.batch.core.JobExecution;
import org.springframework.batch.core.JobExecutionListener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class AuthCleanupJobListener implements JobExecutionListener {

  private static final Logger logger = LoggerFactory.getLogger(AuthCleanupJobListener.class);

  @Autowired
  private AuthCleanupWriter authCleanupWriter;

  @Override
  public void beforeJob(JobExecution jobExecution) {
    logger.info("Starting CBPAUP0C equivalent - Authorization Cleanup Job");
    logger.info("Job Parameters: {}", jobExecution.getJobParameters());
  }

  @Override
  public void afterJob(JobExecution jobExecution) {
    // Equivalente al display final en COBOL
    logger.info("*-------------------------------------*");
    logger.info("# TOTAL SUMMARY DELETED  : {}", authCleanupWriter.getTotalSummaryDeleted());
    logger.info("# DETAILS REC DELETED    : {}", authCleanupWriter.getTotalDetailsDeleted());
    logger.info("*-------------------------------------*");

    if (jobExecution.getStatus().isUnsuccessful()) {
      logger.error("Authorization Cleanup Job failed with status: {}",
          jobExecution.getStatus());
    } else {
      logger.info("Authorization Cleanup Job completed successfully");
    }
  }
}
