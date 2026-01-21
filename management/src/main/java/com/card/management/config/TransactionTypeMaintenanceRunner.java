package com.card.management.config;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.JobParameters;
import org.springframework.batch.core.JobParametersBuilder;
import org.springframework.batch.core.launch.JobLauncher;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.boot.CommandLineRunner;
import org.springframework.stereotype.Component;

@Component
public class TransactionTypeMaintenanceRunner implements CommandLineRunner {

  @Autowired
  private JobLauncher jobLauncher;

  @Autowired
  @Qualifier("transactionTypeMaintenanceJob")
  private Job transactionTypeMaintenanceJob;

  @Override
  public void run(String... args) throws Exception {
    if (args.length > 0 && "transactionTypeMaintenance".equals(args[0])) {
      String inputFile = args.length > 1 ? args[1] : "input/transaction_type_operations.txt";

      JobParameters jobParameters = new JobParametersBuilder()
          .addString("inputFile", inputFile)
          .addLong("timestamp", System.currentTimeMillis())
          .toJobParameters();

      jobLauncher.run(transactionTypeMaintenanceJob, jobParameters);
    }
  }
}
