package com.card.management.config;

import com.card.management.Services.BatchJobService;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.boot.autoconfigure.condition.ConditionalOnProperty;
import org.springframework.context.annotation.Configuration;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;

/**
 * Scheduler para ejecución automática de batch jobs
 * Puede ser habilitado/deshabilitado mediante application.properties
 * 
 * Para habilitar, agregar en application.properties:
 * batch.scheduler.enabled=true
 */
@Configuration
@EnableScheduling
@ConditionalOnProperty(name = "batch.scheduler.enabled", havingValue = "true", matchIfMissing = false)
@RequiredArgsConstructor
@Slf4j
public class BatchJobScheduler {
  
  private final BatchJobService batchJobService;
  
  /**
   * Ejecuta el procesamiento de transacciones diarias
   * Por defecto: Todos los días a las 2:00 AM
   * Configurable via: batch.scheduler.transaction-posting.cron
   */
  @Scheduled(cron = "${batch.scheduler.transaction-posting.cron:0 0 2 * * *}")
  public void scheduledTransactionPosting() {
    log.info("Iniciando ejecución programada de transactionPostingJob");
    try {
      batchJobService.executeTransactionPostingJob();
      log.info("transactionPostingJob ejecutado exitosamente");
    } catch (Exception e) {
      log.error("Error ejecutando transactionPostingJob programado", e);
    }
  }
  
  /**
   * Ejecuta el cálculo de intereses
   * Por defecto: Todos los días a las 3:00 AM
   * Configurable via: batch.scheduler.interest-calculation.cron
   */
  @Scheduled(cron = "${batch.scheduler.interest-calculation.cron:0 0 3 * * *}")
  public void scheduledInterestCalculation() {
    log.info("Iniciando ejecución programada de interestCalculationJob");
    try {
      batchJobService.executeInterestCalculationJob();
      log.info("interestCalculationJob ejecutado exitosamente");
    } catch (Exception e) {
      log.error("Error ejecutando interestCalculationJob programado", e);
    }
  }
  
  /**
   * Ejecuta la generación de estados de cuenta
   * Por defecto: El día 1 de cada mes a las 4:00 AM
   * Configurable via: batch.scheduler.statement-generation.cron
   */
  @Scheduled(cron = "${batch.scheduler.statement-generation.cron:0 0 4 1 * *}")
  public void scheduledStatementGeneration() {
    log.info("Iniciando ejecución programada de statementGenerationJob");
    try {
      batchJobService.executeStatementGenerationJob();
      log.info("statementGenerationJob ejecutado exitosamente");
    } catch (Exception e) {
      log.error("Error ejecutando statementGenerationJob programado", e);
    }
  }
  
  /**
   * Ejecuta la limpieza de autorizaciones expiradas
   * Por defecto: Todos los días a las 1:00 AM
   * Configurable via: batch.scheduler.auth-cleanup.cron
   */
  @Scheduled(cron = "${batch.scheduler.auth-cleanup.cron:0 0 1 * * *}")
  public void scheduledAuthCleanup() {
    log.info("Iniciando ejecución programada de authorizationCleanupJob");
    try {
      batchJobService.executeAuthCleanupJob();
      log.info("authorizationCleanupJob ejecutado exitosamente");
    } catch (Exception e) {
      log.error("Error ejecutando authorizationCleanupJob programado", e);
    }
  }
}

