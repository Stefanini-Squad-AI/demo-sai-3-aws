package com.card.management.batch;

import java.util.Map;

import org.springframework.batch.core.Job;
import org.springframework.batch.core.Step;
import org.springframework.batch.core.configuration.annotation.StepScope;
import org.springframework.batch.core.job.builder.JobBuilder;
import org.springframework.batch.core.repository.JobRepository;
import org.springframework.batch.core.step.builder.StepBuilder;
import org.springframework.batch.item.data.RepositoryItemReader;
import org.springframework.batch.item.data.builder.RepositoryItemReaderBuilder;
import org.springframework.batch.item.file.FlatFileItemReader;
import org.springframework.batch.item.file.LineMapper;
import org.springframework.batch.item.file.builder.FlatFileItemReaderBuilder;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.core.io.FileSystemResource;
import org.springframework.data.domain.Sort;
import org.springframework.transaction.PlatformTransactionManager;
import org.springframework.beans.factory.annotation.Value;

import com.card.management.batch.jobs.AuthCleanupJobListener;
import com.card.management.batch.jobs.InterestCalculationJobListener;
import com.card.management.batch.jobs.StatementGenerationJobListener;
import com.card.management.batch.jobs.TransactionPostingJobListener;
import com.card.management.batch.jobs.TransactionReportJobListener;
import com.card.management.batch.jobs.TransactionTypeMaintenanceJobListener;
import com.card.management.batch.steps.AuthCleanupProcessor;
import com.card.management.batch.steps.AuthCleanupWriter;
import com.card.management.batch.steps.InterestCalculationProcessor;
import com.card.management.batch.steps.InterestTransactionWriter;
import com.card.management.batch.steps.StatementGenerationProcessor;
import com.card.management.batch.steps.StatementWriter;
import com.card.management.batch.steps.TransactionReportProcessor;
import com.card.management.batch.steps.TransactionReportWriter;
import com.card.management.batch.steps.TransactionTypeMaintenanceProcessor;
import com.card.management.batch.steps.TransactionTypeMaintenanceWriter;
import com.card.management.batch.steps.TransactionValidationProcessor;
import com.card.management.batch.steps.TransactionWriter;
import com.card.management.DTOs.AuthCleanupResultDto;
import com.card.management.DTOs.StatementGenerationResultDto;
import com.card.management.DTOs.TransactionReportDetailDto;
import com.card.management.DTOs.TransactionTypeOperationDto;
import com.card.management.Models.CardXrefRecord;
import com.card.management.Models.DailyTransaction;
import com.card.management.Models.InterestCalculationResult;
import com.card.management.Models.PendingAuthSummary;
import com.card.management.Models.TransactionCategoryBalance;
import com.card.management.Models.TransactionProcessingResult;
import com.card.management.Repositories.CardXrefRecordRepository;
import com.card.management.Repositories.DailyTransactionRepository;
import com.card.management.Repositories.PendingAuthSummaryRepository;
import com.card.management.Repositories.TransactionCategoryBalanceRepository;

@Configuration
public class BatchConfiguration {

  @Autowired
  private TransactionPostingJobListener jobListener;

  @Autowired
  private TransactionValidationProcessor transactionProcessor;

  @Autowired
  private TransactionWriter transactionWriter;

  @Autowired
  private DailyTransactionRepository dailyTransactionRepository;

  // Nuevos componentes para el calculador de intereses
  @Autowired
  private InterestCalculationJobListener interestJobListener;

  @Autowired
  private InterestCalculationProcessor interestProcessor;

  @Autowired
  private InterestTransactionWriter interestTransactionWriter;

  @Autowired
  private TransactionCategoryBalanceRepository tcatBalRepository;

  // Nuevos componentes para generación de statements - CBSTM03A
  @Autowired
  private StatementGenerationJobListener statementJobListener;

  @Autowired
  private StatementGenerationProcessor statementProcessor;

  @Autowired
  private StatementWriter statementWriter;

  @Autowired
  private CardXrefRecordRepository cardXrefRepository;

  // Nuevos componentes para reporte de transacciones - CBTRN03C
  @Autowired
  private TransactionReportJobListener transactionReportJobListener;

  @Autowired
  private TransactionReportProcessor transactionReportProcessor;

  @Autowired
  private TransactionReportWriter transactionReportWriter;
  
  @Autowired
  private com.card.management.batch.jobs.TransactionReportStepListener transactionReportStepListener;

  // Proceso para purga de autorizaciones expiradas
  @Autowired
  private AuthCleanupJobListener authCleanupJobListener;

  @Autowired
  private AuthCleanupProcessor authCleanupProcessor;

  @Autowired
  private AuthCleanupWriter authCleanupWriter;

  @Autowired
  private PendingAuthSummaryRepository pendingAuthSummaryRepository;

  // Mantenimiento de tabla tipos de transacción

  @Autowired
  private TransactionTypeMaintenanceJobListener transactionTypeJobListener;

  @Autowired
  private TransactionTypeMaintenanceProcessor transactionTypeProcessor;

  @Autowired
  private TransactionTypeMaintenanceWriter transactionTypeWriter;

  /**
   * Job principal que procesa las transacciones diarias
   * Equivalente al programa COBOL CBTRN02C
   */
  @Bean
  public Job transactionPostingJob(JobRepository jobRepository, Step processTransactionsStep) {
    return new JobBuilder("transactionPostingJob", jobRepository)
        .listener(jobListener)
        .start(processTransactionsStep)
        .build();
  }

  /**
   * Job calculador de intereses
   * Equivalente al programa COBOL CBACT04C
   */
  @Bean
  public Job interestCalculationJob(JobRepository jobRepository, Step calculateInterestStep) {
    return new JobBuilder("interestCalculationJob", jobRepository)
        .listener(interestJobListener)
        .start(calculateInterestStep)
        .build();
  }

  /**
   * Job generador de statements
   * Equivalente al programa COBOL CBSTM03A
   */
  @Bean
  public Job statementGenerationJob(JobRepository jobRepository, Step generateStatementsStep) {
    return new JobBuilder("statementGenerationJob", jobRepository)
        .listener(statementJobListener)
        .start(generateStatementsStep)
        .build();
  }

  /**
   * Job generador de reporte de transacciones
   * Equivalente al programa COBOL CBTRN03C
   */
  @Bean
  public Job transactionReportJob(JobRepository jobRepository, Step generateTransactionReportStep) {
    return new JobBuilder("transactionReportJob", jobRepository)
        .listener(transactionReportJobListener)
        .start(generateTransactionReportStep)
        .build();
  }

  /**
   * Step que procesa las transacciones usando chunk processing
   * Lee de DAILY_TRANSACTIONS, valida y procesa cada transacción
   */
  @Bean
  public Step processTransactionsStep(JobRepository jobRepository,
      PlatformTransactionManager transactionManager) {
    return new StepBuilder("processTransactionsStep", jobRepository)
        .<DailyTransaction, TransactionProcessingResult>chunk(100, transactionManager)
        .reader(dailyTransactionReader())
        .processor(transactionProcessor)
        .writer(transactionWriter)
        .build();
  }

  /**
   * Step que calcula intereses por cuenta
   * Lee balances de categorías de transacciones, calcula intereses y genera
   * transacciones
   * Equivalente al loop principal en CBACT04C
   */
  @Bean
  public Step calculateInterestStep(JobRepository jobRepository,
      PlatformTransactionManager transactionManager) {
    return new StepBuilder("calculateInterestStep", jobRepository)
        .<TransactionCategoryBalance, InterestCalculationResult>chunk(50, transactionManager)
        .reader(transactionCategoryBalanceReader())
        .processor(interestProcessor)
        .writer(interestTransactionWriter)
        .build();
  }

  /**
   * Step que genera statements por cada tarjeta en XREF
   * Lee de CARD_XREF, obtiene datos relacionados y genera statements en texto y
   * HTML
   * Equivalente al loop principal en CBSTM03A
   */
  @Bean
  public Step generateStatementsStep(JobRepository jobRepository,
      PlatformTransactionManager transactionManager) {
    return new StepBuilder("generateStatementsStep", jobRepository)
        .<CardXrefRecord, StatementGenerationResultDto>chunk(10, transactionManager)
        .reader(cardXrefReader())
        .processor(statementProcessor)
        .writer(statementWriter)
        .build();
  }

  /**
   * Step que genera reporte de transacciones detallado
   * Lee transacciones procesadas en rango de fechas, busca datos relacionados
   * y genera reporte formateado con totales
   * Equivalente al loop principal en CBTRN03C
   */
  @Bean
  public Step generateTransactionReportStep(JobRepository jobRepository,
      PlatformTransactionManager transactionManager) {
    return new StepBuilder("generateTransactionReportStep", jobRepository)
        .<DailyTransaction, TransactionReportDetailDto>chunk(20, transactionManager)
        .reader(transactionReportReader(null, null))
        .processor(transactionReportProcessor)
        .writer(transactionReportWriter)
        .listener(transactionReportStepListener)
        .build();
  }

  /**
   * Reader para transacciones diarias usando tu entidad DailyTransaction
   * Equivalente a la lectura del archivo DALYTRAN-FILE en COBOL
   */
  @Bean
  public RepositoryItemReader<DailyTransaction> dailyTransactionReader() {
    return new RepositoryItemReaderBuilder<DailyTransaction>()
        .name("dailyTransactionReader")
        .repository(dailyTransactionRepository)
        .methodName("findByProcessedTimestampIsNull")
        .pageSize(100)
        .sorts(Map.of("transactionId", Sort.Direction.ASC))
        .build();
  }

  /**
   * Reader para balances de categorías de transacciones
   * Equivalente a la lectura del archivo TCATBAL-FILE en COBOL
   * Ordena por accountId para procesar todas las categorías de una cuenta juntas
   */
  @Bean
  public RepositoryItemReader<TransactionCategoryBalance> transactionCategoryBalanceReader() {
    return new RepositoryItemReaderBuilder<TransactionCategoryBalance>()
        .name("transactionCategoryBalanceReader")
        .repository(tcatBalRepository)
        .methodName("findAll")
        .pageSize(50)
        .sorts(Map.of("accountId", Sort.Direction.ASC, "transactionCategoryCode", Sort.Direction.ASC))
        .build();
  }

  /**
   * Reader para referencias cruzadas de tarjetas
   * Equivalente a la lectura del archivo XREFFILE en COBOL
   * Lee secuencialmente todas las tarjetas para generar sus statements
   */
  @Bean
  public RepositoryItemReader<CardXrefRecord> cardXrefReader() {
    return new RepositoryItemReaderBuilder<CardXrefRecord>()
        .name("cardXrefReader")
        .repository(cardXrefRepository)
        .methodName("findAllByOrderByCardNumber")
        .pageSize(10)
        .sorts(Map.of("cardNumber", Sort.Direction.ASC))
        .build();
  }

  /**
   * Reader para transacciones del reporte
   * Equivalente a la lectura del archivo TRANSACT-FILE filtrado por fechas en
   * COBOL
   * Lee transacciones procesadas ordenadas por número de tarjeta para el reporte
   * 
   * Nota: El filtrado por fechas se hace en el processor, aquí leemos todas las transacciones procesadas
   */
  @Bean
  @StepScope
  public RepositoryItemReader<DailyTransaction> transactionReportReader(
      @Value("#{jobParameters['startDate']}") String startDate,
      @Value("#{jobParameters['endDate']}") String endDate) {
    return new RepositoryItemReaderBuilder<DailyTransaction>()
        .name("transactionReportReader")
        .repository(dailyTransactionRepository)
        .methodName("findByProcessedTimestampIsNotNull")
        .pageSize(20)
        .sorts(Map.of("cardNumber", Sort.Direction.ASC, "processedTimestamp", Sort.Direction.ASC))
        .build();
  }

  /**
   * Job de limpieza de autorizaciones expiradas
   * Equivalente al programa COBOL CBPAUP0C
   */
  @Bean
  public Job authorizationCleanupJob(JobRepository jobRepository, Step cleanupExpiredAuthsStep) {
    return new JobBuilder("authorizationCleanupJob", jobRepository)
        .listener(authCleanupJobListener)
        .start(cleanupExpiredAuthsStep)
        .build();
  }

  /**
   * Step que limpia autorizaciones expiradas
   * Lee summaries de autorizaciones pendientes, identifica detalles expirados
   * y elimina tanto detalles como summaries vacíos
   * Equivalente al loop principal en CBPAUP0C
   */
  @Bean
  public Step cleanupExpiredAuthsStep(JobRepository jobRepository,
      PlatformTransactionManager transactionManager) {
    return new StepBuilder("cleanupExpiredAuthsStep", jobRepository)
        .<PendingAuthSummary, AuthCleanupResultDto>chunk(5, transactionManager) // Chunk pequeño para simular
                                                                                // checkpoints frecuentes
        .reader(pendingAuthSummaryReader())
        .processor(authCleanupProcessor)
        .writer(authCleanupWriter)
        .build();
  }

  /**
   * Reader para summaries de autorizaciones pendientes
   * Equivalente a la lectura del archivo PAUTSUM0 en COBOL
   * Lee secuencialmente todos los summaries ordenados por account ID
   */
  @Bean
  public RepositoryItemReader<PendingAuthSummary> pendingAuthSummaryReader() {
    return new RepositoryItemReaderBuilder<PendingAuthSummary>()
        .name("pendingAuthSummaryReader")
        .repository(pendingAuthSummaryRepository)
        .methodName("findAllOrderByAccountId")
        .pageSize(5) // Tamaño pequeño para simular el comportamiento de checkpoint del COBOL
        .sorts(Map.of("accountId", Sort.Direction.ASC))
        .build();
  }

  /**
   * Job de mantenimiento de tipos de transacción
   * Equivalente al programa COBOL COBTUPDT
   */
  @Bean
  public Job transactionTypeMaintenanceJob(JobRepository jobRepository,
      Step processTransactionTypeMaintenanceStep) {
    return new JobBuilder("transactionTypeMaintenanceJob", jobRepository)
        .listener(transactionTypeJobListener)
        .start(processTransactionTypeMaintenanceStep)
        .build();
  }

  /**
   * Step que procesa operaciones de mantenimiento de tipos de transacción
   * Lee archivo de entrada, procesa operaciones A/U/D y actualiza base de datos
   * Equivalente al loop principal en COBTUPDT
   */
  @Bean
  public Step processTransactionTypeMaintenanceStep(JobRepository jobRepository,
      PlatformTransactionManager transactionManager) {
    return new StepBuilder("processTransactionTypeMaintenanceStep", jobRepository)
        .<TransactionTypeOperationDto, TransactionTypeOperationDto>chunk(1, transactionManager)
        .reader(transactionTypeFileReader(null))
        .processor(transactionTypeProcessor)
        .writer(transactionTypeWriter)
        .build();
  }

  /**
   * Reader para archivo de operaciones de mantenimiento
   * Equivalente a la lectura del archivo TR-RECORD en COBOL
   * Lee líneas del archivo y las convierte en DTOs de operación
   */
  @Bean
  @StepScope
  public FlatFileItemReader<TransactionTypeOperationDto> transactionTypeFileReader(
      @Value("#{jobParameters['inputFile']}") String inputFile) {
    return new FlatFileItemReaderBuilder<TransactionTypeOperationDto>()
        .name("transactionTypeFileReader")
        .resource(new FileSystemResource(inputFile))
        .lineMapper(new LineMapper<TransactionTypeOperationDto>() {
          @Override
          public TransactionTypeOperationDto mapLine(String line, int lineNumber) throws Exception {
            return new TransactionTypeOperationDto(line);
          }
        })
        .build();
  }
}
