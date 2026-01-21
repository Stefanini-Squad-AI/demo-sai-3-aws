package com.card.management.batch.steps;

import java.time.LocalDate;
import java.time.LocalDateTime;

import org.springframework.batch.core.configuration.annotation.StepScope;
import org.springframework.batch.item.ItemProcessor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import com.card.management.DTOs.TransactionReportDetailDto;
import com.card.management.Models.CardXrefRecord;
import com.card.management.Models.DailyTransaction;
import com.card.management.Models.TransactionCategory;
import com.card.management.Models.TransactionType;
import com.card.management.Repositories.CardXrefRecordRepository;
import com.card.management.Repositories.TransactionCategoryRepository;
import com.card.management.Repositories.TransactionTypeRepository;

import org.springframework.lang.NonNull;
import lombok.extern.slf4j.Slf4j;

/**
 * Processor para el reporte de transacciones
 * Equivalente a las rutinas 1500-A, 1500-B, 1500-C de lookup en CBTRN03C
 * Filtra transacciones por rango de fechas y enriquece con datos relacionados
 */
@Component
@StepScope
@Slf4j
public class TransactionReportProcessor implements ItemProcessor<DailyTransaction, TransactionReportDetailDto> {
  @Autowired
  private CardXrefRecordRepository cardXrefRepository;

  @Autowired
  private TransactionTypeRepository transactionTypeRepository;

  @Autowired
  private TransactionCategoryRepository transactionCategoryRepository;
  
  @Value("#{jobParameters['startDate']}")
  private String startDateStr;
  
  @Value("#{jobParameters['endDate']}")
  private String endDateStr;

  @Override
  public TransactionReportDetailDto process(@NonNull DailyTransaction transaction) throws Exception {
    log.debug("Processing transaction: {}", transaction.getTransactionId());
    
    // Filtrar por rango de fechas
    if (startDateStr != null && endDateStr != null) {
      LocalDate startDate = LocalDate.parse(startDateStr);
      LocalDate endDate = LocalDate.parse(endDateStr);
      LocalDateTime processedTime = transaction.getProcessedTimestamp();
      
      if (processedTime != null) {
        LocalDate transactionDate = processedTime.toLocalDate();
        if (transactionDate.isBefore(startDate) || transactionDate.isAfter(endDate)) {
          // Filtrar transacciones fuera del rango
          log.debug("Filtering out transaction {} - date {} not in range [{}, {}]", 
              transaction.getTransactionId(), transactionDate, startDate, endDate);
          return null;
        }
      }
    }

    try {
      // Lookup XREF - equivalente a 1500-A-LOOKUP-XREF
      CardXrefRecord cardXref = cardXrefRepository.findByCardNumber(transaction.getCardNumber())
          .orElseThrow(() -> new RuntimeException("INVALID CARD NUMBER: " + transaction.getCardNumber()));

      // Lookup Transaction Type - equivalente a 1500-B-LOOKUP-TRANTYPE
      TransactionType transactionType = transactionTypeRepository.findByTransactionTypeCode(transaction.getTypeCode())
          .orElseThrow(() -> new RuntimeException("INVALID TRANSACTION TYPE: " + transaction.getTypeCode()));

      // Lookup Transaction Category - equivalente a 1500-C-LOOKUP-TRANCATG
      TransactionCategory transactionCategory = transactionCategoryRepository
          .findByTransactionTypeCodeAndCategoryCode(transaction.getTypeCode(), transaction.getCategoryCode())
          .orElseThrow(() -> new RuntimeException("INVALID TRAN CATG KEY: " +
              transaction.getTypeCode() + transaction.getCategoryCode()));

      // Construir el detalle del reporte - equivalente a 1120-WRITE-DETAIL
      return TransactionReportDetailDto.builder()
          .transactionId(transaction.getTransactionId())
          .accountId(cardXref.getAccountId())
          .cardNumber(transaction.getCardNumber())
          .typeCode(transaction.getTypeCode())
          .typeDescription(transactionType.getTransactionTypeDescription())
          .categoryCode(transaction.getCategoryCode())
          .categoryDescription(transactionCategory.getTransactionCategoryDescription())
          .source(transaction.getSource())
          .amount(transaction.getAmount())
          .processedTimestamp(transaction.getProcessedTimestamp())
          .build();

    } catch (Exception e) {
      log.error("Error processing transaction {}: {}", transaction.getTransactionId(), e.getMessage());
      throw e;
    }
  }
}
