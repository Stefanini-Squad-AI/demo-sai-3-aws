package com.card.management.batch.steps;

import java.math.BigDecimal;
import java.time.LocalDateTime;

import org.springframework.batch.item.Chunk;
import org.springframework.batch.item.ItemWriter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.card.management.Models.Account;
import com.card.management.Models.DailyTransaction;
import com.card.management.Models.RejectedTransaction;
import com.card.management.Models.TransactionCategoryBalance;
import com.card.management.Models.TransactionProcessingResult;
import com.card.management.Repositories.AccountRepository;
import com.card.management.Repositories.DailyTransactionRepository;
import com.card.management.Repositories.RejectedTransactionRepository;
import com.card.management.Repositories.TransactionRecordRepository;
import com.card.management.Repositories.TransactionCategoryBalanceRepository;

@Component
public class TransactionWriter implements ItemWriter<TransactionProcessingResult> {
  @Autowired
  private TransactionRecordRepository transactionRecordRepository;

  @Autowired
  private RejectedTransactionRepository rejectedTransactionRepository;

  @Autowired
  private AccountRepository accountRepository;

  @Autowired
  private TransactionCategoryBalanceRepository tcatBalanceRepository;

  @Autowired
  private DailyTransactionRepository dailyTransactionRepository;

  @Override
  public void write(Chunk<? extends TransactionProcessingResult> chunk) throws Exception {
    for (TransactionProcessingResult result : chunk) {
      if (result.isRejected()) {
        writeRejectedTransaction(result);
      } else {
        writeValidTransaction(result);
      }

      // Marcar la transacción diaria como procesada
      markDailyTransactionAsProcessed(result.getDailyTransaction());
    }
  }

  /**
   * Escribe transacción rechazada
   * Equivalente a 2500-WRITE-REJECT-REC
   */
  private void writeRejectedTransaction(TransactionProcessingResult result) {
    // Convertir DailyTransaction a String (equivalente a REJECT-TRAN-DATA)
    String transactionData = formatDailyTransactionAsString(result.getDailyTransaction());

    RejectedTransaction rejected = RejectedTransaction.builder()
        .transactionData(transactionData) // MOVE DALYTRAN-RECORD TO REJECT-TRAN-DATA
        .validationFailureReason(result.getRejectReason()) // WS-VALIDATION-FAIL-REASON
        .validationFailureDescription(result.getRejectDescription()) // WS-VALIDATION-FAIL-REASON-DESC
        .rejectedTimestamp(LocalDateTime.now())
        .originalTransactionId(result.getDailyTransaction().getTransactionId())
        .build();

    rejectedTransactionRepository.save(rejected);
  }

  /**
   * Convierte DailyTransaction a formato String para almacenar en
   * TRANSACTION_DATA
   * Equivalente a como COBOL almacena DALYTRAN-RECORD completo
   */
  private String formatDailyTransactionAsString(com.card.management.Models.DailyTransaction dailyTran) {
    // Formato similar al registro COBOL original (350 caracteres)
    return String.format("%-16s%-2s%04d%-10s%-100s%011.2f%09d%-50s%-50s%-10s%-16s%-26s",
        dailyTran.getTransactionId() != null ? dailyTran.getTransactionId() : "",
        dailyTran.getTypeCode() != null ? dailyTran.getTypeCode() : "",
        dailyTran.getCategoryCode() != null ? dailyTran.getCategoryCode() : 0,
        dailyTran.getSource() != null ? dailyTran.getSource() : "",
        dailyTran.getDescription() != null ? dailyTran.getDescription() : "",
        dailyTran.getAmount() != null ? dailyTran.getAmount().doubleValue() : 0.0,
        dailyTran.getMerchantId() != null ? dailyTran.getMerchantId() : 0L,
        dailyTran.getMerchantName() != null ? dailyTran.getMerchantName() : "",
        dailyTran.getMerchantCity() != null ? dailyTran.getMerchantCity() : "",
        dailyTran.getMerchantZip() != null ? dailyTran.getMerchantZip() : "",
        dailyTran.getCardNumber() != null ? dailyTran.getCardNumber() : "",
        dailyTran.getOriginalTimestamp() != null ? dailyTran.getOriginalTimestamp().toString() : "");
  }

  /**
   * Procesa transacción válida
   * Equivalente a 2700-UPDATE-TCATBAL, 2800-UPDATE-ACCOUNT-REC,
   * 2900-WRITE-TRANSACTION-FILE
   */
  private void writeValidTransaction(TransactionProcessingResult result) {
    // Equivalente a 2700-UPDATE-TCATBAL
    updateTransactionCategoryBalance(result);

    // Equivalente a 2800-UPDATE-ACCOUNT-REC
    updateAccountRecord(result);

    // Equivalente a 2900-WRITE-TRANSACTION-FILE
    transactionRecordRepository.save(result.getTransactionRecord());
  }

  /**
   * Actualiza balance de categoría de transacción
   * Equivalente a 2700-UPDATE-TCATBAL
   */
  private void updateTransactionCategoryBalance(TransactionProcessingResult result) {
    Long accountId = result.getAccount().getAccountId();
    String typeCode = result.getDailyTransaction().getTypeCode();
    Integer categoryCode = result.getDailyTransaction().getCategoryCode();

    // Buscar registro existente o crear nuevo (equivalente a INVALID KEY logic)
    TransactionCategoryBalance tcatBalance = tcatBalanceRepository
        .findByAccountIdAndTypeCodeAndCategoryCode(accountId, typeCode, categoryCode)
        .orElse(TransactionCategoryBalance.builder()
            .accountId(accountId)
            .typeCode(typeCode)
            .categoryCode(categoryCode)
            .categoryBalance(BigDecimal.ZERO)
            .build());

    // Equivalente a ADD DALYTRAN-AMT TO TRAN-CAT-BAL
    tcatBalance.setCategoryBalance(tcatBalance.getCategoryBalance().add(result.getDailyTransaction().getAmount()));

    tcatBalanceRepository.save(tcatBalance);
  }

  /**
   * Actualiza registro de cuenta
   * Equivalente a 2800-UPDATE-ACCOUNT-REC
   */
  private void updateAccountRecord(TransactionProcessingResult result) {
    Account account = result.getAccount();
    BigDecimal amount = result.getDailyTransaction().getAmount();

    // ADD DALYTRAN-AMT TO ACCT-CURR-BAL
    account.setCurrentBalance(account.getCurrentBalance().add(amount));

    // IF DALYTRAN-AMT >= 0 ADD DALYTRAN-AMT TO ACCT-CURR-CYC-CREDIT
    // ELSE ADD DALYTRAN-AMT TO ACCT-CURR-CYC-DEBIT
    if (amount.compareTo(BigDecimal.ZERO) >= 0) {
      account.setCurrentCycleCredit(account.getCurrentCycleCredit().add(amount));
    } else {
      account.setCurrentCycleDebit(account.getCurrentCycleDebit().add(amount));
    }

    accountRepository.save(account);
  }

  /**
   * Marca la transacción diaria como procesada
   */
  private void markDailyTransactionAsProcessed(DailyTransaction dailyTransaction) {
    dailyTransaction.setProcessedTimestamp(LocalDateTime.now());
    dailyTransactionRepository.save(dailyTransaction);
  }
}
