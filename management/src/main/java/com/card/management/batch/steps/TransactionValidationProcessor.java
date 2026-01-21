package com.card.management.batch.steps;

import java.math.BigDecimal;
import java.time.LocalDateTime;

import org.springframework.batch.item.ItemProcessor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.card.management.Models.Account;
import com.card.management.Models.CardXrefRecord;
import com.card.management.Models.DailyTransaction;
import com.card.management.Models.TransactionProcessingResult;
import com.card.management.Models.TransactionRecord;
import com.card.management.Repositories.AccountRepository;
import com.card.management.Repositories.CardXrefRecordRepository;

@Component
public class TransactionValidationProcessor implements ItemProcessor<DailyTransaction, TransactionProcessingResult> {
  @Autowired
  private CardXrefRecordRepository cardXrefRepository;

  @Autowired
  private AccountRepository accountRepository;

  @Override
  public TransactionProcessingResult process(DailyTransaction dailyTransaction) throws Exception {
    TransactionProcessingResult result = new TransactionProcessingResult();
    result.setDailyTransaction(dailyTransaction);

    // Equivalente a 1500-VALIDATE-TRAN
    boolean isValid = validateTransaction(dailyTransaction, result);

    if (isValid) {
      // Equivalente a 2000-POST-TRANSACTION - mapear a TransactionRecord
      TransactionRecord transactionRecord = mapToTransactionRecord(dailyTransaction);
      result.setTransactionRecord(transactionRecord);
      result.setRejected(false);
    } else {
      // Ya se configuraron los datos de rechazo en validateTransaction
      result.setRejected(true);
    }

    return result;
  }

  /**
   * Valida la transacción contra archivos de referencia
   * Equivalente a 1500-VALIDATE-TRAN, 1500-A-LOOKUP-XREF, 1500-B-LOOKUP-ACCT
   */
  private boolean validateTransaction(DailyTransaction transaction, TransactionProcessingResult result) {
    // Equivalente a 1500-A-LOOKUP-XREF
    CardXrefRecord cardXref = cardXrefRepository.findByCardNumber(transaction.getCardNumber()).orElse(null);
    if (cardXref == null) {
      result.setRejectReason(100);
      result.setRejectDescription("INVALID CARD NUMBER FOUND");
      return false;
    }
    result.setCardXref(cardXref);

    // Equivalente a 1500-B-LOOKUP-ACCT
    Account account = accountRepository.findById(cardXref.getAccountId()).orElse(null);
    if (account == null) {
      result.setRejectReason(101);
      result.setRejectDescription("ACCOUNT RECORD NOT FOUND");
      return false;
    }
    result.setAccount(account);

    // Validación de límite de crédito
    // COMPUTE WS-TEMP-BAL = ACCT-CURR-CYC-CREDIT - ACCT-CURR-CYC-DEBIT +
    // DALYTRAN-AMT
    BigDecimal tempBalance = account.getCurrentCycleCredit()
        .subtract(account.getCurrentCycleDebit())
        .add(transaction.getAmount());

    if (account.getCreditLimit().compareTo(tempBalance) < 0) {
      result.setRejectReason(102);
      result.setRejectDescription("OVERLIMIT TRANSACTION");
      return false;
    }

    // Validación de fecha de expiración
    if (account.getExpirationDate().isBefore(transaction.getOriginalTimestamp().toLocalDate())) {
      result.setRejectReason(103);
      result.setRejectDescription("TRANSACTION RECEIVED AFTER ACCT EXPIRATION");
      return false;
    }

    return true;
  }

  /**
   * Mapea DailyTransaction a TransactionRecord
   * Equivalente a las asignaciones MOVE en 2000-POST-TRANSACTION
   */
  private TransactionRecord mapToTransactionRecord(DailyTransaction dailyTran) {
    return TransactionRecord.builder()
        .transactionId(dailyTran.getTransactionId()) // MOVE DALYTRAN-ID TO TRAN-ID
        .transactionTypeCode(dailyTran.getTypeCode()) // MOVE DALYTRAN-TYPE-CD TO TRAN-TYPE-CD
        .transactionCategoryCode(dailyTran.getCategoryCode()) // MOVE DALYTRAN-CAT-CD TO TRAN-CAT-CD (Integer)
        .transactionSource(dailyTran.getSource()) // MOVE DALYTRAN-SOURCE TO TRAN-SOURCE
        .transactionDescription(dailyTran.getDescription()) // MOVE DALYTRAN-DESC TO TRAN-DESC
        .transactionAmount(dailyTran.getAmount()) // MOVE DALYTRAN-AMT TO TRAN-AMT
        .merchantId(dailyTran.getMerchantId()) // MOVE DALYTRAN-MERCHANT-ID TO TRAN-MERCHANT-ID
        .merchantName(dailyTran.getMerchantName()) // MOVE DALYTRAN-MERCHANT-NAME TO TRAN-MERCHANT-NAME
        .merchantCity(dailyTran.getMerchantCity()) // MOVE DALYTRAN-MERCHANT-CITY TO TRAN-MERCHANT-CITY
        .merchantZipCode(dailyTran.getMerchantZip()) // MOVE DALYTRAN-MERCHANT-ZIP TO TRAN-MERCHANT-ZIP
        .cardNumber(dailyTran.getCardNumber()) // MOVE DALYTRAN-CARD-NUM TO TRAN-CARD-NUM
        .originalTimestamp(dailyTran.getOriginalTimestamp()) // MOVE DALYTRAN-ORIG-TS TO TRAN-ORIG-TS
        .processedTimestamp(LocalDateTime.now()) // Equivalente a Z-GET-DB2-FORMAT-TIMESTAMP
        .build();
  }
}
