package com.card.management.batch.steps;

import org.springframework.batch.item.Chunk;
import org.springframework.batch.item.ItemWriter;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import com.card.management.Models.InterestCalculationResult;
import com.card.management.Models.TransactionRecord;
import com.card.management.Repositories.TransactionRecordRepository;

import lombok.extern.slf4j.Slf4j;
import java.time.LocalDateTime;
import java.util.List;

/**
 * Writer que genera transacciones de interés
 * Equivalente a: 1300-B-WRITE-TX en CBACT04C
 */
@Component
@Slf4j
public class InterestTransactionWriter implements ItemWriter<InterestCalculationResult> {
  @Autowired
  private TransactionRecordRepository transactionRepository;

  @Override
  public void write(Chunk<? extends InterestCalculationResult> chunk) throws Exception {

    List<? extends InterestCalculationResult> items = chunk.getItems();

    for (InterestCalculationResult result : items) {
      if (result != null) {
        writeInterestTransaction(result);
      }
    }
  }

  /**
   * Escribe una transacción de interés
   * Equivalente a la lógica en 1300-B-WRITE-TX
   */
  private void writeInterestTransaction(InterestCalculationResult result) {

    TransactionRecord transaction = new TransactionRecord();

    // Campos equivalentes a los MOVE en COBOL
    transaction.setTransactionId(result.getTransactionId());
    transaction.setTransactionTypeCode("01"); // MOVE '01' TO TRAN-TYPE-CD
    transaction.setTransactionCategoryCode(5); // MOVE '05' TO TRAN-CAT-CD (Integer)
    transaction.setTransactionSource("System"); // MOVE 'System' TO TRAN-SOURCE

    // STRING 'Int. for a/c ', ACCT-ID DELIMITED BY SIZE INTO TRAN-DESC
    transaction.setTransactionDescription("Int. for a/c " + result.getAccountId());

    transaction.setTransactionAmount(result.getInterestAmount()); // MOVE WS-MONTHLY-INT TO TRAN-AMT
    transaction.setMerchantId(0L); // MOVE 0 TO TRAN-MERCHANT-ID
    transaction.setMerchantName(""); // MOVE SPACES TO TRAN-MERCHANT-NAME
    transaction.setMerchantCity(""); // MOVE SPACES TO TRAN-MERCHANT-CITY
    transaction.setMerchantZipCode(""); // MOVE SPACES TO TRAN-MERCHANT-ZIP
    transaction.setCardNumber(result.getCardNumber()); // MOVE XREF-CARD-NUM TO TRAN-CARD-NUM

    // Equivalente a: PERFORM Z-GET-DB2-FORMAT-TIMESTAMP
    LocalDateTime timestamp = LocalDateTime.now();

    transaction.setOriginalTimestamp(timestamp); // MOVE DB2-FORMAT-TS TO TRAN-ORIG-TS
    transaction.setProcessedTimestamp(timestamp); // MOVE DB2-FORMAT-TS TO TRAN-PROC-TS

    try {
      transactionRepository.save(transaction);
      log.debug("Interest transaction written: {}", transaction.getTransactionId());
    } catch (Exception e) {
      log.error("ERROR WRITING TRANSACTION RECORD: {}", e.getMessage());
      throw new RuntimeException("Error writing transaction record", e);
    }
  }
}
