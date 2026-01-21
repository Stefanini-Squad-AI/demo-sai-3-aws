package com.card.management.batch.steps;

import com.card.management.DTOs.TransactionTypeOperationDto;
import com.card.management.Models.TransactionType;
import com.card.management.Repositories.TransactionTypeRepository;
import org.springframework.batch.item.ItemProcessor;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

@Component
public class TransactionTypeMaintenanceProcessor
    implements ItemProcessor<TransactionTypeOperationDto, TransactionTypeOperationDto> {

  @Autowired
  private TransactionTypeRepository transactionTypeRepository;

  @Override
  public TransactionTypeOperationDto process(TransactionTypeOperationDto item) throws Exception {

    System.out.println("PROCESSING   " + item.getOperationType() +
        item.getTrType() + item.getTrDescription());

    // Equivalente al EVALUATE INPUT-REC-TYPE en COBOL
    switch (item.getOperationType()) {
      case "A":
        System.out.println("ADDING RECORD");
        processInsert(item);
        break;
      case "U":
        System.out.println("UPDATING RECORD");
        processUpdate(item);
        break;
      case "D":
        System.out.println("DELETING RECORD");
        processDelete(item);
        break;
      case "*":
        System.out.println("IGNORING COMMENTED LINE");
        break;
      default:
        throw new RuntimeException("ERROR: TYPE NOT VALID - " + item.getOperationType());
    }

    return item;
  }

  // Equivalente a 10031-INSERT-DB
  private void processInsert(TransactionTypeOperationDto item) {
    try {
      TransactionType entity = new TransactionType(item.getTrType(), item.getTrDescription());
      transactionTypeRepository.save(entity);
      System.out.println("RECORD INSERTED SUCCESSFULLY");
    } catch (Exception e) {
      throw new RuntimeException("Error accessing: TRANSACTION_TYPE table. Error: " + e.getMessage());
    }
  }

  // Equivalente a 10032-UPDATE-DB
  private void processUpdate(TransactionTypeOperationDto item) {
    try {
      TransactionType existing = transactionTypeRepository.findById(item.getTrType())
          .orElseThrow(() -> new RuntimeException("No records found."));

      existing.setTransactionTypeDescription(item.getTrDescription());
      transactionTypeRepository.save(existing);
      System.out.println("RECORD UPDATED SUCCESSFULLY");
    } catch (RuntimeException e) {
      throw e;
    } catch (Exception e) {
      throw new RuntimeException("Error accessing: TRANSACTION_TYPE table. Error: " + e.getMessage());
    }
  }

  // Equivalente a 10033-DELETE-DB
  private void processDelete(TransactionTypeOperationDto item) {
    try {
      if (!transactionTypeRepository.existsById(item.getTrType())) {
        throw new RuntimeException("No records found.");
      }

      transactionTypeRepository.deleteById(item.getTrType());
      System.out.println("RECORD DELETED SUCCESSFULLY");
    } catch (RuntimeException e) {
      throw e;
    } catch (Exception e) {
      throw new RuntimeException("Error accessing: TRANSACTION_TYPE table. Error: " + e.getMessage());
    }
  }
}
