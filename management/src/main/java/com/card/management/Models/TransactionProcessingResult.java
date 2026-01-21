package com.card.management.Models;

import lombok.Data;

@Data
public class TransactionProcessingResult {
  private DailyTransaction dailyTransaction; // Tu entidad de entrada
  private TransactionRecord transactionRecord; // Tu entidad de salida
  private boolean isRejected;
  private Integer rejectReason;
  private String rejectDescription;

  // Datos de validación (para uso interno)
  private CardXrefRecord cardXref;
  private Account account;
}
