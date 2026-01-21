package com.card.management.DTOs;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;
import java.math.BigDecimal;
import java.util.List;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class TransactionListResponseDto {
  private List<TransactionItem> transactions;
  private Integer currentPage;
  private Boolean hasNextPage;
  private Boolean hasPreviousPage;
  private String firstTransactionId;
  private String lastTransactionId;
  private String message;
  private String errorMessage;

  @Data
  @NoArgsConstructor
  @AllArgsConstructor
  public static class TransactionItem {
    private String transactionId;
    private String date; // Formato MM/DD/YY como en COBOL
    private String description;
    private BigDecimal amount;
  }
}
