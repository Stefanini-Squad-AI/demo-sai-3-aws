package com.card.management.DTOs;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class TransactionAddResponseDto {
  private String transactionId;
  private String message;
  private boolean success;

  public static TransactionAddResponseDto success(String transactionId) {
    return new TransactionAddResponseDto(
        transactionId,
        "Transaction added successfully. Your Tran ID is " + transactionId + ".",
        true);
  }

  public static TransactionAddResponseDto error(String message) {
    return new TransactionAddResponseDto(null, message, false);
  }
}
