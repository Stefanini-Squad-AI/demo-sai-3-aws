package com.card.management.DTOs;

import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.AllArgsConstructor;
import java.math.BigDecimal;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class BillPaymentResponseDto {
  private Long accountId;
  private BigDecimal currentBalance;
  private BigDecimal paymentAmount;
  private Long transactionId;
  private String message;
  private boolean success;
  private String errorMessage;
}
