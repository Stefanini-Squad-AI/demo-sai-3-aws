package com.card.management.Models;

import java.math.BigDecimal;

import lombok.Builder;
import lombok.Data;

@Builder
@Data
public class InterestCalculationResult {
  private Long accountId;
  private String transactionId;
  private BigDecimal interestAmount;
  private BigDecimal totalInterest;
  private String cardNumber;
  private Account account;
}
