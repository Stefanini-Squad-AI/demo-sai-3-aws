package com.card.management.DTOs;

import java.math.BigDecimal;

import lombok.Data;

@Data
public class AuthCleanupResultDto {
  private Long accountId;
  private int detailsDeleted;
  private boolean summaryDeleted;
  private BigDecimal approvedAmountAdjusted;
  private BigDecimal declinedAmountAdjusted;
  private int approvedCountAdjusted;
  private int declinedCountAdjusted;

  public AuthCleanupResultDto(Long accountId) {
    this.accountId = accountId;
    this.detailsDeleted = 0;
    this.summaryDeleted = false;
    this.approvedAmountAdjusted = BigDecimal.ZERO;
    this.declinedAmountAdjusted = BigDecimal.ZERO;
    this.approvedCountAdjusted = 0;
    this.declinedCountAdjusted = 0;
  }
}
