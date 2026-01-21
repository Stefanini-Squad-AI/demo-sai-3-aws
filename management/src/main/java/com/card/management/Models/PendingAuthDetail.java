package com.card.management.Models;

import jakarta.persistence.*;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDate;

@Entity
@Data
@Table(name = "PENDING_AUTH_DETAIL")
public class PendingAuthDetail {
  @Id
  @GeneratedValue(strategy = GenerationType.IDENTITY)
  private Long id;

  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = "ACCT_ID")
  private PendingAuthSummary summary;

  @Column(name = "AUTH_DATE")
  private LocalDate authDate;

  @Column(name = "AUTH_RESP_CODE")
  private String authResponseCode;

  @Column(name = "APPROVED_AMT")
  private BigDecimal approvedAmount = BigDecimal.ZERO;

  @Column(name = "TRANSACTION_AMT")
  private BigDecimal transactionAmount = BigDecimal.ZERO;

}
