package com.card.management.Models;

import jakarta.persistence.*;
import lombok.Data;

import java.math.BigDecimal;
import java.util.List;

@Entity
@Data
@Table(name = "PENDING_AUTH_SUMMARY")
public class PendingAuthSummary {
  @Id
  @Column(name = "ACCT_ID")
  private Long accountId;

  @Column(name = "APPROVED_AUTH_CNT")
  private Integer approvedAuthCount = 0;

  @Column(name = "APPROVED_AUTH_AMT")
  private BigDecimal approvedAuthAmount = BigDecimal.ZERO;

  @Column(name = "DECLINED_AUTH_CNT")
  private Integer declinedAuthCount = 0;

  @Column(name = "DECLINED_AUTH_AMT")
  private BigDecimal declinedAuthAmount = BigDecimal.ZERO;

  @OneToMany(mappedBy = "summary", cascade = CascadeType.ALL, fetch = FetchType.LAZY)
  private List<PendingAuthDetail> details;

}
