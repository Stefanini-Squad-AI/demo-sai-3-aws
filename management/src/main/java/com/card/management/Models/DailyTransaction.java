package com.card.management.Models;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * Entidad JPA que representa una transacción diaria (DALYTRAN)
 * Migrada desde estructura COBOL DALYTRAN-RECORD (RECLN = 350)
 * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:16:01 CDT
 */
@Entity
@Table(name = "DAILY_TRANSACTIONS")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class DailyTransaction {
  @Id
  @Column(name = "TRANSACTION_ID", length = 16, nullable = false)
  private String transactionId; // DALYTRAN-ID PIC X(16)

  @Column(name = "TYPE_CODE", length = 2, nullable = false)
  private String typeCode; // DALYTRAN-TYPE-CD PIC X(02)

  @Column(name = "CATEGORY_CODE", nullable = false)
  private Integer categoryCode; // DALYTRAN-CAT-CD PIC 9(04)

  @Column(name = "SOURCE", length = 10)
  private String source; // DALYTRAN-SOURCE PIC X(10)

  @Column(name = "DESCRIPTION", length = 100)
  private String description; // DALYTRAN-DESC PIC X(100)

  @Column(name = "AMOUNT", precision = 11, scale = 2)
  private BigDecimal amount; // DALYTRAN-AMT PIC S9(09)V99 (signed decimal)

  @Column(name = "MERCHANT_ID")
  private Long merchantId; // DALYTRAN-MERCHANT-ID PIC 9(09)

  @Column(name = "MERCHANT_NAME", length = 50)
  private String merchantName; // DALYTRAN-MERCHANT-NAME PIC X(50)

  @Column(name = "MERCHANT_CITY", length = 50)
  private String merchantCity; // DALYTRAN-MERCHANT-CITY PIC X(50)

  @Column(name = "MERCHANT_ZIP", length = 10)
  private String merchantZip; // DALYTRAN-MERCHANT-ZIP PIC X(10)

  @Column(name = "CARD_NUMBER", length = 16)
  private String cardNumber; // DALYTRAN-CARD-NUM PIC X(16)

  @Column(name = "ORIGINAL_TIMESTAMP")
  private LocalDateTime originalTimestamp; // DALYTRAN-ORIG-TS PIC X(26)

  @Column(name = "PROCESSED_TIMESTAMP")
  private LocalDateTime processedTimestamp; // DALYTRAN-PROC-TS PIC X(26)

  // FILLER PIC X(20) - No se migra ya que es espacio reservado sin uso

  // Relación N:1 con Card - Múltiples transacciones diarias pueden pertenecer a una card
  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = "CARD_NUMBER", insertable = false, updatable = false)
  private Card card;
}
