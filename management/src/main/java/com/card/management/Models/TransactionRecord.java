package com.card.management.Models;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.math.BigDecimal;
import java.time.LocalDateTime;

/**
 * Entidad JPA que representa un registro de transacción
 * Migrado desde estructura COBOL TRAN-RECORD (RECLN = 350)
 * Ver: CardDemo_v1.0-15-g27d6c6f-68 Date: 2022-07-19 23:16:01 CDT
 */
@Entity
@Table(name = "TRANSACTION_RECORDS")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class TransactionRecord {
  @Id
  @Column(name = "TRAN_ID", length = 16, nullable = false)
  private String transactionId; // TRAN-ID PIC X(16)

  @Column(name = "TRAN_TYPE_CD", length = 2, nullable = false)
  private String transactionTypeCode; // TRAN-TYPE-CD PIC X(02)

  @Column(name = "TRAN_CAT_CD", nullable = false)
  private Integer transactionCategoryCode; // TRAN-CAT-CD PIC 9(04) - Integer to match TransactionCategory

  @Column(name = "TRAN_SOURCE", length = 10)
  private String transactionSource; // TRAN-SOURCE PIC X(10)

  @Column(name = "TRAN_DESC", length = 100)
  private String transactionDescription; // TRAN-DESC PIC X(100)

  @Column(name = "TRAN_AMT", precision = 11, scale = 2)
  private BigDecimal transactionAmount; // TRAN-AMT PIC S9(09)V99 - signed decimal with 2 decimal places

  @Column(name = "TRAN_MERCHANT_ID", precision = 9)
  private Long merchantId; // TRAN-MERCHANT-ID PIC 9(09)

  @Column(name = "TRAN_MERCHANT_NAME", length = 50)
  private String merchantName; // TRAN-MERCHANT-NAME PIC X(50)

  @Column(name = "TRAN_MERCHANT_CITY", length = 50)
  private String merchantCity; // TRAN-MERCHANT-CITY PIC X(50)

  @Column(name = "TRAN_MERCHANT_ZIP", length = 10)
  private String merchantZipCode; // TRAN-MERCHANT-ZIP PIC X(10)

  @Column(name = "TRAN_CARD_NUM", length = 16)
  private String cardNumber; // TRAN-CARD-NUM PIC X(16)

  @Column(name = "TRAN_ORIG_TS")
  private LocalDateTime originalTimestamp; // TRAN-ORIG-TS PIC X(26) - converted to LocalDateTime

  @Column(name = "TRAN_PROC_TS")
  private LocalDateTime processedTimestamp; // TRAN-PROC-TS PIC X(26) - converted to LocalDateTime

  // FILLER PIC X(20) - No se migra ya que es espacio reservado sin uso específico

  /**
   * Método de conveniencia para formatear el monto de la transacción
   */
  public String getFormattedAmount() {
    return transactionAmount != null ? String.format("$%,.2f", transactionAmount) : "$0.00";
  }

  /**
   * Método de conveniencia para obtener el número de tarjeta enmascarado
   */
  public String getMaskedCardNumber() {
    if (cardNumber == null || cardNumber.length() < 4) {
      return "****";
    }
    return "**** **** **** " + cardNumber.substring(cardNumber.length() - 4);
  }

  // Relación N:1 con Card - Múltiples transacciones pueden pertenecer a una card
  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = "TRAN_CARD_NUM", referencedColumnName = "CARD_NUM", insertable = false, updatable = false)
  private Card card;

  // Relación N:1 con TransactionType - Múltiples transacciones pueden ser del mismo tipo
  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = "TRAN_TYPE_CD", referencedColumnName = "TRAN_TYPE", insertable = false, updatable = false)
  private TransactionType transactionType;

  // Relación N:1 con TransactionCategory - Múltiples transacciones pueden ser de la misma categoría
  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumns({
    @JoinColumn(name = "TRAN_TYPE_CD", referencedColumnName = "TRAN_TYPE_CD", insertable = false, updatable = false),
    @JoinColumn(name = "TRAN_CAT_CD", referencedColumnName = "TRAN_CAT_CD", insertable = false, updatable = false)
  })
  private TransactionCategory transactionCategory;
}
