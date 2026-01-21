package com.card.management.Models;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import lombok.Builder;

import java.math.BigDecimal;

/**
 * Entidad JPA que representa el balance de categoría de transacción
 * Migrada desde COBOL TRAN-CAT-BAL-RECORD
 * Longitud de registro original: 50 bytes
 */
@Entity
@Table(name = "TRANSACTION_CATEGORY_BALANCE")
@Data
@NoArgsConstructor
@AllArgsConstructor
@Builder
@IdClass(TransactionCategoryKey.class)
public class TransactionCategoryBalance {

  /**
   * ID de cuenta - equivalente a TRANCAT-ACCT-ID PIC 9(11)
    * Parte de la clave compuesta
    */
  @Id
  @Column(name = "ACCOUNT_ID", nullable = false, length = 11)
  private Long accountId;

  /**
   * Código de tipo - equivalente a TRANCAT-TYPE-CD PIC X(02)
    * Parte de la clave compuesta
    */
  @Id
  @Column(name = "TYPE_CODE", nullable = false, length = 2)
  private String typeCode;

  /**
   * Código de categoría - equivalente a TRANCAT-CD PIC 9(04)
    * Parte de la clave compuesta - Changed to String to match TransactionCategory
    */
  @Id
  @Column(name = "CATEGORY_CODE", nullable = false, length = 4)
  private Integer categoryCode;

  /**
   * Balance de categoría de transacción - equivalente a TRAN-CAT-BAL PIC S9(09)V99
    * Usa BigDecimal para mantener precisión decimal y manejar valores negativos
    */
  @Column(name = "CATEGORY_BALANCE", nullable = false, precision = 11, scale = 2)
  private BigDecimal categoryBalance;

  // Relación N:1 con Account - Múltiples balances pueden pertenecer a una account
  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumn(name = "ACCOUNT_ID", insertable = false, updatable = false)
  private Account account;

  // Relación N:1 con TransactionCategory - Múltiples balances pueden ser de la misma categoría
  @ManyToOne(fetch = FetchType.LAZY)
  @JoinColumns({
    @JoinColumn(name = "TYPE_CODE", referencedColumnName = "TRAN_TYPE_CD", insertable = false, updatable = false),
    @JoinColumn(name = "CATEGORY_CODE", referencedColumnName = "TRAN_CAT_CD", insertable = false, updatable = false)
  })
  private TransactionCategory transactionCategory;

  // Nota: FILLER PIC X(22) no se migra ya que es espacio reservado sin uso
}