package com.card.management.Models;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.math.BigDecimal;

/**
 * Entidad JPA que representa un registro de grupo de divulgación
 * Migrado desde la estructura COBOL DIS-GROUP-RECORD
 * Longitud de registro original: 50 bytes
 */
@Entity
@Table(name = "disclosure_group")
@Data
@NoArgsConstructor
@AllArgsConstructor
@IdClass(DisclosureGroupKey.class)
public class DisclosureGroup {
  /**
   * Clave compuesta - ID del grupo de cuenta (10 caracteres)
   * Equivale a DIS-ACCT-GROUP-ID PIC X(10)
   */
  @Id
  @Column(name = "account_group_id", length = 10, nullable = false)
  private String accountGroupId;

  /**
   * Clave compuesta - Código de tipo de transacción (2 caracteres)
   * Equivale a DIS-TRAN-TYPE-CD PIC X(02)
   */
  @Id
  @Column(name = "transaction_type_code", length = 2, nullable = false)
  private String transactionTypeCode;

  /**
   * Clave compuesta - Código de categoría de transacción (4 dígitos numéricos)
   * Equivale a DIS-TRAN-CAT-CD PIC 9(04)
   */
  @Id
  @Column(name = "transaction_category_code", nullable = false)
  private Integer transactionCategoryCode;

  /**
   * Tasa de interés con signo y 2 decimales
   * Equivale a DIS-INT-RATE PIC S9(04)V99
   * Rango: -9999.99 a +9999.99
   */
  @Column(name = "interest_rate", precision = 6, scale = 2)
  private BigDecimal interestRate;

  // FILLER de 28 bytes no se migra ya que representa espacio no utilizado
}
