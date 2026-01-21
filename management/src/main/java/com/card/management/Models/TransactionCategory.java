package com.card.management.Models;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Entidad JPA que representa una categoría de transacción
 * Migrada desde la estructura COBOL TRAN-CAT-RECORD
 * Longitud de registro original: 60 caracteres
 */
@Entity
@Table(name = "TRANSACTION_CATEGORY")
@Data
@NoArgsConstructor
@AllArgsConstructor
@IdClass(TransactionCategoryId.class)
public class TransactionCategory {
  /**
   * Código de tipo de transacción (TRAN-TYPE-CD)
   * Campo alfanumérico de 2 caracteres
   */
  @Id
  @Column(name = "TRAN_TYPE_CD", length = 2, nullable = false)
  private String transactionTypeCode;

  /**
   * Código de categoría de transacción (TRAN-CAT-CD)
   * Campo numérico de 4 dígitos
   */
  @Id
  @Column(name = "TRAN_CAT_CD", length = 4, nullable = false)
  private Integer transactionCategoryCode;

  /**
   * Descripción del tipo de categoría de transacción (TRAN-CAT-TYPE-DESC)
   * Campo alfanumérico de 50 caracteres
   */
  @Column(name = "TRAN_CAT_TYPE_DESC", length = 50)
  private String transactionCategoryDescription;

  // FILLER de 4 caracteres omitido intencionalmente ya que no contiene datos
  // útiles
}
