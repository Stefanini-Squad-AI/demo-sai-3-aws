package com.card.management.Models;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

/**
 * Clase de clave compuesta para TransactionCategoryBalance
 * Representa la clave TRAN-CAT-KEY del COBOL original
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class TransactionCategoryKey implements Serializable {
  /**
   * ID de cuenta
   */
  private Long accountId;

  /**
   * Código de tipo
   */
  private String typeCode;

  /**
   * Código de categoría - Must match the type in TransactionCategoryBalance entity
   */
  private Integer categoryCode;
}
