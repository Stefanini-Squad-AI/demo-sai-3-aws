package com.card.management.Models;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import java.io.Serializable;

/**
 * Clase de clave compuesta para DisclosureGroup
 * Representa la clave primaria compuesta DIS-GROUP-KEY del COBOL
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class DisclosureGroupKey implements Serializable {

  /**
   * ID del grupo de cuenta
   */
  private String accountGroupId;

  /**
   * Código de tipo de transacción
   */
  private String transactionTypeCode;

  /**
   * Código de categoría de transacción
   */
  private Integer transactionCategoryCode;

}
