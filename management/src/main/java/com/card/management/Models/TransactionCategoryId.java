package com.card.management.Models;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.io.Serializable;

/**
 * Clase de clave compuesta para TransactionCategory
 * Representa la clave TRAN-CAT-KEY del COBOL original
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class TransactionCategoryId implements Serializable {
  /**
     * Código de tipo de transacción
     */
    private String transactionTypeCode;

    /**
     * Código de categoría de transacción
     */
    private Integer transactionCategoryCode;
}

